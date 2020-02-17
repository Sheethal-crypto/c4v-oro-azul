library(shiny)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(DT)
library(shinyEffects)
library(lubridate)

# format daily values

server <- function(input, output, session) {
  library(bigrquery)
  
  # Library dependencies
  if (!require("bigrquery")) install.packages("bigrquery")
  
  # Authenticate user
  bq_auth(path="https://storage.googleapis.com/angostura-public/hult-hackathon-key.json")
  
  # SQL request and store into dataframe
  project_id <- "event-pipeline"
  sql <- 'SELECT * from `angostura_dev.eh_health_survey_response`'
  df <- query_exec(sql, project_id, use_legacy_sql = FALSE)
  
  library(plyr)
  library(dplyr)
  
  #CONVERTING ALL Categories to numbers
  
  cols <- c("operability_icu",
            "operability_icu_p",
            "operability_er",
            "operability_sx",
            "operability_lab",
            "operability_uls",
            "operability_ct_mri",
            "operability_xr",
            "er_avail_adrenalin",
            "er_avail_atropine",
            "er_avail_dopamine",
            "er_avail_cephalosporins_betalactams",
            "er_avail_aminoglycosides_quinolone",
            "er_avail_vancomycin_clindamycin",
            "er_avail_lidocaine",
            "er_avail_minor_opioids",
            "er_avail_major_opioids",
            "er_avail_iv_fluids",
            "er_avail_diazepam_dph",
            "er_avail_heparin",
            "er_avail_steroids",
            "er_avail_insulin",
            "er_avail_asthma",
            "er_avail_blood_pressure",
            "er_avail_defibrillator",
            "er_avail_ott_intubation",
            "er_avail_catheter",
            "er_avail_oxygen_suction",
            "sx_avail_minor_opioids",
            "sx_avail_major_opioids",
            "sx_avail_anesthetic_gases",
            "sx_avail_anesthetics_iv",
            "sx_avail_relaxants",
            "sx_avail_ott_intubation",
            "sx_avail_patient_lingerie_kit",
            "sx_avail_disposables_mask_gloves_gown",
            "sx_avail_oxygen_suction",
            "power_generator_available",
            "nutr_num",
            "nutr_daily_freq_meal",
            "nutr_quality",
            "nutr_avail",
            "nutr_operability",
            "nutr_freq_milk_formulas",
            "power_outage",
            "wash_failure_icu",
            "wash_failure_er",
            "wahs_failure_sx",
            "power_outage_days_count",
            "power_outage_equipment_failure",
            "nCoV_face_mask_avail",
            "nCoV_respiratory_isolation_protocol_avail",
            "nCoV_isolation_area_avail",
            "power_outage_mortatility"
            
  )
  
  
  rep_key <- c(
    "Funciona todos los dA-as", #3
    "Funciona menos de 3 dA-as", #1
    "Menos de 3 de dA-as", #1
    "Entre 3 y 5 dA-as", #2
    "No existe", #NA
    "No operativo", #0
    "No operativa", #0 
    "Hay pero no funciona",  #0
    "Nunca ha existido", #NA
    "Todos los dA-as", #3
    "Funciona todos los dÃas", #3
    "Todos los dA-as", #3
    "No hubo", #NA
    "Entre 1 y 2 dA-as", #1
    "SA-", #1
    "No", #0
    "3 a 5 dA-as , sin soporte  alterno", #  scale from 0 to 5 starts here #2
    "< 3 dA-as, sin soporte alterno (cisternas)", #1
    "3 a 5 dA-as , sin soporte  alterno", #3
    "3 a 5 dA-as, con soporte alterno", #4
    "Hubo agua todos los dA-as", #5
    "No hubo agua ningAon dia", # NA
    "Menos de 3 dA-as", #1 #back to old scale from 0 to 3
    "Entre 3 y 5 dA-as", #2
    "Todos los dA-as", #3
    "Funciona entre 3 y 5 dA-as", #2
    "Si",#1
    "Option 1" #1
  )
  
  
  rep_value <- c("3", "1", "1", "2", "", "0", "0", "0","","3","3","3","","1","1","0","2","1","3","4","5","","1","2","3","2","1","1")
  
  df_copy <- df[, cols]
  
  # Convert all columns of interest to ascii
  for (i in cols){
    df_copy[, i] <- df_copy[, i] %>% iconv(from="UTF-8", to="ASCII//TRANSLIT")
    df_copy[, i] <- df_copy[, i] %>% mapvalues(rep_key, rep_value, warn_missing = FALSE)
  }
  
  # Transfer columns to orignal
  for (i in cols){
    df[, i] <- df_copy[, i]
  }
  
  
  select_cols<- c("er_avail_dopamine",
                  "er_avail_cephalosporins_betalactams",
                  "er_avail_aminoglycosides_quinolone",
                  "er_avail_vancomycin_clindamycin",
                  "er_avail_lidocaine",
                  "er_avail_minor_opioids",
                  "er_avail_major_opioids",
                  "er_avail_iv_fluids",
                  "er_avail_diazepam_dph",
                  "er_avail_heparin",
                  "er_avail_steroids",
                  "er_avail_insulin",
                  "er_avail_asthma",
                  "er_avail_blood_pressure",
                  "er_avail_defibrillator",
                  "er_avail_ott_intubation",
                  "er_avail_catheter",
                  "er_avail_oxygen_suction",
                  "er_avail_adrenalin",
                  "er_avail_atropine")
  
  
  for (i in select_cols){
    df[,i] <- as.integer(df[,i])
  }
  
  
  #replaced all empty numerical ccells with 0
  df[is.na(df)]<-as.numeric("0")
  
  
  df$weight <- sum(df$er_avail_dopamine+
                     df$er_avail_cephalosporins_betalactams+
                     df$er_avail_aminoglycosides_quinolone+
                     df$er_avail_vancomycin_clindamycin+
                     df$er_avail_lidocaine+
                     df$er_avail_minor_opioids+
                     df$er_avail_major_opioids+
                     df$er_avail_iv_fluids+
                     df$er_avail_diazepam_dph+
                     df$er_avail_heparin+
                     df$er_avail_steroids+
                     df$er_avail_insulin+
                     df$er_avail_asthma+
                     df$er_avail_blood_pressure+
                     df$er_avail_defibrillator+
                     df$er_avail_ott_intubation+
                     df$er_avail_catheter+
                     df$er_avail_oxygen_suction+
                     df$er_avail_adrenalin+
                     df$er_avail_atropine)
  
  df$new_weight <- rowSums(df[,select_cols])
  
  
  
  #Creating Is-Defined Function
  is.defined = function(x)!is.null(x)
  
  #summary(df)
  
  
  #Add Column for Percentage of Normal Beds, ER Beds and Pavilion Beds
  df$op_beds_count <- as.numeric(df$op_beds_count)
  df$op_beds_er_count <- as.numeric(df$op_beds_er_count)
  df$op_pavilions_count <- as.numeric(df$op_pavilions_count)
  
  df$Percentage_FunctionalBedsOverall <-as.numeric((df$op_beds_count + df$op_beds_er_count + df$op_pavilions_count) / df$arch_beds_count)
  
  #Add Relative Bed Score
  df$op_beds_count <- as.numeric(df$op_beds_count) #numeric op bed count
  max_bed <- max(df$op_beds_count, na.rm = TRUE) #find global maxima in op bed count
  df$RelativeBed_Size <-as.numeric(df$op_beds_count / max_bed) #calculate relative hospital weight based on op bed counts
  
  #Missed value count
  df$percentage_missing_values <- rowSums(is.na(df) | df == "" | df == " ")/ncol(df)*100
  
  
  perc_parts_list <- unique(df$hospital_code)
  perc_parts_no_total <- length(perc_parts_list)
  
  perc_parts_no_current <- ""
  perc_parts_no_current <- as.data.frame(perc_parts_no_current)
  
  perc_parts_no_current <-  df %>% filter(report_week == last(df$report_week,order_by = df$timestamp)) %>%
    select(hospital_code) %>%
    unique()
  
  df$Weighted_Metric <-as.numeric(df$new_weight * df$RelativeBed_Size)
  
  View(df)
  
  df <<- df 
  df$timestamp<-as.Date.character(df$timestamp)
  #---------------------Data Clarity-------------------
  #------Missing values-Donut chart
  missing_values <- sum(is.na(df))
  total_obs<- nrow(df)*ncol(df)
  existing_values<- total_obs - missing_values
  # Create test data.
  donut_data <- data.frame(
    category=c("Existing", "Missing"),
    count=c(existing_values, missing_values) )
  
  # Compute percentages
  donut_data<-donut_data %>% mutate(fraction = count / sum(donut_data$count))
  # Compute the cumulative percentages (top of each rectangle)
  donut_data<-donut_data %>% mutate(ymax = cumsum(donut_data$fraction))
  # Compute the bottom of each rectangle
  donut_data$ymin = c(0, head(donut_data$ymax, n=-1))
  # Compute label position
  donut_data$labelPosition <- (donut_data$ymax + donut_data$ymin) / 2
  # Compute a good label
  donut_data$label <- paste0(donut_data$category, "\n values: ", round(donut_data$fraction*100,2), "%")
  # Make the plot
  output$plot1<-renderPlot(ggplot(donut_data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
                             geom_rect() +
                             geom_text(x=2, aes(y=labelPosition, label=label, color=category), size=6) + # x here controls label position (inner / outer)
                             scale_fill_brewer(palette=3) +
                             scale_color_brewer(palette=3) +
                             coord_polar(theta="y") +
                             xlim(c(-1, 4)) +
                             theme_void() +
                             theme(legend.position = "none") )
  
  #------Table of the missing values of each col
  
  miss_df<-data.frame(column_name=colnames(df))
  miss_df$missing_value<-c()
  
  for (i in 1:nrow(miss_df)) {
    miss_df$missing_value[i]<-sum(is.na(df[colnames(df)[i]]))
  }
  
  miss_df$total_obs<-nrow(df)
  missing_table<- miss_df %>% mutate(percentage= round(miss_df$missing_value/total_obs,2)) %>% arrange(desc(percentage))
  missing_table$total_obs<-NULL
  output$table1 <- renderDataTable({datatable(missing_table)})
  
  #-------Valuebox to see how many hospital did not send back survey
  
  response_survey<- df %>% filter(report_week == last(df$report_week,order_by = df$timestamp)) %>% count(hospital_code) %>% count()
  response_rate_week<-response_survey/length(unique(df$hospital_code))
  response_rate<-paste0(round(response_rate_week,2)*100,"%")
  
  
  df$hospital_code
  
  sub_df <- reactive({
    df %>% filter(timestamp > input$date1 | timestamp < input$date2)  })
  
  output$eletricity<-renderPlot({
    
    dl<- df %>% group_by("Hospital") 
    if (Metrics == "Humanitarian"){
      dh<- dl %>% select(report_week,er_staff_residents_and_rural_day_on_call) %>%
        ggplot(aes(report_week, er_staff_residents_and_rural_day_on_call))+geom_col()
      dh
    }
    
    
    
    dh<- dl %>% select(report_week,er_staff_residents_and_rural_day_on_call) %>%
      ggplot(aes(report_week, er_staff_residents_and_rural_day_on_call))+geom_col()
    
    de <- dl%>% select(report_week,epidemiological_emergency_suspected_diseases,nCoV_face_mask_avail) %>%
      ggplot(aes(report_week, epidemiological_emergency_suspected_diseases))+geom_col()
    
    do <- dl %>% select(report_week, weighted_metric, new_weight) %>%
      ggplot(aes(report_week, weighted_metric))+geom_col()
    
    
    di <- dl %>% select(report_week, power_outage, power_generator_available) %>%
      ggplot(aes(report_week, power_outage))+geom_col()
    
    electricity_plot<-sub_df %>% 
      select(power_outage_avg_failures_per_day, timestamp) %>%
      group_by(month=floor_date(timestamp, "month")) %>%
      summarize(avg_failures_per_day_mean=mean(power_outage_avg_failures_per_day)) %>%
      ggplot(aes(y=avg_failures_per_day_mean,x=month))
    +geom_line() 
    
    electricity_plot
    
  })
  
}


ui <- dashboardPage(
  #Header 
  dashboardHeader(
    title = "Dashboard",
    titleWidth = 200
  ),
  #Siderbar
  dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem("Main Dashboard", tabName = "Main Dashboard")

      )
    ),
  #Body  
  dashboardBody(
    tabItems(
      tabItem(tabName = "Main Dashboard",
              sidebarMenu(
                selectInput(
                  inputId = "Hospital",
                  label = "Hospital Codes",
                  choices = c("AMA000","ANZ000","APU000","ARA001","ARA002","BAR000","BOL000","BOL001","CAR000","CAR001","DCA000","DCA001","DCA002","DCA003","DCA004","DCA005","DCA006","DCA007","DEL000","FAL000","GUA000","LAR000","MER000","MIR000","MIR001","MON000","NES000","POR000","SUC000","TAC000","TAC001","VAR000","ZUL000","ZUL001"),
                  selected = "DL",
                  selectize = FALSE
                ),
                selectInput(
                  inputId = "Metrics",
                  label = "Metics",
                  choices = c("Humanitarian","Infrastructure","Operations","Epidemology"),
                  selected = "DL",
                  selectize = FALSE
                ),
                dateInput("date1", label = "Start date:", "2020-01-01"),
                dateInput("date2", label = "End date:", "2020-01-01"),
                selectInput(
                  inputId = "hospital",
                  label = "Choose a hospital:",
                  choices = c("Over all",unique(df$hospital_code)),
                  selected = "DL",
                  selectize = FALSE
                )
              ),
              box(title = "How is the electricity scarcity?", 
                  solidHeader = T, 
                  status = "primary",
                  plotOutput("eletricity")
              ))
                )
        ))

# Run the application 
shinyApp(ui = ui, server = server)
