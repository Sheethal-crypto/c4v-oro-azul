library(shiny)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(DT)
library(shinyEffects)
library(lubridate)

# format daily values

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
        menuItem("Infrastructure", tabName = "infrastructure"),
        menuItem("Humanilarian", tabName ="human"),
        menuItem("Epidenmology", tabName ="epidenmology"),
        menuItem("Operation", tabName ="operation")
      )
    ),
  #Body  
  dashboardBody(
    tabItems(
      tabItem(tabName = "infrastructure",
              sidebarMenu(
                selectInput(
                  inputId = "state",
                  label = "Choose a state:",
                  choices = c("AMA","ANZ","APU","ARA","BAR","BOL","CAR","DAC","DEL","FAL","GUA","LAR","MER","MIR","MON","NES","POR","SUC","TAC","VAR","AUL"),
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
                  plotOutput("eletricity")),
              ),
      
      tabItem(tabName ="data",
                fluidRow(
                box(title = "How many % of missing values in the dataset?", 
                    solidHeader = T, 
                    status = "primary",
                    plotOutput("plot1")),
                 box(title = "Number of missing values for each variable",
                    solidHeader = T, 
                    status = "primary",
                    width =250,
                    height=600,
                    column(width = 12,
                           DT::dataTableOutput("table1"),
                           style = "height:500px; overflow-y: scroll;overflow-x: scroll;")))
                ),
      tabItem(tabName ="human"),
      tabItem(tabName ="epidenmology"),
      tabItem(tabName ="operation")
        
        ))
)



    
    

  


server <- function(input, output, session) {
  library(bigrquery)
  
  # Authenticate user
  bq_auth(path="https://storage.googleapis.com/angostura-public/hult-hackathon-key.json")
  
  # SQL request and store into dataframe
  project_id <- "event-pipeline"
  sql <- 'SELECT * from `angostura_dev.eh_health_survey_response`'
  df <- query_exec(sql, project_id, use_legacy_sql = FALSE)
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
 
  
  
  #-------
  
  sub_df <- df %>% filter(timestamp > '2019-01-01' | timestamp < '2020-02-01')  

  output$eletricity<-renderPlot({
    
    electricity_plot<-sub_df %>% 
      #select(power_outage_avg_failures_per_day, timestamp) %>%
      group_by(month=floor_date(timestamp, "month")) %>%
      summarize(avg_failures_per_day_mean=mean(power_outage_avg_failures_per_day)) %>%
      ggplot(aes(y=avg_failures_per_day_mean,x=month))+geom_line() 
    
    electricity_plot
    
    })
  
}
# Run the application 
shinyApp(ui = ui, server = server)
