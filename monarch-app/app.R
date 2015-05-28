## app.R ##

## Libraries

# Shiny and UI (shinydashboard uses Bootstrap 3)
library(shiny)
library(shinydashboard)

# Graphics
library(ggvis)
library(ggplot2)
library(rCharts)

# data manipulation
library(dplyr)

## Load Data
data <- readRDS('./data/student-referrals.rds')

## Arrange Count by frequency for number of referrals graphic
# Number of events per student table
eventTable <- table(data$student_id)
# create data frame from table
tframe <- as.data.frame(eventTable)
names(tframe) <- c("id", "freq")
## How It was plotted in data science competition
# plot(tframe[[2]], type = 'h', xlab = "Student ID", ylab = " Number of Offenses")

tframe <- arrange(tframe, desc(freq))

header <- dashboardHeader(
        title = "Monarch Referral App",
        dropdownMenu(type = "messages",
                     messageItem(
                             from = "Welcome!",
                             message = "To The Monarch Analytics Dashboard"
                     ),
                     messageItem(
                             from = "New Here?",
                             message = "How about a tutorial to get started?",
                             icon = icon("question"),
                             time = "13:45"
                     ),
                     messageItem(
                             from = "Support",
                             message = "Contact the Developer.",
                             icon = icon("life-ring")
                     ),
                     messageItem(
                             from = "Example Data Input",
                             message = c("Click here to see a potential data input form",a(href = "https://40.media.tumblr.com/3d01ef923a34a3546133a12833e5aae2/tumblr_np0zi0MOM31uwjn2uo1_540.png")),
                             icon = icon("edit")
                     )
        ))

sidebar  <- dashboardSidebar(
                menuItem("X-Axis", icon = icon("line-chart")),
        selectInput("yVariable", "Select Variable to Count:",
                    c("Student ID" = "student_id",
                      "Grade" = "grade"),selected = "student_id"),
        uiOutput("ggvisPlot_ui"),
        radioButtons("xaxis", label = ("X-Axis"),
                     choices = list("Student ID" = "student_id",
                                    "Location" = "environment",
                                    "Time" = "date_time",
                                    "Student Grade" = "grade"),
                     selected = "student_id"),
        menuItem(text = "Y-Axis", tabName = "Time", icon = icon("line-chart")),
        radioButtons("yaxis", label = ("Y-Axis"),
                     choices = list("Student ID" = "student_id",
                                    "Location" = "environment",
                                    "Time" = "date_time",
                                    "Student Grade" = "grade")),
        checkboxInput("conditionalPanel", "Count"),
        conditionalPanel(
                condition = "input.conditionalPanel != true",
                selectInput("conditionalYAxis", "Y-Axis",
                            list("Teacher ID" = "documenting_staff_id",
                                 "Location" = "environment",
                                 "Time" = "date_time",
                                 "Student Grade" = "grade"))
        )
)

body  <- dashboardBody(
        # fluid rows have a width of 12 the entire page
        fluidRow("Visualization",
                 # tabBox will be used to select the form of visualization
                 # tabBox args: 
                 ## title (displayed on UI), id (for use by server input$id)
                 ## height & width (similar to box arguments), side (which side the tabs are on)
                 tabsetPanel("Plot Type",
                             tabPanel("Interactive Count",
                                      column(12,
                                             ggvisOutput("ggvisPlot"),
                                             conditionalPanel(
                                                     condition = "input.yVariable == student_id",
                                                     radioButtons(inputId = "arrage",
                                                                  label = "Arrage By:",
                                                                  choices = list("Student ID" = "id",
                                                                                 "Frequency" = "freq"),
                                                                  selected = "id")
                                             ),
                                             tags$img(src = "https://40.media.tumblr.com/3d01ef923a34a3546133a12833e5aae2/tumblr_np0zi0MOM31uwjn2uo1_540.png", width = "550px", height = "600px")
                                      )
                             ),
                             # Histogram of Referrals per student
                             tabPanel("Arrange Count", 
                                      column(12,
                                             plotOutput("plot1", height = "500px"),
                                             radioButtons("referralCount", label = "Arrange By:",
                                                          choices = list("Student ID" = "id", "Frequency" = "freq"),
                                                          selected = "id"
                                             )
                                      )
                             ),
                             # histogram of Referrals per hour AND per month
                             tabPanel("Time",
                                      column(12,
                                             plotOutput("time", height = "500px"),
                                             radioButtons("timeType", label = "",
                                                          choices = list("Hours of the Day" = "H",
                                                                         "Day of the Week" = "A",
                                                                         "Month of the Year" = "m"),
                                             ),
                                             ggvisOutput("gtime")
                                      )
                             ),
                             tabPanel("Reporting Staff",
                                      column(12,
                                             ggvisOutput("staff")
                                      )
                             ),
                             tabPanel("Environment",
                                      column(12,
                                             ggvisOutput("env")
                                             )
                                      ),
                             tabPanel("Interactive Example",
                                      column(12,
                                             showOutput("myChart", "polycharts"),
                                             selectInput(inputId = "x",
                                                         label = "Choose X",
                                                         choices = c('SepalLength', 'SepalWidth', 'PetalLength', 'PetalWidth'),
                                                         selected = "SepalLength"),
                                             selectInput(inputId = "y",
                                                         label = "Choose Y",
                                                         choices = c('SepalLength', 'SepalWidth', 'PetalLength', 'PetalWidth'),
                                                         selected = "SepalWidth")
                                             )
                                      )
                 )
        )
)

ui <- dashboardPage(header, sidebar, body, skin = "yellow")

server <- function(input, output) {
        
        # create ggFinal
        observe(intput_x <- input$xaxis)
        
        # SAMPLE
        plotData <- reactive({
                df <- data[,c("id",input$yVariable)]
                names(df) <- c("id","x")
                df
        })
        # define axis for labeling
        label <- c("Student ID: ")
        
        ## Create Interactive Count Plot
        #TODO: still trying to change labels for dynamic "hover" effect
        label <- reactive({
                if(input$yVariable == "student_id") return("Student ID: ")
                else if(input$yVariable == "grade") return("Grade: ")
        })
        # Generate Histogram of count
        reactive({
                if(input$yVariable == "student_id"){
                        # student_id Histogram
                        plotData() %>%  ggvis(x=~x) %>%
                                layer_histograms(width = 1, fill := "green") %>%
                                #arrange(y) %>%
                                add_axis("x", title = "Student ID") %>%
                                add_tooltip(function(df){paste0("Student ID: ",
                                                                df$xmin+0.5,
                                                                "<br>",
                                                                "Referrals: ",
                                                                df$stack_upr_)}, "hover")
                } else if(input$yVariable == "grade"){
                        # grade Histogram
                        plotData() %>%  ggvis(x=~x) %>%
                                layer_histograms(width = 1, fill := "green") %>%
                                #arrange(y) %>%
                                add_axis("x", title = "Grade Level") %>%
                                add_tooltip(function(df){paste0("Grade: ",
                                                                df$xmin+0.5,
                                                                "<br>",
                                                                "Referrals: ",
                                                                df$stack_upr_)}, "hover")}  
        }) %>%  bind_shiny("ggvisPlot", "ggvisPlot_ui")
        

        output$plot1 <- renderPlot({
                # create histogram of misbehavior counts by student_id
               if (input$referralCount == "id") {
                       # using hist {stat}
                       hist(data$student_id, xlab = "Student ID",
                            ylab = "Number of Referrals",
                            main = "Referrals per Student",
                            nclass = 275)
               } else if(input$referralCount == "freq") {
                       plot(tframe[[2]], type = 'h', xaxt='n',
                            xlab = "Student ID", ylab = " Number of Offenses",
                            main = "<-- Most Offenses to Least Offenses -->")
               }
        })

        ## Creat Time Plot
        output$time <- renderPlot({
                if(input$timeType == "H") {
                        hist(as.numeric((format(data$date_time, format = "%H"))),
                             ylab = "Number of Referrals", xlab = "Hour of the Day",
                             main = "Number of Referrals across time", nclass = 24)
                } else if(input$timeType == "m") {
                        hist(as.numeric((format(data$date_time, format = "%m"))),
                             ylab = "Number of Referrals", xlab = "Month of the year",
                             main = "Number of Referrals across time", nclass = 12)
                } else if(input$timeType == "A") {
                        hist(as.numeric((format(data$date_time, format = "%u"))),
                             ylab = "Number of Referrals", xlab = "Day of the Week",
                             main = "Number of Referrals per Day of the Week", nclass = 7)
                }
        })
        
        ## gtime Plot TODO
        data$time <- as.Date(data$date_time, format = "%H")
        p <- data %>%
                ggvis(~time, ~grade) %>%
                layer_points(fill = ~environment)
        p %>% scale_datetime("x", nice = "minute", clamp = TRUE) %>% bind_shiny("gtime")
        
        ## Create Reporting Staff Graphics
        # Histogram with repoting staff
        data %>%
                ggvis(~reporting_staff_id) %>%
                layer_histograms(width = 1, fill := "green") %>%
                add_axis("x", title = "Reporting Staff ID") %>%
                add_tooltip(function(df){paste0("Reporting Staff ID: ", df$xmin + 0.5)}, "hover") %>%
                bind_shiny("staff")
        
        ## Creat Environment Plot
        
        p <- data %>%
                ggvis(~student_id, ~reporting_staff_id) %>%
                layer_points(fill = ~environment, shape = ~environment) %>%
                add_tooltip(function(df){paste0("Student ID: ",
                                                df$student_id,
                                                "<br>",
                                                "Grade: ",
                                                df$grade,
                                                "<br>",
                                                "Reporting Staff: ",
                                                df$reporting_staff_id,
                                                "<br>",
                                                "Location: ",
                                                df$environment
                                                )}, "hover") %>%
                bind_shiny("env")        
        
        ## A Sample of Interactive Output Using rCharts
        output$myChart <- renderChart({
                names(iris) = gsub("\\.", "", names(iris))
                p1 <- rPlot(input$x, input$y, data = iris, color = "Species", 
                            facet = "Species", type = 'point')
                p1$addParams(dom = 'myChart')
                return(p1)
        })
}

shinyApp(ui, server)