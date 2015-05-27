## app.R ##

## Libraries

# Shiny and UI (shinydashboard uses Bootstrap 3)
library(shiny)
library(shinydashboard)

# Graphics
library(ggvis)
library(ggplot2)

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
plot(tframe[[2]], type = 'h', xlab = "Student ID", ylab = " Number of Offenses")
quantile(eventTable)
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
                     )
        ))

sidebar  <- dashboardSidebar(
        radioButtons("xaxis", label = ("X-Axis"),
                     choices = list("Student ID" = "student_id",
                                    "Location" = "environment",
                                    "Time" = "date_time",
                                    "Student Grade" = "grade")),
        sidebarMenu(
                menuItem("Y-Axis", tabName = "Yaxis", icon = icon("line-chart"))),
        menuSubItem("Y-Axis", tabName = "subAxis", icon = icon("line-chart")),
        radioButtons("yaxis", label = ("Y-Axis"),
                     choices = list("Student ID" = "student_id",
                                    "Location" = "environment",
                                    "Time" = "date_time",
                                    "Student Grade" = "grade")),
        checkboxInput("conditionalPanel", "Count"),
        conditionalPanel(
                condition = "input.conditionalPanel == true",
                selectInput("conditionalYAxis", "Y-Axis",
                            list("Student ID" = "student_id",
                                 "Location" = "environment",
                                 "Time" = "date_time",
                                 "Student Grade" = "grade"))
        )
        )

body  <- dashboardBody(
        # fluid rows have a width of 12 the entire page
        fluidRow(12,
        # tabBox will be used to select the form of visualization
                # tabBox args: 
                ## title (displayed on UI), id (for use by server input$id)
                ## height & width (similar to box arguments), side (which side the tabs are on)
        tabsetPanel("Plot Type",
        # Histogram of Referrals per student
                tabPanel("Count", 
                        column(12,
                               plotOutput("plot1", height = "500px"),
                               radioButtons("referralCount", label = "Arrange By:",
                                            choices = list("Student ID" = "id", "Frequency" = "freq"),
                                            selected = "id"
                                        )
                                )
                        ),
                tabPanel("ggCount", 
                         column(12,
                                ggvisOutput("ggvis1"),
                                radioButtons("ggreferralCount", label = "Arrange By:",
                                             choices = list("Student ID" = "id", "Frequency" = "freq"),
                                             selected = "id"
                                )
                         )
                ),
        # Histogram of Referrals per grade
                tabPanel("Grade",
                         column(12,
#                                  radioButtons("fill", "Select Fill Variable:",
#                                               choices = list("Environment" = "environment")),
                                ggvisOutput("plotGrade")
                                )
                        ),
        # histogram of Referrals per hour AND per month
                tabPanel("Time",
                         column(12,
                                plotOutput("time", height = "500px"),
                                radioButtons("timeType", label = "",
                                             choices = list("Hours of the Day" = "H",
                                                            "Day of the Week" = "A",
                                                            "Month of the Year" = "m")
                                             )
                                )
                        )
#                 ),
#                 tabPanel("ggplot2",  plotOutput("ggvis1")),
#                 )
        )
        )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
        set.seed(122)
        histdata <- rnorm(500)
        
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
        
        
        
        # create a reactive expression wrapper for input$size
        #input_fill <- reactive(input$fill)
        data %>% 
                ggvis(~grade) %>% 
                layer_histograms(width = 1) %>% 
                add_tooltip(function(data){paste0("Grade: ", data$xmin + 0.5)}, "hover") %>%
                bind_shiny("plotGrade")

                # using layer_histograms() {ggvis}

        
        data %>% 
                ggvis(~student_id) %>% 
                layer_histograms(width = 1) %>% 
                add_tooltip(function(data) {paste0("Student ID: ",
                                                   data$xmin + 0.5,
                                                   "<br>",
                                                   "referrals: ", data$stack_upr_)}, "hover") %>% 
                bind_shiny("ggvis1")
        
        ## Creat Time Plot
        output$time <- renderPlot({
                if(input$timeType == "H") {
                        hist(as.numeric((format(data$date_time, format = "%H"))),
                             ylab = "Number of Referrals", xlab = "Hour of the Day",
                             main = "Number of Referrals across time", nclass = 24)
                } else if(input$timeType == "m") {
                        hist(as.numeric((format(data$date_time, format = "%m"))),
                             ylab = "Number of Referrals", xlab = "Month",
                             main = "Number of Referrals across time", nclass = 12)
                } else if(input$timeType == "A") {
                        hist(as.numeric((format(data$date_time, format = "%u"))),
                             ylab = "Number of Referrals", xlab = "Month",
                             main = "Number of Referrals per Day of the Week", nclass = 7)
                }
        })
}

shinyApp(ui, server)