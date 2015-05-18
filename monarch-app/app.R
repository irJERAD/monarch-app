## app.R ##

## Libraries

# Shiny and UI (shinydashboard uses Bootstrap 3)
library(shiny)
library(shinydashboard)

## Load Data
data <- readRDS('/Users/irJERAD/Documents/Data-Apps/monarch-dashboard/monarch-app/data/student-referrals.rds')

header <- dashboardHeader(title = "Monarch Referral App",
                          dropdownMenu(type = "messages",
                                       messageItem(
                                               from = "Welcome!",
                                               message = "To The Monarch Analytics Dashboard"
                                       ),
                                       messageItem(
                                               from = "New Here?",
                                               message = "How about a tutorial to get you started on working with this new platform?",
                                               icon = icon("question"),
                                               time = "13:45"
                                       ),
                                       messageItem(
                                               from = "Support",
                                               message = "The new server is ready.",
                                               icon = icon("life-ring"),
                                               time = "2014-12-01"
                                       )
                          ))

sidebar  <- dashboardSidebar(
        radioButtons("xaxis", label = ("X-Axis"),
                     choices = list("Student ID" = "student_id", "Location" = "environment"))
        )

body  <- dashboardBody()

ui <- dashboardPage(header, sidebar,
        dashboardBody(
                # Boxes need to be put in a row (or column)
                fluidRow(
                        box(
                                title = "Histogram Title", width = 12,
                                plotOutput("plot1", height = 500))
                ),
                fluidRow(
                        box(
                                title = "Change Bar Width",
                                sliderInput("slider", "Number of observations:", 1, 50, 1)
                        )
                )
        )
)

server <- function(input, output) {
        set.seed(122)
        histdata <- rnorm(500)
        
        output$plot1 <- renderPlot({
                data <- histdata[seq_len(input$slider)]
                hist(data)
        })
}

shinyApp(ui, server)