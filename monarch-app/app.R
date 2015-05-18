## app.R ##

## Libraries

# Shiny and UI (shinydashboard uses Bootstrap 3)
library(shiny)
library(shinydashboard)

# Graphics
library(ggviz)

## Load Data
data <- readRDS('./data/student-referrals.rds')

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
                             message = "The new server is ready.",
                             icon = icon("life-ring"),
                             time = "2014-12-01"
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
                condition = "input.conditionalPanel != true",
                selectInput("conditionalYAxis", "Y-Axis",
                            list("Student ID" = "student_id",
                                 "Location" = "environment",
                                 "Time" = "date_time",
                                 "Student Grade" = "grade"))
        )
        )

body  <- dashboardBody(
        tabBox(
                title = "Referral Count",
                        
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
        ),
        tabBox(
                title = "ggvis",
                column(6,
                       box(
                               title = "",
                               plotOutput("ggviz1")))
        )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
        set.seed(122)
        histdata <- rnorm(500)
        
        output$plot1 <- renderPlot({
               if (input$conditionalPanel == TRUE) {
                       hist(data$student_id)
               } else {
                data2 <- histdata[seq_len(input$slider)]
                hist(data2)}
        })
        
        output$ggviz1 <- renderPlot({
                data %>%
        })
}

shinyApp(ui, server)