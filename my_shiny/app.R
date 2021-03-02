#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(Stat2Data)
data(ArcheryData)

ArcheryData$Sex = factor(ifelse(ArcheryData$Sex == "m", "Male" , "Female"))
list_choices <-  unique(ArcheryData$Sex)

# Define UI for application that draws a histogram
ui <- navbarPage("Archery Data App",
                 tabPanel("Scatterplot",
                          fluidPage(
                              
                              # Application title
                              sidebarLayout(
                                  sidebarPanel(
                                      selectInput("select", label = h3("Plot participants according to Sex"), 
                                                  choices = character(0),
                                                  selected = 1)
                                  ),
                                  mainPanel(
                                      plotOutput(outputId = "plot")
                                  )
                              ))),
                 tabPanel("References",
                          includeMarkdown("Untitled.Rmd")
                 ))


col_scale <- scale_colour_discrete(limits = unique(ArcheryData$Sex))

# Define server logic required to draw a scatterplot
server <- function(input, output, session) {
    updateSelectInput(session, "select",
                      choices = list_choices,
                      selected = tail(list_choices, 1)
    )
    output$plot <- renderPlot({
        ggplot(ArcheryData  %>% filter(Sex == input$select), aes(Attendance, Improvement, colour = Sex)) +
            scale_x_log10() +
            col_scale +
            geom_point()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
