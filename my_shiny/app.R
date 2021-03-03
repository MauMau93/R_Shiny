library(shiny)
library(ggplot2)
library(dplyr)
library(Stat2Data)
data(ArcheryData)

ArcheryData$Sex = factor(ifelse(ArcheryData$Sex == "m", "Male" , "Female"))
list_choices <-  unique(ArcheryData$Sex)
list_choices2 <- colnames(ArcheryData[-3][-6])
list_choices3 <- colnames(ArcheryData[-3][-6])


# Define UI for application that draws a histogram
ui <- navbarPage("Archery Data App",
                 tabPanel("Scatterplot",
                          fluidPage(
                              
                              # Application title
                              sidebarLayout(
                                  sidebarPanel(
                                      selectInput("select", label = h3("Plot participants according to Sex"), 
                                                  choices = character(0),
                                                  selected = 1),
                                      selectInput("select2", label = h3("Choose Variable for x-axis"), 
                                                  choices = character(0),
                                                  selected = 1),
                                      selectInput("select3", label = h3("Choose Variable for y-axis"), 
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
    updateSelectInput(session, "select2",
                      choices = list_choices2,
                      selected = tail(list_choices2, 1)
    )
    updateSelectInput(session, "select3",
                      choices = list_choices3,
                      selected = tail(list_choices3, 1)
    )
    output$plot <- renderPlot({
        ejex <- as.numeric(input$select2)
        ejey <- as.numeric(input$select3)
        ggplot(ArcheryData  %>% filter(Sex == input$select), aes(x = ejex ,y= ejey, colour = Sex)) +
            col_scale +
            geom_point()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
