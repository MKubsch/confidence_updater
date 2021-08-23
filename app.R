#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Confidence Updater"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput(inputId = "Hypothesis",
                      label = "My hypothesis:",
                      value = ""),
            
            radioButtons("Conf_init",
                        "How sure are you about your hypothesis? Use the slider to express how confident your are that your hypothesis is correct!",
                        c("rather sure that it is correct" = 0.75,
                          "No idea!" = 0.5,
                          "rather sure that it is incorrect" = 0.25),
                        selected = character(0)),
        
            radioButtons("Updating_factor", "Does the evidence support your initial hypothesis? Choose the option that best fits the evidence!" ,
                        c("strongly supports initial hypothesis" = 2,
                          "Evidence not conclusive" = 1,
                          "strongly rejects initial hypothesis" = 0.5), selected = character(0))
            
        ),
        # Show a plot of the generated distribution
        mainPanel(
            h3(textOutput("Hypothesis0", container = span)),
            em(h3(textOutput("Hypothesis", container = span))),
            h3(textOutput("confidence", container = span))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    Conf_init_input <- reactive({
        input$Conf_init
    })
    
    update_input <- reactive({
    input$Updating_factor
    })
    
    output$Hypothesis0 <- renderText({
       "After considering the evidence for my hypothesis"
        
    })
    output$Hypothesis <- renderText({
        paste("'",input$Hypothesis,"'")
        
    })
    
    output$confidence <- renderText({
        c <- as.numeric(input$Updating_factor)*as.numeric(input$Conf_init) / (as.numeric(input$Updating_factor)*as.numeric(input$Conf_init)+1-as.numeric(input$Conf_init))
        d <- ifelse(c > 0.5,"can be rather sure that it is correct.",ifelse(c == 0.5,"need more evidence and remain undecided about it.","can be rather sure that it is incorrect."))
        paste("I",d)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
