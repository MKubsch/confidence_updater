#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("united"),

    # Application title
    titlePanel("Confidence Updater"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput(inputId = "Hypothesis",
                      label = "My hypothesis:",
                      value = ""),
            
            radioButtons("Conf_init",
                        "How sure are you about your hypothesis? Choose the option that best fits what you already know!",
                        c("absolutely certain that it is correct" = 1,
                          "really sure that it is correct" = 0.9,
                          "sure that it is correct" = 0.75,
                          "rather sure that it is correct" = 0.6,
                          "No idea!" = 0.5,
                          "rather sure that it is incorrect" = 0.4,
                          "sure that it is incorrect" = 0.25,
                          "really sure that it is incorrect" = 0.1,
                          "absolutely certain that it is incorrect" = 0),
                        selected = character(0)),
        
            radioButtons("Updating_factor", "How compatible is the evidence with your hypothesis vs. an alternative hypothesis? Choose the best fitting option!" ,
                        c("Data strongly favors my hypothesis" = 20,
                          "Data favors my hypothesis" = 6,
                          "Data somewhat favors my hypothesis" = 3,
                          "Evidence not conclusive" = 1,
                          "Data somewhat favors an alternative hypothesis" = 1/3,
                          "Data favors an alternative hypothesis" = 1/6,
                          "Data strongly favors an alternative hypothesis" = 1/20), selected = character(0)),
            checkboxInput("NumericOutput", "Show numeric confidence level.")
            
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
        d <- ifelse(c > 0.9,"can be very sure that it is correct.", ifelse(c > 0.8,"can be sure that it is correct.",
                    ifelse(c > 0.6,"can be rather sure that it is correct.",
                    ifelse(c > 0.4,"need more evidence and remain undecided about it.",
                           ifelse(c > 0.2, "can be rather sure that it is incorrect",
                                  ifelse(c > 0.1, "can be sure that it is incorrect.", "can be very sure that it is incorrect." ))))))
        if(input$NumericOutput == 1){
            paste("I",d,"(", round(c*100,2),"% confidence)")
        } else{
            paste("I",d)
            } 
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
