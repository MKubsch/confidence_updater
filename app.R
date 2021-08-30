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
ui <- fluidPage(theme = shinytheme("cerulean"),
                
                # Application title
                titlePanel("Confidence Updater"),
                
                tabsetPanel(id = "maintabset",
                            tabPanel("What I know",
                                     
                                     textInput(inputId = "Hypothesis",
                                               label = "My hypothesis:",
                                               value = ""),
                                     
                                     radioButtons("Conf_init",
                                                  "How sure are you about your hypothesis? Choose the option that best fits what you already know!",
                                                  c("absolutely certain that my hypothesis is correct" = 1,
                                                    "very sure that my hypothesis is correct" = 0.9,
                                                    "sure that my hypothesis is correct" = 0.75,
                                                    "rather sure that my hypothesis is correct" = 0.6,
                                                    "no idea!" = 0.5,
                                                    "rather sure that my hypothesis is incorrect" = 0.4,
                                                    "very that my hypothesis is incorrect" = 0.25,
                                                    "really sure that my hypothesis is incorrect" = 0.1,
                                                    "absolutely certain that my hypothesis is incorrect" = 0),
                                                  selected = character(0)),
                                     
                                     radioButtons("Updating_factor", "How compatible is the evidence with your hypothesis relative to an alternative hypothesis? Choose the best fitting option!" ,
                                                  c("Data strongly favors my hypothesis" = 20,
                                                    "Data favors my hypothesis" = 6,
                                                    "Data somewhat favors my hypothesis" = 3,
                                                    "Evidence not conclusive" = 1,
                                                    "Data somewhat favors an alternative hypothesis" = 1/3,
                                                    "Data favors an alternative hypothesis" = 1/6,
                                                    "Data strongly favors an alternative hypothesis" = 1/20), selected = character(0)),
                                     checkboxInput("NumericOutput", "Show numeric confidence level."),
                                     actionButton("run", "Run!")
                            ),
                            tabPanel("Estimated confidence",
                                     
                                     h4("After considering the evidence for my hypothesis"),
                                     em(h4(textOutput("Hypothesis", container = span))),
                                     h4(textOutput("confidence", container = span))
                                     
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
    
    output$Hypothesis <- renderText({
        input$Hypothesis
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
