library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),
                
                # Application title
                titlePanel("Confidence Updater"),
                
                tabsetPanel(id = "maintabset",
                            tabPanel("What I know",
                                     value = "inputs",
                                     
                                     p(),
                                     
                                     textInput(inputId = "Hypothesis",
                                               label = "What is your hypothesis?",
                                               value = "",
                                               width = "100%"),
                                     
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
                                                  selected = character(0),
                                                  width = "100%"),
                                     
                                     radioButtons("Updating_factor", "How compatible is the evidence with your hypothesis relative to an alternative hypothesis? Choose the best fitting option!" ,
                                                  c("the evidence strongly favors my hypothesis" = 20,
                                                    "the evidence favors my hypothesis" = 6,
                                                    "the evidence somewhat favors my hypothesis" = 3,
                                                    "the evidence not conclusive" = 1,
                                                    "the evidence somewhat favors an alternative hypothesis" = 1/3,
                                                    "the evidence favors an alternative hypothesis" = 1/6,
                                                    "the evidence strongly favors an alternative hypothesis" = 1/20), 
                                                  selected = character(0),
                                                  width = "100%"),
                                     hr(),
                                     checkboxInput("NumericOutput", "Show numeric confidence level."),
                                     actionButton("button", "Run!")
                            ),
                            tabPanel("Estimated confidence",
                                     value = "estimate",
                                     p(),
                                     tags$b("After considering the evidence for my hypothesis:"),
                                     p(),
                                     em(textOutput("Hypothesis", container = span)),
                                     p(),
                                     h3(textOutput("confidence", container = span))
                                     
                            )
                ),
                p(),
                hr(),
                p("This Shiny app uses a form of Bayes Theorem (https://en.wikipedia.org/wiki/Bayes%27_theorem) to update hypotheses in light of evidence in a principled yet flexible way. 
                It was created by Marcus Kubsch with contributions from Joshua Rosenberg, E.J. Wagenmakers, and Mine Dogucu. 
                A pre-print related to this app is available at https://osf.io/aznyq/.
                It was inspired in part by Warren's (2018) work (https://doi.org/10.1119/1.5012750).
                  If you have any questions, you may contact Marcus Kubsch (kubsch@leibniz-ipn.de). 
                  The code for this app may be found at https://github.com/MKubsch/confidence_updater")
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observeEvent(input$button, {
        updateTabsetPanel(session, 
                          "maintabset",
                          selected = "estimate")
    })
    
    output$Hypothesis <- renderText({
        paste(input$Hypothesis)
        
    })
    
    observeEvent(input$button, {
        
        output$confidence <- renderText({
            c <- as.numeric(input$Updating_factor)*as.numeric(input$Conf_init) / (as.numeric(input$Updating_factor)*as.numeric(input$Conf_init)+1-as.numeric(input$Conf_init))
            d <- ifelse(c > 0.9,"can be very sure that it is correct.", ifelse(c > 0.8,"can be sure that it is correct",
                                                                               ifelse(c > 0.6,"can be rather sure that it is correct",
                                                                                      ifelse(c > 0.4,"need more evidence and remain undecided about it",
                                                                                             ifelse(c > 0.2, "can be rather sure that it is incorrect",
                                                                                                    ifelse(c > 0.1, "can be sure that it is incorrect", 
                                                                                                           "can be very sure that it is incorrect" ))))))
            if(input$NumericOutput == 1){
                paste0("I ",d," (", round(c*100,2),"% confidence)")
            } else{
                paste0("I ",d)
            } 
            
        })
    }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)