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
                                               width = "400px"),
                                     
                                     # Input for prior
                                     sliderInput("Conf_init_num",
                                                  "How sure are you that your hypothesis is true? Use the slider to select a percentage value that best fits with what you already know!",
                                                  min = 0, max = 100, value = 50, ticks = F, pre = "%", width = "400px"),
                                     
                    
                                     # input for bayes factor 
                                     radioButtons("bayes_factor", "How compatible is the evidence with your hypothesis relative to an alternative hypothesis? Choose the best fitting option!" ,
                                                  c("the evidence strongly favors my hypothesis" = 20,
                                                    "the evidence favors my hypothesis" = 6,
                                                    "the evidence somewhat favors my hypothesis" = 3,
                                                    "the evidence not conclusive" = 1,
                                                    "the evidence somewhat favors an alternative hypothesis" = 1/3,
                                                    "the evidence favors an alternative hypothesis" = 1/6,
                                                    "the evidence strongly favors an alternative hypothesis" = 1/20), 
                                                  selected = character(0),
                                                  width = "400px"),
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
                A publication related to this app is available here https://link.springer.com/article/10.1007/s11191-022-00341-3.
                It was inspired in part by Warren's (2018) work (https://doi.org/10.1119/1.5012750).
                  If you have any questions, you may contact Marcus Kubsch (kubsch@leibniz-ipn.de). 
                  The code for this app may be found at https://github.com/MKubsch/confidence_updater
                This work is licensed under a Creative Commons Attribution-ShareAlike 4.0 International License (http://creativecommons.org/licenses/by-sa/4.0/).")
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
            prior <- input$Conf_init_num/100 #prior
            bf <- as.numeric(input$bayes_factor) #bayes factor
            posterior <- bf*prior / (bf*prior+1-prior) # calculate posterior
            # select verbal confidence level statement bases on posterior
            posterior_conf_level <- ifelse(posterior >= 0.99,"can be nearly certain that it is correct.", ifelse(posterior >= 0.9,"can be very sure that it is correct",
                                                                                      ifelse(posterior >= 0.66,"can be rather sure that it is correct",
                                                                                             ifelse(posterior >= 0.33,"need more evidence and remain undecided about it",
                                                                                                    ifelse(posterior > 0.1, "can be rather sure that it is incorrect",
                                                                                                           ifelse(posterior >= 0.01, "can be very sure that it is incorrect", 
                                                                                                                  "can be nearly certain that it is incorrect" ))))))
            if(input$NumericOutput == 1){
                paste0("I ",posterior_conf_level ," (", round(posterior*100,2),"% confidence)")
            } else{
                paste0("I ",posterior_conf_level )
            } 
            
        })
    }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)