require(shiny)
runApp(
  list(ui = pageWithSidebar(
    
    headerPanel(withMathJax("$$\\text{Here it works }X_n=X_{n-1}-\\varepsilon$$")),
    
    sidebarPanel(radioButtons("test",  "\\(X_n=X_{n-1}\\)",
                                             choices = c('chromium iii' = '$$e^{(0.8190[ln Hardness]+0.6848)}$$',
                                                         "Rohe Skalierung"         = "raw",
                                                         "Ueber alle Werte"        = "std",
                                                         "Innerhalb der Personen"  = "gstd"))),
    
    
    mainPanel(textOutput('wqseq'))
  ),
  server= function(input, output, session){
    
    output$wqseq <- renderText(input$test)
    
  }
  )
)

