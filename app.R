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
library(reshape2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Bank Investment Variables"),
   
   fluidRow(
            
            column(4, 
                   sliderInput("initial", 
                   h3("Initial amount"),
                   min = 0, 
                   max = 100000, 
                   value = 1000,
                   step = 500)),
            column(4, 
                   sliderInput("return",
                   h3("Return rate (in %)"),
                   min = 0,
                   max = 20,
                   value = 5,
                   step = 0.1)),
            column(4, 
                   sliderInput("years",
                   h3("Years"),
                   min = 0,
                   max = 50,
                   value = 10,
                   step = 1))
     ),
   fluidRow(
            column(4, 
                   sliderInput("annual", 
                   h3("Annual Contribution"),
                   min = 0, 
                   max = 50000,  
                   value = 2000,
                   step = 500)),
            column(4,
                   sliderInput("growth",
                   h3("Growth rate (in %)"),
                   min = 0,
                   max = 20,
                   value = 2,
                   step = 0.1)),
            column(4, 
                   selectInput("facet", 
                   h3("Facet?"),
                   choices = c("yes", "no")))
     ),
       plotOutput("modalityPlot"),
       verbatimTextOutput("modalityTable"), width = 12
)

# Define server logic required to draw a table and graph
server <- function(input, output) {
  
  modalities <- reactive({
    no_contrib <- c(input$initial, rep(0, input$years))
    fixed_contrib <- c(input$initial, rep(0, input$years))
    growing_contrib <- c(input$initial, rep(0, input$years))
    return <- input$return/100
    growth <- input$growth/100
    
    for (y in 1:input$years) {
      no_contrib[y+1] <- input$initial*(1+return)^(y)
      fixed_contrib[y+1] <- input$initial*(1+return)^(y) + input$annual*(((1+return)^y)-1)/return
      growing_contrib[y+1] <- input$initial*(1+return)^(y) + input$annual*((((1+return)^y)-((1+growth)^y))/(return-growth))
    }
    modalities <- data.frame(
      year = 0:input$years,
      no_contrib = no_contrib,
      fixed_contrib = fixed_contrib,
      growing_contrib = growing_contrib
    )
    return(modalities)
  })
  
  
  
  output$modalityPlot <- renderPlot({
    mod.melt <- melt(modalities(), id.vars = "year")
      if (input$facet == "yes") {
        ggplot(data = mod.melt, aes(x = year, y = value, color = variable)) +
        geom_area(aes(fill = variable), alpha = 0.4) +
        geom_point() +
        labs(x = "Years", y = "Money in Bank", title = "Money made with different saving money methods") +
        theme_minimal() +
        facet_wrap(.~variable)
      }
      else {
        ggplot(data = modalities()) +
        geom_line(aes(x = year, y = no_contrib, color = "No Contribution")) +
          geom_point(aes(x = year, y = no_contrib, color = "No Contribution")) +
        geom_line(aes(x = year, y = fixed_contrib, color = "Fixed Contribution")) +
          geom_point(aes(x = year, y = fixed_contrib, color = "Fixed Contribution")) +
        geom_line(aes(x = year, y = growing_contrib, color = "Growing Contribution")) +
          geom_point(aes(x = year, y = growing_contrib, color = "Growing Contribution")) +
        labs(x = "Years", y = "Money in Bank", title = "Money made with different saving money methods") +
        theme_minimal() +
        scale_color_manual(name = "Modality", values = c("blue", "red", "green"))
      }
  })
  
  output$modalityTable <- renderPrint({
    modalities()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

