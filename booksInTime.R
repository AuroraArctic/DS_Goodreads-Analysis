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
   titlePanel("Evoluzione dei libri nel corso del tempo"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        sliderInput("range", "Periodo storico:",
                    min = -720, max = 2019,
                    value = c(1000,2019))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("books")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$books <- renderPlot({
     goodreads %>%
       filter(year>=input$range[1] & year<=input$range[2])%>%
       group_by(year) %>%
       summarise(n_books = n()) %>%
       ggplot(aes(year,n_books))+
         geom_line()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

