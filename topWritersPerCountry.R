#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
top30countries <- goodreads %>%
  select(birthplace, author_gender) %>%
  count(birthplace, author_gender) %>%
  filter(birthplace !="unknown") %>%
  arrange(-n) %>%
  head(30)

countries<- sort(unique(top30countries$birthplace))

library(shiny)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Top 10 writers"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("countries", "Paese:",choices = c("All",countries))
      ),
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("countries"),
         DT::dataTableOutput("country")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$countries <- renderPlot({
     dataset<-top30countries
     if(input$countries =="All"){
       ggplot(top30countries,aes(reorder(birthplace,-n),n))+
         geom_bar(stat="identity", position="dodge", aes(fill=author_gender))+
         geom_segment(aes(x=birthplace, 
                          xend=birthplace, 
                          y=min(n), 
                          yend=max(n)), 
                      linetype="dashed", 
                      size=0.05,
                      color="grey")+
         coord_flip()+
         theme_classic()
     }else{
       top30countries %>%
         filter(grepl(input$countries,birthplace)) %>%
         ggplot(aes(birthplace,n))+
          geom_bar(stat="identity", position="dodge",aes(fill=author_gender))
     }
   })
   output$country <- DT::renderDataTable({
     if(input$countries == "All"){
       authors %>%
         inner_join(select(goodreads,author_name,first_genre,birthplace)) %>%
         unique()
     }else{
     authors %>%
       inner_join(select(goodreads,author_name,first_genre,birthplace)) %>%
       unique() %>%
       filter(grepl(input$countries,birthplace))
    }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

