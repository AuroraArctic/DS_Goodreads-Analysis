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
   titlePanel("Top 10 writers"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("genre", "Genere:",choices = c("All",
                                                    "Classics" ="classics",
                                                    "Contemporary" = "contemporary",
                                                    "Fantasy" = "fantasy",
                                                    "Fiction" ="fiction",
                                                    "Yound Adult" = "young adult",
                                                    "Science Fiction" = "science fiction",
                                                    "Romance" = "romance",
                                                    "Mystery" = "mystery",
                                                    "Historical" = "historical",
                                                    "Nonfiction" = "nonfiction"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("authors")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$authors <- renderPlot({
     dataset<- goodreads %>%
         select(author_name,first_genre,other_genre, author_rating_count,author_review_count) %>%
         mutate(author_name = stringr::str_replace(author_name,"\n","")) %>%
         group_by(author_name) %>%
         summarise(first_genre = first(first_genre),
                   other_genre = first(other_genre),
                   ratings = sum(author_rating_count),
                   reviews = sum(author_review_count)) %>%
         mutate(first_genre = stringr::str_replace_all(first_genre,"-"," "),
                other_genre = stringr::str_replace_all(other_genre,"-"," ")) %>%
         arrange(-reviews)
     
     if(input$genre =="All"){
       most_rated_authors <- dataset %>%
         arrange(-ratings) %>%
         head(n=10) %>%
         gather("ratings","reviews", key="type",value="number")
       
       ggplot(most_rated_authors, aes(x=reorder(author_name,-number)))+
         geom_bar(stat="identity", position="dodge",aes(y=number/1000000,fill=type)) +
         labs(x = "Nome dell'autore", y="Numero di rating", title="Numero di valutazioni e recensioni (in milioni)")+
         geom_text(aes(label=author_name), stat="count", size=5,angle=90, vjust=-0.5, hjust=-0.2)+
         theme(axis.text.x=element_blank())
     }else{
       most_rated_authors <- dataset %>%
         filter(grepl( input$genre,first_genre) | grepl(input$genre, other_genre)) %>%
         arrange(-ratings) %>%
         head(n=10) %>%
         gather("ratings","reviews", key="type",value="number")
       
       if(input$genre == "contemporary"){
       ggplot(most_rated_authors, aes(x=reorder(author_name,-number)))+
         geom_bar(stat="identity", position="dodge",aes(y=number/1000000,fill=type)) +
         labs(x = "Nome dell'autore", y="Numero di rating", title="Numero di valutazioni e recensioni (in milioni)")+
         geom_text(aes(label=author_name), stat="count", size=5,angle=90, vjust =-0.2,hjust=2)+
         theme(axis.text.x=element_blank()) 
       }else{
         ggplot(most_rated_authors, aes(x=reorder(author_name,-number)))+
           geom_bar(stat="identity", position="dodge",aes(y=number/1000000,fill=type)) +
           labs(x = "Nome dell'autore", y="Numero di rating", title="Numero di valutazioni e recensioni (in milioni)")+
           geom_text(aes(label=author_name), stat="count", size=5,angle=90, vjust=-0.5, hjust=-0.1)+
           theme(axis.text.x=element_blank()) 
       }
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

