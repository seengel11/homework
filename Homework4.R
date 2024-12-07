library(shiny)
library(tidytext)
library(topicmodels)
library(ggplot2)
library(dplyr)
library(tm)

christmas_songs = readLines("https://gist.githubusercontent.com/DeastinY/899d532069febdb969d50eb68b7be583/raw/d4c2b7d6cd58639274fa2f061db6905c58853947/input.txt")


song_data <- tibble(document_id = 1:length(christmas_songs), text = christmas_songs)

song_words <- song_data %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  filter(!str_detect(word, "[0-9]")) 

song_word_counts <- song_words %>%
  count(document_id, word) %>%
  ungroup()

ui <- fluidPage(
  titlePanel("Interactive Topic Model Visualization"),
  
  fluidRow(
    column(12,
           sliderInput("num_topics", "Number of Topics:", 
                       min = 2, max = 10, value = 4, step = 1),  
           actionButton("run_model", "Run Topic Model")       
    )
  ),
  
  fluidRow(
    column(12,
           plotOutput("topicPlot", height = "600px", width = "100%")  
    )
  )
)


server <- function(input, output) {
  
 
  lda_model <- reactive({
    input$run_model 
    

    dtm <- song_word_counts %>%
      cast_dtm(document_id, word, n)
    
   
    LDA(dtm, k = input$num_topics, control = list(seed = 1234))
  })
  

  top_terms <- reactive({
    model <- lda_model()
    

    tidy(model, matrix = "beta") %>%
      group_by(topic) %>%
      top_n(10, beta) %>%
      ungroup() %>%
      arrange(topic, -beta)
  })
  

  output$topicPlot <- renderPlot({
    top_terms() %>%
      mutate(term = reorder_within(term, beta, topic)) %>%
      ggplot(aes(term, beta, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      coord_flip() +
      scale_x_reordered() +
      labs(title = "Top Terms for Each Topic",
           x = "Term", 
           y = "Beta (Importance of Term in Topic)")
  })
}

shinyApp(ui = ui, server = server)

