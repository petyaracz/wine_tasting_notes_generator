# --- libs --- #

library(shiny) 
library(tidyverse)
library(ngram)

# --- source --- #

texts = read_tsv('wine_ngram_texts.tsv')

# This is based on a kaggle dataset.
# Only varieties with "enough" reviews were kept.
# Source:
# https://www.kaggle.com/zynicide/wine-reviews

# the possible varieties
variety_choices = texts %>%
  ungroup %>% 
  distinct(variety) %>% 
  pull(variety) %>% 
  sort()

# get category (good or bad wine) and variety (see above), return review
writeNotes = function(my_category, my_variety){
  
  my_text = texts %>% 
    filter(category == my_category, variety == my_variety) %>%
    pull(text)
  
  # build 4grams from texts
  my_ngram = ngram(my_text, n = 4, sep = ' ')
  
  # throw a die
  my_seed = round(runif(1, 1, 1000), 0)
  
  # make 4gram babble
  my_notes = babble(my_ngram, genlen = 120, seed = my_seed) %>% 
    str_replace('[^\\.]*. ', '') %>% # drop first sentence
    str_replace('[\\.]([^\\.]*)$', '.') %>% # drop last sentence
    str_replace_all(' ,', ',') %>% # put back comma
    str_replace_all(' \\.', '.') # put back dot
  
  return(my_notes)
}

# the ui consists of a title, a side panel with two drop-downs and a main panel that displays text
ui <- fluidPage(
  titlePanel("Wine tasting notes generator"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Choose a variety:"),
      
      selectInput("variety", 
                  label = "Choose a variety",
                  choices = variety_choices,
                  selected = "Chardonnay"),
      
      helpText("Is this wine good or bad:"),
      
      selectInput("category", 
                  label = "Is this wine good or bad?",
                  choices = c('good', 'bad'),
                  selected = "bad")
    ),
    
    mainPanel(
      strong(textOutput("choices")),
      p(textOutput('notes')),
      em('Credit: https://www.kaggle.com/zynicide/wine-reviews')
    )
  )
)

server <- function(input, output) {
  
  output$choices <- renderText({
    
    paste0(
      "Your picked a ", 
      input$category, 
      ' ', 
      input$variety, 
      '. Here are your notes:'
      )
    })
  
  output$notes <- renderText({
    
    writeNotes(input$category,input$variety)
    
  })
  
}

shinyApp(ui, server)
