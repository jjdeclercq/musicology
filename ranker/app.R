## Example shiny app with rank list

library(shiny)
library(sortable)
require(tidyverse)
require(reactable)
require(reactablefmtr)
require(googlesheets4)
library(googledrive)

options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)

# now use googledrive with no need for explicit auth
gs4_auth(
  cache = ".secrets",
  email = "jjdeclercq@gmail.com"
)


jpa <- read.csv("jp_albums.csv")
# jp <- read.csv("jp.csv")
jp <- read_sheet("1HrP0_-kRKp0Uxpi_xmcPBpqH0fXviVmw7kZ--znjMk8", sheet = "rank_order")

jpa %<>% left_join(., 
                   jp %>% count(order),
                   by = c("album" = "order")) %>% 
  mutate(n = replace_na(n, 0), p = 1-(n/(1+max(n))))


ui <- fluidPage(
  fluidRow(
    column(
      width = 12,
      tags$h2("Judas Priest, Martha!"),
        type = "tabs",
        tabPanel(
          "Default",
          column(2, uiOutput("sortable")),
          column(10, reactableOutput("jp_table")),
          actionButton("btnSubmit", label = "Submit rankings"),
          actionButton("btnReset", label = "Reset albums")
        )

    )
  )
)

server <- function(input, output, session) {
  init_albums <- sample_n(jpa, 5, weight = p)
  rv <- reactiveValues(order = init_albums$album, 
                       albums = init_albums,
                       rankings = jp)
  

  output$jp_table <- renderReactable({
    reactable(rv$albums)
  })


  observeEvent(input$rank_list_basic, {
    rv$order <- input$rank_list_basic

    rv$albums <- left_join(data.frame(album = rv$order),
                           rv$albums,
                           by = "album")
  })

  output$sortable <- renderUI({
    rank_list_basic <- rank_list(
      text = "Rank albums",
      labels =  rv$albums$album,  
      input_id = "rank_list_basic"
    )
  })
  
  
  observeEvent(input$btnSubmit, {

    rv$rankings <- bind_rows(rv$rankings, data.frame(trial = (max(rv$rankings$trial) + 1), order = rv$albums$album))
    # write.csv(rv$rankings , "jp.csv", row.names = FALSE)

    write_sheet(rv$rankings, "1HrP0_-kRKp0Uxpi_xmcPBpqH0fXviVmw7kZ--znjMk8", sheet = "rank_order")
    # sheet_append(data =rv$rankings, ss = "1HrP0_-kRKp0Uxpi_xmcPBpqH0fXviVmw7kZ--znjMk8", sheet = "rank_order")

     rv$albums <- sample_n(jpa, 5, weight = p)
    
  }, ignoreInit = FALSE)
  
  observeEvent(input$btnReset, {
    
    rv$albums <- sample_n(jpa, 5, weight = p)
    
  }, ignoreInit = FALSE)


}

shinyApp(ui, server)