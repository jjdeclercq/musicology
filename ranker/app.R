## Example shiny app with rank list

library(shiny)
library(sortable)
require(tidyverse)
require(reactable)
require(reactablefmtr)
require(googlesheets4)
library(googledrive)

gen_jp_labels <- function(dat){
  TT <- list()
  for(i in 1:nrow(dat)){
    
    TT[[i]] <- htmltools::div( htmltools::strong(dat$album[i]), 
                               htmltools::br(),  
                               dat$artist[i], 
                               htmltools::br(),
                               paste0("(", dat$Year[i], ")")
                               )
    
    names(TT)[i] <- dat$order[i]
    
    
  }
  return(TT)
}


options(
  gargle_oauth_cache = ".secrets",
  gargle_oauth_email = TRUE
)

# now use googledrive with no need for explicit auth
gs4_auth(
  cache = ".secrets",
  email = "jjdeclercq@gmail.com"
)


jpa <- read.csv("jp_albums.csv") #%>% select(-Order) ## list of albums

jp <- read_sheet("1HrP0_-kRKp0Uxpi_xmcPBpqH0fXviVmw7kZ--znjMk8", sheet = "rank_order")

## add probs
jpa %<>% left_join(., 
                  jp %>% count(order),
                  by = c( "order")) %>% 
  mutate(n = replace_na(n, 0), p = 1-(n/(1+max(n))), p = p^2) %>% 
  mutate(order = gsub("\n", " ", order))




ui <- fluidPage(
  fluidRow(
    column(
      width = 12,
      tags$h2("Judas Priest, Martha!"),
        type = "tabs",
        tabPanel(
          "Default",
          column(4, uiOutput("sortable")),
          column(8, reactableOutput("jp_table")),
          actionButton("btnSubmit", label = "Submit rankings"),
          actionButton("btnReset", label = "Reset albums"),
          verbatimTextOutput("results_basic")
        )

    )
  )
)

server <- function(input, output, session) {
  init_albums <- sample_n(jpa, 5, weight = p)
  
  rv <- reactiveValues(order = init_albums$order, 
                       albums = init_albums,
                       rankings = jp)
  

  output$jp_table <- renderReactable({
    reactable(rv$albums %>% select(-order))
  })


  observeEvent(input$rank_list_basic, {
    rv$order <- input$rank_list_basic

    rv$albums <- left_join(data.frame(order = rv$order),
                           jpa,
                           by = "order")
  })

  output$sortable <- renderUI({
    rank_list_basic <- rank_list(
      text = "Rank albums",
      labels =  gen_jp_labels(rv$albums),
      input_id = "rank_list_basic"
    )
  })
  
  output$results_basic <- renderPrint({
    input$rank_list_basic # This matches the input_id of the rank list
  })
  
  
  observeEvent(input$btnSubmit, {

    new_rank_rows <- rv$albums %>% mutate(date = Sys.Date(), trial = max(rv$rankings$trial) + 1) %>% 
      select(trial,date, album, artist, Year, order)
    
    rv$rankings <- rbind(rv$rankings,new_rank_rows )
    # write.csv(rv$rankings , "jp.csv", row.names = FALSE)

    # write_sheet(rv$rankings, "1HrP0_-kRKp0Uxpi_xmcPBpqH0fXviVmw7kZ--znjMk8", sheet = "rank_order")
    sheet_append(data =new_rank_rows, ss = "1HrP0_-kRKp0Uxpi_xmcPBpqH0fXviVmw7kZ--znjMk8", sheet = "rank_order")

     rv$albums <- sample_n(jpa, 5, weight = p)
    
  }, ignoreInit = FALSE)
  
  observeEvent(input$btnReset, {
    
    rv$albums <- sample_n(jpa, 5, weight = p)
    
  }, ignoreInit = FALSE)


}

shinyApp(ui, server)