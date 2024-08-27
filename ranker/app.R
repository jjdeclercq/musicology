

library(shiny)
library(sortable)
require(tidyverse)
require(reactable)
require(reactablefmtr)
require(googlesheets4)
library(googledrive)
require(magrittr)
require(sjlabelled)
require(gargle)

devtools::source_url("https://raw.githubusercontent.com/jjdeclercq/VUMisC/main/JDmisc.R")


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


jp <- read_sheet("1HrP0_-kRKp0Uxpi_xmcPBpqH0fXviVmw7kZ--znjMk8", sheet = "rank_order")

jp_matches <- jp %>% group_by(trial) %>% mutate(n = n()-1) %>% group_by(order) %>% summarise(matches = sum(n)) 


# jpa <- read.csv("jp_albums.csv") #%>% select(-Order) ## list of albums
jpa <- read_sheet("1F1bi8Y12cnmyCnNB3HWpVdj9oi918ohHi7qBITB73Ks", sheet = "All") %>% 
  filter(ranker ==1) %>% 
  select(artist, album, Year, a_rank, plays = n, last_played)%>%
  mutate(order = paste(album, artist, Year, sep = " "))%>% 
  left_join(., 
            jp_matches,
            by = "order") %>% 
  mutate(matches = replace_na(matches, 0), 
         p = 1-(matches/(1+max(matches))), p = ifelse(matches <= 60, p*50, p), #p = ifelse(p < 1, 1, p),
         p_save = p)%>%
  mutate(p = round(p, 3), last_played = ymd(last_played))




ui <- navbarPage(
      "Judas Priest, Martha!",
        tabPanel(
          "Rank",
          column(4, 
                 uiOutput("sortable"),
                 actionButton("btnSubmit", label = "Submit rankings"),
                 actionButton("btnReset", label = "Reset albums"),
                 selectizeInput("jp_album", "Album", jpa$album, selected = NULL, multiple = T, options = NULL),
                 selectizeInput("jp_artist", "Artist", jpa$artist, selected = NULL, multiple = T, options = NULL),
                 selectizeInput("jp_year", "Year", unique(jpa$Year), selected = NULL, multiple = T, options = NULL),
                 sliderInput("jp_slider", "Current ranking", min = 1, max = max(jpa$a_rank, na.rm = TRUE), 
                             value = c(1, max(jpa$a_rank, na.rm = TRUE)), ticks = TRUE)),
                 
          column(8, reactableOutput("jp_table")),
          verbatimTextOutput("results_basic")
        ),
      tabPanel(
        "JPA",
        column(9,
               reactableOutput("jpa_table")
               )
      )

  #   )
  # )
)

server <- function(input, output, session) {
  init_albums <- sample_n(jpa, 5, weight = p)
  
  rv <- reactiveValues(order = init_albums$order, 
                       albums = init_albums,
                       rankings = jp,
                       jpa = jpa,
                       new_rank_rows = data.frame())
  

  output$jp_table <- renderReactable({
    j.reactable(rv$albums %>% select(-order, -p_save, -a_rank))
  })
  
  output$jpa_table <- renderReactable({
    rv$jpa %>% select(-order, -p_save)  %>% 
    j.reactable(., defaultPageSize = 50, filterable = TRUE)
  })


  observeEvent(input$rank_list_basic, {
    rv$order <- input$rank_list_basic

    rv$albums <- left_join(data.frame(order = rv$order),
                           rv$jpa,
                           by = "order")
    
    rv$new_rank_rows <- rv$albums %>% mutate(date = Sys.Date(), trial = max(rv$rankings$trial) + 1) %>% 
      select(trial,date, album, artist, Year, order)
    
  })

  output$sortable <- renderUI({
    rank_list_basic <- rank_list(
      text = "choose.",
      labels =  gen_jp_labels(rv$albums),
      input_id = "rank_list_basic"
    )
  })
  
  output$results_basic <- renderPrint({
    input$rank_list_basic # This matches the input_id of the rank list
    c(max(rv$new_rank_rows$trial), max(rv$rankings$trial))

  })
  observeEvent(
    c(input$jp_album, input$jp_artist, input$jp_year),

    if(!is.null(input$jp_album)){
    rv$jpa %<>% mutate(p= p_save, p = ifelse(album %in% input$jp_album & p < 10, 500*p, p))
    }

    else if(!is.null(input$jp_artist)){
      rv$jpa %<>% mutate(p= p_save, p = ifelse(artist %in% input$jp_artist & p < 10, 500*p, p))
    }
    else if(!is.null(input$jp_year)){
      rv$jpa %<>% mutate(p= p_save, p = ifelse(Year %in% input$jp_year & p < 10, 500*p, p))
    }

  )

  observeEvent(
    c(input$jp_slider),
    {
      
      rv$jpa %<>% mutate(p= p_save, 
                         p = ifelse(a_rank >= input$jp_slider[1] & a_rank <= input$jp_slider[2] |is.na(a_rank),
                                p*5*(nrow(rv$jpa )/(input$jp_slider[2] - input$jp_slider[1]-1)), p) )
    }
    
  )
  
  
  observeEvent(input$btnSubmit, {
    
    rv$rankings <- rbind(rv$rankings,rv$new_rank_rows  )

    sheet_append(data =rv$new_rank_rows , ss = "1HrP0_-kRKp0Uxpi_xmcPBpqH0fXviVmw7kZ--znjMk8", sheet = "rank_order")

     rv$albums <- sample_n(rv$jpa, 5, weight = p)
    
  }, ignoreInit = FALSE)
  
  observeEvent(input$btnReset, {
    
    rv$albums <- sample_n(rv$jpa, 5, weight = p)
    
  }, ignoreInit = FALSE)


}

shinyApp(ui, server)