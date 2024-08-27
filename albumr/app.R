## VIEW SPREADSHEET


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


# jp <- read_sheet("1HrP0_-kRKp0Uxpi_xmcPBpqH0fXviVmw7kZ--znjMk8", sheet = "rank_order")
# 
# jp_matches <- jp %>% group_by(trial) %>% mutate(n = n()-1) %>% group_by(order) %>% summarise(matches = sum(n)) 


# jpa <- read.csv("jp_albums.csv") #%>% select(-Order) ## list of albums
jpa <- read_sheet("1F1bi8Y12cnmyCnNB3HWpVdj9oi918ohHi7qBITB73Ks", sheet = "All") %>% 
  mutate(last_played = ymd(last_played), ranked = ifelse(is.na(a_rank), "No", "Yes")) 

Load(art_tags)
genres <- read.csv("genres.csv") %>% filter(use ==1)

AT <- art_tags %>% filter(tag %in% genres$tag) %>% group_by(artist) %>% slice(1:4) 

artist_table <- jpa %>% group_by(artist)%>% 
  arrange(a_rank) %>% 
  summarise(albums = n(), plays = sum(n, na.rm = TRUE), 
            tracks = sum(tracks, na.rm = TRUE), 
            last_played = max(last_played, na.rm = TRUE),
            ranked_albums = sum(!is.na(a_rank)), 
            top100 = sum(a_rank <= 100, na.rm = TRUE), 
            top250 = sum(a_rank <= 250, na.rm = TRUE),
            mean3 = ifelse(ranked_albums >=3, floor(mean(head(a_rank, 3))), NA),
            keep = sum(Status %in% c("Add", "Ranked")),
            blue = sum(Status == "Blue"),
            homework = sum(Status == "Homework")) 

year_table <- jpa %>% group_by(Year)%>% 
  arrange(a_rank) %>% 
  summarise(albums = n(), plays = sum(n, na.rm = TRUE), 
            tracks = sum(tracks, na.rm = TRUE), 
            last_played = max(last_played, na.rm = TRUE),
            ranked_albums = sum(!is.na(a_rank)), 
            top100 = sum(a_rank <= 100, na.rm = TRUE), 
            top250 = sum(a_rank <= 250, na.rm = TRUE),
            mean3 = ifelse(ranked_albums >=3, floor(mean(head(a_rank, 3))), NA),
            keep = sum(Status %in% c("Add", "Ranked")),
            blue = sum(Status == "Blue"),
            homework = sum(Status == "Homework")) 


ui <- navbarPage(
        "Too much, too much",
        tabPanel(
          "Albums",
          column(4, 
                 selectizeInput("jp_album", "Album", jpa$album, selected = NULL, multiple = T, options = NULL),
                 selectizeInput("jp_artist", "Artist", unique(jpa$artist), selected = NULL,multiple = T, options = NULL),
                 selectizeInput("not_artist", "Exclude artist", choices = c(" ", unique(jpa$artist)), selected = " ",multiple = T, options = NULL),
                 selectizeInput("jp_year", "Year", unique(jpa$Year), selected = NULL, multiple = T, options = NULL),
                 selectizeInput("jp_status", "Status", jpa$Status, selected = c("Ranked", "Add", "Blue", "Active", "Homework"), multiple = T, options = NULL),
                 selectizeInput("at_genre", "Genre", unique(AT$tag), selected = NULL, multiple = T, options = NULL),
                 sliderInput("jp_slider", "Current ranking", min = 1, max = max(jpa$a_rank, na.rm = TRUE), 
                             value = c(1, max(jpa$a_rank, na.rm = TRUE)), ticks = TRUE),
                 checkboxGroupInput("jp_ranked", "Ranked", c("No", "Yes"), inline = TRUE, selected = c("No", "Yes")),),
                 
          column(8, reactableOutput("jpa_table")#,
                 # verbatimTextOutput("xx_artist")
                 )
        ),
        tabPanel(
          "Artists",
          column(4, 
                 # selectizeInput("jp_album", "Album", jpa$album, selected = NULL, multiple = T, options = NULL),
                 # selectizeInput("jp_artist", "Artist", unique(jpa$artist), selected = NULL,multiple = T, options = NULL),
                 # selectizeInput("not_artist", "Exclude artist", choices = c(" ", unique(jpa$artist)), selected = " ",multiple = T, options = NULL),
                 # selectizeInput("jp_year", "Year", unique(jpa$Year), selected = NULL, multiple = T, options = NULL),
                 # selectizeInput("jp_status", "Status", jpa$Status, selected = c("Keep", "Blue", "Active", "Homework"), multiple = T, options = NULL),
                 # selectizeInput("at_genre", "Genre", unique(AT$tag), selected = NULL, multiple = T, options = NULL),
                 # sliderInput("jp_slider", "Current ranking", min = 1, max = max(jpa$a_rank, na.rm = TRUE), 
                 #             value = c(1, max(jpa$a_rank, na.rm = TRUE)), ticks = TRUE),
                 # checkboxGroupInput("jp_ranked", "Ranked", c("No", "Yes"), inline = TRUE, selected = c("No", "Yes")),),
          ),
          
          column(8, reactableOutput("art_table")#,
                 # verbatimTextOutput("xx_artist")
          )
        ),
        tabPanel(
          "Year",
          column(4, 
                 # selectizeInput("jp_album", "Album", jpa$album, selected = NULL, multiple = T, options = NULL),
                 # selectizeInput("jp_artist", "Artist", unique(jpa$artist), selected = NULL,multiple = T, options = NULL),
                 # selectizeInput("not_artist", "Exclude artist", choices = c(" ", unique(jpa$artist)), selected = " ",multiple = T, options = NULL),
                 # selectizeInput("jp_year", "Year", unique(jpa$Year), selected = NULL, multiple = T, options = NULL),
                 # selectizeInput("jp_status", "Status", jpa$Status, selected = c("Keep", "Blue", "Active", "Homework"), multiple = T, options = NULL),
                 # selectizeInput("at_genre", "Genre", unique(AT$tag), selected = NULL, multiple = T, options = NULL),
                 # sliderInput("jp_slider", "Current ranking", min = 1, max = max(jpa$a_rank, na.rm = TRUE), 
                 #             value = c(1, max(jpa$a_rank, na.rm = TRUE)), ticks = TRUE),
                 # checkboxGroupInput("jp_ranked", "Ranked", c("No", "Yes"), inline = TRUE, selected = c("No", "Yes")),),
          ),
          
          column(8, reactableOutput("year_table")#,
                 # verbatimTextOutput("xx_artist")
          )
        )

)

server <- function(input, output, session) {

  rv <- reactiveValues( jpa = jpa)
  
  # observe(
  #         {
  #         xx <- input$jp_artist
  #         output$xx_artist <- renderPrint({xx})
  #         }
  #         )
  
observeEvent(c(input$jp_album,
               input$jp_status, 
               input$jp_year, 
               input$jp_ranked,
               input$jp_artist, 
               input$not_artist, 
               input$at_genre,
               input$jp_slider),
             {
               
             df <- rv$jpa 
             
             atg <- AT %>% filter(tag %in% input$at_genre)
            
             df <- df[grepl(paste(as.character(input$jp_artist),collapse="|"), df$artist),]
             df <- df[grepl(paste(as.character(input$jp_album),collapse="|"), df$album),]
             df <- df[grepl(paste(as.character(input$jp_year),collapse="|"), df$Year),]
             df <- df[grepl(paste(as.character(input$jp_status),collapse="|"), df$Status),]
             df <- df[grepl(paste(as.character(input$jp_ranked),collapse="|"), df$ranked),]
             df <- df[grepl(paste(as.character(atg$artist),collapse="|"), df$artist),]
             
             df %<>% filter(dplyr::between(a_rank, input$jp_slider[1], input$jp_slider[2])|is.na(a_rank))
             
  output$jpa_table <- renderReactable({
    df %>% select(-ranked, -p_rank, -ranker, -Rank, -Tier) %>% 
    j.reactable(., defaultPageSize = 20, filterable = TRUE, height = 900,)
  })
 
  
     
             }#, ignoreInit = TRUE   
)
observe({ 
  
  output$art_table <- renderReactable({

      j.reactable(artist_table, defaultPageSize = 20, filterable = TRUE, height = 900,)
  })
  
  output$year_table <- renderReactable({
    
    j.reactable(year_table, defaultPageSize = 20, filterable = TRUE, height = 900,)
  })
  
  })




  # observeEvent(
  #   c(input$jp_album, input$jp_artist, input$jp_year),
  # 
  #   if(!is.null(input$jp_album)){
  #   rv$jpa %<>% mutate(p= p_save, p = ifelse(album %in% input$jp_album & p < 10, 500*p, p))
  #   }
  # 
  #   else if(!is.null(input$jp_artist)){
  #     rv$jpa %<>% mutate(p= p_save, p = ifelse(artist %in% input$jp_artist & p < 10, 500*p, p))
  #   }
  #   else if(!is.null(input$jp_year)){
  #     rv$jpa %<>% mutate(p= p_save, p = ifelse(Year %in% input$jp_year & p < 10, 500*p, p))
  #   }
  # 
  # )

  # observeEvent(
  #   c(input$jp_slider),
  #   {
  #     
  #     rv$jpa %<>% mutate(p= p_save, 
  #                        p = ifelse(a_rank >= input$jp_slider[1] & a_rank <= input$jp_slider[2] |is.na(a_rank),
  #                               p*5*(nrow(rv$jpa )/(input$jp_slider[2] - input$jp_slider[1]-1)), p) )
  #   }
  #   
  # )
  # 
  
  # observeEvent(input$btnSubmit, {
  #   
  #   rv$rankings <- rbind(rv$rankings,rv$new_rank_rows  )
  # 
  #   sheet_append(data =rv$new_rank_rows , ss = "1HrP0_-kRKp0Uxpi_xmcPBpqH0fXviVmw7kZ--znjMk8", sheet = "rank_order")
  # 
  #    rv$albums <- sample_n(rv$jpa, 5, weight = p)
  #   
  # }, ignoreInit = FALSE)
  # 
  # observeEvent(input$btnReset, {
  #   
  #   rv$albums <- sample_n(rv$jpa, 5, weight = p)
  #   
  # }, ignoreInit = FALSE)


}

shinyApp(ui, server)