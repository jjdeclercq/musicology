
library(shiny)

create_con_panel <- function(NN){
  COND <- paste0("input.d_dims > ", NN-1)
  conditionalPanel(COND, 
                   fluidRow(
                     column(8,
                   textInput(paste0("d_dim", NN),
                             paste0("Dimension ", NN),
                             paste0("Enter criterion ", NN))),
                   column(4,
                   numericInput(paste0("d_w",NN), 
                                paste0("Weight ", NN), 
                                1, min = 0.1, max = 10)
                   
                   ))
  )
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("DECIDE!"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("d_dims", "Input number of dimensions", 2, min = 2, max = 10),
            textInput("d_choices", "Input choices separated by a comma", "A, B, C, D"),
            map(1:10, create_con_panel)
        ),

        # Show a plot of the generated distribution
        mainPanel(
          verbatimTextOutput("d_out_vector"),
          uiOutput("d_sortable"),
          actionButton("btnSubmit", label = "Submit rankings"),
          reactableOutput("choice_table"),
          verbatimTextOutput("d_out_ragg")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  
  observeEvent(c(input$d_dims, input$d_dim1, input$d_dim2, input$d_dim3, 
                 input$d_dim4, input$d_dim5, input$d_dim6, input$d_dim7, 
                 input$d_dim8, input$d_dim9, input$d_dim10),{
                   
    rv_dims$criteria <- c(input$d_dim1, input$d_dim2, input$d_dim3, input$d_dim4, input$d_dim5, 
      input$d_dim6, input$d_dim7, input$d_dim8, input$d_dim9, input$d_dim10)[1:input$d_dims]
    
  })
  
  observeEvent(c(input$d_w1, input$d_w2, input$d_w3, 
                 input$d_w4, input$d_w5, input$d_w6, input$d_w7, 
                 input$d_w8, input$d_w9, input$d_w10),{
  
    rv_dims$d_weights <- c(input$d_w1, input$d_w2, input$d_w3, 
                           input$d_w4, input$d_w5, input$d_w6, input$d_w7, 
                           input$d_w8, input$d_w9, input$d_w10)
    
})
  
  observeEvent(input$d_choices,
               {
                 rv_dims$choices <- str_trim(unlist(str_split(input$d_choices, ",")))
               })
  
  rv_dims <- reactiveValues(criteria = c(), choices = c(), 
                            ddf = data.frame(), dim_submitted = 0, ragg = NULL, d_weights = c())
  

  output$d_out_vector <- renderPrint({
    
    cc <- rv_dims$dim_submitted + 1

    paste0("Rank along criteria ",cc,": ", rv_dims$criteria[cc])
})
  
  
  output$d_sortable <- renderUI({
    rank_list_basic <- rank_list(
      text = "rank from best to worst",
      labels =  rv_dims$choices,
      input_id = "rank_choices"
    )
  })
  
  observeEvent(input$rank_choices, {
    rv_dims$choices <- input$rank_choices
  })
  
  observeEvent(input$btnSubmit, {
    
    rv_dims$dim_submitted <- rv_dims$dim_submitted + 1
    
    if(rv_dims$dim_submitted <= length(rv_dims$criteria)){
    rv_dims$ddf <- bind_rows(rv_dims$ddf,
                             data.frame(criteria = rv_dims$criteria[rv_dims$dim_submitted], 
                                        # n = rv_dims$dim_submitted,
                                        order = rv_dims$choices,
                                        nn = 1:length(rv_dims$choices))%>% 
                               pivot_wider(., names_from = "nn", values_from = "order")%>% 
                               # select(-n) %>% 
                               column_to_rownames("criteria")
                             )
    }
    
    
    if(rv_dims$dim_submitted == length(rv_dims$criteria)){
      
      rv_dims$ragg <- RankAggreg(as.matrix(rv_dims$ddf), length(rv_dims$choices), seed=100, 
                                 importance  = rv_dims$d_weights, 
                                 rho=.1, verbose = FALSE)
      
      output$d_out_ragg <- renderPrint({rv_dims$ragg })
    }
    
    output$choice_table <- renderReactable({
      rv_dims$ddf %>% 
        j.reactable(., defaultPageSize = 50)
    })
    

   
    
  })
  

  
}

# Run the application 
shinyApp(ui = ui, server = server)
