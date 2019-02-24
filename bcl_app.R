# based on the excellent example on https://deanattali.com/blog/building-shiny-apps-tutorial/

library(shiny)
library(ggplot2)
library(data.table)
library(DT) # for `renderDataTable`

# The data are put in a local folder named `Data`
bcl <- setDT(read.csv("./Data/bcl-data.csv", stringsAsFactors = FALSE))

ui <- fluidPage(
  titlePanel("Shiny Example: Liquor Store Selection"),
  
# sidebarLayout  
  sidebarLayout(
    # side panel
        sidebarPanel(
    # price, type, and country 
          sliderInput(inputId = "price_input", label = "Price", min = 0, max = 100,
                      value = c(0, 50), pre = "$"),
          checkboxGroupInput("type_input", "Product Type",
                       choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                       selected = c("BEER","WINE")),
          
          # a conditional panel to select sweetness 
          conditionalPanel(
            condition = "input.type_input.indexOf('WINE')!=-1",
            p("Filter by sweetness if wine is selected:"),
            sliderInput(inputId = "sweet_input", label = "Sweetness Level", min = 0, max = 10,
                        value = c(0, 5))
          ),
          
          checkboxInput("sort_by_price", "Sort results table by price", value = TRUE),
          checkboxInput("show_country", "Filter by country", value = FALSE),
          # populate the country selection
          conditionalPanel(
            condition = "input.show_country",
            p("Available selection from these countries:"),
            uiOutput("country_output")
            )
          ),
    
    # main panel for contents
        mainPanel(
            tabsetPanel(
              tabPanel("Results Table",
                      h3(textOutput("text_summary")), # add a headline of short summary
                      downloadLink('download_data', 'Download'), # add download option   
                      br(),br(),
                      plotOutput("myplot0"),
                      DT::dataTableOutput("results_table")),
              tabPanel("Alcohol%", plotOutput("myplot1")),
              tabPanel("Sweetness", plotOutput("myplot2")) 
            )
          )
    )
)

server <- function(input, output) {
  # use a reactive function to screen for available countries to be shown in list
  # the input would be `country_input`
  output$country_output <- renderUI({
    selectInput("country_input", "Country",
                select.country(), # only show available countries from list
                selected = "CANADA")
  })
  
  # the select.country function to return the list of `Country`
  select.country <- reactive({
    # : 
    bcl_temp <- bcl[Price >= input$price_input[1]&
          Price <= input$price_input[2]&
          Type %in% input$type_input,]
    if (!is.null(input$sweet_input)) bcl_temp <- bcl_temp[Sweetness >= input$sweet_input[1]&
                                                            Sweetness <= input$sweet_input[2]]
    bcl_temp[, sort(unique(Country))]
  })
  
  # subset the dataset
  filtered <- reactive({
    bcl_temp <- bcl[Price >= input$price_input[1]&
                 Price <= input$price_input[2]&
                 Type %in% input$type_input]
    if (input$show_country) bcl_temp <- bcl_temp[Country %in% input$country_input]
    if (!is.null(input$sweet_input)) bcl_temp <- bcl_temp[Sweetness >= input$sweet_input[1]&
                                                            Sweetness <= input$sweet_input[2]]
    if (input$sort_by_price) bcl_temp[order(Price),] 
    else bcl_temp[order(Country),]
  })
  
  # Add text: "There are in total # selections:"
  output$text_summary <- renderText({
    paste("There are in total", filtered()[,.N], "options:")
  })
  
  # The data table
  output$results_table <- DT::renderDataTable({
    filtered()
  })
  
  # Download buttom
  output$download_data <- downloadHandler(
    filename = function() {
      paste('BCL_Data_Selection', Sys.Date(), '.csv', sep="")
    },
    content = function(file) {
      write.csv(filtered(), file, row.names = F)
    }
  )
  
  # Figures
  ## fig.1 summary of price above results table
  output$myplot0 <- renderPlot({
      if (is.null(filtered())){
        return()
      }
        ggplot(filtered(), aes(x = Price, fill = Type)) +
        geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity') + 
        theme_bw() + 
        scale_fill_viridis_d(begin = 1, end = 0)+
        ggtitle("Distribution of Price in the Selection") + 
        labs(y = "Frequency")
    })
  
   
  ## fig.1 Alcohol Content
  output$myplot1 <- renderPlot({
    if (is.null(filtered())){
      return()
    }
    ggplot(filtered(), aes(x = Alcohol_Content, fill = Type)) +
      geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity') + 
      theme_bw() + 
      scale_fill_viridis_d(begin = 1, end = 0)+
      ggtitle("Distribution of Alcohol Content in the Selection") + 
      labs(y = "Count")
  })
  
  ## fig.2 Sweetness 
  output$myplot2 <- renderPlot({
    if (is.null(filtered())){
      return()
    }
    ggplot(filtered(), aes(x = Sweetness, fill = Type)) +
      geom_histogram(alpha = 0.7, aes(y = ..density..), position = 'identity') + 
      theme_bw() + 
      scale_fill_viridis_d(begin = 1, end = 0)+
      ggtitle("Distribution of Sweetness in the Selection") + 
      labs(y = "Count")
  })
}

shinyApp(ui = ui, server = server)


