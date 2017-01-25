library(shiny)
library(NLP)
library(tm)
library(igraph)
library(networkD3)
library(readr)


w <- "240px"
h <- "240px"


if (interactive()) {
u <- shinyUI(fluidPage(
  titlePanel("Social Network Analysis"),
  sidebarLayout(
    position = "left",
    sidebarPanel(
      h2("Controls"),
      # import adjacency matrix and attribute matrix
      fileInput("file1", "Adjacency CSV file",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      #fileInput('attr_file', 'Attributes CSV file', accept = c('text/csv','.csv')),
      checkboxInput('header', 'Header', TRUE),
      checkboxInput('directed', 'Directed', TRUE),
      checkboxInput('weighted', 'Weighted', TRUE),
      tags$hr(),
      #sliderInput("sparse", "Sparsity:", 0.9, 1, 0.994,0.002),
      numericInput("fmrseed", "F-R Seed:", 1234, 1, 10000, 1)
    ),
    mainPanel(
      h2("Network Graphs"),
      tabsetPanel(
        tabPanel("Fruchterman-Reingold", plotOutput("fmr")),
        tabPanel("Summary", tableOutput("contents"))
        )
      )
    )
  ))

attributes <- read.csv('https://raw.githubusercontent.com/Amirosimani/network_graph_visualization/master/atts.csv', header=TRUE) # see  Lazega-atts.csv  on Courseworks

  
s <- shinyServer(
  function(input, output)
  {
    fmrlayout <- reactive({
      set.seed(input$fmrseed)
      
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      adj_data <- read.csv(inFile$datapath, header = input$header)
      
               
      adj_mat <- as.matrix(adj_data)
      
      g <- graph.adjacency(adj_mat, weighted = T, mode = "undirected")
      g <- simplify(g)
      V(g)$label <- V(g)$name
      V(g)$degree <- degree(g)
      layout <- layout.fruchterman.reingold(g)
      rv <- list()
      rv$g <- g
      rv$layout <- layout
      rv
    })
    
  
    ###Different Social Network Graphics
    
    # Fruchterman-Reingold Network
    output$fmr <- renderPlot({

      rv <- fmrlayout()
      plot(rv$g, layout = rv$layout)
    })
    
    output$contents <- renderTable({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      read.csv(inFile$datapath, header = input$header)
    })
  }
)

shinyApp(ui = u,server = s)
}