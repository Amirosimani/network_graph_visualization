library(shiny)
library(NLP)
library(tm)
library(igraph)
library(networkD3)
library(readr)


w <- "240px"
h <- "240px"
u <- shinyUI(fluidPage(
  titlePanel("Social Network Analysis"),
  sidebarLayout(
    position = "left",
    sidebarPanel(
      h2("Controls"),
      # import adjacency matrix and attribute matrix
      fileInput('adj_file', 'Adjacency CSV file', accept = c('text/csv','.csv')),
      fileInput('attr_file', 'Attributes CSV file', accept = c('text/csv','.csv')),
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
        tabPanel("Summary", dendroNetworkOutput("dendro"))
        )
      )
    )
  ))
adj_file <- read.csv('https://raw.githubusercontent.com/Amirosimani/network_graph_visualization/master/matrix.csv' ,header=TRUE,row.names=NULL,check.names=FALSE)
attributes <- read.csv('https://raw.githubusercontent.com/Amirosimani/network_graph_visualization/master/atts.csv', header=TRUE) # see  Lazega-atts.csv  on Courseworks

  
s <- shinyServer(
  function(input, output)
  {
  mat <- as.matrix(adj_file)

    fmrlayout <- reactive({
      set.seed(input$fmrseed)
      g <- graph.adjacency(mat, weighted = T, mode = "undirected")
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
  }
)

shinyApp(ui = u,server = s)