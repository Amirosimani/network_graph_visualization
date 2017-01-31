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
      fileInput("file1", "Adjacency Matrix CSV",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      fileInput('attr_file', 'Attributes CSV', accept = c('text/csv','.csv')),
      checkboxInput('header', 'Header', TRUE),
      checkboxInput('directed', 'Directed', FALSE),
      checkboxInput('weighted', 'Weighted', TRUE),
      tags$hr(),
      #sliderInput("sparse", "Sparsity:", 0.9, 1, 0.994,0.002),
      selectInput("layout", label = h3("Select a Layout"), 
                  choices = list("Fruchterman Reingold" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                  selected = 1),
      numericInput("fmrseed", "Seed:", 1234, 1, 10000, 1)
    ),
    mainPanel(
      h2("Network Graphs"),
      tabsetPanel(
        tabPanel("Network Graph", plotOutput("fmr")),
        tabPanel("Summary", tableOutput("contents"))
        )
      )
    )
  ))

#attributes <- read.csv('https://raw.githubusercontent.com/Amirosimani/network_graph_visualization/master/atts.csv', header=TRUE) # see  Lazega-atts.csv  on Courseworks

  
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

      if (input$directed == TRUE)
         directed_choice <- 'directed'
      else
         directed_choice <- 'undirected' 
      
      g <- graph.adjacency(adj_mat, weighted = input$weighted, mode = directed_choice)
      g <- simplify(g)
      V(g)$label <- V(g)$name
      V(g)$degree <- degree(g)
      if (input$layout == 1){
        layout <- layout.fruchterman.reingold(g)}
      rv <- list()
      rv$g <- g
      rv$layout <- layout
      rv
    })
    
    attr_fnc <- reactive({
      attr_inFile <- input$attr_file
      
      if (is.null(attr_inFile))
        return(NULL)
      
      attr_data <- read.csv(attr_inFile$datapath, header = input$header)
      
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      adj_data <- read.csv(inFile$datapath, header = input$header)
      
    })
#http://stackoverflow.com/questions/41385314/network-analysis-in-shiny

    #  Network Graph
    output$fmr <- renderPlot({

      rv <- fmrlayout()
      plot(rv$g,
           layout = rv$layout,
           edge.arrow.size=0.5
           )
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
