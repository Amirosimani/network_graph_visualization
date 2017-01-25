library(shiny)
library(NLP)
library(tm)
library(igraph)
library(networkD3)
library(RCurl)


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

adj_file <- read.csv('/Users/Amiros/Downloads/matrix.csv' ,header=TRUE,row.names=NULL,check.names=FALSE)
attributes <- read.csv('/Users/Amiros/Downloads/atts.csv', header=TRUE) # see  Lazega-atts.csv  on Courseworks

  
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
    
   graph_summary <- reactive({
     graph <- graph.adjacency(mat, weighted = T, mode = "undirected")
     one_direction_edges <- data.frame(get.edgelist(graph))
     vertex_attr(graph, index=attributes$ID) <- attributes
     
     colnames(one_direction_edges)  <- c('ID1', 'ID2')
     #  (nothing to do with the band)
     
     #  The above data.frame only has one direction for each edge, we need the data fram to contain both
     #  e.g. Louis -> Niall and Niall -> Louis, so:
     all_edges <- rbind(  # Add a bunch of rows
       one_direction_edges, # to our list of edges
       setNames( # which are contained in a new data frame created by
         one_direction_edges, # copying one_direction_edges
         rev(names(one_direction_edges)) # and changing the column names to be in the reverse order
         # i.e. call ID1 ID2
       )
     )
     
     all_edges_with_attributes <- merge(all_edges, #  Merging our exisitng list of edges
                                        setNames(attributes,  # With a new data.frame created from `lazega_attributes`
                                                 paste0(names(attributes) ,'1')), # by changing the names of its 
                                        # columns to append a '1'
                                        by='ID1') # and we want the elemments of all_edges which have a given ID1 to
     #  be matched with the elements of the new data.frame which have the same value for ID1
     
     
     #  Pretty much the same thing to add the attributes for the second vertex in each edge
     all_edges_with_attributes <- merge(all_edges_with_attributes,  # except that 
                                        # we're merging with our new edge list, so that we end up
                                        # with attributes for the first and second vertices
                                        setNames(attributes, 
                                                 paste0(names(attributes) ,'2')),
                                        by='ID2')
     
     #  Add columns to all_edges_with_attributes which inidcate if the gender of
     #  the first vertex in each edge is the same as that of the second vertex
     all_edges_with_attributes$homo_gender = ifelse(
       all_edges_with_attributes$gender1==all_edges_with_attributes$gender2,  # if the genders for this edge are the same
       1, # the value of homo_gender is 1
       0) # otherwise it's 0
     
     all_edges_with_attributes$homo_status = ifelse(all_edges_with_attributes$status1==all_edges_with_attributes$status2, 1,0)
     
     ego_homophily_stats <- aggregate(all_edges_with_attributes[,c('homo_gender', 'homo_status', 'gender1')], by=list(ID1=all_edges_with_attributes$ID1), FUN=mean, na.rm=TRUE)
     attributes <- merge(attributes,ego_homophily_stats,  by.x="ID", by.y="ID1")
     
     summary(lm(homo_gender ~ gender, attributes))
     
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