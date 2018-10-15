# Author: Joris Wiethase
# 
rm(list = ls(all=TRUE))  
library(shiny)

# Make the user interface
ui <- shiny::fluidPage(theme = "bootstrap.css",
                       tags$head(
                         tags$style(HTML("
                                         .shiny-output-error-validation {
                                         color: red;
                                         }
                                         "))),
                       # Add a side panel for inputs
                       shiny::sidebarPanel(width = 2,
                                           shiny::fileInput(inputId = 'dataset', 
                                                            label = h4('Choose .csv file to upload'),
                                                            accept = c('.csv')
                                           ),
                                           helpText("Warning: Dataset has to include all of the following column names:"),
                                           hr(),
                                           helpText("'Band.ID' (Format: 'Size-Sequence')"),
                                           helpText("'Species'"),
                                           helpText("'Date' (Format: dmy)"),
                                           helpText("'Site'"),
                                           hr(),
                                           shiny::selectInput(inputId = "Plot type", 
                                                              label = h4("Choose error seeking option"),
                                                              choices = c('None',
                                                                          'NA in species or band number',
                                                                          'Species - band number discrepancies',
                                                                          'Same-season recaptures', 
                                                                          'Band sequence discrepancies',
                                                                          'Unusual band size'))
                                           
                       ),
                       mainPanel(plotOutput("plot"), style = "height:900px; overflow-y: scroll;overflow-x: scroll;")
                         )

# Make the server functions
server <- function(input, output, session) {
  data <- reactive({
    req(input$dataset)
    data <- read.csv(input$dataset$datapath) 
    req.names <- c("Band.ID", "Species", "Date", "Site")
    validate(
      need(all(req.names %in% colnames(data), TRUE) == TRUE,
           message = paste("\nError: Missing or miss-spelled column names.\nUnmatched columns:\n\n", paste(c(req.names[req.names %in% colnames(data) == FALSE]), collapse="\n"), sep="")
      )
    )
    
    data$Date <- dmy(data$Date)
    data$band_size <- sapply(strsplit(as.character(data$Band.ID), split="-"), `[`, 1)
    data$band_sequence <- as.numeric(sapply(strsplit(as.character(data$Band.ID), split="-"), `[`, 2))
    data
  })
 }

shiny::shinyApp(ui, server)
