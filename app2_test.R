#### phyloregion shiny app
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(leaflet)
library(raster)
library(rgdal)
require(leaflet.extras)
library(shinyLP)
library(shinyWidgets)
library(DT)
library(plotly)
library(ape)

header <- dashboardHeader(title = "Shiny phyloregion", titleWidth = 230)

sidebar <- dashboardSidebar(
  sidebarMenu(
    br(),
    fluidRow(
      column(12,offset = 3,
             img(src="www/logo_phylo.png", height= 97.5, width=  114.075, align = "center"))
    ),
    br(),
    menuItem("Introduction", tabName ="Tutorial", icon = icon("home")),
    menuItem("Upload species data", tabName ="UploadSpp",icon = icon("upload")),
    menuItem("Analyses and Visualization", tabName = "Classify", icon = icon("gear"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "Tutorial", 
            box(width = NULL,
                includeMarkdown("intro.Rmd"),
                fluidRow(
                  column(width = 7, offset=2,
                         imageOutput('tbl')
                  )
                ),
            )
    ),
    tabItem(tabName = "UploadSpp", 
            fluidRow(
              box(width = 4, height = NULL, 
                  title = "Occurrence data",
                  status = "success", solidHeader = T, 
                  fileInput("file.occ", "Occurrence data"),
                  radioButtons("file.type", "File type:", 
                               choices = c("Points", "Polygons", "Raster"), selected = "Points"),
                  numericInput("res", "Resolution:", 1),
                  actionButton("ex_spp", "Use an example")
              ),
              box(width = 8, height = NULL,
                  title = "Occurence matrix table",
                  status = "success", solidHeader = T,
                  
                  DT::DTOutput(outputId = "commDT"),
                  verbatimTextOutput("verbatimDT"),
                  downloadButton("download_comm_data.csv","Download occurrence data table"),br(),br()
              )
            ),
            fluidRow(
              box(width = 4, height = NULL, 
                  title = "Phylogeny",
                  status = "success", solidHeader = T, 
                  fileInput("file.phylo", "Newick file"),
                  actionButton("ex_phylo", "Phylogeny example")
              ),
              box(width = 8, height = NULL, 
                  title = "Phylogenetic tree",
                  status = "success",
                  solidHeader = T,
                  fluidRow(column(6,
                                  radioButtons("phylo_type", "Phylogeny",
                                               choices = c("circular", "rectangular"), selected = "rectangular")
                  )
                  ),
                  plotly::plotlyOutput(outputId = "phylo_plotly")
              )
            )
    ),
    
    tabItem(tabName = "Classify", 
            fluidRow(
              column(3,
                     box(width = NULL,
                         checkboxGroupButtons(
                           inputId = "grbox", label = "Choose a metric", 
                           choices = c("Phylo Diversity" = "PD",
                                       "Phylo Endemism" = "PE",
                                       "Weighted Endemism" = "WPE",
                                       "EDGE" = "EDGE"),
                           justified = T, status = 'info', size = "xs", direction = "vertical",
                           checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
                           selected = c("PE"),
                           width = "100%"
                         )
                     ),
                     box(width = NULL, title = "Classification",
                         solidHeader = T, status = "success",
                         textOutput("sel_display"),
                         radioButtons(inputId = "classification_type",
                                      label = "Classification type",
                                      choices = c("Taxonomic", "Phylogenetic", "Functional"),
                                      selected = "Taxonomic")
                     ),
                     box(width = NULL, title = "Download",
                         solidHeader = T, status = "success",
                         textOutput("sel_display"),
                         downloadButton("download_map_phylo.ras", "Download raster file"),br(),br()
                     )
              ), 
              column(width = 9,
                box(width = NULL, title = "Classification result",
                    solidHeader = T, status = "success"),
                column(width = 6, box(width = NULL, title = "Diversity patterns",
                    solidHeader = T, status = "success")),
                column(width = 6, box(width = NULL, title = "Membership",
                                      solidHeader = T, status = "success"))
        )
    )
  )
)
)


ui <- dashboardPage(header, sidebar, body)


server <- function(input, output, session){
  
  # reactive values to receive data 
  val <- reactiveValues()
  values <- reactiveValues()
  valuesMap <- reactiveValues()
  
  # Upload species file
  observeEvent(!is.null(input$file.occ),{
    choice <- reactiveVal(input$file.type)
    if(choice() == "Points"){
      val$comm <- read.csv(input$file.occ$datapath, 
                           sep = ",", encoding = "UTF-8", stringsAsFactors = F, header = TRUE)
      comm <- as.data.frame(val$comm)
      comm <- points2comm(dat = comm, res = input$res, lon = "lon", lat = "lat")$comm_dat
    }
    if(choice() == "Polygons"){
      shpdf <- input$file.occ
      tempdirname <- dirname(shpdf$datapath[1])
      
      for (i in 1:nrow(shpdf)) {
        file.rename(
          shpdf$datapath[i],
          paste0(tempdirname, "/", shpdf$name[i])
        )
      }
      
      occ_data <- readOGR(paste(tempdirname,
                                shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
                                sep = "/"
      ))
      comm <- polys2comm(dat = occ_data)$comm_dat
    }
    if(choice() == "Raster"){
      stop("not implemented")
    }
    
    df <- as.data.frame(sparse2dense(comm))
    output$commDT <- DT::renderDataTable({comm})
    
  })
  
  # Using example of species file
  observeEvent(input$ex_spp,{
    val$comm <- read.csv("www/comm_africa.csv", 
                         sep = ",", encoding = "UTF-8", stringsAsFactors = F, header = TRUE)
    comm <- val$comm 
    output$commDT <- DT::renderDT({comm})
    output$verbatimDT <- renderPrint(DT::renderDT({comm}))
  })
  
  # Using uploaded file for phylogeny
  observeEvent(input$file.phylo,{
    values$phylo <- ape::read.tree(input$file.phylo$datapath)
    phylo <- as.phylo(values$phylo)
    type_phylo <- reactive({
      input$phylo_type
    })
    output$phylo_plotly <- plotly::renderPlotly({
      height <- session$clientData$output_p_height
      width <- session$clientData$output_p_width
      plot_interact(tree = phylo, 
                    type = type_phylo(),
                    tip.label = FALSE, height = height, width = width)})
  }) 
  
  
  # Using example phylogeny 
  observeEvent(input$ex_phylo,{
    values$phylo <- ape::read.tree("www/phylo_africa.txt")
    phylo <- as.phylo(values$phylo)
    output$phylo_plotly <- plotly::renderPlotly({
      height <- session$clientData$output_p_height
      width <- session$clientData$output_p_width
      plot_interact(tree = phylo, 
                    type = input$phylo_type,
                    tip.label = FALSE, height = height, width = width)})
  })
  
}


shinyApp(ui, server)
