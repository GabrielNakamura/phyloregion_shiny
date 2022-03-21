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

header <- dashboardHeader(title = "phyloregion", titleWidth = 230)

sidebar <- dashboardSidebar(
  sidebarMenu(
    br(),
    fluidRow(
      column(12,offset = 3,
             img(src="logo_phylo.png", height= 90.5, width=  114.075, align = "center"))
    ),
    br(),
    menuItem("Introduction", tabName ="Tutorial", icon = icon("home")),
    menuItem("Upload Data", tabName ="Upload",icon = icon("upload")),
    menuItem("Classification", tabName ="Classify",icon = icon("person-digging")),
    menuItem("Visualize Spatial Patterns", tabName ="Viz",icon = icon("download"))
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
    tabItem(tabName = "Upload", 
            fluidRow(
              box(width = 4, height = NULL, 
                  title = "Occurrence matrix",
                  status = "success", solidHeader = T, 
                  fileInput("file.occ", "Occurrence matrix"),
                  actionButton("ex_spp", "Use an example"),
                  radioButtons("file.type", "File type:", 
                               choices = c("csv","txt (not implemented)"))
                  ),
              box(width = 8, height = NULL,
                  title = "Occurence matrix table",
                  status = "success", solidHeader = T,
                  DT::dataTableOutput(outputId = "commDT")
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
                  title = "Phylogeny representation",
                  status = "success",
                  solidHeader = T,
                  fluidRow(column(6,
                                  radioButtons("phylo_type", "Phylogenetic representation",
                                               choices = c("circular", "regular"))
                  )
                  )
              )
              )
            ),
    
    tabItem(tabName = "Validation", 
            fluidRow(
              column(3,
                     box(width = NULL,
                         checkboxGroupButtons(
                           inputId = "grbox", label = "What metrics should be shown in the map", 
                           choices = c("Phylo Diversity" = "PD",
                                       "Phylo Endemism" = "PE",
                                       "Weighted Endemism" = "WPE",
                                       "EDGE" = "EDGE"),
                           justified = T, status = 'info', size = "xs", direction = "vertical",
                           checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
                           selected = c("PD",
                                        "PE", 
                                        "WPE", 
                                        "EDGE"
                                        ),
                           width = "100%"
                         ),
                         materialSwitch("del_mkr_button", 
                                        label = "Delete points with click", 
                                        status = "danger")
                         
                         
                     ),
                     box(width = NULL, title = "Download",
                         solidHeader = T, status = "success",
                         
                         
                         textOutput("sel_display"),
                         downloadButton("download_grid_filter.csv","Download from grid filter"),br(),br(),
                         textOutput("down.class.text"),
                         downloadButton("download_classified.csv","Download from classifier")
                     )
              ),
              column(9,
                     leafletOutput("map", height = 500)
              )
            )
    )
  )
)


ui <- dashboardPage(header, sidebar, body)


server <- function(input, output, session){
  
  # map with phyloregions
  output$map <- leaflet::renderLeaflet({
    map <- leaflet() %>% 
      addTiles()
    map
  })
  
  # reactive values to receive data 
  val <- reactiveValues()
  values <- reactiveValues()
  val$comm <- matrix()
  
  
  # Upload species file
  observeEvent(!is.null(input$file.occ),{
    req(input$file.occ)
    
    val$comm <- read.csv(input$file.occ$datapath, 
                       sep = ",", encoding = "UTF-8", stringsAsFactors = F, header = TRUE)
    comm <- as.data.frame(val$comm)
    df <- val$comm
    output$commDT <- DT::renderDataTable({comm})
  })
  
  # Using example of species file
  observeEvent(input$ex_spp,{
    val$comm <- read.csv("www/comm_africa.csv", 
                         sep = ",", encoding = "UTF-8", stringsAsFactors = F, header = TRUE)
    comm <- as.data.frame(val$comm)
    output$commDT <- DT::renderDataTable({comm})
  })
     
}


shinyApp(ui, server)
