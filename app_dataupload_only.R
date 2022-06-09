#### phyloregion shiny app
source("functions.R")
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(ggtree)
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
    menuItem("Upload species data", tabName ="UploadSpp", icon = icon("upload")),
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
    

    # updload data tab ---------------------------------------------------------

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
                                               choices = c("rectangular", "dendrogram", "fan"), selected = "rectangular")
                  )
                  ),
                  plotly::plotlyOutput(outputId = "phylo_plotly")
              )
            )
    ),
    
    # classification and analysis tab -----------------------------------------
    
    tabItem(tabName = "Classify", 
            fluidRow(
              column(3,
                     box(width = NULL, title = "Classification",
                         solidHeader = T, status = "success",
                         textOutput("sel_display"),
                         radioButtons(inputId = "classification_type",
                                      label = "Classification type",
                                      choices = c("Taxonomic", "Phylogenetic", "Functional"),
                                      selected = "Phylogenetic")
                     ),
                     box(width = NULL, title = "Download",
                         solidHeader = T, status = "success",
                         textOutput("sel_display"),
                         downloadButton("download_map_phylo.ras", "Download all figures"),br(),br()
                     )
              ), 
              column(width = 9,
                     box(width = NULL, title = "Classification result",
                         solidHeader = T, status = "success"),
                     column(width = 8, box(width = NULL, title = "Diversity patterns",
                                           solidHeader = T, status = "success",
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
                                           ),
                                           tabBox(
                                             side = "right", width = 8,
                                             selected = "PE",
                                             tabPanel("PD", "Phylogenetic diversity"),
                                             tabPanel("PE", "Phylogenetic endemism"),
                                             tabPanel("WE", "Weighted endemism"),
                                             tabPanel("EDGE", "EDGE")
                                           )
                                           )
                            ),
                     column(width = 4, box(width = NULL, title = "Membership",
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
  
  
  # Using example of species file
  observeEvent(input$ex_spp,{
    val$comm <- read.csv("www/comm_africa.csv", 
                         sep = ",", encoding = "UTF-8", stringsAsFactors = F, header = TRUE)
    
    comm <- val$comm 
    output$commDT <- DT::renderDT({DT::datatable(comm)})
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
                    tip.label = FALSE, height = height, width = width)}) # doesn't working for circular plots
  })
  
}


shinyApp(ui, server)
