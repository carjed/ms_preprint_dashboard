#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(shinythemes)
library(kableExtra)
library(shinyjs)
library(shinydashboard)
library(shinyBS)
library(lubridate)
library(stringr)
library(plotly)
library(DT)
library(reactable)

# Define UI for application that draws a histogram
# shinyUI(
navbarPage(
# shinyUI(dashboardPage(
        # theme = "cerulean",  # <--- To use a theme, uncomment this
        "Multiple Sclerosis Preprint Index",
        # shinyjs::useShinyjs(),
        theme = shinytheme("flatly"),
        
        # tags$head(tags$script(HTML(JScode))),
        
        # tabPanel("Basic",
        #   sidebarLayout(
        #     sidebarPanel(
        #       tableOutput("topic_table")
        #     ),
        #     
        #     mainPanel(
        #       DT::dataTableOutput("plotdat")
        #     )
        #   )         
        #          
        # ),
        # 
        # tabsetPanel()
        tabPanel("App",
             useShinyjs(),
            # shinythemes::themeSelector(),
            
            # Application title
            # titlePanel("Multiple Sclerosis Preprint Index"),
        
            # Sidebar with a slider input for number of bins
            sidebarLayout(
                sidebarPanel(
                  shinyWidgets::setSliderColor(c("", "YellowGreen"), c(2)),
                  
                  
                  div(
                    id="settings",
                    prettyCheckboxGroup("search_choice", label="Search",
                                        choices = c("titles", "abstracts", "authors"),
                                        selected = c("titles", "abstracts", "authors"),
                                        inline=TRUE,
                    ),
                    
                    searchInput(
                      inputId = "search", label = NULL,
                      placeholder = "search",
                      btnSearch = icon("search"),
                      btnReset = icon("remove"),
                      width = "450px"
                    ),
                  
                  fluidRow(
                    align = "center",
                    column(9,
                      prettyCheckboxGroup("preprint_topics", label="Filter by Topic",
                                          choiceValues = c(1:10),
                                          selected = c(1:10),
                                          choiceNames = str_pad(c(1:10), width=2, side="left", pad="0"),
                                          inline=TRUE,
                                          width="300px"
                      ),
                    ),
                  ),
                  
                  fluidRow(
                    align = "center",
                    column(12,
                      column(3, actionButton("select_all", "Select All")),
                      column(3, actionButton("deselect_all", "Deselect All")),
                      column(3, actionButton("invert", "Invert")),
                    )
                  ),
                  
                  br(),

                  dateRangeInput('date_range',
                                 label = 'Date range:',
                                 start = ymd("2014-12-01"), end = Sys.Date()
                  ),
                  
                ),
                
                actionButton("resetAll", "Reset Selection"),
                checkboxInput("toggle", "Show Legend", TRUE),
                bsTooltip("toggle", "Toggle the legend with detailed info about each topic", "right"),
                tableOutput("topic_table"),
                ),
                
        
                # Show a plot of the generated distribution
                mainPanel(
                    # selectInput("method", "Visualization Method:",
                    #             c("t-SNE" = "tsne",
                    #               "UMAP" = "umap")),
                    # 
                    # 
                    # 
                    # p("Preprints are clustered according to similarity in their topics, and the size of each point indicates that preprint's Altmetric Attention Score. Mouse over points to view preprint details, and click to view on the preprint site. Alternatively, browse selected preprints in the table below."),
                    # 
                    # plotlyOutput("distPlot"),
                    
                    
                    # tableOutput("plotdat")
                    
                    p("Expand a row to view the top 5 most closely-related papers in the corpus"),
                  
                    reactableOutput("plotdat")
                    
                    # verbatimTextOutput("hoverIndex"),
                    # verbatimTextOutput("clickIndex"),
                    # verbatimTextOutput("dblclickIndex"),
                    
                    # bsTooltip("method", "Dimensionality reduction method for visualizing the topic clusters", "right"),
                    
                    
                    
                )
            )
        ),
        
        tabPanel("About",
                 includeMarkdown("about.md"),
        )#,
        
        # tabPanel("Data",
        #          downloadLink('downloadData', 'Download'))
)
# )
