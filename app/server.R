#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(lubridate)
library(kableExtra)
library(shinyjs)
library(plotly)
library(tidyverse)
library(kableExtra)
# library(randomcoloR)
library(htmlwidgets)
# library(yaml)
# library(mongolite)
library(DT)
# library(shinydashboard)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  source("functions.R")
  n_topics <- 20
  # cols <- distinctColorPalette(k = n_topics, altCol = FALSE, runTsne = FALSE)
  cols <- readRDS("color_palette.rds")
  
#  keys <- yaml.load_file("/home/jedidiah/covid19_twitter/config.yaml")
  
  ncov_df_topics_am <- readRDS("ms_df_topics.rds")
  ncov_df_topics_am <- ncov_df_topics_am %>%
    mutate(topic=str_wrap(topic, width=60))
  ncov_df_topics_am <- ncov_df_topics_am %>%
    mutate(topic=factor(topic, levels=str_sort(unique(ncov_df_topics_am$topic), numeric=T)))
  
  tf_table <- readRDS("ms_tf_table.rds")
  
  output$hoverIndex <- renderText({
    UI_out <- input$hoverIndexJS
    return(paste("hover column info", UI_out))
  })
  
  output$clickIndex <- renderText({
    UI_out <- input$clickIndexJS
    return(paste("click cell info:", UI_out))
  })
  
  output$dblclickIndex <- renderText({
    UI_out <- input$dblclickIndexJS
    return(paste("dblclick row info:", UI_out))
  })
  
    addClass(selector = "body", class = "sidebar-collapse")
    
    # observeEvent(input$am_min,{
    #     if(as.numeric(input$am_min) != input$am_range[1]){
    #         updateSliderInput(
    #             session = session,
    #             inputId = 'am_range',
    #             value = c(input$am_min, input$am_max)
    #         ) # updateSliderInput
    #     }#if
    #     
    #     
    # })
    # 
    # observeEvent(input$am_max,{
    #     if(as.numeric(input$am_max) != input$am_range[2]){
    #         updateSliderInput(
    #             session = session,
    #             inputId = 'am_range',
    #             value = c(input$am_min, input$am_max)
    #         ) # updateSliderInput
    #     }#if
    #     
    #     
    # })
    
    # observeEvent(input$am_range,{
    #     if(as.numeric(input$am_min) != input$am_range[1] | as.numeric(input$am_max) != input$am_range[2]){
    #         updateTextInput(
    #             session = session,
    #             inputId = 'am_min',
    #             value = input$am_range[1]
    #         )
    #         
    #         updateTextInput(
    #             session = session,
    #             inputId = 'am_max',
    #             value = input$am_range[2]
    #         ) # updateTextInput
    #         
    #     }#if
    #     
    # })
    
    observeEvent(input$search,{
      search_data <- data.frame(term=input$search, date=Sys.Date())
    })
    
    output$topic_table <- function() {
        knitr::kable(tf_table, format="html", escape=F) %>%
        column_spec(2, width_max = "200em; display: inline-block;") %>%
        kable_styling("striped", full_width = F) %>%
        scroll_box(width = "100%", height = "600px")
    }
    
    # observeEvent(input$toggle, {
    #     toggle("topic_table")
    #     # toggle("plot") if you want to alternate between hiding and showing
    # })
    
    observe({
        toggle(id = "topic_table", condition = input$toggle)
    })
    
    observeEvent(input$resetAll, {
      reset("settings")
    })
    
    observeEvent(input$invert,{
      checked <- input$preprint_topics
      unchecked <- setdiff(1:20, checked)
      
      updateCheckboxGroupInput(
        session = session,
        inputId = 'preprint_topics',
        selected = unchecked
        # value = c(input$am_min, input$am_max)
      ) # updateSliderInput
    })
    
    observeEvent(input$select_all,{
      # checked <- input$preprint_topics
      # unchecked <- setdiff(1:20, checked)
      
      updateCheckboxGroupInput(
        session = session,
        inputId = 'preprint_topics',
        selected = c(1:10)
        # value = c(input$am_min, input$am_max)
      ) # updateSliderInput
    })
    
    observeEvent(input$deselect_all,{
      # checked <- input$preprint_topics
      # unchecked <- setdiff(1:20, checked)
      
      updateCheckboxGroupInput(
        session = session,
        inputId = 'preprint_topics',
        selected = c(NA)
        # value = c(input$am_min, input$am_max)
      ) # updateSliderInput
    })
    
    plotdat <- reactive({
      
        search <- unlist(strsplit(tolower(input$search), " "))
      
      
        filtered_df <- ncov_df_topics_am %>% 
            # mutate(score_range=ceiling(log10(as.numeric(score)+1))) %>%
            # mutate(score_range=recode(score_range, `1`="<10", `2`="10-99", `3`="100-999", `4`="1,000+", .default=NA_character_)) %>%
            dplyr::filter(date %within% interval(input$date_range[1], input$date_range[2]) &
                              topic_group %in% input$preprint_topics &
                              # (all(stringr::str_detect(title, unlist(strsplit(input$search, " ")))) | all(stringr::str_detect(abstract, unlist(strsplit(input$search, " ")))) | all(stringr::str_detect(authors, unlist(strsplit(input$search, " ")))) ) )
                              (grepl(paste0("(?=.*", search, ")", collapse=""), tolower(title), perl=TRUE) | 
                                 grepl(paste0("(?=.*", search, ")", collapse=""), tolower(authors), perl=TRUE) | 
                                 grepl(paste0("(?=.*", search, ")", collapse=""), tolower(abstract), perl=TRUE)))
        # 
        # if(input$ntile<0){
        #   filtered_df <- filtered_df %>%
        #     dplyr::filter(pub_ntile<=input$ntile)
        # } else if (input$ntile>0){
        #   filtered_df <- filtered_df %>%
        #     dplyr::filter(pub_ntile>=input$ntile)
        # }
        
        # if(input$ht_only==TRUE){
        #   filtered_df <- filtered_df %>%
        #     dplyr::filter(as.numeric(cited_by_tweeters_count) > 100)
        # }
        
        return(filtered_df)
        
    })
    
    output$plotdat <- DT::renderDataTable(plotdat() %>%
                                              # dplyr::select(abs=abstract, topic=topic_group, date=date, title=title, journal=site, link, images.small) %>%
                                              dplyr::select(topic=topic_group, date=date, title=title, journal=server, url) %>%
                                              # dplyr::filter(!is.na(topic)) %>%
                                              mutate(topic=cell_spec(topic, "html", 
                                                                     color="black", align = "c",
                                                                     background=cols[topic])) %>%
                                              mutate(journal=cell_spec(tolower(journal), "html", 
                                                                     color="white", align = "c",
                                                                     background=ifelse(journal=="biorxiv", "#bc2635", "#0e4c92"))) %>%
                                              # mutate(score = cell_spec(
                                              #   round(as.numeric(score), 1), color = "white", bold = T, align="c",
                                              #   background = spec_color(log(as.numeric(score)), end = 1, option = "D", direction = -1))) %>%
                                            mutate(title=paste0("<a href='", url,"' target='_blank'>", title,"</a>")) %>%
                                              dplyr::select(-c(url)) %>%
                                              arrange(desc(date)), server = FALSE, escape=FALSE, rownames=F,
                                          
                                          # options=list(columnDefs = list(list(visible=FALSE, targets=c(0)))),
                                          # 
                                          # callback = JS("
                                          #       /* code for columns on hover */
                                          #       table.on('mouseenter', 'td', function() {
                                          #           var td = $(this);
                                          #           var info_out = table.cell( this ).index().columnVisible;
                                          #           Shiny.onInputChange('hoverIndexJS', info_out);
                                          #       });
                                          #       /* code for cell content on click */
                                          #       table.on('click', 'td', function() {
                                          #          var td = $(this);
                                          #          var info_out = table.cell( this ).data();
                                          #          Shiny.onInputChange('clickIndexJS', info_out);
                                          #       });
                                          #       /* code for columns on doubleclick */
                                          #       table.on('dblclick', 'td', function() {
                                          #           var td = $(this);
                                          #           var info_out = table.cell( this ).index().row;
                                          #           Shiny.onInputChange('dblclickIndexJS', info_out);
                                          #       });"
                                          #               
                                          # )
                                          # 
                                          )
    
    output$distPlot <- renderPlotly({

        # generate bins based on input$bins from ui.R
        # x    <- faithful[, 2]
        # bins <- seq(min(x), max(x), length.out = input$bins + 1)
        # if(input$date == "all"){
        #     plotdat <- ncov_df_topics_am
        # } else {
        #     plotdat <- ncov_df_topics_am %>%
        #         dplyr::filter(months(date) == input$date)
        # }
        # 
        # if(input$am_range == "all"){
        #     plotdat <- ncov_df_topics_am
        # }
        
        # plotdat <- ncov_df_topics_am %>% 
        #     # mutate(score_range=ceiling(log10(as.numeric(score)+1))) %>%
        #     # mutate(score_range=recode(score_range, `1`="<10", `2`="10-99", `3`="100-999", `4`="1,000+", .default=NA_character_)) %>%
        #     dplyr::filter(date %within% interval(input$date_range[1], input$date_range[2]) &
        #                       as.numeric(score) >= input$am_range[1] &
        #                       as.numeric(score) <= input$am_range[2] &
        #                       (grepl(tolower(input$search), tolower(title)) | grepl(tolower(input$search), tolower(authors)) | grepl(tolower(input$search), tolower(abstract)))
        #                   )

        # draw the histogram with the specified number of bins
        # hist(x, breaks = bins, col = 'darkgray', border = 'white')
        plot_embedding_scatter(plotdat(), input$method, cols)

    })
    

    output$downloadData <- downloadHandler(
      filename <- function() {
        paste("covid19_preprints_20200619", "txt", sep=".")
      },
      
      content <- function(con) {
        file.copy("covid19_preprints_20200619.txt", con)
      }
    )
    

})
