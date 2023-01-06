

clean_txt <- function(url){
  pdf_url <- paste0(url, ".full.pdf")
  pdf_txt <- try(pdf_text(pdf_url))
  if(!inherits(pdf_txt, "try-error")){
    return(str_squish(paste0(pdf_txt, collapse = " ")))
  } else {
    return("")
  }
}

plot_embedding_scatter <- function(plotdat, method, cols){

  if(method=="umap"){
    plotdat <- plotdat %>% 
      mutate(X1=U1, X2=U2)  
    
    label_coords <- plotdat %>%
      group_by(topic_group, topic) %>%
      summarise(X1=median(U1), X2=median(U2))
    
  } else if(method=="tsne") {
    plotdat <- plotdat %>% 
      mutate(X1=V1, X2=V2)  
    
    label_coords <- plotdat %>%
      group_by(topic_group, topic) %>%
      summarise(X1=median(V1), X2=median(V2))
  }
  
  p <- ggplot()+
    geom_point(data=plotdat, aes(x=X1, y=X2, label=title, colour=topic), alpha=0.6)+
    geom_text(data=label_coords, aes(x=X1, y=X2, colour=topic, label=topic_group), size=10, alpha=0.8)+
    scale_colour_manual(values=cols, drop=FALSE)+
    scale_fill_manual(values=cols, drop=FALSE)+
    theme_classic()+
    theme(legend.position="none")
  
  ply <- plotly_build(p) %>%
    layout(
      hoverlabel=list(
        opacity=0.5,
        font_size=16
      )
    )
  # rangeslider()
  # layout(
  #   updatemenus = list(
  #     list(
  #       y = 0.8,
  #       buttons = list(
  #         
  #         list(method = "restyle",
  #              args = list("type", "scatter"),
  #              label = "Scatter"),
  #         
  #         list(method = "restyle",
  #              args = list("type", "histogram2d"),
  #              label = "2D Histogram")))
  #     )
  # )
  
  
  
  ply$layout$height = 1200
  ply$layout$width = 1200
  
  # Clickable points link to profile URL using onRender: https://stackoverflow.com/questions/51681079
  # for(i in 1:nrow(plotdat)){
  #   ply$x$data[[i]]$customdata <- paste0("https://dx.doi.org/", plotdat$doi)
  # }
  
  for(i in 1:length(ply$x$data)){
    # query_preprint <- unique(gsub(".*title: ", "", ply$x$data[[i]]$text))
    #   
    # ply$x$data[[i]]$customdata <- paste0("https://dx.doi.org/", plotdat[plotdat$title==query_preprint,]$doi)
    # ply$x$data[[i]]$customdata <- paste0(gsub("cgi/content/short", "content/10.1101", plotdat[plotdat$topic_group==i,]$rel_link), ".full.pdf")
    ply$x$data[[i]]$customdata <- paste0("https://dx.doi.org/", plotdat[plotdat$topic_group==i,]$doi)
  }
  
  # ply$x$data[[1]]$customdata <- paste0(gsub("cgi/content/short", "content/10.1101", plotdat$rel_link), ".full.pdf")
  
  #pp  <- add_markers(pp, customdata = ~url)
  plyout <- onRender(ply, "
                     function(el, x) {
                     el.on('plotly_click', function(d) {
                     var url = d.points[0].customdata;
                     //url
                     window.open(url);
                     });
                     }
                     ")
  
  plyout
}