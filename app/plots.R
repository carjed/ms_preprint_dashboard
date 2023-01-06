
# topic barplots
lda_gamma2 <- left_join(lda_gamma, ncov_df %>% dplyr::select(rel_title, document=rel_doi)) %>% 
    mutate(document=paste0(rel_title, " [doi:", document, "]"))

docs_order2 <- lda_gamma2 %>%
    group_by(document) %>%
    arrange(topic, -gamma) %>%
    top_n(1, gamma) %>%
    rename(topic_group = topic) %>%
    dplyr::select(-gamma)

plot_embedding_bars <- function(plotdat, docs_order){
    
    plotdat <- plotdat %>%
        mutate(topic_lab=paste0("topic", topic)) %>%
        ungroup() %>%
        mutate(document=factor(document, levels=docs_order$document)) %>%
        left_join(topics_terms, by="topic") %>%
        mutate(topic=paste0(topic, ": ", top_10)) # %>%
    # mutate(topic=ifelse(topic_lab %in% topic_ids, paste0("🎓", topic), topic)) #%>%
    
    # mutate(urls=paste0("https://twitter.com/", document))
    
    p <- plotdat %>%
        mutate(topic=factor(topic, levels=unique(plotdat$topic))) %>%
        ggplot(aes(x=document, y=gamma, fill=topic))+
        geom_bar(stat="identity", position="stack")+
        scale_fill_manual(values=cols)+
        scale_y_continuous(expand = c(0,0))+
        scale_x_discrete(position = "top")+ 
        xlab("APreprint")+
        ylab("Topic Fraction")+
        theme(legend.position="right",
              axis.title.y=element_blank(),
              axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())+
        guides(fill=guide_legend(ncol=1))
    
    ply <- plotly_build(p) %>%
        layout(legend = list(orientation = "v",   # show entries horizontally
                             xanchor = "center",  # use center of legend as anchor
                             yanchor = "bottom",
                             x = 0, y=-1.5))
    #
    ply$layout$height = 1800
    ply$layout$width = 900
    
    # Clickable points link to profile URL using onRender: https://stackoverflow.com/questions/51681079
    for(i in 1:20){
        ply$x$data[[i]]$customdata <- paste0("https://dx.doi.org/", docs_order$document)
    }
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


lda_bars <- plot_embedding_bars(lda_gamma2, docs_order2)

# date distributions by topic
date_dist <- ggplot(ncov_df_topics_am %>%
                        mutate(topic_group=factor(topic_group, levels=seq(1:20))) %>%
                        # mutate(topic_group=as.character(str_pad(topic_group, 2, "0", side = "left"))) %>%
                        arrange(topic_group),
                    aes(x = rel_date, y = fct_rev(topic_group), fill=topic_group)) +
    geom_density_ridges(jittered_points=T) +
    scale_fill_manual(values=cols)+
    theme_classic()+
    theme(legend.position="none")+
    xlab("Date Posted")+
    ylab("Topic")


# altmetric score distributions by topic
am_dist <- ggplot(ncov_df_topics_am %>%
                      mutate(topic_group=factor(topic_group, levels=seq(1:20))), 
                  aes(x = as.numeric(score), y = fct_rev(topic_group), fill=topic_group)) +
    geom_density_ridges(jittered_points=T) +
    scale_fill_manual(values=cols)+
    scale_x_log10()+
    theme_classic()+
    theme(legend.position="none")+
    xlab("Altmetric Attention Score")+
    ylab("Topic")

# tSNE plot

label_coords <- ncov_df_topics_am %>% group_by(topic_group) %>% summarise(V1=mean(V1), V2=mean(V2))
label_coords_umap <- ncov_df_topics_am %>% group_by(topic_group) %>% summarise(U1=mean(U1), U2=mean(U2))

tsne_scatter <- ggplot()+
    geom_point(data=ncov_df_topics_am, aes(x=V1, y=V2, colour=factor(topic_group), size=as.numeric(score)), alpha=0.6)+
    geom_label(data=label_coords, aes(x=V1, y=V2, label=topic_group, fill=factor(topic_group)), alpha=0.8)+
    scale_colour_manual(values=cols)+
    scale_fill_manual(values=cols)+
    theme_classic()+
    theme(legend.position="none")

umap_scatter <- ggplot()+
    geom_point(data=ncov_df_topics_am, aes(x=U1, y=U2, colour=factor(topic_group), size=as.numeric(score)), alpha=0.6)+
    geom_label(data=label_coords_umap, aes(x=U1, y=U2, label=topic_group, fill=factor(topic_group)), alpha=0.8)+
    scale_colour_manual(values=cols)+
    scale_fill_manual(values=cols)+
    theme_classic()+
    theme(legend.position="none")


plot_embedding_scatter <- function(plotdat, method){
    plotdat <- plotdat %>%
        mutate(altmetric_score=as.numeric(score)) %>%
        mutate(topic=factor(topic, levels=str_sort(unique(ncov_df_topics_am$topic), numeric=T)))
    
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
        geom_point(data=plotdat, aes(x=X1, y=X2, size=altmetric_score, label=title, colour=topic), alpha=0.6)+
        geom_text(data=label_coords, aes(x=X1, y=X2, colour=topic, label=topic_group), size=10, alpha=0.8)+
        scale_colour_manual(values=cols, drop=F)+
        scale_fill_manual(values=cols, drop=F)+
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
    #   ply$x$data[[i]]$customdata <- paste0("https://dx.doi.org/", plotdat$rel_doi)
    # }
    
    for(i in 1:length(ply$x$data)){
        # query_preprint <- unique(gsub(".*title: ", "", ply$x$data[[i]]$text))
        #   
        # ply$x$data[[i]]$customdata <- paste0("https://dx.doi.org/", plotdat[plotdat$title==query_preprint,]$rel_doi)
        # ply$x$data[[i]]$customdata <- paste0(gsub("cgi/content/short", "content/10.1101", plotdat[plotdat$topic_group==i,]$rel_link), ".full.pdf")
        ply$x$data[[i]]$customdata <- paste0("https://dx.doi.org/", plotdat[plotdat$topic_group==i,]$rel_doi)
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

plot_embedding_scatter(ncov_df_topics_am, "umap")

tsne_animate <- ggplot(ncov_df_topics_am %>% mutate(week=strftime(rel_date, format = "%V")), aes(x=V1, y=V2, colour=factor(topic_group)))+
    geom_point(size=3, alpha=0.8)+
    # geom_label(data=label_coords, aes(x=V1, y=V2, label=topic_group, fill=factor(topic_group)), alpha=0.8)+
    scale_colour_manual(values=cols)+
    scale_fill_manual(values=cols)+
    theme_classic()+
    theme(legend.position="none")+
    transition_manual(week, cumulative = TRUE)+
    labs(title = 'Weeks since first preprint: {frame-1}')
#transition_time(rel_date)

umap_animate <- ggplot(ncov_df_topics_am %>% mutate(week=strftime(rel_date, format = "%V")), aes(x=U1, y=U2, colour=factor(topic_group)))+
    geom_point(size=3, alpha=0.8)+
    # geom_label(data=label_coords, aes(x=V1, y=V2, label=topic_group, fill=factor(topic_group)), alpha=0.8)+
    scale_colour_manual(values=cols)+
    scale_fill_manual(values=cols)+
    theme_classic()+
    theme(legend.position="none")+
    transition_manual(week, cumulative = TRUE)+
    labs(title = 'Weeks since first preprint: {frame-1}')

# transition_states(rel_date,
#                   transition_length = 1,
#                   state_length = 1)+
#   shadow_mark()#+
#   # ease_aes('linear')





# knitr::kable(tf_table, format="html", escape=F) %>%
#   column_spec(2, width_max = "200em; display: inline-block;") %>%
#   kable_styling("striped", full_width = F) %>%
#   scroll_box(width = "100%", height = "600px")
