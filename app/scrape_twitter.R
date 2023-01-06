ncov_df_topics_am <- ncov_df_topics_am %>%
    mutate(article_full_url = paste0("https://", rel_site, ".altmetric.com/details/", altmetric_id))


events_from_altmetric <- function(article_full_url){
    summary_page <- read_html(article_full_url)
    twitter_url <- paste0(article_full_url, "/twitter")
    twitter_page <- read_html(twitter_url)
    
    # number of pages is the ceiling of total tweets/100
    totals <- twitter_page %>% html_nodes("div.text strong") %>% html_text()
    npages <- ceiling(as.integer(totals[1])/100)
    
    # loop through pages of tweets on altmetric to get handles of tweeting users
    events <- data.frame()
    for(page in 1:npages){
        # url <- paste0(article_base_url, id, "/twitter/page:", page)
        page_url <- paste0(twitter_url, "/page:", page)
        page <- read_html(page_url)
        
        screen_name <- gsub("@", "", html_nodes(page, "div.handle") %>% html_text())
        
        status <- gsub(".*tweet_id=", "", html_nodes(page, "a.favorite") %>% 
                           html_attr("href"))
        
        timestamp <- html_nodes(page, "time") %>% html_attrs() %>% unlist()
        
        events <- bind_rows(events, data.frame(screen_name, timestamp, status, stringsAsFactors = F))
    }
    
    handles <- unique(events$screen_name)
    
    skip_langs <- c("ar", "ja", "zh-CN", "ko")
    
    user_data <- try(lookup_users(handles, token=token1))
    
    tweets <- try(lookup_tweets(events$status, token=token1))
    
    if(!inherits(user_data, "try-error") & !inherits(tweets, "try-error") & "screen_name" %in% names(tweets)){
        
        user_data <- user_data %>%
            dplyr::filter(protected==FALSE)
        
        tweets <- tweets %>%
            dplyr::select(screen_name, 
                          tweets=text, 
                          retweet_screen_name, 
                          status=status_id)
        
        out <- left_join(events, tweets, by = c("screen_name", "status")) %>%
            left_join(user_data, by = c("screen_name")) %>%
            as_tibble()
    } else {
        out <- tibble()
    }
    
    return(tryCatch(out, error=function(e) NULL))
}

library(rtweet)

keys <- yaml.load_file("../config.yaml")

token1 <- create_token(
    app = keys$app_name,
    consumer_key = keys$consumer_key,
    consumer_secret = keys$consumer_secret,
    access_token=keys$access_token,
    access_secret=keys$access_secret,
    set_renv = FALSE)

ncov_df_topics_am_filtered <- ncov_df_topics_am %>%
    dplyr::filter(cited_by_tweeters_count >= 20)

events <- list()

# for(url in ncov_df_topics_am_filtered$article_full_url){}

events <- lapply(ncov_df_topics_am_filtered$article_full_url, events_from_altmetric)

