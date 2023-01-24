library(tidyverse)
# library(pdftools)
library(stringr)
library(tidytext)
library(topicmodels)
library(ldatuning)
library(tm)
library(qlcMatrix)
library(randomcoloR)
library(ggridges)
library(gganimate)
library(htmlwidgets)
library(plotly)
library(Rtsne)
library(umap)
library(rAltmetric)
library(kableExtra)
library(rvest)

set.seed(20200409)
n_topics <- 10
force_read <- FALSE
topic_cols <- readRDS("color_palette.rds")


# corpus_cached <- readRDS("ms_preprint_corpus.rds")
preprint_metadata_cached <- readRDS("ms_preprint_metadata_cached.rds")
# preprint_metadata <- readRDS("ms_preprint_metadata_cached.rds")

preprint_metadata <- data.frame()

# biorxiv
# for(i in 0:51){
for(i in 0:1){
        results <- paste0("https://www.biorxiv.org/search/%2522multiple%252Bsclerosis%2522%20numresults:75%20sort:publication-date%20direction:descending?page=", i)
        dois_it <- read_html(results) %>% 
            html_elements("span.highwire-cite-metadata-doi.highwire-cite-metadata") %>% 
            html_text2() %>%
            gsub("doi: ", "", .) %>%
            gsub("https://doi.org/", "", .)
        
        for(doi in dois_it){
          doi_json <- rjson::fromJSON(file=paste0("https://api.biorxiv.org/details/biorxiv/", doi))
          
          if(doi_json$messages[[1]]$status != "no posts found"){
            doi_df <- do.call(rbind, lapply(doi_json$collection, as_tibble)) %>% 
              nest(authors=c(authors)) %>%
              dplyr::select(-authors) %>%
              mutate(date=as.Date(date)) %>%
              arrange(date) %>% 
              mutate(url=paste0("https://www.biorxiv.org/content/", doi, ".full"))
            preprint_metadata <- bind_rows(preprint_metadata, doi_df)
          } else { next }
        }
        
}

# medrxiv
# for(i in 0:17){
for(i in 0:1){
    results <- paste0("https://www.medrxiv.org/search/%2522multiple%252Bsclerosis%2522%20numresults:75%20sort:publication-date%20direction:descending?page=", i)
    dois_it <- read_html(results) %>% 
        html_elements("span.highwire-cite-metadata-doi.highwire-cite-metadata") %>% 
        html_text2() %>%
        gsub("doi: ", "", .) %>%
        gsub("https://doi.org/", "", .)
    
    for(doi in dois_it){
        doi_json <- rjson::fromJSON(file=paste0("https://api.biorxiv.org/details/medrxiv/", doi))
        
        if(doi_json$messages[[1]]$status != "no posts found"){
          doi_df <- do.call(rbind, lapply(doi_json$collection, as_tibble)) %>% 
              nest(authors=c(authors)) %>%
              dplyr::select(-authors) %>%
              mutate(date=as.Date(date)) %>%
              arrange(date) %>% 
              mutate(url=paste0("https://www.medrxiv.org/content/", doi, ".full"))
          preprint_metadata <- bind_rows(preprint_metadata, doi_df)
        } else { next }
    }
    
}

# get latest version and add full-text URL
preprint_metadata <- preprint_metadata %>% 
    group_by(doi) %>% 
    filter(version == max(version)) %>%
    mutate(url=paste0("https://www.", server, ".org/content/", doi, "v", version, ".full")) %>%
    dplyr::filter(!(doi %in% preprint_metadata_cached$doi))

preprint_metadata <- bind_rows(preprint_metadata, preprint_metadata_cached)

saveRDS(preprint_metadata, "ms_preprint_metadata_cached.rds")

# function to pull full text HTML
clean_txt_html <- function(url){
  tryCatch(download.file(url, destfile = "scrapedpage.html", quiet=TRUE), error=function(e) return(""))

  tryCatch(paper_txt <- read_html("scrapedpage.html") %>% 
                        html_elements(".article.fulltext-view") %>% 
                        html_text2()
      , error=function(e) return(""))
  tryCatch(str_squish(paste0(paper_txt, collapse = " ")), error=function(e) return(""))
}

# load cached corpus
corpus_cached <- readRDS("ms_preprint_corpus_full.rds")

# preprint_metadata_old <- preprint_metadata[(preprint_metadata$doi %in% corpus_cached$doi),]
# 
# preprint_metadata_old_doc <- preprint_metadata_old %>%
#   left_join(corpus_cached)

# exclude papers already cached
preprint_metadata_new <- preprint_metadata[!(preprint_metadata$doi %in% corpus_cached$doi),]

preprint_metadata_new_doc <- preprint_metadata_new %>%
  rowwise() %>%
  mutate(doc = clean_txt_html(url))

# ms_preprint_corpus_full <- bind_rows(preprint_metadata_new_doc, preprint_metadata_old_doc) %>%
#   ungroup()

ms_preprint_corpus_full <- bind_rows(preprint_metadata_new_doc, corpus_cached) %>%
  ungroup()

saveRDS(ms_preprint_corpus_full, "ms_preprint_corpus_full.rds")

# get full text as list
# corpus_new_list <- lapply(preprint_metadata_new$url, clean_txt_html)
# 
# unscraped <- preprint_metadata_new %>%
#   dplyr::filter()
# 
# corpus_new_list <- lapply(preprint_metadata_new$url, clean_txt_html)
# 
# doi_str_new <- preprint_metadata_new$doi
# 
# corpus_new <- data.frame(doi=preprint_metadata_new$doi, 
#                          doc=unlist(corpus_new_list), stringsAsFactors = F) %>% 
#     as_tibble()
# 
# corpus_merged <- bind_rows(corpus_cached, corpus_new)
# 
# # save corpus to disk
# saveRDS(corpus_merged, "ms_preprint_corpus_full.rds")

# subset to preprints only with "multiple sclerosis" in title or abstract
on_topic_preprints <- ms_preprint_corpus_full %>%
  dplyr::filter(grepl("multiple sclerosis", tolower(abstract)) | grepl("multiple sclerosis", tolower(title)))




custom_stopwords <- c("preprint", "doi", "doi.org", "medrxiv", "biorxiv", 
                      "https", "10.1101", "et", "al", "fig", "figure", "abstract",
                      "results", "methods", "discussion", "openurlcrossrefpubmed",
                      "openurlcrossrefpubmedweb", "openurl", "openurlabstract")

# tokenize + generate word counts, dropping stopwords
# txt_tokenized <- corpus_merged %>%
txt_tokenized <- on_topic_preprints %>%
    # dplyr::select(doi, doc=abstract) %>%
  dplyr::select(doi, doc) %>%
    unnest_tokens(word, doc, strip_punct=TRUE)

word_counts <- txt_tokenized %>%
    dplyr::count(doi, word, sort = TRUE) %>%
    anti_join(stop_words) %>%
    dplyr::filter(!(word %in% custom_stopwords)) %>%
    dplyr::filter(n>=10) %>%
    dplyr::filter(!grepl('[[:digit:]]+', word))

txt_dtm <- word_counts %>%
    mutate(doi=as.character(doi)) %>%
    cast_dtm(doi, word, n)

# run lda model
lda_model <- LDA(txt_dtm, k = n_topics, control = list(alpha=0.1, seed = 5678), method="VEM")
lda_model_tidy <- tidy(lda_model)

top_terms <- lda_model_tidy %>%
    group_by(topic) %>%
    top_n(30, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)

topics_terms <- top_terms %>% 
    dplyr::select(-beta) %>% 
    # mutate(topic=paste0("t", topic)) %>%
    group_by(topic) %>% 
    summarise(top_10=paste(term, collapse=", ")) %>% 
    ungroup()

topics_terms_levels <- paste0(topics_terms$topic, ": ", topics_terms$top_10)

lda_gamma <- tidy(lda_model, matrix = "gamma") %>%
    rowwise() %>%
    mutate(gamma=gamma+runif(1,0,0.0001))


# calculate pairwise distances based on LDA gammas
gamma_matrix <- lda_gamma %>% 
  pivot_wider(id_cols=document, names_from=topic, values_from=gamma) %>% 
  as.matrix()

rownames(gamma_matrix) <- gamma_matrix[,1]
gamma_matrix <- gamma_matrix[,-1]

d1 <- dist(gamma_matrix)

dist_tidy <- tidy(d1)

dist_top5 <- dist_tidy %>%
  group_by(item1) %>%
  top_n(-5) %>%
  dplyr::select(item1, doi=item2, distance) %>%
  nest(neighbors=c(doi, distance)) %>%
  rename(doi=item1)


  

docs_order <- lda_gamma %>%
    group_by(document) %>%
    arrange(topic, -gamma) %>%
    top_n(1, gamma) %>%
    dplyr::rename(topic_group = topic) %>%
    dplyr::select(-gamma)

lda_gamma_wide <- lda_gamma %>% spread(topic, gamma, sep="") 

tsne <- Rtsne(lda_gamma_wide[,-1], dims = 2, perplexity=50, verbose=TRUE, max_iter = 1000, num_threads=6)
tsne_df <- cbind(doi=lda_gamma_wide$document, tsne$Y %>% as.data.frame())

umap <- umap(lda_gamma_wide[,-1], method="umap-learn", metric="hellinger")
umap_df <- cbind(doi=lda_gamma_wide$document, umap$layout %>% as.data.frame()) %>%
    dplyr::rename(U1=V1, U2=V2)

# top topic per user
lda_gammas <- lda_gamma %>%
    ungroup() %>%
    mutate(document=factor(document, levels=docs_order$document)) %>%
    left_join(topics_terms, by="topic") %>%
    mutate(topic=paste0(topic, ": ", top_10)) %>%
    mutate(topic=factor(topic, levels=topics_terms_levels)) %>%
    group_by(document) %>%
    arrange(topic, -gamma) %>%
    top_n(1, gamma) %>%
    dplyr::rename(account=document)

lda_gammas_count <- lda_gamma %>% 
    group_by(topic) %>% 
    summarise(n=sum(gamma)) %>%
    left_join(topics_terms, by="topic") %>%
    mutate(topic=paste0(topic, ": ", top_10)) %>%
    mutate(topic=factor(topic, levels=topics_terms_levels)) %>%
    ungroup() %>%
    mutate(pct=n/sum(n)) %>%
    dplyr::select(topic, top_10, n, pct)

tf_table <- lda_gammas_count %>% 
    mutate(topic=paste0("topic", gsub(":.*", "", topic))) %>%
    dplyr::select(topic, top_terms=top_10, n_users=n, pct_total=pct) %>%
    ungroup() %>%
    # full_join(acad_topics2, by="topic") %>%
    mutate(topic_lab=topic) %>%
    # mutate(topic=factor(topic, levels=unique(lda_gammas_count$topic))) %>%
    # arrange(topic_lab) %>%
    # mutate(topic=factor(topic, levels=paste0("topic", 1:12))) %>%
    # full_join(match_scores) %>%
    mutate(topic=cell_spec(topic, "html", 
                           color="black", align = "c",
                           background=c(topic_cols[as.numeric(gsub("topic", "", topic))]))) %>%
    # mutate(topic=ifelse(topic_lab %in% topic_ids, paste0(topic, "🎓"), topic)) %>%
    # dplyr::select(-c(topic_lab, pct, target_cat, score_wt, score_wt2)) %>%
    dplyr::select(topic, top_terms, n_users, pct_total) %>%
    mutate(n_users=round(n_users),
           pct_total=round(pct_total, 3)) %>%
    dplyr::rename("Number of preprints (estimated)" = "n_users",
                  "Top 30 Terms" = "top_terms", 
                  "Fraction of total corpus" = "pct_total")

saveRDS(tf_table, "ms_tf_table.rds")

ms_df_topics <- left_join(ms_preprint_corpus_full %>%
                            select(-doc), 
                          docs_order %>% 
                            select(doi=document, topic_group)) %>% 
    left_join(dist_top5, by="doi") %>%
    mutate(date = as.Date(date)) %>%
    dplyr::filter(!is.na(topic_group)) %>%
    left_join(tsne_df, by="doi") %>%
    left_join(umap_df, by="doi") %>%
    unite("authors", tidyselect::starts_with("auth"), sep=", ", na.rm=T) %>%
    left_join(topics_terms %>% dplyr::select(topic_group=topic, top_10), by="topic_group") %>%
    mutate(topic=paste0(topic_group, ": ", top_10)) %>%
    as_tibble()

saveRDS(ms_df_topics, "ms_df_topics.rds")


# on_topic_dois_old <- ms_df_topics2 %>%
#   dplyr::filter(grepl("multiple sclerosis", tolower(abstract))) %>%
#   pull(doi)
