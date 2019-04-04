#####################################
# NLP on top 5,000 YouTube channels #
#####################################

packages <- c("tidyverse", "tidytext", "tm", "topicmodels")
lapply(packages, require, character.only=T)

# Get YouTube top 5,000 dataset (https://www.kaggle.com/mdhrumil/top-5000-youtube-channels-data-from-socialblade#data.csv)
path <- "C:\\Users\\Eva\\Rstudio"
yt_data <- read_csv(paste0(path, "./data.csv"))
names(yt_data) <- str_replace_all(names(yt_data), c(" " = "_"))

############## Unsupervised Learning ##############

topwords_LDA <- function(var, numtopics){
  corpus <- Corpus(VectorSource(var))
  dtm <- DocumentTermMatrix(corpus)
  dtm <- dtm[unique(dtm$i),]
  
  lda <- LDA(dtm, numtopics)
  topics <- tidy(lda, matrix="beta")
  
  topwords <- topics  %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)

  topwords %>%
    mutate(term=reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill=factor(topic))) +
    geom_col(show.legend=F) +
    facet_wrap(~ topic, scales="free") +
    labs(x=NULL, y="Beta") +
    coord_flip()
}

# YouTube channel name: top ten words by topic
topwords_LDA(yt_data$Channel_name, numtopics=4)
