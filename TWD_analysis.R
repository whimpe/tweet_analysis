##################################################################
############### Analyzing for The Walking Dead ###################
##################################################################


setwd("C:\\Users\\Willem Himpe\\OneDrive\\UGENT - NOW\\SOCIAL MEDIA EN WEB ANALYSIS\\group_assignments")
rm(list=ls())


if (!require("pacman")) install.packages("pacman") ; require("pacman")
p_load(rtweet, httr,tidyverse)
p_load(tm,textclean, topicmodels, tidytext) #wordcloud, tm,
if (!require("devtools")){
    install.packages("devtools") 
    devtools::install_github("hadley/emo")
}else {require("devtools")}


library(dplyr)
library(data.table)
library(stringr)
library(emo)
library(textstem)



######################
### Importing data ###
######################


# load the tweets
load("TWD_2020-02-29.RData")
load("TWD_2020-03-11.RData")
load("TWD_2020-03-15.RData")
load('TWD_2020-03-20.RData')

dictionary <- read_csv("dictionary.csv")
wordlist <- load(file="wordListSpelling.Rdata")


# tweets %>% as.data.frame %>% arrange(created_at) %>% select(created_at) %>% tail
# tweets %>% as.data.frame %>% arrange(created_at) %>% select(created_at) %>% head
################################
###### extend  dictionary ######
################################

create_neg_dictionary <- function(dictionary){
    dictionary$VALENCE <- abs(dictionary$VALENCE)-5
    dictionary$AROUSAL <- NULL
    dictionary$DOMINANCE <- NULL
    
    #1) Put not in front and new valence scores
    dictionary_new <- dictionary[,c('Word','VALENCE')]
    dictionary_new$Word <- paste('not', dictionary$Word)
    dictionary_new$VALENCE <- ((dictionary_new$VALENCE)*(-1))
    
    #2)rbind the new and the old dictionary
    final_dict <- rbind(dictionary,dictionary_new)
    return(final_dict)
} #creates a new dictionary with not and full
dictionary <- create_neg_dictionary(dictionary)

#################################
#### Preprocessing functions ####
################################

calculate_mean_max_median <- function(datatable, column, drop_median = FALSE){
    datatable$col_to_analyze <- datatable[[column]] # quickfix
    
    out <- datatable %>% group_by(period_before_episode) %>% summarise(mean = mean(col_to_analyze, na.rm = TRUE), 
                                                                       max = max(col_to_analyze), 
                                                                       median = median(col_to_analyze, na.rm = TRUE))
    out$mean_lagg <- lag(out$mean, n = 1)
    out$max_lagg <- lag(out$max, n = 1)
    out$median_lagg <- lag(out$median, n = 1)
    
    #standaardizeren van die waarde
    out[paste0("mean_", column, "_ratio")] <- out$mean / out$mean_lagg
    out[paste0("max_", column, "_ratio")] <- out$max / out$max_lagg
    out[paste0("median_", column, "_ratio")] <- out$median / out$median_lagg
    
    if(drop_median == FALSE){
        out <- out %>% select(period_before_episode, paste0("mean_", column, "_ratio"), paste0("max_", column, "_ratio"), paste0("median_", column, "_ratio")) # median is always 0, so we can skip it (checked in summarize step)
    } else {
        out <- out %>% select(period_before_episode, paste0("mean_", column, "_ratio"), paste0("max_", column, "_ratio"))
    }
    
    return(out)
} ### Define function to calculate mean, max and median 

calculate_perc <- function(datatable, column){
    datatable$column_to_analyze <- datatable[[column]]
    
    out <- datatable %>% group_by(period_before_episode) %>% summarise(perc = sum(column_to_analyze) / length(column_to_analyze))
    out$perc_lagg <- lag(out$perc, n = 1)
    out[paste0(column, "_perc_ratio")] <- out$perc / out$perc_lagg
    
    (out <- out %>% select(period_before_episode, paste0(column, "_perc_ratio")))
} #calculates a percentage of how many is true -- > for boolean columns




clean_text <- function(text,emoji){
    if(!emoji){
        text <- replace_emoji(x = text)
        text <- replace_emoticon(x =text)
    }else{
        text <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", text)
    }
    
    text<- tolower(text)
    text <-  gsub("[[:punct:]]","", text) #substitue whitespace with empty string ""
    text <-  gsub("[[:digit:]]","", text) #substitue numbers with empty string ""
    text <- stripWhitespace(text)
    
    return(text)
}   #replaces emoji to text, removes punctuation marks, numbers, large whitespace

correct_spelling <- function(string) { 
    words <- strsplit(string," ")
    words_corrected <- "" 
    for(i in 1:lengths(words)){
        # How dissimilar is this word from all words in the wordlist?
        edit_dist <- adist(words[[1]][[i]], wordlist)
        # Is there a word that reasonably similar? 
        # If yes, which ones?
        # Select the first result (because wordlist is sorted 
        # from most common to least common)
        # If no, append the original word
        words_corrected <- paste(words_corrected,(c(wordlist[edit_dist <= min(edit_dist,2)],words[[1]][[i]])[1]), sep = " ")
        
    }
    return(words_corrected)
} #replaces a word-vector with the correct spelling word vector


improve_text <- function(text, correct){  
    text <- tolower(text)
    text <- replace_contraction(text)
    text <- replace_internet_slang(text)
    text <- replace_word_elongation(text)
    text <- replace_symbol(text)
    text <- replace_rating(text)
    text <- replace_grade(text)
    text <- replace_email(text)
    text <- replace_date(text)
    
    
    if(correct){
        text <- correct_spelling(text)
    }
    
    return(text)
} #transforms to lower ,remove afkortingen,slang, verlengingen, create words for symbols, grades,emails,dates,ratings


count_emoji <- function(text){
    count <- ji_count(text) 
    
    return(count)
} #creates integer that returns amount of emoji in text

#alleen nodig als test voor make_corpus
unique_word_count <- function(data){
    content <- character(length(data))
    if (any(class(data) %in% c("VCorpus", "Corpus","SimpleCorpus"))) {
        for (i in 1:length(data)) content[i] <- data[[i]]$content
    } else {
        content <- data
    }
    uniquewords <- unique(unlist(sapply(
        strsplit(as.character(content)," "),
        unique)))
    length(uniquewords)
}
make_corpus <- function(column_of_text){
    
    tweets_corpus <- Corpus(VectorSource(column_of_text))
    print(unique_word_count(tweets_corpus))
    
    tweets_corpus <- tm_map(tweets_corpus, content_transformer(tolower))
    print(unique_word_count(tweets_corpus))
    
    tweets_corpus <- tm_map(tweets_corpus,   removePunctuation)
    print(unique_word_count(tweets_corpus))
    
    tweets_corpus <- tm_map(tweets_corpus,  removeNumbers)
    print(unique_word_count(tweets_corpus))
    
    forremoval <- stopwords('english')
    forremoval <- c(forremoval,"TWD","twd","thewalkingdead","dead","walking",",","https")
    
    tweets_corpus <-  tm_map(tweets_corpus,  removeWords,  forremoval)
    print(unique_word_count(tweets_corpus))
    
    tweets_corpus <-  tm_map(tweets_corpus,  stripWhitespace)
    print(unique_word_count(tweets_corpus))
    
    print(as.character(tweets_corpus[[20]]))
    
    
    return(tweets_corpus$content)
}


extract_hastags <- function(text){
    text<- str_extract_all(text, "(?<![a-zA-Z0-9])#(\\w+)\\S+" )
    
    return(text)
}  #returns a list with all the hashtags (overbodig)
extract_mentions <- function(text){
    text<- str_extract_all(text, "(?<![a-zA-Z0-9])@(\\w+)\\S+" )
    
    return(text)
} #returns a list with all the mentions (overbodig)

stemming_corpus <- function(column){
    column <- stem_strings(column)
    return(column)
}  #set the different words to a basis word consultancy,consulting --> consult
lemmatize_words <- function(column){
    column <- lemmatize_strings(column)
    return(column)
}  # combine words to one :  carrot, broccoli,..  -> vegetable

make_document_by_term <-function(column, sparseness_allowed_pct=NULL, weighting="tf-idf" ){
    corpus_of_column <- Corpus(VectorSource(column))
    
    if(weighting == "frequency"){
        dtm <- DocumentTermMatrix(corpus_of_column, 
                                  control =list(wordLengths = c(2, Inf),
                                                weighting=function(x) weightTf(x)) )
    }else if(weighting == "tf-idf"){
        dtm <- DocumentTermMatrix(corpus_of_column, 
                                  control =list(wordLengths = c(2, Inf),
                                                weighting=function(x) weightTfIdf(x)) )
    }else{
        print("please specify which weighting to use")
    }
    
    #if sparseness_allowed_pct is not specified no sparse terms are removed
    if(!is.null(sparseness_allowed_pct)){
        dtm <- removeSparseTerms(dtm, sparse= sparseness_allowed_pct)
    }
    return(dtm)
}

determine_best_number_of_topics <- function(column,sparseness_allowed_pct = NULL, Max_K = 20){
    dtm_frequency <- make_document_by_term(column, sparseness_allowed_pct = sparseness_allowed_pct, weighting = "frequency")
    
    ldas <- list()
    j <- 0
    for (i in 2:Max_K) {
        j <- j+1
        print(i)
        #We set a seed for the LDA algorithm such that the results are predictable and comparable
        #This uses the VEM optimization algorithm as defined by the inventor (Blei)
        #You can also choose to perform Gibbs sampling (method option)
        ldas[[j]] <- LDA(x = dtm_frequency, k = i, control = list(seed = 5))
    }
    
    AICs <- data.frame(k = 2:Max_K, aic = sapply(ldas, AIC))
    K <- AICs$k[which.min(AICs$aic)]
    
    plot(x= AICs$k, y= AICs$aic)
    
    return(K)
} # run through full data set to get the perfect amount of topics, returns perfect k

make_topic_model <- function(column,K , sparseness_allowed_pct = NULL){
    dtm_frequency <- make_document_by_term(column, sparseness_allowed_pct = sparseness_allowed_pct, weighting = "frequency")
    
    lda_best <- LDA(x = dtm_frequency, k = K, control = list(seed = 5))
    
    return(lda_best)
}

get_topic_per_document <- function(column,best_K, display = TRUE){
    
    
    topic_model <- make_topic_model(column,best_K)
    
    topic_term <- tidy(topic_model, matrix = 'gamma')
    
    highest_gamma <- aggregate(topic_term$gamma,FUN = max, by =list(topic_term$document), simplify= TRUE)
    
    documents_and_topic <- merge(x = topic_term, y= highest_gamma, by.x =c("gamma","document"), by.y =c("x","Group.1") )
    
    documents_and_topic$document <- as.numeric(documents_and_topic$document)
    
    documents_and_topic <- documents_and_topic[order(documents_and_topic$document),]
    #display the content of the topics
    if(display){
        topic_term_beta <- tidy(topic_model, matrix = 'beta')
        
        top_terms <- topic_term_beta %>%
            group_by(topic) %>%
            top_n(10, beta) %>%
            ungroup() %>%
            arrange(topic, desc(beta))
        top_terms
        
        show(top_terms %>%
                 mutate(term = reorder(term, beta)) %>%
                 ggplot(aes(term, beta, fill = factor(topic))) +
                 geom_col(show.legend = FALSE) +
                 facet_wrap(~ topic, scales = "free") +
                 coord_flip())
    }
    
    return(documents_and_topic)
    
}

determine_sentiment <- function(text){
    
    Encoding(text) <- "latin1"
    
    text <- iconv(text,'latin1', 'ascii', sub = '')
    
    
    tweetsplit <- strsplit(text,split=" ")[[1]]
    
    
    #find the positions of the words in the Tweet in the dictionary
    m <- match(tweetsplit, dictionary$Word)
    
    #which words are present in the dictionary?
    present <- !is.na(m)
    #tweetsplit[present]
    
    #of the words that are present, select their valence
    wordvalences <- dictionary$VALENCE[m[present]]
    
    #compute the mean valence of the tweet
    scoretweet <- mean(wordvalences, na.rm=TRUE)
    
    #handle the case when none of the words is in the dictionary
    if (is.na(scoretweet)) scoretweet <- 0 else scoretweet <- scoretweet
    
    return(scoretweet)
} #returns sentiment score  between -4

create_transformed_text <- function(subset_tweets){
    
    random_tw_text <- subset_tweets[,c(1:5)]
    random_tw_text$hashtags <- subset_tweets[["hashtags"]]
    random_tw_text$mentions <- subset_tweets[["mentions_screen_name"]]
    
    #FUNCTIES DOORVOEREN OP RANDOM SAMPLE VOOR OPTIMAL TOPICS AANTAL
    random_tw_text$cleaned_text <- apply(random_tw_text[,5], 1, clean_text,emoji = TRUE)
    random_tw_text$cleaned_text_without_emojis <- apply(random_tw_text[,5], 1, clean_text,emoji = FALSE)
    
    random_tw_text$amount_hashtags <- rapply(random_tw_text[,6], length)
    random_tw_text$amount_mentions <- rapply(random_tw_text[,7],function(x){ if(!is.na(x)) length(x) else (0)})
    
    
    random_tw_text$nbr_emoji <- apply(random_tw_text[,5], 1, count_emoji)
    
    # next step we will alter the text to correct spelling, remove slang, alongations and abbreviations,to lower.
    random_tw_text$improved_text <- apply(random_tw_text[,8], 1, improve_text, correct = FALSE)
    
    #remove stopwords
    random_tw_text$text_without_stop <- make_corpus(random_tw_text$cleaned_text)
    
    #stemm and lemmatize
    random_tw_text$stemmed <- stemming_corpus(random_tw_text$text_without_stop)
    random_tw_text$lemmatized <- lemmatize_words(random_tw_text$text_without_stop)
    
    #Determine the sentiment
    random_tw_text$sentiment <- apply(as.matrix(random_tw_text$text_without_stop), 1, determine_sentiment)
    
    return(random_tw_text)
} #returns basetable with counts+sentiment_analysis

get_mean_variance <-function(basetable,columns_in_vec){
    index = 1
    avg<-c(1:4)
    vari <-c(1:4)
    for(i in columns_in_vec){
        avg[index]<-(sum(basetable[[i]])/dim(basetable)[1])
        index = index+1
    }
    index=1
    for(i in columns_in_vec){
        vari[index] <-var(basetable[[i]])
        index=index+1
    }
    
    opl <-rbind(avg,vari)
    colnames(opl) <-columns_in_vec
    return(opl)
} #returns the average and variance values of the columns_in_vec argument


#######################
#### Preprocessing ####
#######################


tweets_to_merge <- list(tweets, tweets_2020_03_11, tweets_2020_03_15, tweets_2020_03_20)
all_tweets <- rbind(tweets, tweets_2020_03_11, tweets_2020_03_15, tweets_2020_03_20)
all_tweets <- all_tweets[!duplicated(all_tweets$status_id),]
all_tweets<- all_tweets %>% arrange(created_at)

# Define the following episodes :
# Episode 1  --> February 23, 2020
# Episode 2  --> March 1, 2020
# Episode 3  --> March 8, 2020
# Episode 4  --> March 15, 2020
# Episode 5  --> March 22, 2020
# Episode 6  --> March 29, 2020

# Create an indicator for the period before each episode 
all_tweets$period_before_episode <- ifelse(all_tweets$created_at <= as.POSIXlt("2020-02-24 03:00:00"), 1,   # POSIXlt puts time in CET, created_at is in UTC
                                           ifelse(all_tweets$created_at <= as.POSIXlt("2020-03-1 03:00:00"), 2, 
                                                  ifelse(all_tweets$created_at <= as.POSIXlt("2020-03-8 02:00:00"), 3,
                                                         ifelse(all_tweets$created_at <= as.POSIXlt("2020-03-15 02:00:00"), 4,
                                                                ifelse(all_tweets$created_at <= as.POSIXlt("2020-03-22 02:00:00"), 5,
                                                                       ifelse(all_tweets$created_at <= as.POSIXlt("2020-03-29 03:00:00"), 6, 0))))))

unique(all_tweets$period_before_episode)
all_tweets <- as.data.table(all_tweets)



#creation of subsets
tweets_ep1 <- all_tweets[all_tweets$period_before_episode==1,]
tweets_ep2 <- all_tweets[all_tweets$period_before_episode==2,]
tweets_ep3 <- all_tweets[all_tweets$period_before_episode==3,]
tweets_ep4 <- all_tweets[all_tweets$period_before_episode==4,]
tweets_ep5 <- all_tweets[all_tweets$period_before_episode==5,]
tweets_ep6 <- all_tweets[all_tweets$period_before_episode==6,]

random_tweets_index <- sample(1:dim(all_tweets)[1], size=2000)
random_tweets <- all_tweets[random_tweets_index,]

#TOPICMODELING

#dtm_frequency <- make_document_by_term(random_tw_text$text_without_stop, sparseness_allowed_pct = 0.97, weighting = "frequency")
"dtm <- make_document_by_term(random_tw_text$text_without_stop)

# Determine which number of topics is best for full data set---> 3 
best_k <-  determine_best_number_of_topics(column = random_tw_text[,14],sparseness_allowed_pct = 0.88,Max_K = 5)
##### MOET JE NIET MEER RUNNEN OVER HEEL DE DATA SET IS 3 TOPICS HET MEEST REPRESENTATIEF

# Get a table with per document the highest matching topic and display the topics
topics_per_document <- get_topic_per_document(random_tw_text[,14],best_K = 3)
"


#############################################################################





# 1. number of posts of current period divided by number of posts previous period (hoge waarde= current period veel meer tweets)

number_of_posts <- all_tweets %>% group_by(period_before_episode) %>% summarise(cnt = n())
number_of_posts$lag <- lag(number_of_posts$cnt, n = 1)
number_of_posts$number_of_posts_ratio <- number_of_posts$cnt / number_of_posts$lag
number_of_posts <- number_of_posts %>% select(period_before_episode, number_of_posts_ratio)

# 2. Variance in time between posts
all_tweets <- as.data.table(all_tweets)   # set as data table
all_tweets[, created_at_lagg:=c(NA, created_at[-.N]), by=period_before_episode]   # create lagged variable for each group 
all_tweets$timediff_posts <- as.integer(all_tweets$created_at - as.POSIXlt(all_tweets$created_at_lagg, origin = "1970-01-01"))  # timedifference in seconds

var_timediff_posts = all_tweets %>% group_by(period_before_episode) %>% summarise(timediff_variance = var(timediff_posts, na.rm = TRUE)) # get variance

var_timediff_posts$lag <- lag(var_timediff_posts$timediff_variance, n = 1)  # lagg the variance one period 
var_timediff_posts$timediff_var_ratio <- var_timediff_posts$timediff_variance / var_timediff_posts$lag # compute the ratio: variance of current period divided by the variance of previous period
# var_timediff_posts$timediff_var_ratio[is.na(var_timediff_posts$timediff_var_ratio)] <- 0  # for the first period, we don't know the variance ratio

var_timediff_posts <- var_timediff_posts %>% select(period_before_episode, timediff_var_ratio) # only keep the period_before_episode ID and the timediff_var_ratio

# 3+4 Average,Variance sentiment posts ,numbr_#,nmbr_@, nbr_emoji

m_v_1 <- get_mean_variance(create_transformed_text(tweets_ep1),columns_in_vec = c("amount_hashtags","amount_mentions","nbr_emoji","sentiment"))
m_v_2  <- get_mean_variance(create_transformed_text(tweets_ep2),columns_in_vec = c("amount_hashtags","amount_mentions","nbr_emoji","sentiment"))
m_v_3 <- get_mean_variance(create_transformed_text(tweets_ep3),columns_in_vec = c("amount_hashtags","amount_mentions","nbr_emoji","sentiment"))
m_v_4 <- get_mean_variance(create_transformed_text(tweets_ep4),columns_in_vec = c("amount_hashtags","amount_mentions","nbr_emoji","sentiment"))
m_v_5 <- get_mean_variance(create_transformed_text(tweets_ep5),columns_in_vec = c("amount_hashtags","amount_mentions","nbr_emoji","sentiment"))
m_v_6 <- get_mean_variance(create_transformed_text(tweets_ep6),columns_in_vec = c("amount_hashtags","amount_mentions","nbr_emoji","sentiment"))
avg_var_sent <- rbind(m_v_1,m_v_2,m_v_3,m_v_4,m_v_5)

df <- matrix(1:45,nrow=5)
df[1,]<-NA 
df[1,1]<-1
df[2,2:5] = avg_var_sent[1,]
df[3,2:5] = avg_var_sent[3,]
df[4,2:5] = avg_var_sent[5,]
df[5,2:5] = avg_var_sent[7,]
df[2,6:9] = avg_var_sent[2,]
df[3,6:9] = avg_var_sent[4,]
df[4,6:9] = avg_var_sent[6,]
df[5,6:9] = avg_var_sent[8,]
colnames(df) <- c("period_before_episode","amount_hashtags_avg","amount_mentions_avg","nbr_emoji_avg","sentiment_avg","amount_hashtags_var","amount_mentions_var","nbr_emoji_var","sentiment_var")
avgerag_variance_sentim.hash.ment.nbr_emo <-df



# 5. Number of unique people who tweeted 
nr_unique_tweeters_dt <- all_tweets %>% group_by(period_before_episode) %>% summarise(nr_unique_tweeters = n_distinct(screen_name))
nr_unique_tweeters_dt$lagg <- lag(nr_unique_tweeters_dt$nr_unique_tweeters, n = 1)
nr_unique_tweeters_dt$nr_unique_tweeters_ratio <- nr_unique_tweeters_dt$nr_unique_tweeters / nr_unique_tweeters_dt$lagg
(nr_unique_tweeters_dt <- nr_unique_tweeters_dt %>% select(period_before_episode, nr_unique_tweeters_ratio))

# 6. Number of different sources used
nr_unique_sources_dt <- all_tweets %>% group_by(period_before_episode) %>% summarise(nr_unique_sources = n_distinct(source))
nr_unique_sources_dt$lagg <- lag(nr_unique_sources_dt$nr_unique_sources, n = 1)
nr_unique_sources_dt$nr_unique_sources_ratio <- nr_unique_sources_dt$nr_unique_sources / nr_unique_sources_dt$lagg
(nr_unique_sources_dt <- nr_unique_sources_dt %>% select(period_before_episode, nr_unique_sources_ratio))

# 7. Favorite_count (number of times the post is liked) --> mean and max
favorite_count_dt <- calculate_mean_max_median(all_tweets, "favorite_count", drop_median = TRUE)


# 8. Retweet_count --> mean and max
retweet_count_dt <- calculate_mean_max_median(all_tweets, "retweet_count", drop_median = TRUE)


# 9. is_quote --> percentage 
quote_perc_dt <- calculate_perc(all_tweets, "is_quote")

# 10. hashtags --> mean and max 
all_tweets$hashtag_count <- stringr::str_count(all_tweets$hashtags, ",")+1 # calculate the number of hashtags, if there is no comma, then the hashtag was only #TWD
hashtag_count_dt <- calculate_mean_max_median(all_tweets, "hashtag_count")

# 11. urls_url --> percentage of posts with an url present
all_tweets$urls_url_present <- !is.na(all_tweets$urls_url)
urls_url_perc_dt <- calculate_perc(all_tweets, "urls_url_present")


# 12. media_url --> percentage of posts with a media url present
all_tweets$media_url_present <- !is.na(all_tweets$media_url)
media_url_perc_dt <- calculate_perc(all_tweets, "media_url_present")

# 13. Followers_count --> mean and max 

followers_count_dt <- calculate_mean_max_median(all_tweets, "followers_count", drop_median = TRUE)

# 14. friends_count --> mean and max 
friends_count_dt <- calculate_mean_max_median(all_tweets, "friends_count", drop_median = TRUE)

# 15. listed_count  --> mean and max
listed_count_dt <- calculate_mean_max_median(all_tweets, "listed_count", drop_median = TRUE)




# merge all the stuff 
basetable <- Reduce(merge,list(number_of_posts,
                               var_timediff_posts,
                               nr_unique_tweeters_dt, 
                               nr_unique_sources_dt, 
                               favorite_count_dt, 
                               retweet_count_dt,
                               quote_perc_dt, 
                               hashtag_count_dt, 
                               urls_url_perc_dt,
                               media_url_perc_dt, 
                               followers_count_dt, 
                               friends_count_dt,
                               listed_count_dt,
                               avgerag_variance_sentim.hash.ment.nbr_emo))



