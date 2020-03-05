# PART.1 - Import and merge the data ------
rm(list=ls())
setwd("D:\\Preslava\\uni\\Business Analytics\\Boryana Saturday\\TEDx")
ddmain=read.csv("ted_main.csv",na.strings = c(""," ","NA"), stringsAsFactors = FALSE)
dd=read.csv("transcripts.csv",na.strings = c(""," ","NA"), stringsAsFactors = FALSE)

# 1. Summarize variables class for each data.frame
mainclass=data.frame(names(ddmain),rapply(ddmain,class))
colnames(mainclass)=c("Varname","Varclass")
(length(unique(ddmain$name))) #[1] 2550
(length(unique(ddmain$url))) #[1] 2550

ddclass=data.frame(names(dd),rapply(dd,class))
colnames(ddclass)=c("Varname","Varclass")
(length(unique(dd$url))) #[1] 2464 # There are duplicates
dd$url[duplicated(dd$url)==T]
# [1] "https://www.ted.com/talks/jonathan_haidt_humanity_s_stairway_to_self_transcendence\n"
# [2] "https://www.ted.com/talks/rob_reid_the_8_billion_ipod\n"     
# [3] "https://www.ted.com/talks/brene_brown_listening_to_shame\n" 
(which(duplicated(dd$url)==T)) #[1] 1115 1116 1117

# 2. Remove duplicates
dd=dd[-(which(duplicated(dd$url)==T)),]

# 3. Identify matches and differences
url=list()
url$both=intersect(ddmain$url,dd$url)
(length(url$both)) #[1] 2464
url$ddmain=setdiff(ddmain$url,dd$url)
(length(url$ddmain)) #[1] 86

# 4. Merge data.frames
dd=merge(dd,ddmain,by="url")

# PART.2 - Data preparation------
# 1. Fixing the dates
dd$film_date<-as.POSIXct(dd$film_date,origin='1970-01-01')
dd$film_date<-as.Date(dd$film_date)
dd$published_date<-as.POSIXct(dd$published_date, origin='1970-01-01')
dd$published_date<-as.Date(dd$published_date)

#We will work with a predefined period of time in order to have realistic picture of the number and the rating of the videos
dd=dd[dd$published_date>="2010-01-01"&dd$published_date<="2016-12-31",]
max(dd$published_date)
min(dd$published_date)

# 2. Cleaning text from different symbols,capital letters etc.- requires text analytics libraries
#install.packages("sentimentr")
#install.packages("stringr")
#install.packages("SnowballC")
#install.packages("tm")
library(sentimentr)
library(stringr)
library(SnowballC)
library(quanteda)
library(tm)
dd$transcript=gsub("\\(|\\)|'"," ",dd$transcript)
dd$transcript=gsub("[[:digit:]]", "", dd$transcript) # cleans numbers
dd$transcript=gsub("http\\w+", "", dd$transcript) # cleans web references
dd$transcript=gsub("[ \t]{2,}", "", dd$transcript) # cleans 2 or more tabs
dd$transcript=gsub("^\\s+|\\s+$", "", dd$transcript) # cleans 1 or more
dd$transcript=tolower(dd$transcript) # convert all upper cases to lower cases

# 3. Finding out most popular themes
perfect_tag <- function(x){
  x <- unlist(strsplit(x, "'"))
  val = x[2]
  for (i in 3:length(x))
    if (nchar(x[i]) >2)
      val <- c(val, x[i])
  return (val)
  
}
# 4. Arranging them into dataframe
dd$processed_tags <-  lapply(dd$tags, perfect_tag)
processed_tags <- dd$processed_tags
length(processed_tags)
processed_tags <- unlist(processed_tags, recursive=FALSE)
length(processed_tags)
processed_tags <- as.data.frame(table(processed_tags))
#install.packages("dplyr")
library(dplyr)
processed_tags <- arrange(processed_tags, desc(Freq))
head(processed_tags, 10) #the most popular video tags

# PART.3 - Top results------
# 1. Top 10 most popular themes for Ted Talk based on hashtags
#install.packages("ggplot2")
library(ggplot2)
windows()
processed_tags$processed_tags <- as.character(processed_tags$processed_tags)
processed_tags$processed_tags <- factor(processed_tags$processed_tags, levels=processed_tags$processed_tags)
p15 <- ggplot(data = head(processed_tags, 10), aes(processed_tags, Freq, fill = processed_tags)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = Freq), vjust = 1.6, color = "white", size = 3) +
  ggtitle("Most popular themes of Ted Talks")
p15

# 2. Top 10 most popular talks based on comments
ten_talks <- arrange(dd, comments)
keeps <- c("title", "main_speaker", "views", "comments")
ten_talks <- subset(ten_talks, select = keeps)
# 3. Top 10 least commented videos
last_10_talks <- ten_talks[0:10,]
last_10_talks[1,]
# PART.4 - Finalizing data sets------
# 1. Creating a list of top 10 Ted talks
ten_talks <- arrange(dd, desc(comments))
ten_talks <- subset(ten_talks, select = keeps)
top_10_talks <- ten_talks[0:10,]
top_10_talks
windows()
p8 <- ggplot(data = top_10_talks, aes(main_speaker, comments, fill = comments)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = comments), vjust = 1.6, color = "white", size = 3) +
  ggtitle("Talks with most number of comments") 
p8

# 2. Top 10 talks based on views
ten_talks_view <- arrange(dd, views)
keeps <- c("title", "main_speaker", "views", "comments")
ten_talks_view <- subset(ten_talks_view, select = keeps)
ten_talks_view <- arrange(ten_talks_view, desc(views))
ten_talks_view <- ten_talks_view[0:10,]
ten_talks_view[1,]
windows()
p9 <- ggplot(data = ten_talks_view, aes(main_speaker, views, fill = views)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = views), vjust = 1.6, color = "white", size = 3) +
  ggtitle("Talks with most number of views") 
p9

# 3. Top talks with largest amount of discussion (ratio - number of comments/number of views)
library(dplyr)
dd$dis_quo <- dd$comments/dd$views
ten_talks_r <- arrange(dd, desc(dis_quo))
keeps <- c("title", "main_speaker", "views", "comments", "dis_quo")
ten_talks_r <- subset(ten_talks_r, select = keeps)
top_10_talks_r <- ten_talks_r[0:10,]
top_10_talks_r[1,]
windows()
p10 <- ggplot(data = top_10_talks_r, aes(main_speaker, dis_quo, fill = dis_quo)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = comments), vjust = 1.6, color = "white", size = 3) +
  ggtitle("Talks with the biggest ratio comments/views") 
p10

# PART.5 - Main analysis for most commented video "Your body language may shape who you are"------
top_video=dd[dd$title=="Your body language may shape who you are",]
# 1. Sentiment analysis on video "Your body language may shape who you are"
top_tr=top_video$transcript
#install.packages("tm")
library(tm)
top_tr<-removeWords(top_tr,stopwords("en"))
top_tr<-unlist(top_tr)
top_tr_which<-unlist(top_tr)
top_tr_which<-sentiment(top_tr_which)
windows()
plot(top_tr_which)
top_tr_words<-sentiment(get_sentences(top_tr),
                        polarity_dt=lexicon::hash_sentiment_jockers_rinker,
                        valence_shifters_dt=2,
                        neutral.nonverb.like = TRUE)
windows()
plot(top_tr_words$sentence_id,top_tr_words$sentiment,type="l",xlab="sentence",
     ylab="sentiment",lwd=2,col="darkgreen") # plotting the curve
words1=extract_sentiment_terms(top_tr,polarity_dt=lexicon::hash_sentiment_jockers_rinker,
                               hyphen="") # most used words
emotions1=extract_emotion_terms(top_tr,emotion_dt=lexicon::hash_nrc_emotions,
                                un.as.negation=TRUE) # most common emotions
# 2. Do's and Don'ts lists
dos1=unlist(words1$positive)
dos1[!duplicated(dos1)]
unique(dos1) #80 words

dont1=unlist(words1$negative)
dont1[!duplicated(dont1)]
unique(dont1) #63 words
# 3. WorldCloud for Do's
library(wordcloud)
windows()
set.seed(1234)
wordcloud(words=dos1,min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
# 4.Emotions
happy1=unlist(emotions1$joy)
happy1[!duplicated(happy1)]
# 5. WorldCloud for emotions
windows()
set.seed(1234)
wordcloud(words=happy1,min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# PART.6 - Main analysis for video "E-voting without fraud" with highest ratio comment/views------
top_ratio=top_10_talks_r[top_10_talks_r$dis_quo==max(top_10_talks_r$dis_quo),]
top_ratio=dd[dd$title=="E-voting without fraud",]
# 1. Sentiment analysis on video "E-voting without fraud"
top_ratio_tr=top_ratio$transcript
#install.packages("tm")
library(tm)
top_ratio_tr<-removeWords(top_ratio_tr,stopwords("en"))
top_ratio_tr<-unlist(top_ratio_tr)
top_ratio_tr_which<-unlist(top_ratio_tr)
top_ratio_tr_which<-sentiment(top_ratio_tr_which)
windows()
plot(top_ratio_tr_which)
top_ratio_words<-sentiment(get_sentences(top_ratio_tr),
                        polarity_dt=lexicon::hash_sentiment_jockers_rinker,
                        valence_shifters_dt=2,
                        neutral.nonverb.like = TRUE)
windows()
plot(top_ratio_words$sentence_id,top_ratio_words$sentiment,type="l",xlab="sentence",
     ylab="sentiment",lwd=2,col="red") # plotting the curve
words2=extract_sentiment_terms(top_ratio_tr,polarity_dt=lexicon::hash_sentiment_jockers_rinker,
                               hyphen="") # most used words
emotions2=extract_emotion_terms(top_ratio_tr,emotion_dt=lexicon::hash_nrc_emotions,
                                un.as.negation=TRUE) # most common emotions
# 2. Do's and Don'ts lists
dos2=unlist(words2$positive)
dos2[!duplicated(dos2)]
unique(dos2) #26 words

dont2=unlist(words2$negative)
dont2[!duplicated(dont2)]
unique(dont2) #20 words
# 3. WorldCloud for Do's
windows()
set.seed(1234)
wordcloud(words=dos2,min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
# 4.Emotions
happy2=unlist(emotions2$joy)
happy2[!duplicated(happy2)]
# 5. WorldCloud for emotions
windows()
set.seed(1234)
wordcloud(words=happy2,min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# PART.7 - Main analysis for most watched video of Brene Brown "The power of vulnerability------
unique(dd$main_speaker)
BB=dd[dd$main_speaker=="Brené Brown",]
BB=BB[BB$views==max(BB$views),]
# 1. Sentiment analysis on video "The power of vulnerability"
BB_tr=BB$transcript
BB_tr<-removeWords(BB_tr,stopwords("en"))
BB_words<-unlist(BB_tr)
BB_words_which<-unlist(BB_tr)
BB_words_which<-sentiment(BB_words_which)
windows()
plot(BB_words_which)
BB_words<-sentiment(get_sentences(BB_tr),
                           polarity_dt=lexicon::hash_sentiment_jockers_rinker,
                           valence_shifters_dt=2,
                           neutral.nonverb.like = TRUE)
windows()
plot(BB_words$sentence_id,BB_words$sentiment,type="l",xlab="sentence",
     ylab="sentiment",lwd=2,col="blue") # plotting the curve
words3=extract_sentiment_terms(BB_tr,polarity_dt=lexicon::hash_sentiment_jockers_rinker,
                               hyphen="") # most used words
emotions3=extract_emotion_terms(BB_tr,emotion_dt=lexicon::hash_nrc_emotions,
                                un.as.negation=TRUE) # most common emotions
# 2. Do's and Don'ts lists
dos3=unlist(words3$positive)
dos3[!duplicated(dos3)]
unique(dos3) #95 words

dont3=unlist(words3$negative)
dont3[!duplicated(dont3)]
unique(dont3) #71 words
# 3. WorldCloud for Do's
windows()
set.seed(1234)
wordcloud(words=dos3,min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
# 4.Emotions
happy3=unlist(emotions3$joy)
happy3[!duplicated(happy3)]
# 5. WorldCloud for emotions
windows()
set.seed(1234)
wordcloud(words=happy3,min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Part.8 - Video ratings------
rt=list()
for (i in 1:nrow(dd)){
  rating = unlist(str_extract_all(dd$ratings[i], "\\{[^{}]+\\}"))
  rating=substring(rating, 2, nchar(rating)-1)
  rating=strsplit(rating, split="\\, |\\: ")
  rt[[i]]=data.frame(name=as.character(sapply(rating,"[",4)),count=as.numeric(sapply(rating,"[",6)))
  rt[[i]]$name=gsub("'","",rt[[i]]$name)
  rt[[i]]=rt[[i]][order(rt[[i]]$name),]
  rm(rating)
}
dd[,21:(21+14-1)]=NA
colnames(dd)[21:ncol(dd)]=rt[[1]]$name
for (j in 1:14){
  for (i in 1:nrow(dd)){
    dd[i,(20+j)]=rt[[i]]$count[j]
  }
}

# Part.9 - Main analysis for the funniest video "This is what happens when you reply to spam email"------
funniest_video=dd[dd$Funny==max(dd$Funny),]
funniest_tr=funniest_video$transcript
# 1. Sentiment analysis on the video "This is what happens when you reply to spam email"
library(tm)
funniest_tr<-removeWords(funniest_tr,stopwords("en"))
funniest_words<-unlist(funniest_tr)
funniest_words_which<-unlist(funniest_tr)
funniest_tr_which<-sentiment(funniest_words_which)
windows()
plot(funniest_tr_which)
funniest_words<-sentiment(get_sentences(funniest_tr),
                    polarity_dt=lexicon::hash_sentiment_jockers_rinker,
                    valence_shifters_dt=2,
                    neutral.nonverb.like = TRUE)
windows()
plot(funniest_words$sentence_id,funniest_words$sentiment,type="l",xlab="sentence",
     ylab="sentiment",lwd=2,col="black") # plotting the curve
words4=extract_sentiment_terms(funniest_tr,polarity_dt=lexicon::hash_sentiment_jockers_rinker,
                               hyphen="") # most used words
emotions4=extract_emotion_terms(funniest_tr,emotion_dt=lexicon::hash_nrc_emotions,
                                un.as.negation=TRUE) # most common emotions
# 2. Do's and Don'ts lists
dos4=unlist(words4$positive)
dos4[!duplicated(dos4)]
unique(dos4) #49 words

dont4=unlist(words4$negative)
dont4[!duplicated(dont4)]
unique(dont4) #22 words
# 3. WorldCloud for Do's
windows()
set.seed(1234)
wordcloud(words=dos4,min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
# 4.Emotions
happy4=unlist(emotions4$joy)
happy4[!duplicated(happy4)]
# 5. WorldCloud for emotions
windows()
set.seed(1234)
wordcloud(words=happy4,min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# PART.10 - Main analysis for the most unconvincing video "Enough with the fear of fat"------
worst_video=dd[dd$Unconvincing==max(dd$Unconvincing),]
# 1. Sentiment analaysis on video "Enough with the fear of fat"
worst_tr=worst_video$transcript
library(tm)
worst_tr<-removeWords(worst_tr,stopwords("en"))
worst_words<-unlist(worst_tr)
worst_words_which<-unlist(worst_tr) 
worst_words_which<-sentiment(worst_words_which)
windows()
plot(worst_words_which)
worst_words<-sentiment(get_sentences(worst_tr),
                          polarity_dt=lexicon::hash_sentiment_jockers_rinker,
                          valence_shifters_dt=2,
                          neutral.nonverb.like = TRUE)
windows()
plot(worst_words$sentence_id,worst_words$sentiment,type="l",xlab="sentence",
     ylab="sentiment",lwd=2,col="purple") # plotting the curve
words5=extract_sentiment_terms(worst_tr,polarity_dt=lexicon::hash_sentiment_jockers_rinker,
                               hyphen="") # most used words
emotions5=extract_emotion_terms(worst_tr,emotion_dt=lexicon::hash_nrc_emotions,
                                un.as.negation=TRUE) # most common emotions
# 2. Do's and Don'ts lists
dos5=unlist(words5$positive)
dos5[!duplicated(dos5)]
unique(dos5) #97 words

dont5=unlist(words5$negative)
dont5[!duplicated(dont5)]
unique(dont5) #71 words
# 3. WorldCloud for Do's
windows()
set.seed(1234)
wordcloud(words=dos5,min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
# 4.Emotions
happy5=unlist(emotions5$joy)
happy5[!duplicated(happy5)]
# 5. WorldCloud for emotions
windows()
set.seed(1234)
wordcloud(words=happy5,min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# PART.11 - Final analysis------
# 1. Finding common words
all_words1 <- paste(words1, collapse = "")
all_words2 <- paste(words2, collapse = "")
all_words3 <- paste(words3, collapse = "")
all_words4 <- paste(words4, collapse = "")
all_words5 <- paste(words5, collapse = "")
all=c(all_words1,all_words2,all_words3,all_words4,all_words5)
all_vector=VectorSource(all)
all_corpus=VCorpus(all_vector)
# 2. Clean_corpus() function
#install.packages("quanteda")
library(quanteda)
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, 
                   c(stopwords("en"),"character"," ␔"))
  return(corpus)
}
all_clean <- clean_corpus(all_corpus)
all_tdm <- TermDocumentMatrix(all_clean)
all_m <- as.matrix(all_tdm)
# 3. Making commonalitiy and comparison cloud
windows()
commonality.cloud(all_m,comonality.measure=min,max.words=300,colors="steelblue1")
windows()
comparison.cloud(all_m,colors = c("orange", "blue", "green","red","black"),max.words = 100)
# 4. All dos and donts words
library(dplyr)
alldos=c(dos1,dos2,dos3,dos4,dos5)
length(alldos) #736
alldos=as.data.frame(table(alldos))
alldos=arrange(alldos, desc(Freq)) #266
dododo=head(alldos, 10) #the most powerful do-words

alldonts=c(dont1,dont2,dont3,dont4,dont5)
length(alldonts) #419
alldonts=as.data.frame(table(alldonts))
alldonts=arrange(alldonts, desc(Freq)) #212
nonono=head(alldonts, 10) #the most powerful don't-words
# 5. Comparison cloud for words
top10df=data.frame(x = dododo$alldos,y = nonono$alldonts)
allwords=VCorpus(VectorSource(top10df[,1:2]))
allwords=TermDocumentMatrix(allwords)
colnames(allwords)=c("do's","don'ts")
allwords=as.matrix(allwords)
windows()
comparison.cloud(allwords,colors=c("green","red"),max.words=100)
