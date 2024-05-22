#데이터 수집

library(rvest); library(tidyverse)

all_reviews <- NULL
url_base <- "https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=38444&type=after&onlyActualPointYn=N&order=newest&page="

for (i in 48:499) { #(499-48)*10 = 리뷰 4510건 수집
  newr <- NULL
  url <- paste(url_base, i, sep='')
  txt <- readLines(url, encoding="UTF-8", warn=FALSE)
  
  reviews <- txt[which(str_detect(txt, "id=\"_filtered_ment"))+4]
  reviews <- gsub("<.+?>|\t","", reviews)
  
  newr <- cbind(reviews)
  all_reviews <- rbind(all_reviews, newr)
}

write.table(all_reviews, "C:\\Temp\\movie_review.txt")

#기본분석

library(plyr); library(KoNLP); library(dplyr); library(biztextp)
library(tidytext); library(textdata); library(stringr)
useSejongDic()
options(max.print = 20000)
review <- readLines("C:\\Temp\\movie_review.txt")
review <- str_replace_all(review,"\\W"," ") 
review <- gsub("\\d+","",review) 
review <- gsub("\\n+","",review) 
review <- gsub("[A-z]","",review)
review <- gsub("영화","",review) 

nouns <- extractNoun(review)
nouns <- unlist(nouns)
nouns <- as.data.frame(nouns)
nouns <- nouns %>% tibble()
wordcount <- table(unlist(nouns)) 
re_word <- as.data.frame(wordcount, stringsAsFactors = F)
re_word <- dplyr::rename(re_word,word=Var1,freq=Freq)
re_word <- filter(re_word,nchar(word)>=2)
top100 <- re_word %>% arrange(desc(freq)) %>% head(100)
top100 #가장 많이 사용된 단어 100개
write.table(top100, "C:\\Temp\\movie_review_nouns.txt")

#워드클라우드

library(wordcloud)
pal<-brewer.pal(8,"Dark2")
set.seed(1234)
wordcloud(words = re_word$word,
          freq = re_word$freq,
          min.freq = 2,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          scale = c(4, 0.3),
          colors = pal)

#군산대 감성사전으로 정서 분석

library(SentimentAnalysis)
setwd("C://Temp")
review_nouns <- read.table("movie_review_nouns.txt")
senti_words_kr <- readr::read_delim("SentiWord_Dict.txt", delim='\t', col_names=c("term", "score"))
dim(senti_words_kr)
x <- duplicated(senti_words_kr$term)
senti_words_kr2 <- senti_words_kr[!x, ]
senti_dic_kr <- SentimentDictionaryWeighted(words = senti_words_kr2$term, 
                                            scores = senti_words_kr2$score)
senti_dic_kr <- SentimentDictionary(senti_words_kr2$term[senti_words_kr2$score > 0], 
                                    senti_words_kr2$term[senti_words_kr2$score < 0])
summary(senti_dic_kr) #긍정어 비율은 약 33.14% / 부정어 비율은 약 66.86% 

res_sentiment <- analyzeSentiment(review_nouns$word,
                                  language="korean",
                                  rules=list("KoreanSentiment"=list(ruleSentiment, senti_dic_kr)),
                                  removeStopwords = F, stemming = F)
df2 <- data.frame(round(res_sentiment,3), review_nouns)
df3 <- df2 %>% mutate(pos_neg=if_else(KoreanSentiment>=0, "Positive","Negative")) %>% select(pos_neg, everything())
write.csv(df3, "review_nouns_senti.csv")
df3
