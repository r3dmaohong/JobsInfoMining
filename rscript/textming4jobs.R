#' Analysis of jobs from my 104
#' 
library(jiebaR)
library(stringr)
library(dplyr)
library(wordcloud)
library(ggplot2)
library(ropencc)

setwd('workspace/datascience/r/JobsInfoMining')

fn <- list.files('output/', full.names = TRUE)
fn <- fn[grepl('jobs_result.csv', fn)]
result <- read.csv(fn, stringsAsFactors = FALSE)

# default skills
cutter <- worker('tag', bylines = TRUE)
new_user_word(cutter,'深度學習',"n")
new_user_word(cutter,'機器學習',"n")
new_user_word(cutter,'machine_learning',"n")
new_user_word(cutter,'deep_learning',"n")
input_string <- tolower( paste0(result$description, result$other) )
input_string <- gsub('machine learning', 'machine_learning', input_string)
input_string <- gsub('deep learning', 'deep_learning', input_string)


jieba_result <- sapply(input_string, function(string) cutter<=string)
total_word_counts <- toupper( unlist( jieba_result ) ) %>% table %>% data.frame(., stringsAsFactors = FALSE) %>% arrange(-Freq) %>% `colnames<-`(c("word", "freq"))
#word_counts <- word_counts#[nchar(as.character(word_counts$word))>=2,]

default_skills <- c('R', 'PYTHON', 'HADOOP', 'SPARK', 'MONGODB', 'TABLEAU', 'SQL', '機器學習', 'AI', '機器人', '人工智慧', '深度學習', 'TENSORFLOW', 'DEEP_LEARNING', 'MACHINE_LEARNING')
word_counts <- total_word_counts[total_word_counts$word %in% default_skills, ]
word_counts$word <- factor(word_counts$word, levels = word_counts$word[order(word_counts$freq)])

skills_plot <- ggplot(word_counts, aes(x=word, y=freq)) + 
  geom_bar(stat='identity', fill = '#49A2CC') + theme_classic() +
  ggtitle('Default skills') + labs(x = 'skills', y = 'freq') +
  theme(text=element_text(family="Hiragino Sans CNS W3", size=14)) +
  coord_flip()
plot(skills_plot)

total_word_counts <- total_word_counts[is.na(as.numeric(as.character(total_word_counts$word))),]

#filter_segment(, converter(S2T)[readLines(STOPPATH)])
total_word_counts <- total_word_counts[!(total_word_counts$word %in% toupper(converter(S2T)[readLines(STOPPATH)])),]
total_word_counts <- total_word_counts[total_word_counts$word!="NA",]

wordcloud(total_word_counts$word[1:500], total_word_counts$freq[1:500],
          random.order = F, ordered.colors = F, 
          colors = rainbow(length(row.names(total_word_counts))),
          family = 'Hiragino Sans CNS W3')
