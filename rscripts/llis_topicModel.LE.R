install.packages("formattable")

library(topicmodels)
library(tidyverse)
library(dplyr)
library(tidytext)
library(readr)
library(lubridate)
library(LDAvis)
library(corrplot)
library(tidytext)
library(formattable)

# Create Data Frame of the lessons learned
llis_df <- read_csv("data/llis.csv", col_types = cols(LessonDate = col_date(format = "%m/%d/%y")))

# Quick glimpse of the data (Lou: This part is commented out coz its optional, and not necessary for topic generation)
# glimpse(llis_df)

llis_df

# Pull of the lesson ID, title, and text of lesson
by_lesson <- llis_df %>% 
  transmute(lessonID = LessonId, title = Title, lesson = Lesson, submitter=Submitter1)

by_lesson

# split into words
by_lesson_word <- by_lesson %>% 
  unnest_tokens(word, lesson)

by_lesson_word

# generic stop_words library
data(stop_words)

# find document-word counts and remove stop words
word_counts <- by_lesson_word %>% 
  anti_join(stop_words) 
word_counts


############### Sentiment Scores for Submitters #################

#bring in sentiment score per word

sentiments <- get_sentiments("afinn") 
sentiments

wordCountsSentiment <- word_counts %>%
  inner_join(sentiments)
wordCountsSentiment

# author sentiment scores
authorSentimentSummary <- wordCountsSentiment %>% 
  group_by(submitter) %>%
  summarize(avgSentimentScore = round(mean(score, na.rm = TRUE),2), 
            medSentimentScore = round(median(score, na.rm = TRUE),2), 
            countWords = length(word))

aSentScore <-authorSentimentSummary %>%
  arrange(avgSentimentScore) %>%
  filter(countWords >=30)
aSentScore

#view of sentiment score by author
formattable(aSentScore, 
            list("avgSentimentScore" = color_tile("transparent", "#00A86B")))


#box plot

wcs <- wordCountsSentiment %>% 
  inner_join(aSentScore) 
wcs

unique(wcs$submitter)

#graph
ggplot(wcs, aes(x=submitter, y=score)) + 
  geom_boxplot() +
  labs(y = "Range of Sentiment Scores for Lessons Learned Submitters",
       x = NULL) +
  coord_flip()  +
  theme_bw() + theme_minimal() 

############### Keywords for Submitters #################

# find document-word counts and remove stop words
word_counts <- by_lesson_word %>% 
  anti_join(stop_words) 
word_counts

keywordAuthor <- word_counts %>%
  count(submitter, word, sort = TRUE) 
keywordAuthor 

keywordAuthor <- keywordAuthor %>%
  arrange(submitter)