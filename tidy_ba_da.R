library(tidyverse)
library(scales)
library(lubridate)
library(tidytext)
library(wordcloud)
library(devtools)
library(tm)
library(stringr)

#clean data set
ba <- read.csv('BusinessAnalyst.csv', na.strings=c("","NA"))
da <- read.csv('DataAnalyst.csv', na.strings=c("","NA"))
ba1 <- ba[1:3692, -c(1:2)]
ba2 <- ba[3693:4092, -c(16:17)]
colnames(ba2) <- colnames(ba1)
alt_ba <- rbind(ba1, ba2)
alt_ba <- alt_ba[, -c(4, 7:10, 13:14)]
alt_da <- da[, -c(1, 5, 8:11, 14:15)]
ba_da <- rbind(alt_ba, alt_da)

ba_da$job_id <- 1:nrow(ba_da)
ba_da_short <- ba_da[, c(1,9)]

sector_ba_da <- ba_da %>%
  group_by(Sector) %>%
  filter(Sector != -1) %>%
  summarise(total = n()) %>%
  arrange(desc(total)) 

sector_ba_da$id <- 1:nrow(sector_ba_da)

cleanFun <- function(htmlString) {
    return(gsub("<.*?>", "", htmlString))
}

ba_da <- ba_da %>%
  mutate(Description = cleanFun(Job.Description)) %>%
  filter(Sector != -1)

common = readLines('common.csv')

reviews_ba_da <- ba_da %>%
  group_by(Sector, Job.Title, job_id) %>%
  summarise(desc = Description) %>%
  mutate(lower = tolower(desc)) %>%
  mutate(cleaned = removeWords(lower, stopwords("en"))) %>%
  mutate(cleaned = gsub(x = cleaned, pattern = paste(common, collapse = "|"), 
                        replacement = "", ignore.case = TRUE))
  
example_desc_ba_da <- reviews_ba_da %>%
  unnest_tokens(word, cleaned) %>%
  count(word) %>%
  top_n(40) %>%
  anti_join(stop_words) 
# %>%
#   left_join(sector_ba_da, by = 'Sector') %>%
#   left_join(ba_da_short, by = 'Job.Title') %>%
#   arrange(id, job_id)

# example_desc_ba_da <- select(example_desc_ba_da, -c('total'))

# example_desc_ba_da <- example_desc_ba_da %>%
#   arrange(id, Job.Title, desc(n))
# 
# example_desc_ba_da <- example_desc_ba_da[example_desc_ba_da$id != 1, ]

#unique(example_desc$word)

#one <- ex_stop$cleaned[3]
#gsub(x = one, pattern = paste(common, collapse = "|"), replacement = "", ignore.case = TRUE)

#bigrams of the job descriptions
ex_2_ba_da <- reviews_ba_da %>%
  unnest_tokens(word, cleaned, token = "ngrams", n = 2) %>%
  count(word) %>%
  top_n(40) 

# ex_2_ba_da <- ex_2_ba_da %>%
#   left_join(sector_ba_da, by = 'Sector') %>%
#   arrange(id)

# ex_2_ba_da <- select(ex_2_ba_da, -c('total'))
# ex_2_ba_da <- ex_2_ba_da[ex_2_ba_da$id != 1, ] %>%
#   arrange(Sector, desc(n)) 

example_desc_ba_da <- example_desc_ba_da %>% arrange(Sector, desc(n))

combined <- rbind(ex_2_ba_da, example_desc_ba_da) %>%
  arrange(Sector, desc(n))

# unique(combined$word)
# 
# write_csv(combined, 'combined.csv')
# write_csv(ex_2, 'bigram.csv')
# write_csv(example_desc, 'single_words.csv')
# ds.id <-  filter(cleaned, 
#                    Job_Title=='Data Scientist')$Job_Title
# 
# ds.desc <- example %>%
#   filter(Job_Title=='Data Scientist') %>%
#   group_by(Company_Industry) %>%
#   summarise(jobs = top_n(15))
#   #top_n(25)
# 
# 
# DSTidy <- ds.desc %>%
#   select(review_id,text,stars) %>%
#   unnest_tokens(word,text)
# 
# DSFreqWords <- DSTidy %>%
#   count(word)
# 
# ds.desc %>%
#   ggplot(aes(x=fct_reorder(word,n),y=n)) + geom_bar(stat='identity') + 
#   coord_flip() + theme_bw()+
#   labs(title='Top 25 DS Words in Consulting & Business Services',
#        x='Word',
#        y= 'Count')
