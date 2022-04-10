library(tidyverse)
library(scales)
library(lubridate)
library(tidytext)
library(wordcloud)
library(devtools)
library(tm)
library(stringr)

#clean data set
gddata <- read.csv('G.DS.csv', na.strings=c("","NA"))
alt_gd <- gddata[, -c(4, 7:10, 13:15)]


#removing html strings
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

gd <- alt_gd %>% mutate(Description = cleanFun(Job.Description)) %>% filter(Sector != -1)

# common and stopwords
common = readLines('common.csv')
g=stopwords("en")
h=stopwords("SMART")
i=c(g,h)
stop=i[i != i[532]]

# location
state <- read.csv('state.csv')

#function for grouping by title
titleconv <- function(title) {
  if (grepl("Scientist", title ,ignore.case = TRUE)==TRUE) { 
    return("Data Scientist")
  } else if (grepl("Engineer", title ,ignore.case = TRUE)==TRUE) {
    return("Data Engineer")
  } else if  (grepl("Analy", title ,ignore.case = TRUE)==TRUE) {
    return("Business / Data Analyst")
  } else if (grepl("Intelligence", title, ignore.case = TRUE)==TRUE) {
    return("Business / Data Analyst")
  } else if (grepl("Architect", title, ignore.case = TRUE)==TRUE) {
    return("Business / Data Analyst")
  } else if  (grepl("Model", title ,ignore.case = TRUE)==TRUE) {
    return("Business / Data Analyst")
  } else if (grepl("Consult", title, ignore.case = TRUE)==TRUE) {
    return("Consultant")
  } else if (grepl("Develop", title, ignore.case = TRUE)==TRUE) {
    return("Business Developer")
  } else if (grepl("Project", title, ignore.case = TRUE)==TRUE) {
    return("Project Manager")
  } else if(grepl("Manager", title, ignore.case = TRUE)==TRUE) {
    return("Business Manager")
  } else {
    return("Executive")
  }
}
# cleaning
reviews_gd <- gd %>%
  group_by(Sector, Job.Title, Location) %>%
  summarise(desc = Description) %>%
  mutate(lower = tolower(desc)) %>%
  mutate(cleaned = removeWords(lower, stop)) %>%
  mutate(cleaned = gsub(x = cleaned, pattern = paste(common, collapse = "|"), 
                        replacement = "", ignore.case = TRUE)) %>% left_join(state,by=c("Location"))




-----------------------------------------------------------------#
# split the locations and then execute the below codes for each locations

# Step-1-single words of the job descriptions
example_desc_gd <- reviews_gd %>%
  unnest_tokens(word, cleaned) %>%
  count(word) %>%
  anti_join(stop_words) %>%
  arrange(Sector,Job.Title,desc(n)) %>% slice(1:40)



# Step-2-bigrams of the job descriptions
ex_2_gd <- reviews_gd %>%
  unnest_tokens(word, cleaned, token = "ngrams", n = 2) %>%
  count(word) %>% arrange(Sector,Job.Title,desc(n)) %>% slice(1:40)


# Step-3-combine singles and bigrams

combined_gd <- rbind(ex_2_gd, example_desc_gd) %>% 
  group_by(Sector, Job.Title,word) %>% summarise(n = sum(n)) %>% 
  arrange(Sector, Job.Title, desc(n)) %>% mutate(Title = titleconv(Job.Title))






# Note

#--------------------------#
#group by sector- Use combined files of ba_da, gd and id and make finalcombined and perform group by sector and match.

#--------------------------#
#group by title- Use combined files of ba_da, gd and id and make finalcombined and perform group by title. Note that some title may be NA. 
#manually eliminate NA by checking the Job.Title and match.

#--------------------------#
#group by region- Use reviews_gd,review_id,reviews_ba_da files rbind and then Repeat steps 1 to 3 for each of the 6 regions and match.