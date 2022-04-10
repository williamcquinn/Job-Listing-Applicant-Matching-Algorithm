library(tidyverse)
library(scales)
library(lubridate)
library(tidytext)
library(wordcloud)
library(devtools)
library(tm)
library(stringr)
library(reticulate)
library(docxtractr)
library(pdftools)
library(ggplot2)
library(dplyr)
library(leaflet)
library(rgdal)
library(maps)


#clean data set-ba_da
ba <- read.csv('~/Desktop/Predictive Analytics 1/data_assignment5/BusinessAnalyst.csv', na.strings=c("","NA"))
da <- read.csv('~/Desktop/Predictive Analytics 1/data_assignment5/DataAnalyst.csv', na.strings=c("","NA"))
ba1 <- ba[1:3692, -c(1:2)]
ba2 <- ba[3693:4092, -c(16:17)]
colnames(ba2) <- colnames(ba1)
alt_ba <- rbind(ba1, ba2)
alt_ba <- alt_ba[, -c(4, 7:10, 13:14)]
alt_da <- da[, -c(1, 5, 8:11, 14:15)]
ba_da <- rbind(alt_ba, alt_da) %>% filter(Sector != -1)

#clean data set-gd
gddata <- read.csv('~/Desktop/Predictive Analytics 1/data_assignment5/G.DS.csv', na.strings=c("","NA"))
alt_gd <- gddata[, -c(4, 7:10, 13:15)]

#clean data set-id
iddata <- read.csv('~/Desktop/Predictive Analytics 1/data_assignment5/I.DS.csv', na.strings=c("","NA"))
alt_id <- iddata[, -c(1, 3, 5, 7, 9:12, 14:15, 17:43)] %>% mutate(sector=Company_Industry)
colnames(alt_id)=colnames(alt_gd)

#removing html strings
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

# common and stopwords
common = readLines('~/Desktop/Predictive Analytics 1/data_assignment5/common.csv')
g=stopwords("en")
h=stopwords("SMART")
i=c(g,h)
stop=i[i != i[532]]

# location
state <- read.csv('~/Desktop/Predictive Analytics 1/data_assignment5/state.csv')
state_codes <- read.csv('~/Desktop/Predictive Analytics 1/data_assignment5/state_codes.csv')

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


# cleaning-ba_da

ba_da <- ba_da %>% mutate(Description = cleanFun(Job.Description)) %>% filter(Sector != -1)
reviews_ba_da <- ba_da %>%
  group_by(Sector, Job.Title, Location) %>%
  summarise(desc = Description) %>%
  mutate(lower = tolower(desc)) %>%
  mutate(cleaned = removeWords(lower, stop)) %>%
  mutate(cleaned = gsub(x = cleaned, pattern = paste(common, collapse = "|"), 
                        replacement = "", ignore.case = TRUE)) %>% left_join(state,by="Location")

# cleaning-gd

gd <- alt_gd %>% mutate(Description = cleanFun(Job.Description)) %>% filter(Sector != -1)
reviews_gd <- gd %>%
  group_by(Sector, Job.Title, Location) %>%
  summarise(desc = Description) %>%
  mutate(lower = tolower(desc)) %>%
  mutate(cleaned = removeWords(lower, stop)) %>%
  mutate(cleaned = gsub(x = cleaned, pattern = paste(common, collapse = "|"), 
                        replacement = "", ignore.case = TRUE)) %>% left_join(state,by=c("Location"))

# cleaning-id

id <- alt_id %>% mutate(Description = cleanFun(Job.Description)) %>% filter(Sector != -1)
reviews_id <- id %>%
  group_by(Sector, Job.Title, Location) %>%
  summarise(desc = Description) %>%
  mutate(lower = tolower(desc)) %>%
  mutate(cleaned = removeWords(lower, stop)) %>%
  mutate(cleaned = gsub(x = cleaned, pattern = paste(common, collapse = "|"), 
                        replacement = "", ignore.case = TRUE)) %>% left_join(state_codes,by=c("Location"))

# BA_DA
# Step-1-single words of the job descriptions
example_desc_ba_da <- reviews_ba_da %>%
  unnest_tokens(word, cleaned) %>%
  count(word) %>%
  anti_join(stop_words) %>%
  arrange(Sector,Job.Title,desc(n)) %>% slice(1:40)
# Step-2-bigrams of the job descriptions
ex_2_ba_da <- reviews_ba_da %>%
  unnest_tokens(word, cleaned, token = "ngrams", n = 2) %>%
  count(word) %>% arrange(Sector,Job.Title,desc(n)) %>% slice(1:40)
# Step-3-combine singles and bigrams
combined_ba_da <- rbind(ex_2_ba_da, example_desc_ba_da) %>% 
  group_by(Sector, Job.Title,word) %>% summarise(n = sum(n)) %>% 
  arrange(Sector, Job.Title, desc(n)) %>% mutate(Title = titleconv(Job.Title))


#GD
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

#ID
# Step-1-single words of the job descriptions
example_desc_id <- reviews_id %>%
  unnest_tokens(word, cleaned) %>%
  count(word) %>%
  anti_join(stop_words) %>%
  arrange(Sector,Job.Title,desc(n)) %>% slice(1:40)
# Step-2-bigrams of the job descriptions
ex_2_id <- reviews_id %>%
  unnest_tokens(word, cleaned, token = "ngrams", n = 2) %>%
  count(word) %>% arrange(Sector,Job.Title,desc(n)) %>% slice(1:40)
# Step-3-combine singles and bigrams
combined_id <- rbind(ex_2_id, example_desc_id) %>% 
  group_by(Sector, Job.Title,word) %>% summarise(n = sum(n)) %>% 
  arrange(Sector, Job.Title, desc(n)) %>% mutate(Title = titleconv(Job.Title))

#--------------------------------------------------------------------------#

#combined data
sectconv <- read.csv('~/Desktop/Predictive Analytics 1/data_assignment5/Sector.csv', na.strings=c("","NA"))
combined <- rbind(combined_ba_da,combined_gd,combined_id)

#By sector
#--------------------------------------------------------------------------#
sector <- combined %>% left_join(sectconv,by=c("Sector"="Term")) %>% 
  group_by(Sect,word) %>% summarise(n=sum(n)) %>% 
  arrange(Sect,desc(n)) %>% slice(1:100) %>% select('Sect','word')


#sector wise dictionary
dict_sector <- vector(mode="list",length = 33)
names(dict_sector) <- c(unique(sector$Sect))
dict_sector[[1]] <- c(sector$word[1:100])
dict_sector[[2]] <- c(sector$word[101:200])
dict_sector[[3]] <- c(sector$word[201:300])
dict_sector[[4]] <- c(sector$word[301:400])
dict_sector[[5]] <- c(sector$word[401:500])
dict_sector[[6]] <- c(sector$word[501:600])
dict_sector[[7]] <- c(sector$word[601:700])
dict_sector[[8]] <- c(sector$word[701:800])
dict_sector[[9]] <- c(sector$word[801:900])
dict_sector[[10]] <- c(sector$word[901:1000])
dict_sector[[11]] <- c(sector$word[1001:1100])
dict_sector[[12]] <- c(sector$word[1101:1200])
dict_sector[[13]] <- c(sector$word[1201:1300])
dict_sector[[14]] <- c(sector$word[1301:1400])
dict_sector[[15]] <- c(sector$word[1401:1500])
dict_sector[[16]] <- c(sector$word[1501:1600])
dict_sector[[17]] <- c(sector$word[1601:1700])
dict_sector[[18]] <- c(sector$word[1701:1800])
dict_sector[[19]] <- c(sector$word[1801:1900])
dict_sector[[20]] <- c(sector$word[1901:2000])
dict_sector[[21]] <- c(sector$word[2001:2100])
dict_sector[[22]] <- c(sector$word[2101:2200])
dict_sector[[23]] <- c(sector$word[2201:2300])
dict_sector[[24]] <- c(sector$word[2301:2400])
dict_sector[[25]] <- c(sector$word[2401:2500])
dict_sector[[26]] <- c(sector$word[2501:2600])
dict_sector[[27]] <- c(sector$word[2601:2700])
dict_sector[[28]] <- c(sector$word[2701:2800])
dict_sector[[29]] <- c(sector$word[2801:2900])
dict_sector[[30]] <- c(sector$word[2901:3000])
dict_sector[[31]] <- c(sector$word[3001:3100])
dict_sector[[32]] <- c(sector$word[3101:3200])
dict_sector[[33]] <- c(sector$word[3201:3300])

dict_sector

#By title
#--------------------------------------------------------------------------#
title <- combined %>% group_by(Title,word) %>% summarise(n=sum(n)) %>% 
  arrange(Title,desc(n)) %>% slice(1:100) %>% select('Title','word')

dict_title <- vector(mode="list",length = 8)
names(dict_title) <- c(unique(title$Title))
dict_title[[1]] <- c(title$word[1:100])
dict_title[[2]] <- c(title$word[101:200])
dict_title[[3]] <- c(title$word[201:300])
dict_title[[4]] <- c(title$word[301:400])
dict_title[[5]] <- c(title$word[401:500])
dict_title[[6]] <- c(title$word[501:600])
dict_title[[7]] <- c(title$word[601:700])
dict_title[[8]] <- c(title$word[701:800])

dict_title


#By region-title
#--------------------------------------------------------------------------#

reviewrbind <- rbind(reviews_gd, reviews_id, reviews_ba_da) %>% left_join(sectconv,by=c("Sector"="Term")) %>% mutate(Sector=Sect)
regions <- reviewrbind %>%
  group_by(Region) %>%
  summarize(n=n()) 
unique(reviewrbind$Region)
#Central region
central1 <- reviewrbind %>%
  filter(Region == regions$Region[1]) %>%
  unnest_tokens(word, cleaned) %>%
  count(word) %>%
  anti_join(stop_words) %>%
  arrange(Sector,Job.Title,desc(n)) %>% slice(1:100)

central2 <- reviewrbind %>%
  filter(Region == regions$Region[1]) %>%
  unnest_tokens(word, cleaned, token = "ngrams", n = 2) %>%
  count(word) %>% arrange(Sector,Job.Title,desc(n)) %>% slice(1:100)

central3 <- rbind(central2, central1) %>% 
  group_by(Sector, Job.Title,word) %>% summarise(n = sum(n)) %>% 
  arrange(Sector, Job.Title, desc(n)) %>% mutate(Title = titleconv(Job.Title))

centraltop <- central3 %>% group_by(Title,word) %>% summarise(n=sum(n)) %>% 
  arrange(Title,desc(n)) %>% slice(1:100) %>% select('Title','word')

centraltop1 <- central3 %>% group_by(Sector,word) %>% summarise(n=sum(n)) %>% 
  arrange(Sector,desc(n)) %>% slice(1:100) %>% select('Sector','word')


dict_title_central <- vector(mode="list",length = 6)
names(dict_title_central) <- c(unique(centraltop$Title))
dict_title_central[[1]] <- c(centraltop$word[1:100])
dict_title_central[[2]] <- c(centraltop$word[101:200])
dict_title_central[[3]] <- c(centraltop$word[201:300])
dict_title_central[[4]] <- c(centraltop$word[301:400])
dict_title_central[[5]] <- c(centraltop$word[401:500])
dict_title_central[[6]] <- c(centraltop$word[501:600])

dict_sector_central <- vector(mode="list",length = 32)
names(dict_sector_central) <- c(unique(centraltop1$Sector))
dict_sector_central[[1]] <- c(centraltop1$word[1:100])
dict_sector_central[[2]] <- c(centraltop1$word[101:200])
dict_sector_central[[3]] <- c(centraltop1$word[201:300])
dict_sector_central[[4]] <- c(centraltop1$word[301:400])
dict_sector_central[[5]] <- c(centraltop1$word[401:500])
dict_sector_central[[6]] <- c(centraltop1$word[501:600])
dict_sector_central[[7]] <- c(centraltop1$word[601:700])
dict_sector_central[[8]] <- c(centraltop1$word[701:800])
dict_sector_central[[9]] <- c(centraltop1$word[801:900])
dict_sector_central[[10]] <- c(centraltop1$word[901:1000])
dict_sector_central[[11]] <- c(centraltop1$word[1001:1100])
dict_sector_central[[12]] <- c(centraltop1$word[1101:1200])
dict_sector_central[[13]] <- c(centraltop1$word[1201:1300])
dict_sector_central[[14]] <- c(centraltop1$word[1301:1400])
dict_sector_central[[15]] <- c(centraltop1$word[1401:1500])
dict_sector_central[[16]] <- c(centraltop1$word[1501:1600])
dict_sector_central[[17]] <- c(centraltop1$word[1601:1700])
dict_sector_central[[18]] <- c(centraltop1$word[1701:1800])
dict_sector_central[[19]] <- c(centraltop1$word[1801:1900])
dict_sector_central[[20]] <- c(centraltop1$word[1901:2000])
dict_sector_central[[21]] <- c(centraltop1$word[2001:2100])
dict_sector_central[[22]] <- c(centraltop1$word[2101:2200])
dict_sector_central[[23]] <- c(centraltop1$word[2201:2300])
dict_sector_central[[24]] <- c(centraltop1$word[2301:2400])
dict_sector_central[[25]] <- c(centraltop1$word[2401:2500])
dict_sector_central[[26]] <- c(centraltop1$word[2501:2600])
dict_sector_central[[27]] <- c(centraltop1$word[2601:2700])
dict_sector_central[[28]] <- c(centraltop1$word[2701:2800])
dict_sector_central[[29]] <- c(centraltop1$word[2801:2900])
dict_sector_central[[30]] <- c(centraltop1$word[2901:3000])
dict_sector_central[[31]] <- c(centraltop1$word[3001:3100])
dict_sector_central[[32]] <- c(centraltop1$word[3101:3200])


#Midwest region
midwest1 <- reviewrbind %>%
  filter(Region == regions$Region[2]) %>%
  unnest_tokens(word, cleaned) %>%
  count(word) %>%
  anti_join(stop_words) %>%
  arrange(Sector,Job.Title,desc(n)) %>% slice(1:100)

midwest2 <- reviewrbind %>%
  filter(Region == regions$Region[2]) %>%
  unnest_tokens(word, cleaned, token = "ngrams", n = 2) %>%
  count(word) %>% arrange(Sector,Job.Title,desc(n)) %>% slice(1:100)

midwest3 <- rbind(midwest2, midwest1) %>% 
  group_by(Sector, Job.Title,word) %>% summarise(n = sum(n)) %>% 
  arrange(Sector, Job.Title, desc(n)) %>% mutate(Title = titleconv(Job.Title))


mwtop <- midwest3 %>% group_by(Title,word) %>% summarise(n=sum(n)) %>% 
  arrange(Title,desc(n)) %>% slice(1:100) %>% select('Title','word')

mwtop1 <- midwest3 %>% group_by(Sector,word) %>% summarise(n=sum(n)) %>% 
  arrange(Sector,desc(n)) %>% slice(1:100) %>% select('Sector','word')


dict_title_mw <- vector(mode="list",length = 7)
names(dict_title_mw) <- c(unique(mwtop$Title))
dict_title_mw[[1]] <- c(mwtop$word[1:100])
dict_title_mw[[2]] <- c(mwtop$word[101:200])
dict_title_mw[[3]] <- c(mwtop$word[201:300])
dict_title_mw[[4]] <- c(mwtop$word[301:400])
dict_title_mw[[5]] <- c(mwtop$word[401:500])
dict_title_mw[[6]] <- c(mwtop$word[501:600])
dict_title_mw[[7]] <- c(mwtop$word[601:700])

dict_sector_mw <- vector(mode="list",length = 32)
names(dict_sector_mw) <- c(unique(mwtop1$Sector))
dict_sector_mw[[1]] <- c(mwtop1$word[1:100])
dict_sector_mw[[2]] <- c(mwtop1$word[101:200])
dict_sector_mw[[3]] <- c(mwtop1$word[201:300])
dict_sector_mw[[4]] <- c(mwtop1$word[301:400])
dict_sector_mw[[5]] <- c(mwtop1$word[401:500])
dict_sector_mw[[6]] <- c(mwtop1$word[501:600])
dict_sector_mw[[7]] <- c(mwtop1$word[601:700])
dict_sector_mw[[8]] <- c(mwtop1$word[701:800])
dict_sector_mw[[9]] <- c(mwtop1$word[801:900])
dict_sector_mw[[10]] <- c(mwtop1$word[901:1000])
dict_sector_mw[[11]] <- c(mwtop1$word[1001:1100])
dict_sector_mw[[12]] <- c(mwtop1$word[1101:1200])
dict_sector_mw[[13]] <- c(mwtop1$word[1201:1300])
dict_sector_mw[[14]] <- c(mwtop1$word[1301:1400])
dict_sector_mw[[15]] <- c(mwtop1$word[1401:1500])
dict_sector_mw[[16]] <- c(mwtop1$word[1501:1600])
dict_sector_mw[[17]] <- c(mwtop1$word[1601:1700])
dict_sector_mw[[18]] <- c(mwtop1$word[1701:1800])
dict_sector_mw[[19]] <- c(mwtop1$word[1801:1900])
dict_sector_mw[[20]] <- c(mwtop1$word[1901:2000])
dict_sector_mw[[21]] <- c(mwtop1$word[2001:2100])
dict_sector_mw[[22]] <- c(mwtop1$word[2101:2200])
dict_sector_mw[[23]] <- c(mwtop1$word[2201:2300])
dict_sector_mw[[24]] <- c(mwtop1$word[2301:2400])
dict_sector_mw[[25]] <- c(mwtop1$word[2401:2500])
dict_sector_mw[[26]] <- c(mwtop1$word[2501:2600])
dict_sector_mw[[27]] <- c(mwtop1$word[2601:2700])
dict_sector_mw[[28]] <- c(mwtop1$word[2701:2800])
dict_sector_mw[[29]] <- c(mwtop1$word[2801:2900])
dict_sector_mw[[30]] <- c(mwtop1$word[2901:3000])
dict_sector_mw[[31]] <- c(mwtop1$word[3001:3100])
dict_sector_mw[[32]] <- c(mwtop1$word[3101:3200])

#Mountain region
mountain1 <- reviewrbind %>%
  filter(Region == regions$Region[3]) %>%
  unnest_tokens(word, cleaned) %>%
  count(word) %>%
  anti_join(stop_words) %>%
  arrange(Sector,Job.Title,desc(n)) %>% slice(1:100)

mountain2 <- reviewrbind %>%
  filter(Region == regions$Region[3]) %>%
  unnest_tokens(word, cleaned, token = "ngrams", n = 2) %>%
  count(word) %>% arrange(Sector,Job.Title,desc(n)) %>% slice(1:100)

mountain3 <- rbind(mountain2, mountain1) %>% 
  group_by(Sector, Job.Title,word) %>% summarise(n = sum(n)) %>% 
  arrange(Sector, Job.Title, desc(n)) %>% mutate(Title = titleconv(Job.Title))

mountaintop <- mountain3 %>% group_by(Title,word) %>% summarise(n=sum(n)) %>% 
  arrange(Title,desc(n)) %>% slice(1:100) %>% select('Title','word')
mountaintop1 <- mountain3 %>% group_by(Sector,word) %>% summarise(n=sum(n)) %>% 
  arrange(Sector,desc(n)) %>% slice(1:100) %>% select('Sector','word')

dict_title_mountain <- vector(mode="list",length = 6)
names(dict_title_mountain) <- c(unique(mountaintop$Title))
dict_title_mountain[[1]] <- c(mountaintop$word[1:100])
dict_title_mountain[[2]] <- c(mountaintop$word[101:200])
dict_title_mountain[[3]] <- c(mountaintop$word[201:300])
dict_title_mountain[[4]] <- c(mountaintop$word[301:400])
dict_title_mountain[[5]] <- c(mountaintop$word[401:500])
dict_title_mountain[[6]] <- c(mountaintop$word[501:600])


dict_sector_mountain <- vector(mode="list",length = 32)
names(dict_sector_mountain) <- c(unique(mountaintop1$Sector))
dict_sector_mountain[[1]] <- c(mountaintop1$word[1:100])
dict_sector_mountain[[2]] <- c(mountaintop1$word[101:200])
dict_sector_mountain[[3]] <- c(mountaintop1$word[201:300])
dict_sector_mountain[[4]] <- c(mountaintop1$word[301:400])
dict_sector_mountain[[5]] <- c(mountaintop1$word[401:500])
dict_sector_mountain[[6]] <- c(mountaintop1$word[501:600])
dict_sector_mountain[[7]] <- c(mountaintop1$word[601:700])
dict_sector_mountain[[8]] <- c(mountaintop1$word[701:800])
dict_sector_mountain[[9]] <- c(mountaintop1$word[801:900])
dict_sector_mountain[[10]] <- c(mountaintop1$word[901:1000])
dict_sector_mountain[[11]] <- c(mountaintop1$word[1001:1100])
dict_sector_mountain[[12]] <- c(mountaintop1$word[1101:1200])
dict_sector_mountain[[13]] <- c(mountaintop1$word[1201:1300])
dict_sector_mountain[[14]] <- c(mountaintop1$word[1301:1400])
dict_sector_mountain[[15]] <- c(mountaintop1$word[1401:1500])
dict_sector_mountain[[16]] <- c(mountaintop1$word[1501:1600])
dict_sector_mountain[[17]] <- c(mountaintop1$word[1601:1700])
dict_sector_mountain[[18]] <- c(mountaintop1$word[1701:1800])
dict_sector_mountain[[19]] <- c(mountaintop1$word[1801:1900])
dict_sector_mountain[[20]] <- c(mountaintop1$word[1901:2000])
dict_sector_mountain[[21]] <- c(mountaintop1$word[2001:2100])
dict_sector_mountain[[22]] <- c(mountaintop1$word[2101:2200])
dict_sector_mountain[[23]] <- c(mountaintop1$word[2201:2300])
dict_sector_mountain[[24]] <- c(mountaintop1$word[2301:2400])
dict_sector_mountain[[25]] <- c(mountaintop1$word[2401:2500])
dict_sector_mountain[[26]] <- c(mountaintop1$word[2501:2600])
dict_sector_mountain[[27]] <- c(mountaintop1$word[2601:2700])
dict_sector_mountain[[28]] <- c(mountaintop1$word[2701:2800])
dict_sector_mountain[[29]] <- c(mountaintop1$word[2801:2900])
dict_sector_mountain[[30]] <- c(mountaintop1$word[2901:3000])
dict_sector_mountain[[31]] <- c(mountaintop1$word[3001:3100])
dict_sector_mountain[[32]] <- c(mountaintop1$word[3101:3200])


#North east region
ne1 <- reviewrbind %>%
  filter(Region == regions$Region[4]) %>%
  unnest_tokens(word, cleaned) %>%
  count(word) %>%
  anti_join(stop_words) %>%
  arrange(Sector,Job.Title,desc(n)) %>% slice(1:100)

ne2 <- reviewrbind %>%
  filter(Region == regions$Region[4]) %>%
  unnest_tokens(word, cleaned, token = "ngrams", n = 2) %>%
  count(word) %>% arrange(Sector,Job.Title,desc(n)) %>% slice(1:100)

ne3 <- rbind(ne2, ne1) %>% 
  group_by(Sector, Job.Title,word) %>% summarise(n = sum(n)) %>% 
  arrange(Sector, Job.Title, desc(n)) %>% mutate(Title = titleconv(Job.Title))

netop <- ne3 %>% group_by(Title,word) %>% summarise(n=sum(n)) %>% 
  arrange(Title,desc(n)) %>% slice(1:100) %>% select('Title','word')

netop1 <- ne3 %>% group_by(Sector,word) %>% summarise(n=sum(n)) %>% 
  arrange(Sector,desc(n)) %>% slice(1:100) %>% select('Sector','word')

dict_title_ne <- vector(mode="list",length = 7)
names(dict_title_ne) <- c(unique(netop$Title))
dict_title_ne[[1]] <- c(netop$word[1:100])
dict_title_ne[[2]] <- c(netop$word[101:200])
dict_title_ne[[3]] <- c(netop$word[201:300])
dict_title_ne[[4]] <- c(netop$word[301:400])
dict_title_ne[[5]] <- c(netop$word[401:500])
dict_title_ne[[6]] <- c(netop$word[501:600])
dict_title_ne[[7]] <- c(netop$word[601:700])

dict_sector_ne <- vector(mode="list",length = 32)
names(dict_sector_ne) <- c(unique(netop1$Sector))
dict_sector_ne[[1]] <- c(netop1$word[1:100])
dict_sector_ne[[2]] <- c(netop1$word[101:200])
dict_sector_ne[[3]] <- c(netop1$word[201:300])
dict_sector_ne[[4]] <- c(netop1$word[301:400])
dict_sector_ne[[5]] <- c(netop1$word[401:500])
dict_sector_ne[[6]] <- c(netop1$word[501:600])
dict_sector_ne[[7]] <- c(netop1$word[601:700])
dict_sector_ne[[8]] <- c(netop1$word[701:800])
dict_sector_ne[[9]] <- c(netop1$word[801:900])
dict_sector_ne[[10]] <- c(netop1$word[901:1000])
dict_sector_ne[[11]] <- c(netop1$word[1001:1100])
dict_sector_ne[[12]] <- c(netop1$word[1101:1200])
dict_sector_ne[[13]] <- c(netop1$word[1201:1300])
dict_sector_ne[[14]] <- c(netop1$word[1301:1400])
dict_sector_ne[[15]] <- c(netop1$word[1401:1500])
dict_sector_ne[[16]] <- c(netop1$word[1501:1600])
dict_sector_ne[[17]] <- c(netop1$word[1601:1700])
dict_sector_ne[[18]] <- c(netop1$word[1701:1800])
dict_sector_ne[[19]] <- c(netop1$word[1801:1900])
dict_sector_ne[[20]] <- c(netop1$word[1901:2000])
dict_sector_ne[[21]] <- c(netop1$word[2001:2100])
dict_sector_ne[[22]] <- c(netop1$word[2101:2200])
dict_sector_ne[[23]] <- c(netop1$word[2201:2300])
dict_sector_ne[[24]] <- c(netop1$word[2301:2400])
dict_sector_ne[[25]] <- c(netop1$word[2401:2500])
dict_sector_ne[[26]] <- c(netop1$word[2501:2600])
dict_sector_ne[[27]] <- c(netop1$word[2601:2700])
dict_sector_ne[[28]] <- c(netop1$word[2701:2800])
dict_sector_ne[[29]] <- c(netop1$word[2801:2900])
dict_sector_ne[[30]] <- c(netop1$word[2901:3000])
dict_sector_ne[[31]] <- c(netop1$word[3001:3100])
dict_sector_ne[[32]] <- c(netop1$word[3101:3200])




#South east region
se1 <- reviewrbind %>%
  filter(Region == regions$Region[5]) %>%
  unnest_tokens(word, cleaned) %>%
  count(word) %>%
  anti_join(stop_words) %>%
  arrange(Sector,Job.Title,desc(n)) %>% slice(1:100)

se2 <- reviewrbind %>%
  filter(Region == regions$Region[5]) %>%
  unnest_tokens(word, cleaned, token = "ngrams", n = 2) %>%
  count(word) %>% arrange(Sector,Job.Title,desc(n)) %>% slice(1:100)

se3 <- rbind(se2, se1) %>% 
  group_by(Sector, Job.Title,word) %>% summarise(n = sum(n)) %>% 
  arrange(Sector, Job.Title, desc(n)) %>% mutate(Title = titleconv(Job.Title))

setop <- se3 %>% group_by(Title,word) %>% summarise(n=sum(n)) %>% 
  arrange(Title,desc(n)) %>% slice(1:100) %>% select('Title','word')

setop1 <- se3 %>% group_by(Sector,word) %>% summarise(n=sum(n)) %>% 
  arrange(Sector,desc(n)) %>% slice(1:100) %>% select('Sector','word')

dict_title_se <- vector(mode="list",length = 8)
names(dict_title_se) <- c(unique(setop$Title))
dict_title_se[[1]] <- c(setop$word[1:100])
dict_title_se[[2]] <- c(setop$word[101:200])
dict_title_se[[3]] <- c(setop$word[201:300])
dict_title_se[[4]] <- c(setop$word[301:400])
dict_title_se[[5]] <- c(setop$word[401:500])
dict_title_se[[6]] <- c(setop$word[501:600])
dict_title_se[[7]] <- c(setop$word[601:700])
dict_title_se[[8]] <- c(setop$word[701:800])

dict_sector_se <- vector(mode="list",length = 32)
names(dict_sector_se) <- c(unique(setop1$Sector))
dict_sector_se[[1]] <- c(setop1$word[1:100])
dict_sector_se[[2]] <- c(setop1$word[101:200])
dict_sector_se[[3]] <- c(setop1$word[201:300])
dict_sector_se[[4]] <- c(setop1$word[301:400])
dict_sector_se[[5]] <- c(setop1$word[401:500])
dict_sector_se[[6]] <- c(setop1$word[501:600])
dict_sector_se[[7]] <- c(setop1$word[601:700])
dict_sector_se[[8]] <- c(setop1$word[701:800])
dict_sector_se[[9]] <- c(setop1$word[801:900])
dict_sector_se[[10]] <- c(setop1$word[901:1000])
dict_sector_se[[11]] <- c(setop1$word[1001:1100])
dict_sector_se[[12]] <- c(setop1$word[1101:1200])
dict_sector_se[[13]] <- c(setop1$word[1201:1300])
dict_sector_se[[14]] <- c(setop1$word[1301:1400])
dict_sector_se[[15]] <- c(setop1$word[1401:1500])
dict_sector_se[[16]] <- c(setop1$word[1501:1600])
dict_sector_se[[17]] <- c(setop1$word[1601:1700])
dict_sector_se[[18]] <- c(setop1$word[1701:1800])
dict_sector_se[[19]] <- c(setop1$word[1801:1900])
dict_sector_se[[20]] <- c(setop1$word[1901:2000])
dict_sector_se[[21]] <- c(setop1$word[2001:2100])
dict_sector_se[[22]] <- c(setop1$word[2101:2200])
dict_sector_se[[23]] <- c(setop1$word[2201:2300])
dict_sector_se[[24]] <- c(setop1$word[2301:2400])
dict_sector_se[[25]] <- c(setop1$word[2401:2500])
dict_sector_se[[26]] <- c(setop1$word[2501:2600])
dict_sector_se[[27]] <- c(setop1$word[2601:2700])
dict_sector_se[[28]] <- c(setop1$word[2701:2800])
dict_sector_se[[29]] <- c(setop1$word[2801:2900])
dict_sector_se[[30]] <- c(setop1$word[2901:3000])
dict_sector_se[[31]] <- c(setop1$word[3001:3100])
dict_sector_se[[32]] <- c(netop1$word[3101:3200])



#West region
west1 <- reviewrbind %>%
  filter(Region == regions$Region[6]) %>%
  unnest_tokens(word, cleaned) %>%
  count(word) %>%
  anti_join(stop_words) %>%
  arrange(Sector,Job.Title,desc(n)) %>% slice(1:100)

west2 <- reviewrbind %>%
  filter(Region == regions$Region[6]) %>%
  unnest_tokens(word, cleaned, token = "ngrams", n = 2) %>%
  count(word) %>% arrange(Sector,Job.Title,desc(n)) %>% slice(1:100)

west3 <- rbind(west2, west1) %>% 
  group_by(Sector, Job.Title,word) %>% summarise(n = sum(n)) %>% 
  arrange(Sector, Job.Title, desc(n)) %>% mutate(Title = titleconv(Job.Title))

westtop <- west3 %>% group_by(Title,word) %>% summarise(n=sum(n)) %>% 
  arrange(Title,desc(n)) %>% slice(1:100) %>% select('Title','word')

westtop1 <- west3 %>% group_by(Sector,word) %>% summarise(n=sum(n)) %>% 
  arrange(Sector,desc(n)) %>% slice(1:100) %>% select('Sector','word')

dict_title_west <- vector(mode="list",length = 7)
names(dict_title_west) <- c(unique(westtop$Title))
dict_title_west[[1]] <- c(westtop$word[1:100])
dict_title_west[[2]] <- c(westtop$word[101:200])
dict_title_west[[3]] <- c(westtop$word[201:300])
dict_title_west[[4]] <- c(westtop$word[301:400])
dict_title_west[[5]] <- c(westtop$word[401:500])
dict_title_west[[6]] <- c(westtop$word[501:600])
dict_title_west[[7]] <- c(westtop$word[601:700])

dict_sector_west <- vector(mode="list",length = 32)
names(dict_sector_west) <- c(unique(westtop1$Sector))
dict_sector_west[[1]] <- c(westtop1$word[1:100])
dict_sector_west[[2]] <- c(westtop1$word[101:200])
dict_sector_west[[3]] <- c(westtop1$word[201:300])
dict_sector_west[[4]] <- c(westtop1$word[301:400])
dict_sector_west[[5]] <- c(westtop1$word[401:500])
dict_sector_west[[6]] <- c(westtop1$word[501:600])
dict_sector_west[[7]] <- c(westtop1$word[601:700])
dict_sector_west[[8]] <- c(westtop1$word[701:800])
dict_sector_west[[9]] <- c(westtop1$word[801:900])
dict_sector_west[[10]] <- c(westtop1$word[901:1000])
dict_sector_west[[11]] <- c(westtop1$word[1001:1100])
dict_sector_west[[12]] <- c(westtop1$word[1101:1200])
dict_sector_west[[13]] <- c(westtop1$word[1201:1300])
dict_sector_west[[14]] <- c(westtop1$word[1301:1400])
dict_sector_west[[15]] <- c(westtop1$word[1401:1500])
dict_sector_west[[16]] <- c(westtop1$word[1501:1600])
dict_sector_west[[17]] <- c(westtop1$word[1601:1700])
dict_sector_west[[18]] <- c(westtop1$word[1701:1800])
dict_sector_west[[19]] <- c(westtop1$word[1801:1900])
dict_sector_west[[20]] <- c(westtop1$word[1901:2000])
dict_sector_west[[21]] <- c(westtop1$word[2001:2100])
dict_sector_west[[22]] <- c(westtop1$word[2101:2200])
dict_sector_west[[23]] <- c(westtop1$word[2201:2300])
dict_sector_west[[24]] <- c(westtop1$word[2301:2400])
dict_sector_west[[25]] <- c(westtop1$word[2401:2500])
dict_sector_west[[26]] <- c(westtop1$word[2501:2600])
dict_sector_west[[27]] <- c(westtop1$word[2601:2700])
dict_sector_west[[28]] <- c(westtop1$word[2701:2800])
dict_sector_west[[29]] <- c(westtop1$word[2801:2900])
dict_sector_west[[30]] <- c(westtop1$word[2901:3000])
dict_sector_west[[31]] <- c(westtop1$word[3001:3100])
dict_sector_west[[32]] <- c(westtop1$word[3101:3200])


#####Dictionary list


dict_sector

dict_title

dict_title_central
dict_title_mw
dict_title_mountain
dict_title_ne
dict_title_se
dict_title_west

dict_sector_central
dict_sector_mw
dict_sector_mountain
dict_sector_ne
dict_sector_se
dict_sector_west


# resume matching
upload=('~/Desktop/Predictive Analytics 1/data_assignment5/myres.pdf')
readin <- function(file) {
  tbls1 <- pdf_text(upload)
  text <- as.character((strsplit(tbls1," ")))
  keywords <- (removeWords(text,stop))
  
  return(keywords)
}
txt=upload

match <- function(ind,file) {
  count <- 0
  keywords <- readin(file)
  for(i in 1:25) {
    if(grepl(ind[i], keywords, ignore.case = TRUE) == TRUE) {
      count <- count + 1
    }
  }
  return(count)
}

#### By sector

sector <- function() {
  ac_l <- dict_sector$`Accounting & Legal`
  ad <- dict_sector$`Aerospace and Defense`
  af <- dict_sector$`Agriculture & Forestry`
  aer <- dict_sector$`Arts, Entertainment & Recreation`
  auto <- dict_sector$`Auto`
  bf <- dict_sector$`Banks and Financial Services`
  b_p <- dict_sector$`Biotech & Pharmaceuticals`
  bs <- dict_sector$`Business Services`
  ce <- dict_sector$`Computers and Electronics`
  con <- dict_sector$`Construction, Repair & Maintenance`
  cons <- dict_sector$`Consulting and Business Services`
  goods <- dict_sector$`Consumer Goods and Services`
  edu <- dict_sector$`Education and Schools`
  energy <- dict_sector$`Energy and Utilities`
  food <- dict_sector$`Food and Beverages`
  gov <- dict_sector$`Government`
  hlth <- dict_sector$`Health Care`
  hr <- dict_sector$`Human Resources and Staffing`
  ind <- dict_sector$`Industrial Manufacturing`
  it <- dict_sector$`Information Technology`
  ins <- dict_sector$`Insurance`
  med <- dict_sector$`Media`
  min <- dict_sector$`Mining & Metals`
  np <- dict_sector$`Non-Profit`
  oil <- dict_sector$`Oil, Gas, Energy & Utilities`
  others <- dict_sector$`Others`
  pharm <- dict_sector$`Pharmaceuticals`
  real <- dict_sector$`Real Estate`
  rest <- dict_sector$`Restaurants, Bars & Food Services`
  ret <- dict_sector$`Retail`
  tel <- dict_sector$`Telecommunications`
  trans <- dict_sector$`Transportation & Logistics`
  travel <- dict_sector$`Travel & Tourism`
  
  to_ret <- c(match(ac_l,txt), match(ad,txt), match(af,txt), match(aer,txt), match(auto,txt), match(bf,txt), match(b_p,txt), match(bs,txt),
              match(ce,txt), match(con,txt), match(cons,txt), match(goods,txt), match(edu,txt), match(energy,txt), match(food,txt), match(gov,txt), 
              match(hlth,txt), match(hr,txt), match(ind,txt), match(it,txt), match(ins,txt), match(med,txt), match(min,txt), match(np,txt), match(oil,txt),
              match(others,txt), match(pharm,txt), match(real,txt), match(rest,txt), match(ret,txt), match(tel,txt), match(trans,txt), match(travel,txt))
  
  per <- (label_percent()((to_ret/25) %>% sort(decreasing = TRUE)))
  sectorwise <- as.data.frame(list('Sector'=names(dict_sector),'Match'=per))
  return(sectorwise)
}


# By Title

title <- function() {
  a_bd <- dict_title$`Business / Data Analyst`
  bd <- dict_title$`Business Developer`
  bm <- dict_title$`Business Manager`
  ct <- dict_title$`Consultant`
  de <- dict_title$`Data Engineer`
  ds <- dict_title$`Data Scientist`
  ex <- dict_title$`Executive`
  pm <- dict_title$`Project Manager`
  
  to_ret <- c(match(a_bd,txt), match(bd,txt), match(bm,txt), match(ct,txt), match(de,txt), match(ds,txt), match(ex,txt), match(pm,txt))
  
  per <- (label_percent()((to_ret/25) %>% sort(decreasing = TRUE)))
  titlewise <- as.data.frame(list('Title'=names(dict_title),'Match'=per))
  return(titlewise)
}

sector()
title()


#Visualization

westcoast = map('state', regions = c('california', 'alaska', 'hawaii', 'oregon', 'washington'), fill = TRUE, plot = FALSE)
northeast = map('state', regions = c("maine", "vermont", "new hampshire", "massachusetts", "connecticut", "rhode island",
                                     "new york", "pennsylvania", "delaware", "district of columbia", "maryland"), fill = TRUE, plot = FALSE)
southeast = map('state', regions = c("kentucky", "new jersey", "tennessee", "virginia", "west virginia", "north carolina", "south carolina", "georgia", "florida"),
                fill = TRUE, plot = FALSE)
midwest = map('state', regions = c("wyoming", "south dakota", "north dakota","nebraska", "kansas", "oklahoma",
                                   "missouri",  "texas", "arkansas", "louisiana"), fill = TRUE, plot = FALSE)
central = map('state', regions = c( "wisconsin", "illinois", "indiana", "michigan", "ohio",
                                    "minnesota", "iowa","alabama", "mississippi"), fill = TRUE, plot = FALSE)
mountain = map('state', regions = c("nevada", "arizona", "new mexico", "montana", "idaho",  "utah", "colorado"), fill = TRUE, plot = FALSE)


maplist<- function(){
  #variable reviewrbind is in the final project code
  cities <- as_tibble(reviewrbind$Location)
  cities <- cities %>%
    group_by(value) %>%
    mutate(num = n())
  cities <- unique(cities)
  
  
  citycol <- c()
  statecol <- c()
  for (i in 1:nrow(cities)) {
    citycol <- c(citycol, strsplit(cities$value[i],", ")[[1]][1])
    statecol <- c(statecol, strsplit(cities$value[i],", ")[[1]][2])
  }
  
  cities <- cbind(cities, citycol, statecol)  
  names(cities)[names(cities) == "...3"] <- "city"
  names(cities)[names(cities) == "...4"] <- "state"
  
  #uscities.csv is in mgta452_project folder
  uscities <- read_csv("~/Desktop/Predictive Analytics 1/data_assignment5/uscities.csv")
  uscities <- uscities %>%
    mutate(value = paste(uscities$city, uscities$state_id, sep=", "))
  
  a <- inner_join(cities, uscities, by = "value")
  a <- subset(a, select = c(value, num, lat, lng))
  
  
  
  
  a$name <- paste0('<b>',a$value,'</b>','<br/>',a$num,' Job(s) Matching Your Resume')
  
  
  #variable regions is in the final project code
  regions <- na.omit(regions)
  
  leaflet(a) %>%
    addTiles() %>%
    addPolygons(data = westcoast, color = 'blue', stroke = FALSE, popup=paste0(regions$n[regions$Region=='West'],' Jobs In This Region: ', '<b>West</b>'),
                popupOptions = popupOptions(closeOnClick = TRUE)) %>%
    addPolygons(data = northeast, color = 'red', stroke = FALSE, popup=paste0(regions$n[regions$Region=='North East'],' Jobs In This Region: ', '<b>North East</b>'),
                popupOptions = popupOptions(closeOnClick = TRUE)) %>%
    addPolygons(data = southeast, color = 'gold', stroke = FALSE, popup=paste0(regions$n[regions$Region=='South East'],' Jobs In This Region: ', '<b>South East</b>'),
                popupOptions = popupOptions(closeOnClick = TRUE)) %>%
    addPolygons(data = midwest, color = 'purple', stroke = FALSE, popup=paste0(regions$n[regions$Region=='Midwest'],' Jobs In This Region: ', '<b>Midwest</b>'),
                popupOptions = popupOptions(closeOnClick = TRUE)) %>%
    addPolygons(data = central, color = 'pink', stroke = FALSE, popup=paste0(regions$n[regions$Region=='Central'],' Jobs In This Region: ', '<b>Central</b>'),
                popupOptions = popupOptions(closeOnClick = TRUE)) %>%
    addPolygons(data = mountain, color = 'lime', stroke = FALSE, popup=paste0(regions$n[regions$Region=='Mountain'],' Jobs In This Region: ', '<b>Mountain</b>'),
                popupOptions = popupOptions(closeOnClick = TRUE)) %>%
    addCircles(lng = ~lng, lat = ~lat, radius = ~(num)^2/1.5, popup = ~name, color = "#05B") %>%
    addProviderTiles("OpenStreetMap.Mapnik")
  
}

maplist()






# 
# regionwise <- function(x) {
# 
#   txt=('~/Desktop/Predictive Analytics 1/data_assignment5/Julian.pdf')
#   region_list <- as.data.frame(list('region'=c('West','South East','North East','Mountain','Mid West','Central'),'Sector wise dict'=c('dict_sector_west','dict_sector_se','dict_sector_ne','dict_sector_mountain','dict_sector_mw','dict_sector_central'),'Title wise dict'=c('dict_title_west','dict_title_se','dict_title_ne','dict_title_mountain','dict_title_mw','dict_title_central')))
#   l=region_list[region_list$region=='West',]
#   # y=l[1,2]
#   # z=l[1,3]
#   y=dict_sector_west
#   z=dict_title_west
#   readin <- function(file) {
#     tbls1 <- pdf_text(upload)
#     text <- as.character((strsplit(tbls1," ")))
#     keywords <- (removeWords(text,stop))
# 
#     return(keywords)
#   }
#   match <- function(ind,file) {
#     count <- 0
#     keywords <- readin(file)
#     for(i in 1:25) {
#       if(grepl(ind[i], keywords, ignore.case = TRUE) == TRUE) {
#         count <- count + 1
#       }
#     }
#     return(count)
#   }
# 
#   #### By sector
# 
# 
#   ac_l <- y$`Accounting & Legal`
#   ad <- y$`Aerospace and Defense`
#   af <- y$`Agriculture & Forestry`
#   aer <- y$`Arts, Entertainment & Recreation`
#   auto <- y$`Auto`
#   bf <- y$`Banks and Financial Services`
#   b_p <- y$`Biotech & Pharmaceuticals`
#   bs <- y$`Business Services`
#   ce <- y$`Computers and Electronics`
#   con <- y$`Construction, Repair & Maintenance`
#   cons <- y$`Consulting and Business Services`
#   goods <- y$`Consumer Goods and Services`
#   edu <- y$`Education and Schools`
#   energy <- y$`Energy and Utilities`
#   food <- y$`Food and Beverages`
#   gov <- y$`Government`
#   hlth <- y$`Health Care`
#   hr <- y$`Human Resources and Staffing`
#   ind <- y$`Industrial Manufacturing`
#   it <- y$`Information Technology`
#   ins <- y$`Insurance`
#   med <- y$`Media`
#   min <- y$`Mining & Metals`
#   np <- y$`Non-Profit`
#   oil <- y$`Oil, Gas, Energy & Utilities`
#   others <- y$`Others`
#   pharm <- y$`Pharmaceuticals`
#   real <- y$`Real Estate`
#   rest <- y$`Restaurants, Bars & Food Services`
#   ret <- y$`Retail`
#   tel <- y$`Telecommunications`
#   trans <- y$`Transportation & Logistics`
#   travel <- y$`Travel & Tourism`
# 
#   to_ret1 <- c(match(ac_l,txt), match(ad,txt), match(af,txt), match(aer,txt), match(auto,txt), match(bf,txt), match(b_p,txt), match(bs,txt),
#                match(ce,txt), match(con,txt), match(cons,txt), match(goods,txt), match(edu,txt), match(energy,txt), match(food,txt), match(gov,txt),
#                match(hlth,txt), match(hr,txt), match(ind,txt), match(it,txt), match(ins,txt), match(med,txt), match(min,txt), match(np,txt), match(oil,txt),
#                match(others,txt), match(pharm,txt), match(real,txt), match(rest,txt), match(ret,txt), match(tel,txt), match(trans,txt), match(travel,txt))
# 
#   per1 <- (to_ret1/25) %>% sort(decreasing = TRUE)
#   perc1=(label_percent()(per1))
#   sectorwise <- as.data.frame(list('Sector'=names(dict_sector),'Match'=perc1))
#   sectorwise
# 
#   # By Title
#   a_bd <- z$`Business / Data Analyst`
#   bd <- z$`Business Developer`
#   bm <- z$`Business Manager`
#   ct <- z$`Consultant`
#   de <- z$`Data Engineer`
#   ds <- z$`Data Scientist`
#   ex <- z$`Executive`
#   pm <- z$`Project Manager`
# 
#   to_ret2 <- c(match(a_bd,txt), match(bd,txt), match(bm,txt), match(ct,txt), match(de,txt), match(ds,txt), match(ex,txt), match(pm,txt))
# 
#   per2 <- (to_ret2/25) %>% sort(decreasing = TRUE)
#   perc2=(label_percent()(per2))
# 
#   titlewise <- as.data.frame(list('title'=names(dict_title),'Match'=perc2))
#   titlewise
# 
# }
# 
# regionwise('West')
# 
# 
