library(tidyverse)
library(scales)
library(lubridate)
library(tidytext)
library(wordcloud)
library(devtools)
library(tm)
library(stringr)
library(docxtractr)

#clean data set-ba_da
ba <- read.csv('BusinessAnalyst.csv', na.strings=c("","NA"))
da <- read.csv('DataAnalyst.csv', na.strings=c("","NA"))
ba1 <- ba[1:3692, -c(1:2)]
ba2 <- ba[3693:4092, -c(16:17)]
colnames(ba2) <- colnames(ba1)
alt_ba <- rbind(ba1, ba2)
alt_ba <- alt_ba[, -c(4, 7:10, 13:14)]
alt_da <- da[, -c(1, 5, 8:11, 14:15)]
ba_da <- rbind(alt_ba, alt_da) %>% filter(Sector != -1)

#clean data set-gd
gddata <- read.csv('G.DS.csv', na.strings=c("","NA"))
alt_gd <- gddata[, -c(4, 7:10, 13:15)]

#clean data set-id
iddata <- read.csv('I.DS.csv', na.strings=c("","NA"))
alt_id <- iddata[, -c(1, 3, 5, 7, 9:12, 14:15, 17:43)] %>% mutate(sector=Company_Industry)
colnames(alt_id)=colnames(alt_gd)

#removing html strings
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

# common and stopwords
common <- readLines('common.csv')
g <- stopwords("en")
h <- stopwords("SMART")
i <- c(g,h)
stop <- i[i != i[532]]

# location
state <- read.csv('state.csv')
state_codes <- read.csv('state_codes.csv')

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
sectconv <- read.csv('Sector.csv', na.strings=c("","NA"))
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

reviewrbind <- rbind(reviews_gd, reviews_id, reviews_ba_da)
regions <- reviewrbind %>%
  group_by(Region) %>%
  summarize(n=n())

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


dict_title_central <- vector(mode="list",length = 6)
names(dict_title_central) <- c(unique(centraltop$Title))
dict_title_central[[1]] <- c(centraltop$word[1:100])
dict_title_central[[2]] <- c(centraltop$word[101:200])
dict_title_central[[3]] <- c(centraltop$word[201:300])
dict_title_central[[4]] <- c(centraltop$word[301:400])
dict_title_central[[5]] <- c(centraltop$word[401:500])
dict_title_central[[6]] <- c(centraltop$word[501:600])

dict_title_central

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

dict_title_mw <- vector(mode="list",length = 7)
names(dict_title_mw) <- c(unique(mwtop$Title))
dict_title_mw[[1]] <- c(mwtop$word[1:100])
dict_title_mw[[2]] <- c(mwtop$word[101:200])
dict_title_mw[[3]] <- c(mwtop$word[201:300])
dict_title_mw[[4]] <- c(mwtop$word[301:400])
dict_title_mw[[5]] <- c(mwtop$word[401:500])
dict_title_mw[[6]] <- c(mwtop$word[501:600])
dict_title_mw[[7]] <- c(mwtop$word[601:700])

dict_title_mw

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

dict_title_mountain <- vector(mode="list",length = 6)
names(dict_title_mountain) <- c(unique(mountaintop$Title))
dict_title_mountain[[1]] <- c(mountaintop$word[1:100])
dict_title_mountain[[2]] <- c(mountaintop$word[101:200])
dict_title_mountain[[3]] <- c(mountaintop$word[201:300])
dict_title_mountain[[4]] <- c(mountaintop$word[301:400])
dict_title_mountain[[5]] <- c(mountaintop$word[401:500])
dict_title_mountain[[6]] <- c(mountaintop$word[501:600])


dict_title_mountain


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

dict_title_ne <- vector(mode="list",length = 7)
names(dict_title_ne) <- c(unique(netop$Title))
dict_title_ne[[1]] <- c(netop$word[1:100])
dict_title_ne[[2]] <- c(netop$word[101:200])
dict_title_ne[[3]] <- c(netop$word[201:300])
dict_title_ne[[4]] <- c(netop$word[301:400])
dict_title_ne[[5]] <- c(netop$word[401:500])
dict_title_ne[[6]] <- c(netop$word[501:600])
dict_title_ne[[7]] <- c(netop$word[601:700])

dict_title_ne




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


dict_title_se <- vector(mode="list",length = 7)
names(dict_title_se) <- c(unique(setop$Title))
dict_title_se[[1]] <- c(setop$word[1:100])
dict_title_se[[2]] <- c(setop$word[101:200])
dict_title_se[[3]] <- c(setop$word[201:300])
dict_title_se[[4]] <- c(setop$word[301:400])
dict_title_se[[5]] <- c(setop$word[401:500])
dict_title_se[[6]] <- c(setop$word[501:600])
dict_title_se[[7]] <- c(setop$word[601:700])

dict_title_se


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

dict_title_west <- vector(mode="list",length = 7)
names(dict_title_west) <- c(unique(westtop$Title))
dict_title_west[[1]] <- c(westtop$word[1:100])
dict_title_west[[2]] <- c(westtop$word[101:200])
dict_title_west[[3]] <- c(westtop$word[201:300])
dict_title_west[[4]] <- c(westtop$word[301:400])
dict_title_west[[5]] <- c(westtop$word[401:500])
dict_title_west[[6]] <- c(westtop$word[501:600])
dict_title_west[[7]] <- c(westtop$word[601:700])

dict_title_west


#####Dictionary list


dict_sector
dict_title
dict_title_central
dict_title_mw
dict_title_mountain
dict_title_ne
dict_title_se
dict_title_west


##### To get resume matches

readin <- function(file) {
  tbls1 <- docx_extract_all_tbls(read_docx(file))
  text <- as.character((strsplit(as.data.frame(tbls1)[1,1]," "))[1])
  keywords <- (removeWords(text,stop))
  return(keywords)
}
readin('Julian.docx')

match <- function(ind) {
  count <- 0
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
  auto <- dict_sector$Auto
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
  gov <- dict_sector$Government
  hlth <- dict_sector$`Health Care`
  hr <- dict_sector$`Human Resources and Staffing`
  ind <- dict_sector$`Industrial Manufacturing`
  it <- dict_sector$`Information Technology`
  ins <- dict_sector$Insurance
  med <- dict_sector$Media
  min <- dict_sector$`Mining & Metals`
  np <- dict_sector$`Non-Profit`
  oil <- dict_sector$`Oil, Gas, Energy & Utilities`
  pharm <- dict_sector$Pharmaceuticals
  real <- dict_sector$`Real Estate`
  rest <- dict_sector$`Restaurants, Bars & Food Services`
  ret <- dict_sector$Retail
  tel <- dict_sector$Telecommunications
  trans <- dict_sector$`Transportation & Logistics`
  travel <- dict_sector$`Travel & Tourism`
  
  to_ret <- c(match(ac_l), match(ad), match(af), match(aer), match(auto), match(bf), match(b_p), match(bs),
    match(ce), match(con), match(cons), match(goods), match(edu), match(energy), match(food), match(gov), 
    match(hlth), match(hr), match(ind), match(it), match(ins), match(med), match(min), match(np), match(oil),
    match(pharm), match(real), match(rest), match(ret), match(tel), match(trans), match(travel))
  
  per <- (to_ret/25) %>%
    sort(decreasing = TRUE)
  
  return(per)
}

sector()

