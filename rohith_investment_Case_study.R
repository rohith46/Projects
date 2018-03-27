getwd()
setwd("/Users/rohithbharatha/Desktop/practice Data science/Analysis/")

companies <- read.delim("companies.txt", sep = "\t",stringsAsFactors = F)
rounds2 <- read.csv("rounds2.csv",stringsAsFactors = F)

library(tidyr)
library(stringr)
library(dplyr)

companies$permalink <- str_to_lower(companies$permalink)
rounds2$permalink <- str_to_lower(rounds2$company_permalink)
companies <- mutate(companies,permalink_lower=str_to_lower(permalink))
companies <- subset(companies,select=-permalink)
rounds2 <- mutate(rounds2,company_permalink_lower=str_to_lower(company_permalink))
rounds2 <- subset(rounds2,select = -company_permalink)

length(unique(companies$permalink))
length(unique(rounds2$company_permalink))

length(which(is.na(companies$permalink)))
length(which(is.na(rounds2$company_permalink)))

colnames(rounds2)[6] <- "permalink"
colnames(companies)[10] <- "permalink"

master_frame <- merge(companies, rounds2, by = "permalink")

sum(is.na(master_frame$raised_amount_usd))
#1st way 
na_indices <-which(is.na(master_frame$raised_amount_usd))
master_frame$raised_amount_usd[na_indices] <- 0

#2nd way 
#master_frame$raised_amount_usd[is.na(master_frame$raised_amount_usd)==T] <-0

sum(is.na(master_frame$raised_amount_usd))

unique(master_frame$funding_round_type)

venture <- subset(master_frame,funding_round_type == "venture")

mean(venture$raised_amount_usd)

angel <- subset(master_frame,funding_round_type == "angel")

mean(angel$raised_amount_usd)

seed <- subset(master_frame,funding_round_type == "seed")

mean(seed$raised_amount_usd)

private_equity <- subset(master_frame,funding_round_type == "private_equity")

mean(private_equity$raised_amount_usd)

## fund_types <- filter(master_frame,funding_round_type == "venture" 
##        | funding_round_type == "angel" | funding_round_type == "seed" |funding_round_type == "private_equity" )

#fund_types %>% 
 # group_by(funding_round_type) %>% 
 # summarise(avg_fund_by_type = mean(raised_amount_usd)) %>%
 # arrange(desc(avg_fund_by_type))

#find by raised amount
master_frame %>%
  group_by(country_code) %>%
  summarise(total_funds_by_country = sum(raised_amount_usd)) %>%
  arrange (desc(total_funds_by_country))

#find missing values 

length(which(master_frame$country_code==" "))
# percentage is 7.5
length(which(master_frame$country_code==" "))/nrow(master_frame)

master_frame$country_code[which(master_frame$country_code== " ")] <- "Missing"

venture$country_code[which(venture$country_code == "")] <- "Missing"

highest_funding_countries <- aggregate(raised_amount_usd~country_code, venture, sum)

highest_funding_countries_sorted <- highest_funding_countries[order
                                        (highest_funding_countries$raised_amount_usd, decreasing = T),]
#top9 <- venture %>% group_by(country_code) %>%
# summarise(total_funds_by_country = sum(raised_amount_usd)) %>%
#  arrange (desc(total_funds_by_country))

top9 <- head(highest_funding_countries_sorted,9)

# top9 <- top9[1:9,]

master_frame$category_list <- gsub("\\|.*","", master_frame$category_list )

# import mapping file
mapping <- read.csv("mapping.csv", header=T, stringsAsFactors = F,check.names = F)

# Let's glance the data and find out the data quality issues

View(mapping) 

# before going ahead, let's change "category_list"column to lowercase. 
mapping$category_list <- tolower(mapping$category_list)

# Also, changing "category_list" column of master_frame to lowercase 
master_frame$category_list <- tolower(master_frame$category_list)

# You will observe that some of "category_list" strings 
# are spelled wrong such as "0notechnology" [index- 472]should be "nanotechnology" 

#or "0tural Language Processing"[index- 473] should be 
# "natural language processing". 

# But incorrect spelling follows a consistent pattern. You can inspect that the "na" is replaced with "0" in both 
# the above examples and also where the na comes along. 

mapping$zeros <- str_detect(mapping$category_list, "0")
sum(mapping$zeros)

# But detect only detects whether 0 occurs, does not count the number of 0s
mapping$zeros <- str_count(mapping$category_list, "0")
sum(mapping$zeros)


# Let's treat this: 
mapping$category_list <- str_replace(mapping$category_list, "[0]", "na")
mapping$zeros <- str_count(mapping$category_list, "0")
sum(mapping$zeros)
# Because str_replace replaces only one occurrence, not multiple

# Hence we use str_replace_all
mapping$category_list <- str_replace_all(mapping$category_list, 
                                              "[0]", "na")
mapping$zeros <- str_count(mapping$category_list, "0")
sum(mapping$zeros)

# However, this may also end up replacing 0 in strings with na, 
# even when we don't want it to.


# "enterprise 2.0" could change to "enterprise 2.na". 
# Let's treat this as well:

mapping$category_list[which(mapping$category_list=="enterprise 2.na")] <- "enterprise 2.0"

# Converting mapping_file to long format: 

mapping <- gather(mapping,main_sector,my_val,-category_list)
mapping <- mapping[which(mapping$my_val==1),]
mapping$my_val <- NULL
mapping <- mapping[order(mapping$category_list),]

master_frame$check = master_frame$category_list %in% mapping$category_list
sum(!master_frame$check)

levels(factor(master_frame$category_list[which(master_frame$check == "FALSE")]))

sector_analysis_merged <- merge(x=master_frame, y= mapping, by = "category_list", all.x = T)

nas <- subset(sector_analysis_merged, is.na(sector_analysis_merged$main_sector))

sector_analysis_merged$main_sector[which(is.na(sector_analysis_merged$main_sector))] <- "Blanks"

levels(factor(sector_analysis_merged$main_sector))

USA <- subset(sector_analysis_merged, country_code == "USA" & funding_round_type == "venture" 
              & between (raised_amount_usd, 5e+06 , 15e+06) )

USA_Summary <- USA %>% group_by(main_sector) %>% 
               summarise (frequency=n(),investment_by_sector = sum(raised_amount_usd)) %>% 
                arrange(desc(frequency))

USA_Summary <- USA_Summary[order(USA_Summary$frequency, decreasing = T), ]

sum(USA_Summary$investment_by_sector)


USA_Summary1 <- USA %>% filter(main_sector == "Social, Finance, Analytics, Advertising") %>%
                group_by(name) %>%
                summarise(frequency=n(),investment_by_company = sum(raised_amount_usd))  %>% 
                arrange(desc(investment_by_company))


USA_company_Summary2 <- USA %>% filter(main_sector == "Cleantech / Semiconductors") %>%
                group_by(name) %>%
                summarise(frequency=n(),investment_by_company=sum(raised_amount_usd)) %>%
                  arrange(desc(investment_by_company))



##GBR summary 

#  GBR <- sector_analysis_merged %>%
#     filter(country_code == "GBR" & funding_round_type == "venture" & between(raised_amount_usd,5e+06,15e+06)) %>%
#     group_by(main_sector) %>%
#      summarise(frequency=n(),investment_by_sector= sum(raised_amount_usd)) %>%
#      arrange(desc(frequency))

GBR <- subset(sector_analysis_merged, country_code == "GBR" & 
                funding_round_type == "venture" & between(raised_amount_usd,5e+06,15e+06))

GBR_Summary <- GBR %>%
              group_by(main_sector) %>%
              summarise(frequency=n(),investment_by_sector= sum(raised_amount_usd)) %>%
              arrange(desc(investment_by_sector))

sum(GBR_Summary$investment_by_sector)

sum(GBR_Summary$frequency)

GBR_Summary_company1<- GBR %>% filter (main_sector=="Cleantech / Semiconductors") %>%
                    group_by(name) %>%
                    summarise(frequency=n(),investment_by_company1=sum(raised_amount_usd)) %>%
                    arrange(desc(investment_by_company1))

GBR_Summary_company2<- GBR %>% filter (main_sector=="Social, Finance, Analytics, Advertising") %>%
  group_by(name) %>%
  summarise(frequency=n(),investment_by_company2=sum(raised_amount_usd)) %>%
  arrange(desc(investment_by_company2))

###india summary

IND <- subset(sector_analysis_merged , country_code == "IND" &
              funding_round_type == "venture" & between(raised_amount_usd,5e+06,15e+06))
      
IND_Summary <- sector_analysis_merged %>%
      filter(country_code == "IND", funding_round_type == "venture", between(raised_amount_usd,5e+06,15e+06)) %>%
      group_by(main_sector) %>%
      summarise(frequency=n(),investment_by_sector= sum(raised_amount_usd)) %>%
      arrange(desc(investment_by_sector))

IND_Summary_company1<- IND %>% filter (main_sector=="News, Search and Messaging") %>%
  group_by(name) %>%
  summarise(frequency=n(),investment_by_company1=sum(raised_amount_usd)) %>%
  arrange(desc(investment_by_company1))

IND_Summary_company2<- IND %>% filter (main_sector=="Social, Finance, Analytics, Advertising") %>%
  group_by(name) %>%
  summarise(frequency=n(),investment_by_company2=sum(raised_amount_usd)) %>%
  arrange(desc(investment_by_company2))

