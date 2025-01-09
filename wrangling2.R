s <- c("5'10", "6'1")
tab <- data.frame(x = s)
library(tidyverse)
tab %>% separate(x, c("feet", "inches"), sep = "'")
tab %>% extract(x, c("feet", "inches"), regex ="(\\d)'(\\d{1,2})")
#second example - some heights with unusual formats 
s <- c("5'10", "6'1\"", "5'8inches")
tab <- data.frame(x =s)

tab %>% separate(x, c("feet", "inches"), sep = "'", fill = "right")
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")
yes <- c("1,7", "1, 8", "2, " )
no <- c("5,8", "5,3,2", "1.7")
s <- c(yes, no)
str_replace(s, "^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2")
library(dslabs)

schedule <- data.frame(day = c("Monday", "Tuesday"), 
           staff = c("Mandy, Chris and Laura", "Steve, Ruth, Frank"))

str_split(schedule$staff, ", | and ")
tidy <- schedule %>% 
  mutate(staff = str_split(staff, ", | and ")) %>% 
  unnest()
tidy1 <- separate(schedule, staff, into = c("s1","s2","s3"), sep = “,”) %>% 
  gather(key = s, value = staff, s1:s3)

data("gapminder")
dat <- gapminder %>% filter(region == "Middle Africa") %>% 
  mutate(country_short = recode(country, 
                                "Central African Republic" = "CAR", 
                                "Congo, Dem. Rep." = "DRC",
                                "Equatorial Guinea" = "Eq. Guinea"))
dat  
  
library(rvest)
library(stringr)  
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[6]] %>% html_table(fill = T)
polls  
names(polls) <- c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes")
head(polls)
polls1 <- polls %>% filter(str_detect(remain, "%"))
dim(polls1)
dim(polls)

parse_number(polls1$remain)/100
as.numeric(str_remove(polls1$remain, "%"))/100
as.numeric(str_replace(polls1$remain, "%", ""))/100

polls1$undecided

str_replace(polls1$undecided, "N/A", "0%")
polls$undecided
polls1$dates
temp <- str_extract_all(polls1$dates, "\\d{1,2}\\s[a-zA-Z]+")
library(lubridate)
now()
Sys.Date()
Sys.time()
hour(now())

#inspect the startdate column of 2016 polls data, a Date type
data("polls_us_election_2016")
polls_us_election_2016$startdate %>% head()
class(polls_us_election_2016$startdate)
as.numeric(polls_us_election_2016$startdate) %>% head
#ggplot is aware of dates
polls_us_election_2016 %>% 
  filter(pollster == "Ipsos" & state == "U.S.") %>%
  ggplot(aes(startdate, rawpoll_trump))+
  geom_line()
set.seed(2)
dates <- sample(polls_us_election_2016$startdate, 10) %>% sort()
dates
#extract month, day, year from date strings
data.frame(date = dates, 
           month = month(dates), 
           day = day(dates), 
           year = year(dates))
month(dates, label = T)
now() %>% minute()
now("GMT")
hms("23:54:12")




