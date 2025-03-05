library(readr)
library(dplyr)
library(forcats)
data <- read.csv("customer_train.csv")
ds_jobs <- data
head(ds_jobs)
glimpse(ds_jobs)

ds_jobs_clean <- ds_jobs %>% 
  mutate(across(where(is.character), as.factor)) %>% 
  mutate(company_size = fct_collapse(company_size, 
                                     'Micro' = '<10',
                                     'Small' = c('10-49', '50-99'), 
                                     'Medium' = c('100-499', '500-999'), 
                                     'Large' = c('1000-4999', '5000-9999', '10000+')
                                     )) %>% 
  mutate(experience = fct_collapse(experience, 
                                   '<5' = c('<1', as.character(1:4)), 
                                   '5-10' = as.character(5:10), 
                                   '>10' = c(as.character(11:20), '>20')
                                   )) %>% 
  filter(company_size == 'Large', experience == '>10')
ds_jobs_clean
