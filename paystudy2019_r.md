---
title: "Appendix B: Analysis in R"
output:
  html_document:
    keep_md: true
---





# Washington Post Newspaper Guild Pay Study 2019

This is the study of Washington Post Guild members' salaries based on data turned over by management of The Washington Post on July 2, 2019, pursuant to a request by members of the Guild. Management turned over two Excel files: one file detailing the salaries of current guild members working for The Post (as of the date of transmission) and one file detailing the salaries of past guild members who worked for The Post and have left the organization in the past five years.

What follows is an attempt to understand pay at The Washington Post. No individual analysis should be taken on its own to mean that disparities in pay do or do not exist. This study will start with summary analysis of trends and will dive deeper as the study goes on. 

The only data manipulation done prior to analysis was taking the data out of Excel and putting the files into CSV files, converting dates from 'MM/DD/YYYY' to 'YYYY-MM-DD' and removing commas from monetary columns where values exceeded 1,000.

## Importing data


```r
library(tidyverse)
library(dplyr)
library(lubridate)
library(data.table)
library(fastDummies)

df <- read_csv('csvs/active_wd.csv')
df2 <- read_csv('csvs/terminated_wd.csv')
```

## Add fields for analysis

### Add age field


```r
data_date <- as.Date('2019-07-02')

df <- df %>% mutate(date_of_birth = ymd(date_of_birth),
                    age = floor(decimal_date(data_date) - decimal_date(date_of_birth)))

df2 <- df2 %>% mutate(date_of_birth = ymd(date_of_birth),
                    age = floor(decimal_date(data_date) - decimal_date(date_of_birth)))
```

### Add years of service field


```r
df <- df %>% mutate(hire_date = ymd(hire_date),
                    years_of_service = floor(decimal_date(data_date) - decimal_date(hire_date)))

df2 <- df2 %>% mutate(hire_date = ymd(hire_date),
                    years_of_service = floor(decimal_date(data_date) - decimal_date(hire_date)))
```

### Add field for 5-year age groups


```r
agebreaks5 <- c(0,25,30,35,40,45,50,55,60,65,100)
agelabels5 <- c('<25','25-29','30-34','35-39','40-44', '45-49','50-54','55-59','60-64','65+')

setDT(df)[, age_group_5 := cut(age, 
                                breaks = agebreaks5, 
                                right = FALSE, 
                                labels = agelabels5)]

setDT(df2)[, age_group_5 := cut(age, 
                                breaks = agebreaks5, 
                                right = FALSE, 
                                labels = agelabels5)]
```

### Add field for 10-year age groups


```r
agebreaks10 <- c(0,25,35,45,55,65,100)
agelabels10 <- c('<25','25-34','35-44','45-54','55-64','65+')

setDT(df)[, age_group_10 := cut(age, 
                                breaks = agebreaks10, 
                                right = FALSE, 
                                labels = agelabels10)]

setDT(df2)[, age_group_10 := cut(age, 
                                breaks = agebreaks10, 
                                right = FALSE, 
                                labels = agelabels10)]
```

### Add field for years-of-service groups


```r
yosbreaks <- c(0,1,3,6,11,16,21,26,100)
yoslabels <- c('0','1-2','3-5','6-10','11-15','16-20','21-25','25+')

setDT(df)[, years_of_service_grouped := cut(years_of_service, 
                                breaks = yosbreaks, 
                                right = FALSE, 
                                labels = yoslabels)]

setDT(df2)[, years_of_service_grouped := cut(years_of_service, 
                                breaks = yosbreaks, 
                                right = FALSE, 
                                labels = yoslabels)]
```

### Group departments


```r
df <- df %>%
    mutate(dept=case_when(
    department == 'News' ~ 'News', 
    department == 'Editorial' ~ 'News',
    department == 'Client Solutions' ~ 'Commercial',
    department == 'Circulation' ~ 'Commercial',
    department == 'Finance' ~ 'Commercial',
    department == 'Marketing' ~ 'Commercial',
    department == 'WP News Media Services' ~ 'Commercial',
    department == 'Production' ~ 'Commercial',
    department == 'Public Relations' ~ 'Commercial',
    department == 'Administration' ~ 'Commercial',
    department == 'Product'~ 'Commercial',
    TRUE ~ 'Other'))

df2 <- df2 %>%
    mutate(dept=case_when(
    department == 'News' ~ 'News', 
    department == 'Editorial' ~ 'News',
    department == 'News Service and Syndicate' ~ 'News', 
    department == 'Audience Development and Insights' ~ 'Commercial',
    department == 'Client Solutions' ~ 'Commercial',
    department == 'Customer Care and Logistics' ~ 'Commercial',
    department == 'Finance' ~ 'Commercial',
    department == 'Legal' ~ 'Commercial',
    department == 'Marketing' ~ 'Commercial',
    department == 'WP News Media Services' ~ 'Commercial',
    department == 'Production' ~ 'Commercial',
    department == 'Public Relations' ~ 'Commercial',
    department == 'Washington Post Live' ~ 'Commercial',
    department == 'Product' ~ 'Commercial',
    TRUE ~ 'Other'))
```

### Group desks


```r
df <- df %>%
    mutate(desk=case_when(
    cost_center_current == '110000 News Operations' ~ 'Operations',
    cost_center_current == '110001 News Digital Operations' ~ 'Operations',
    cost_center_current == '110610 Audience Development and Engagement' ~ 'Audience Development and Engagement',
    cost_center_current == '110620 News Audio' ~ 'Audio',
    cost_center_current == '110604 Presentation Design' ~ 'Design',
    cost_center_current == '110605 Presentation' ~ 'Design',
    cost_center_current == '110664 News National Apps' ~ 'Emerging News Products',
    cost_center_current == '110665 News The Lily' ~ 'Emerging News Products',
    cost_center_current == '110666 News Snapchat' ~ 'Emerging News Products',
    cost_center_current == '110667 News By The Way' ~ 'Emerging News Products',
    cost_center_current == '113210 Economy and Business' ~ 'Financial',
    cost_center_current == '114000 Foreign Administration' ~ 'Foreign',
    cost_center_current == '114095 News Foreign Brazil' ~ 'Foreign',
    cost_center_current == '114100 Foreign Latam' ~ 'Foreign',
    cost_center_current == '114220 News Foreign Istanbul' ~ 'Foreign',
    cost_center_current == '114235 Foreign Western Europe' ~ 'Foreign',
    cost_center_current == '114300 News Foreign West Africa' ~ 'Foreign',
    cost_center_current == '114415 Foreign Hong Kong' ~ 'Foreign',
    cost_center_current == '114405 Foreign Beijing Bureau' ~ 'Foreign',
    cost_center_current == '114105 Foreign Mexico Bureau' ~ 'Foreign',
    cost_center_current == '114005 Foreign Beirut Bureau' ~ 'Foreign',
    cost_center_current == '114400 Foreign India Bureau' ~ 'Foreign',
    cost_center_current == '114410 Foreign Tokyo Bureau' ~ 'Foreign',
    cost_center_current == '114205 Foreign Islamabad Bureau' ~ 'Foreign',
    cost_center_current == '114305 Foreign Nairobi Bureau' ~ 'Foreign',
    cost_center_current == '114240 Foreign Rome Bureau' ~ 'Foreign',
    cost_center_current == '114200 Foreign London Bureau' ~ 'Foreign',
    cost_center_current == '114230 Foreign Moscow Bureau' ~ 'Foreign',
    cost_center_current == '114225 Foreign Cairo Bureau' ~ 'Foreign',
    cost_center_current == '114215 Foreign Berlin Bureau' ~ 'Foreign',
    cost_center_current == '110603 Presentation Graphics' ~ 'Graphics',
    cost_center_current == '110450 Investigative' ~ 'Investigative',
    cost_center_current == '112300 Local Politics and Government' ~ 'Local',
    cost_center_current == '110601 Multiplatform Desk' ~ 'Multiplatform',
    cost_center_current == '110500 Magazine' ~ 'National',
    cost_center_current == '113200 National Politics and Government' ~ 'National',
    cost_center_current == '113205 National Security' ~ 'National',
    cost_center_current == '113215 News National Health & Science' ~ 'National',
    cost_center_current == '113220 National Enterprise' ~ 'National',
    cost_center_current == '113235 National America' ~ 'National',
    cost_center_current == '113240 News National Environment' ~ 'National',
    cost_center_current == '110006 News Content & Research' ~ 'News Content and Research',
    cost_center_current == '110455 News Logistics' ~ 'News Logistics',
    cost_center_current == '110410 Book World' ~ 'Outlook',
    cost_center_current == '110460 Outlook' ~ 'Outlook',
    cost_center_current == '110475 Polling' ~ 'Polling',
    cost_center_current == '110015 Sports Main' ~ 'Sports',
    cost_center_current == '110300 Style' ~ 'Style',
    cost_center_current == '110435 Food' ~ 'Style',
    cost_center_current == '110485 Travel' ~ 'Style',
    cost_center_current == '110495 Local Living' ~ 'Style',
    cost_center_current == '110505 Weekend' ~ 'Style',
    cost_center_current == '110600 Universal Desk' ~ 'Universal Desk',
    cost_center_current == '110652 News Video - General' ~ 'Video',
    cost_center_current == '110663 Wake Up Report' ~ 'Other',
    cost_center_current == '115000 Editorial Administration' ~ 'Editorial',
    TRUE ~ 'non-newsroom'))

df2 <- df2 %>%
    mutate(desk=case_when(
    cost_center_current == '110000 News Operations' ~ 'Operations',
    cost_center_current == '110001 News Digital Operations' ~ 'Operations',
    cost_center_current == '110610 Audience Development and Engagement' ~ 'Audience Development and Engagement',
    cost_center_current == '110620 News Audio' ~ 'Audio',
    cost_center_current == '110604 Presentation Design' ~ 'Design',
    cost_center_current == '110605 Presentation' ~ 'Design',
    cost_center_current == '110664 News National Apps' ~ 'Emerging News Products',
    cost_center_current == '110665 News The Lily' ~ 'Emerging News Products',
    cost_center_current == '110666 News Snapchat' ~ 'Emerging News Products',
    cost_center_current == '110667 News By The Way' ~ 'Emerging News Products',
    cost_center_current == '113210 Economy and Business' ~ 'Financial',
    cost_center_current == '114000 Foreign Administration' ~ 'Foreign',
    cost_center_current == '114095 News Foreign Brazil' ~ 'Foreign',
    cost_center_current == '114100 Foreign Latam' ~ 'Foreign',
    cost_center_current == '114220 News Foreign Istanbul' ~ 'Foreign',
    cost_center_current == '114235 Foreign Western Europe' ~ 'Foreign',
    cost_center_current == '114300 News Foreign West Africa' ~ 'Foreign',
    cost_center_current == '114415 Foreign Hong Kong' ~ 'Foreign',
    cost_center_current == '114405 Foreign Beijing Bureau' ~ 'Foreign',
    cost_center_current == '114105 Foreign Mexico Bureau' ~ 'Foreign',
    cost_center_current == '114005 Foreign Beirut Bureau' ~ 'Foreign',
    cost_center_current == '114400 Foreign India Bureau' ~ 'Foreign',
    cost_center_current == '114410 Foreign Tokyo Bureau' ~ 'Foreign',
    cost_center_current == '114205 Foreign Islamabad Bureau' ~ 'Foreign',
    cost_center_current == '114305 Foreign Nairobi Bureau' ~ 'Foreign',
    cost_center_current == '114240 Foreign Rome Bureau' ~ 'Foreign',
    cost_center_current == '114200 Foreign London Bureau' ~ 'Foreign',
    cost_center_current == '114230 Foreign Moscow Bureau' ~ 'Foreign',
    cost_center_current == '114225 Foreign Cairo Bureau' ~ 'Foreign',
    cost_center_current == '114215 Foreign Berlin Bureau' ~ 'Foreign',
    cost_center_current == '110603 Presentation Graphics' ~ 'Graphics',
    cost_center_current == '110450 Investigative' ~ 'Investigative',
    cost_center_current == '112300 Local Politics and Government' ~ 'Local',
    cost_center_current == '110601 Multiplatform Desk' ~ 'Multiplatform',
    cost_center_current == '110500 Magazine' ~ 'National',
    cost_center_current == '113200 National Politics and Government' ~ 'National',
    cost_center_current == '113205 National Security' ~ 'National',
    cost_center_current == '113215 News National Health & Science' ~ 'National',
    cost_center_current == '113220 National Enterprise' ~ 'National',
    cost_center_current == '113235 National America' ~ 'National',
    cost_center_current == '113240 News National Environment' ~ 'National',
    cost_center_current == '110006 News Content & Research' ~ 'News Content and Research',
    cost_center_current == '110455 News Logistics' ~ 'News Logistics',
    cost_center_current == '110410 Book World' ~ 'Outlook',
    cost_center_current == '110460 Outlook' ~ 'Outlook',
    cost_center_current == '110475 Polling' ~ 'Polling',
    cost_center_current == '110015 Sports Main' ~ 'Sports',
    cost_center_current == '110300 Style' ~ 'Style',
    cost_center_current == '110435 Food' ~ 'Style',
    cost_center_current == '110485 Travel' ~ 'Style',
    cost_center_current == '110495 Local Living' ~ 'Style',
    cost_center_current == '110505 Weekend' ~ 'Style',
    cost_center_current == '110600 Universal Desk' ~ 'Universal Desk',
    cost_center_current == '110652 News Video - General' ~ 'Video',
    cost_center_current == '110663 Wake Up Report' ~ 'Other',
    cost_center_current == '115000 Editorial Administration' ~ 'Editorial',
    TRUE ~ 'non-newsroom'))
```

### Group desks by median salary ranges


```r
df <- df %>%
    mutate(tier=case_when(
    desk == 'National' ~ 'Tier 1',
    desk == 'Foreign' ~ 'Tier 1',
    desk == 'Financial' ~ 'Tier 1',
    desk == 'Investigative' ~ 'Tier 1',
    desk == 'Style' ~ 'Tier 2',
    desk == 'Local' ~ 'Tier 2',
    desk == 'Graphics' ~ 'Tier 2',
    desk == 'Universal Desk' ~ 'Tier 2',
    desk == 'Sports' ~ 'Tier 2',
    desk == 'Outlook' ~ 'Tier 2',
    desk == 'Editorial' ~ 'Tier 2',
    desk == 'Audio' ~ 'Tier 3',
    desk == 'Polling' ~ 'Tier 3',
    desk == 'Design' ~ 'Tier 3',
    desk == 'Operations' ~ 'Tier 3',
    desk == 'Multiplatform' ~ 'Tier 3',
    desk == 'Video' ~ 'Tier 3',
    desk == 'Audience Development and Engagement' ~ 'Tier 3',
    desk == 'News Logistics' ~ 'Tier 4',
    desk == 'News Content and Research' ~ 'Tier 4',
    desk == 'Emerging News Products' ~ 'Tier 4',
    desk == 'Other' ~ 'Tier 4',
    TRUE ~ 'Other'))

df2 <- df2 %>%
    mutate(tier=case_when(
    desk == 'National' ~ 'Tier 1',
    desk == 'Foreign' ~ 'Tier 1',
    desk == 'Financial' ~ 'Tier 1',
    desk == 'Investigative' ~ 'Tier 1',
    desk == 'Style' ~ 'Tier 2',
    desk == 'Local' ~ 'Tier 2',
    desk == 'Graphics' ~ 'Tier 2',
    desk == 'Universal Desk' ~ 'Tier 2',
    desk == 'Sports' ~ 'Tier 2',
    desk == 'Outlook' ~ 'Tier 2',
    desk == 'Editorial' ~ 'Tier 2',
    desk == 'Audio' ~ 'Tier 3',
    desk == 'Polling' ~ 'Tier 3',
    desk == 'Design' ~ 'Tier 3',
    desk == 'Operations' ~ 'Tier 3',
    desk == 'Multiplatform' ~ 'Tier 3',
    desk == 'Video' ~ 'Tier 3',
    desk == 'Audience Development and Engagement' ~ 'Tier 3',
    desk == 'News Logistics' ~ 'Tier 4',
    desk == 'News Content and Research' ~ 'Tier 4',
    desk == 'Emerging News Products' ~ 'Tier 4',
    desk == 'Other' ~ 'Tier 4',
    TRUE ~ 'Other'))
```

### Group race and ethnicity


```r
df <- df %>%
    mutate(race_grouping=case_when(
    race_ethnicity == 'White (United States of America)' ~ 'white',
    race_ethnicity == 'Black or African American (United States of America)' ~ 'person of color',
    race_ethnicity == 'Asian (United States of America)' ~ 'person of color',
    race_ethnicity == 'Hispanic or Latino (United States of America)' ~ 'person of color',
    race_ethnicity == 'Two or More Races (United States of America)' ~ 'person of color',
    race_ethnicity == 'American Indian or Alaska Native (United States of America)' ~ 'person of color',
    race_ethnicity == 'Native Hawaiian or Other Pacific Islander (United States of America)' ~ 'person of color',
    TRUE ~ 'unknown'))

df2 <- df2 %>%
    mutate(race_grouping=case_when(
    race_ethnicity == 'White (United States of America)' ~ 'white',
    race_ethnicity == 'Black or African American (United States of America)' ~ 'person of color',
    race_ethnicity == 'Asian (United States of America)' ~ 'person of color',
    race_ethnicity == 'Hispanic or Latino (United States of America)' ~ 'person of color',
    race_ethnicity == 'Two or More Races (United States of America)' ~ 'person of color',
    race_ethnicity == 'American Indian or Alaska Native (United States of America)' ~ 'person of color',
    race_ethnicity == 'Native Hawaiian or Other Pacific Islander (United States of America)' ~ 'person of color',
    TRUE ~ 'unknown'))
```

### Employee pay change grouping


```r
reason_for_change1 <- df[,c('business_process_reason1','base_pay_change1','effective_date1','pay_rate_type1','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')]
reason_for_change2 <- df[,c('business_process_reason2','base_pay_change2','effective_date2','pay_rate_type2','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')]
reason_for_change3 <- df[,c('business_process_reason3','base_pay_change3','effective_date3','pay_rate_type3','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')]
reason_for_change4 <- df[,c('business_process_reason4','base_pay_change4','effective_date4','pay_rate_type4','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')]
reason_for_change5 <- df[,c('business_process_reason5','base_pay_change5','effective_date5','pay_rate_type5','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')]
reason_for_change6 <- df[,c('business_process_reason6','base_pay_change6','effective_date6','pay_rate_type6','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')]
reason_for_change7 <- df[,c('business_process_reason7','base_pay_change7','effective_date7','pay_rate_type7','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')]
reason_for_change8 <- df[,c('business_process_reason8','base_pay_change8','effective_date8','pay_rate_type8','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')]
reason_for_change9 <- df[,c('business_process_reason9','base_pay_change9','effective_date9','pay_rate_type9','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')]
reason_for_change10 <- df[,c('business_process_reason10','base_pay_change10','effective_date10','pay_rate_type10','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')]
reason_for_change11 <- df[,c('business_process_reason11','base_pay_change11','effective_date11','pay_rate_type11','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')]
reason_for_change12 <- df[,c('business_process_reason12','base_pay_change12','effective_date12','pay_rate_type12','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')]
reason_for_change13 <- df[,c('business_process_reason13','base_pay_change13','effective_date13','pay_rate_type13','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')]
reason_for_change14 <- df[,c('business_process_reason14','base_pay_change14','effective_date14','pay_rate_type14','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')]
reason_for_change15 <- df[,c('business_process_reason15','base_pay_change15','effective_date15','pay_rate_type15','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')]
reason_for_change16 <- df[,c('business_process_reason16','base_pay_change16','effective_date16','pay_rate_type16','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')]
reason_for_change17 <- df[,c('business_process_reason17','base_pay_change17','effective_date17','pay_rate_type17','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')]
reason_for_change18 <- df[,c('business_process_reason18','base_pay_change18','effective_date18','pay_rate_type18','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')]
reason_for_change19 <- df2[,c('business_process_reason1','base_pay_change1','effective_date1','pay_rate_type1','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')]
reason_for_change20 <- df2[,c('business_process_reason2','base_pay_change2','effective_date2','pay_rate_type2','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')]
reason_for_change21 <- df2[,c('business_process_reason3','base_pay_change3','effective_date3','pay_rate_type3','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')]
reason_for_change22 <- df2[,c('business_process_reason4','base_pay_change4','effective_date4','pay_rate_type4','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')]
reason_for_change23 <- df2[,c('business_process_reason5','base_pay_change5','effective_date5','pay_rate_type5','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')]
reason_for_change24 <- df2[,c('business_process_reason6','base_pay_change6','effective_date6','pay_rate_type6','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')]
reason_for_change25 <- df2[,c('business_process_reason7','base_pay_change7','effective_date7','pay_rate_type7','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')]
reason_for_change26 <- df2[,c('business_process_reason8','base_pay_change8','effective_date8','pay_rate_type8','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')]
reason_for_change27 <- df2[,c('business_process_reason9','base_pay_change9','effective_date9','pay_rate_type9','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')]
reason_for_change28 <- df2[,c('business_process_reason10','base_pay_change10','effective_date10','pay_rate_type10','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')]
reason_for_change29 <- df2[,c('business_process_reason11','base_pay_change11','effective_date11','pay_rate_type11','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')]
reason_for_change30 <- df2[,c('business_process_reason12','base_pay_change12','effective_date12','pay_rate_type12','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')]
reason_for_change31 <- df2[,c('business_process_reason13','base_pay_change13','effective_date13','pay_rate_type13','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')]

names(reason_for_change1) <- c('business_process_reason','base_pay_change','effective_date','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')
names(reason_for_change2) <- c('business_process_reason','base_pay_change','effective_date','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')
names(reason_for_change3) <- c('business_process_reason','base_pay_change','effective_date','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')
names(reason_for_change4) <- c('business_process_reason','base_pay_change','effective_date','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')
names(reason_for_change5) <- c('business_process_reason','base_pay_change','effective_date','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')
names(reason_for_change6) <- c('business_process_reason','base_pay_change','effective_date','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')
names(reason_for_change7) <- c('business_process_reason','base_pay_change','effective_date','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')
names(reason_for_change8) <- c('business_process_reason','base_pay_change','effective_date','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')
names(reason_for_change9) <- c('business_process_reason','base_pay_change','effective_date','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')
names(reason_for_change10) <- c('business_process_reason','base_pay_change','effective_date','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')
names(reason_for_change11) <- c('business_process_reason','base_pay_change','effective_date','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')
names(reason_for_change12) <- c('business_process_reason','base_pay_change','effective_date','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')
names(reason_for_change13) <- c('business_process_reason','base_pay_change','effective_date','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')
names(reason_for_change14) <- c('business_process_reason','base_pay_change','effective_date','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')
names(reason_for_change15) <- c('business_process_reason','base_pay_change','effective_date','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')
names(reason_for_change16) <- c('business_process_reason','base_pay_change','effective_date','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')
names(reason_for_change17) <- c('business_process_reason','base_pay_change','effective_date','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')
names(reason_for_change18) <- c('business_process_reason','base_pay_change','effective_date','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')
names(reason_for_change19) <- c('business_process_reason','base_pay_change','effective_date','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')
names(reason_for_change20) <- c('business_process_reason','base_pay_change','effective_date','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')
names(reason_for_change21) <- c('business_process_reason','base_pay_change','effective_date','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')
names(reason_for_change22) <- c('business_process_reason','base_pay_change','effective_date','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')
names(reason_for_change23) <- c('business_process_reason','base_pay_change','effective_date','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')
names(reason_for_change24) <- c('business_process_reason','base_pay_change','effective_date','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')
names(reason_for_change25) <- c('business_process_reason','base_pay_change','effective_date','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')
names(reason_for_change26) <- c('business_process_reason','base_pay_change','effective_date','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')
names(reason_for_change27) <- c('business_process_reason','base_pay_change','effective_date','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')
names(reason_for_change28) <- c('business_process_reason','base_pay_change','effective_date','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')
names(reason_for_change29) <- c('business_process_reason','base_pay_change','effective_date','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')
names(reason_for_change30) <- c('business_process_reason','base_pay_change','effective_date','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')
names(reason_for_change31) <- c('business_process_reason','base_pay_change','effective_date','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating')


reason_for_change_combined <- rbind(reason_for_change1,reason_for_change2,reason_for_change3,reason_for_change4,reason_for_change5,reason_for_change6,reason_for_change7,reason_for_change8,reason_for_change9,reason_for_change10,reason_for_change11,reason_for_change12,reason_for_change13,reason_for_change14,reason_for_change15,reason_for_change16,reason_for_change17,reason_for_change18,reason_for_change19,reason_for_change20,reason_for_change21,reason_for_change22,reason_for_change23,reason_for_change24,reason_for_change25,reason_for_change26,reason_for_change27,reason_for_change28,reason_for_change29,reason_for_change30,reason_for_change31)
```

### Employee performance evaluation grouping


```r
fifteen1 <- df[,c('2015_annual_performance_rating','gender','race_ethnicity','race_grouping','dept')]
fifteen2 <- df2[,c('2015_annual_performance_rating','gender','race_ethnicity','race_grouping','dept')]
sixteen1 <- df[,c('2016_annual_performance_rating','gender','race_ethnicity','race_grouping','dept')]
sixteen2 <- df2[,c('2016_annual_performance_rating','gender','race_ethnicity','race_grouping','dept')]
seventeen1 <- df[,c('2017_annual_performance_rating','gender','race_ethnicity','race_grouping','dept')]
seventeen2 <- df2[,c('2017_annual_performance_rating','gender','race_ethnicity','race_grouping','dept')]
eighteen1 <- df[,c('2018_annual_performance_rating','gender','race_ethnicity','race_grouping','dept')]
eighteen2 <- df2[,c('2018_annual_performance_rating','gender','race_ethnicity','race_grouping','dept')]

names(fifteen1) <- c('performance_rating','gender','race_ethnicity','race_grouping','dept')
names(fifteen2) <- c('performance_rating','gender','race_ethnicity','race_grouping','dept')
names(sixteen1) <- c('performance_rating','gender','race_ethnicity','race_grouping','dept')
names(sixteen2) <- c('performance_rating','gender','race_ethnicity','race_grouping','dept')
names(seventeen1) <- c('performance_rating','gender','race_ethnicity','race_grouping','dept')
names(seventeen2) <- c('performance_rating','gender','race_ethnicity','race_grouping','dept')
names(eighteen1) <- c('performance_rating','gender','race_ethnicity','race_grouping','dept')
names(eighteen2) <- c('performance_rating','gender','race_ethnicity','race_grouping','dept')

ratings_combined <- rbind(fifteen1,fifteen2,sixteen1,sixteen2,seventeen1,seventeen2,eighteen1,eighteen2)
```

### Create departmental data frames


```r
news_salaried <- filter(df, dept == 'News', pay_rate_type == 'Salaried')
news_hourly <- filter(df, dept == 'News', pay_rate_type == 'Hourly')
commercial_salaried <- filter(df, dept == 'Commercial', pay_rate_type == 'Salaried')
commercial_hourly <- filter(df, dept == 'Commercial', pay_rate_type == 'Hourly')

news_salaried2 <-  filter(df2, dept == 'News', pay_rate_type == 'Salaried')
news_hourly2 <-  filter(df2, dept == 'News', pay_rate_type == 'Hourly')
commercial_salaried2 <-  filter(df2, dept == 'Commercial', pay_rate_type == 'Salaried')
commercial_hourly2 <-  filter(df2, dept == 'Commercial', pay_rate_type == 'Hourly')
```

## Supress Results

### Suppress results where there are less than five employees


```r
suppress <- function(results) {
  results <- filter(results, count >= 5)
  return(results)
}
```

### Suppress results and order them by count of employees


```r
suppress_count <- function(results) {
  results <- filter(results, count >= 5)
  results <- results[order(-results$count),]
  return(results)
}
```

### Suppress results and order them by median salary of employees


```r
suppress_median <- function(results) {
  results <- filter(results, count >= 5)
  results <- results[order(-results$median),]
  return(results)
}
```

## Summary Analysis

### Employee counts


```r
current_employee_count = nrow(df)
terminated_employee_count = nrow(df2)

cat('Total employees in data:', current_employee_count + terminated_employee_count,'\n')
```

```
## Total employees in data: 1489
```

```r
cat('Current employees:', current_employee_count,'\n')
```

```
## Current employees: 950
```

```r
cat('Terminated employees:', terminated_employee_count,'\n')
```

```
## Terminated employees: 539
```

```r
current_salaried_employee_count <- nrow(filter(df,pay_rate_type == 'Salaried'))
terminated_salaried_employee_count <- nrow(filter(df2,pay_rate_type == 'Salaried'))

cat('Total salaried employees in data:', current_salaried_employee_count + terminated_salaried_employee_count,'\n')
```

```
## Total salaried employees in data: 989
```

```r
cat('Current salaried employees: ', current_salaried_employee_count,'\n')
```

```
## Current salaried employees:  707
```

```r
cat('Terminated salaried employees: ', terminated_salaried_employee_count,'\n')
```

```
## Terminated salaried employees:  282
```

```r
current_hourly_employee_count <- nrow(filter(df,pay_rate_type == 'Hourly'))
terminated_hourly_employee_count <- nrow(filter(df2,pay_rate_type == 'Hourly'))

cat('Total hourly employees in data: ',current_hourly_employee_count + terminated_hourly_employee_count,'\n')
```

```
## Total hourly employees in data:  500
```

```r
cat('Current hourly employees: ',current_hourly_employee_count,'\n')
```

```
## Current hourly employees:  243
```

```r
cat('Terminated hourly employees: ',terminated_hourly_employee_count,'\n')
```

```
## Terminated hourly employees:  257
```

### Salary information


```r
current_mean_salary = mean(df$current_base_pay[df$pay_rate_type == 'Salaried'])
cat('The mean yearly pay for current salaried employees is $',current_mean_salary,'\n')
```

```
## The mean yearly pay for current salaried employees is $ 112383
```

```r
current_median = median(df$current_base_pay[df$pay_rate_type == 'Salaried'])
cat('The median yearly pay for current salaried employees is $',current_median)
```

```
## The median yearly pay for current salaried employees is $ 99903.95
```


```r
current_mean_hourly <- mean(df$current_base_pay[df$pay_rate_type == 'Hourly'])
cat('The mean rate for current hourly employees at The Washington Post is $',current_mean_hourly,'\n')
```

```
## The mean rate for current hourly employees at The Washington Post is $ 30.19712
```

```r
current_median_hourly <- median(df$current_base_pay[df$pay_rate_type == 'Hourly'])
cat('The median rate for current hourly employees at The Washington Post is $',current_median_hourly)
```

```
## The median rate for current hourly employees at The Washington Post is $ 29.23
```

### Employee gender


```r
current_employee_gender <- df %>% group_by(gender)
current_employee_gender <- current_employee_gender %>% summarise(
  count = length(current_base_pay)
)
suppress(current_employee_gender)
```

```
## # A tibble: 2 x 2
##   gender count
##   <chr>  <int>
## 1 Female   507
## 2 Male     443
```

```r
terminated_employee_gender <- df2 %>% group_by(gender)
terminated_employee_gender <- terminated_employee_gender %>% summarise(
  count = length(current_base_pay)
)
suppress(terminated_employee_gender)
```

```
## # A tibble: 2 x 2
##   gender count
##   <chr>  <int>
## 1 Female   293
## 2 Male     246
```

```r
current_median_gender <- filter(df, pay_rate_type == 'Salaried') %>% group_by(gender)
current_median_gender <- current_median_gender %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_gender)
```

```
## # A tibble: 2 x 3
##   gender count  median
##   <chr>  <int>   <dbl>
## 1 Female   370  91816.
## 2 Male     337 109928.
```

```r
current_median_hourly_gender <- filter(df, pay_rate_type == 'Hourly') %>% group_by(gender)
current_median_hourly_gender <- current_median_hourly_gender %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_hourly_gender)
```

```
## # A tibble: 2 x 3
##   gender count median
##   <chr>  <int>  <dbl>
## 1 Female   137   30.8
## 2 Male     106   25.8
```

```r
current_age_gender_salaried <- filter(df, pay_rate_type == 'Salaried') %>% group_by(gender)
current_age_gender_salaried %>% summarise(
  median_age = median(age)
)
```

```
## # A tibble: 2 x 2
##   gender median_age
##   <chr>       <dbl>
## 1 Female         35
## 2 Male           41
```

### Employee race and ethnicity


```r
current_employee_race_ethnicity <- df %>% group_by(race_ethnicity)
current_employee_race_ethnicity <- current_employee_race_ethnicity %>% summarise(
  count = length(current_base_pay)
)
suppress_count(current_employee_race_ethnicity)
```

```
## # A tibble: 7 x 2
##   race_ethnicity                                       count
##   <chr>                                                <int>
## 1 White (United States of America)                       612
## 2 Black or African American (United States of America)   157
## 3 Asian (United States of America)                        77
## 4 Hispanic or Latino (United States of America)           45
## 5 <NA>                                                    22
## 6 Two or More Races (United States of America)            18
## 7 Prefer Not to Disclose (United States of America)       14
```

```r
terminated_employee_race_ethnicity <- df2 %>% group_by(race_ethnicity)
terminated_employee_race_ethnicity <- terminated_employee_race_ethnicity %>% summarise(
  count = length(current_base_pay)
)
suppress_count(terminated_employee_race_ethnicity)
```

```
## # A tibble: 6 x 2
##   race_ethnicity                                       count
##   <chr>                                                <int>
## 1 White (United States of America)                       291
## 2 Black or African American (United States of America)   162
## 3 Asian (United States of America)                        46
## 4 Hispanic or Latino (United States of America)           20
## 5 Two or More Races (United States of America)            11
## 6 Prefer Not to Disclose (United States of America)        7
```

```r
current_median_race <- filter(df, pay_rate_type == 'Salaried') %>% group_by(race_ethnicity)
current_median_race <- current_median_race %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_median_race)
```

```
## # A tibble: 7 x 3
##   race_ethnicity                                       count  median
##   <chr>                                                <int>   <dbl>
## 1 <NA>                                                    21 140000 
## 2 White (United States of America)                       505 102880 
## 3 Black or African American (United States of America)    62  91881.
## 4 Asian (United States of America)                        59  90780 
## 5 Prefer Not to Disclose (United States of America)       10  82140 
## 6 Hispanic or Latino (United States of America)           33  82000 
## 7 Two or More Races (United States of America)            14  79860
```

```r
current_median_hourly_race <- filter(df, pay_rate_type == 'Hourly') %>% group_by(race_ethnicity)
current_median_hourly_race <- current_median_hourly_race %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_median_hourly_race)
```

```
## # A tibble: 4 x 3
##   race_ethnicity                                       count median
##   <chr>                                                <int>  <dbl>
## 1 White (United States of America)                       107   32.7
## 2 Asian (United States of America)                        18   27.3
## 3 Hispanic or Latino (United States of America)           12   25.6
## 4 Black or African American (United States of America)    95   25.2
```

```r
current_age_race_salaried <- filter(df, pay_rate_type == 'Salaried') %>% group_by(race_ethnicity)
current_age_race_salaried %>% summarise(
  median_age = median(age)
)
```

```
## # A tibble: 9 x 2
##   race_ethnicity                                                 median_age
##   <chr>                                                               <dbl>
## 1 American Indian or Alaska Native (United States of America)          49.5
## 2 Asian (United States of America)                                     33  
## 3 Black or African American (United States of America)                 41.5
## 4 Hispanic or Latino (United States of America)                        37  
## 5 Native Hawaiian or Other Pacific Islander (United States of A…       43  
## 6 Prefer Not to Disclose (United States of America)                    31.5
## 7 Two or More Races (United States of America)                         28  
## 8 White (United States of America)                                     39  
## 9 <NA>                                                                 36
```

```r
current_age_race_hourly <- filter(df, pay_rate_type == 'Hourly') %>% group_by(race_ethnicity)
current_age_race_hourly %>% summarise(
  median_age = median(age)
)
```

```
## # A tibble: 8 x 2
##   race_ethnicity                                              median_age
##   <chr>                                                            <dbl>
## 1 American Indian or Alaska Native (United States of America)       53.5
## 2 Asian (United States of America)                                  32  
## 3 Black or African American (United States of America)              47  
## 4 Hispanic or Latino (United States of America)                     29.5
## 5 Prefer Not to Disclose (United States of America)                 30  
## 6 Two or More Races (United States of America)                      26.5
## 7 White (United States of America)                                  39  
## 8 <NA>                                                              31
```

### Employee gender x race/ethnicity


```r
current_employee_race_gender <- df %>% group_by(race_ethnicity, gender)
current_employee_race_gender <- current_employee_race_gender %>% summarise(
  count = length(current_base_pay)
)
suppress(current_employee_race_gender)
```

```
## # A tibble: 14 x 3
## # Groups:   race_ethnicity [7]
##    race_ethnicity                                       gender count
##    <chr>                                                <chr>  <int>
##  1 Asian (United States of America)                     Female    53
##  2 Asian (United States of America)                     Male      24
##  3 Black or African American (United States of America) Female    80
##  4 Black or African American (United States of America) Male      77
##  5 Hispanic or Latino (United States of America)        Female    24
##  6 Hispanic or Latino (United States of America)        Male      21
##  7 Prefer Not to Disclose (United States of America)    Female     6
##  8 Prefer Not to Disclose (United States of America)    Male       8
##  9 Two or More Races (United States of America)         Female    12
## 10 Two or More Races (United States of America)         Male       6
## 11 White (United States of America)                     Female   318
## 12 White (United States of America)                     Male     294
## 13 <NA>                                                 Female    11
## 14 <NA>                                                 Male      11
```

```r
current_salaried_race_gender <- filter(df, pay_rate_type == 'Salaried') %>% group_by(race_ethnicity, gender)
current_salaried_race_gender <- current_salaried_race_gender %>% summarise(
  count = length(current_base_pay),
)
suppress(current_salaried_race_gender)
```

```
## # A tibble: 14 x 3
## # Groups:   race_ethnicity [7]
##    race_ethnicity                                       gender count
##    <chr>                                                <chr>  <int>
##  1 Asian (United States of America)                     Female    42
##  2 Asian (United States of America)                     Male      17
##  3 Black or African American (United States of America) Female    31
##  4 Black or African American (United States of America) Male      31
##  5 Hispanic or Latino (United States of America)        Female    16
##  6 Hispanic or Latino (United States of America)        Male      17
##  7 Prefer Not to Disclose (United States of America)    Female     5
##  8 Prefer Not to Disclose (United States of America)    Male       5
##  9 Two or More Races (United States of America)         Female     9
## 10 Two or More Races (United States of America)         Male       5
## 11 White (United States of America)                     Female   255
## 12 White (United States of America)                     Male     250
## 13 <NA>                                                 Female    10
## 14 <NA>                                                 Male      11
```

```r
current_hourly_race_gender <- filter(df, pay_rate_type == 'Hourly') %>% group_by(race_ethnicity, gender)
current_hourly_race_gender <- current_hourly_race_gender %>% summarise(
  count = length(current_base_pay),
)
suppress(current_hourly_race_gender)
```

```
## # A tibble: 7 x 3
## # Groups:   race_ethnicity [4]
##   race_ethnicity                                       gender count
##   <chr>                                                <chr>  <int>
## 1 Asian (United States of America)                     Female    11
## 2 Asian (United States of America)                     Male       7
## 3 Black or African American (United States of America) Female    49
## 4 Black or African American (United States of America) Male      46
## 5 Hispanic or Latino (United States of America)        Female     8
## 6 White (United States of America)                     Female    63
## 7 White (United States of America)                     Male      44
```

```r
current_median_race_gender <- filter(df, pay_rate_type == 'Salaried') %>% group_by(race_ethnicity, gender)
current_median_race_gender <- current_median_race_gender %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_race_gender)
```

```
## # A tibble: 14 x 4
## # Groups:   race_ethnicity [7]
##    race_ethnicity                                       gender count median
##    <chr>                                                <chr>  <int>  <dbl>
##  1 Asian (United States of America)                     Female    42 9.11e4
##  2 Asian (United States of America)                     Male      17 9.04e4
##  3 Black or African American (United States of America) Female    31 8.78e4
##  4 Black or African American (United States of America) Male      31 9.99e4
##  5 Hispanic or Latino (United States of America)        Female    16 8.02e4
##  6 Hispanic or Latino (United States of America)        Male      17 9.08e4
##  7 Prefer Not to Disclose (United States of America)    Female     5 7.30e4
##  8 Prefer Not to Disclose (United States of America)    Male       5 8.83e4
##  9 Two or More Races (United States of America)         Female     9 7.50e4
## 10 Two or More Races (United States of America)         Male       5 9.49e4
## 11 White (United States of America)                     Female   255 9.58e4
## 12 White (United States of America)                     Male     250 1.11e5
## 13 <NA>                                                 Female    10 1.38e5
## 14 <NA>                                                 Male      11 1.40e5
```

```r
current_median_hourly_race_gender <- filter(df, pay_rate_type == 'Hourly') %>% group_by(race_ethnicity, gender)
current_median_hourly_race_gender <- current_median_hourly_race_gender %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_hourly_race_gender)
```

```
## # A tibble: 7 x 4
## # Groups:   race_ethnicity [4]
##   race_ethnicity                                       gender count median
##   <chr>                                                <chr>  <int>  <dbl>
## 1 Asian (United States of America)                     Female    11   28.3
## 2 Asian (United States of America)                     Male       7   26.3
## 3 Black or African American (United States of America) Female    49   26.8
## 4 Black or African American (United States of America) Male      46   23.2
## 5 Hispanic or Latino (United States of America)        Female     8   28.2
## 6 White (United States of America)                     Female    63   33.5
## 7 White (United States of America)                     Male      44   31.0
```

### Employee age


```r
current_employee_age_5 <- df %>% group_by(age_group_5)
current_employee_age_5 <- current_employee_age_5 %>% summarise(
  count = length(current_base_pay)
)
suppress(current_employee_age_5)
```

```
## # A tibble: 10 x 2
##    age_group_5 count
##    <fct>       <int>
##  1 <25            59
##  2 25-29         171
##  3 30-34         139
##  4 35-39         125
##  5 40-44          98
##  6 45-49          79
##  7 50-54         106
##  8 55-59          84
##  9 60-64          56
## 10 65+            33
```

```r
terminated_employee_age_5 <- df2 %>% group_by(age_group_5)
terminated_employee_age_5 <- terminated_employee_age_5 %>% summarise(
  count = length(current_base_pay)
)
suppress(terminated_employee_age_5)
```

```
## # A tibble: 10 x 2
##    age_group_5 count
##    <fct>       <int>
##  1 <25             7
##  2 25-29         118
##  3 30-34         115
##  4 35-39          56
##  5 40-44          53
##  6 45-49          40
##  7 50-54          33
##  8 55-59          42
##  9 60-64          29
## 10 65+            44
```

```r
current_employee_age_10 <- df %>% group_by(age_group_10)
current_employee_age_10 <- current_employee_age_10 %>% summarise(
  count = length(current_base_pay)
)
suppress(current_employee_age_10)
```

```
## # A tibble: 6 x 2
##   age_group_10 count
##   <fct>        <int>
## 1 <25             59
## 2 25-34          310
## 3 35-44          223
## 4 45-54          185
## 5 55-64          140
## 6 65+             33
```

```r
terminated_employee_age_10 <- df2 %>% group_by(age_group_10)
terminated_employee_age_10 <- terminated_employee_age_10 %>% summarise(
  count = length(current_base_pay)
)
suppress(terminated_employee_age_10)
```

```
## # A tibble: 6 x 2
##   age_group_10 count
##   <fct>        <int>
## 1 <25              7
## 2 25-34          233
## 3 35-44          109
## 4 45-54           73
## 5 55-64           71
## 6 65+             44
```

```r
current_median_age_5 <- filter(df, pay_rate_type == 'Salaried') %>% group_by(age_group_5)
current_median_age_5 <- current_median_age_5 %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_age_5)
```

```
## # A tibble: 10 x 3
##    age_group_5 count  median
##    <fct>       <int>   <dbl>
##  1 <25            34  64640 
##  2 25-29         126  80000 
##  3 30-34         119  92500 
##  4 35-39         104 105301.
##  5 40-44          72 125924.
##  6 45-49          56  99502.
##  7 50-54          80 110845.
##  8 55-59          61 139717.
##  9 60-64          38 113134.
## 10 65+            17 153061
```

```r
current_median_hourly_age_5 <- filter(df, pay_rate_type == 'Hourly') %>% group_by(age_group_5)
current_median_hourly_age_5 <- current_median_hourly_age_5 %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_hourly_age_5)
```

```
## # A tibble: 10 x 3
##    age_group_5 count median
##    <fct>       <int>  <dbl>
##  1 <25            25   25.6
##  2 25-29          45   30.8
##  3 30-34          20   30.6
##  4 35-39          21   31.2
##  5 40-44          26   29.5
##  6 45-49          23   31.3
##  7 50-54          26   27.2
##  8 55-59          23   27.0
##  9 60-64          18   25.0
## 10 65+            16   27.3
```

```r
current_median_age_10 <- filter(df, pay_rate_type == 'Salaried') %>% group_by(age_group_10)
current_median_age_10 <- current_median_age_10 %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_age_10)
```

```
## # A tibble: 6 x 3
##   age_group_10 count  median
##   <fct>        <int>   <dbl>
## 1 <25             34  64640 
## 2 25-34          245  85500 
## 3 35-44          176 115118.
## 4 45-54          136 108202.
## 5 55-64           99 127059.
## 6 65+             17 153061
```

```r
current_median_hourly_age_10 <- filter(df, pay_rate_type == 'Hourly') %>% group_by(age_group_10)
current_median_hourly_age_10 <- current_median_hourly_age_10 %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_hourly_age_10)
```

```
## # A tibble: 6 x 3
##   age_group_10 count median
##   <fct>        <int>  <dbl>
## 1 <25             25   25.6
## 2 25-34           65   30.8
## 3 35-44           47   30.8
## 4 45-54           49   28.3
## 5 55-64           41   26.5
## 6 65+             16   27.3
```

### Employee department


```r
current_employee_dept <- df %>% group_by(dept)
current_employee_dept <- current_employee_dept %>% summarise(
  count = length(current_base_pay)
)
suppress_count(current_employee_dept)
```

```
## # A tibble: 2 x 2
##   dept       count
##   <chr>      <int>
## 1 News         670
## 2 Commercial   280
```

```r
current_employee_department <- df %>% group_by(department)
current_employee_department <- current_employee_department %>% summarise(
  count = length(current_base_pay)
)
suppress_count(current_employee_department)
```

```
## # A tibble: 9 x 2
##   department             count
##   <chr>                  <int>
## 1 News                     632
## 2 Client Solutions         164
## 3 Circulation               49
## 4 Editorial                 38
## 5 Finance                   31
## 6 Marketing                 11
## 7 WP News Media Services     9
## 8 Production                 6
## 9 Public Relations           5
```

```r
current_employee_dept_salary <- filter(df, pay_rate_type == 'Salaried') %>% group_by(dept)
current_employee_dept_salary <- current_employee_dept_salary %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_employee_dept_salary)
```

```
## # A tibble: 2 x 3
##   dept       count  median
##   <chr>      <int>   <dbl>
## 1 News         574 104670.
## 2 Commercial   133  86105.
```

```r
current_employee_department_salary <- filter(df, pay_rate_type == 'Salaried') %>% group_by(department)
current_employee_department_salary <- current_employee_department_salary %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_employee_department_salary)
```

```
## # A tibble: 7 x 3
##   department             count  median
##   <chr>                  <int>   <dbl>
## 1 Editorial                 33 105000 
## 2 News                     541 104560.
## 3 Finance                    8  90576.
## 4 WP News Media Services     9  86105.
## 5 Client Solutions         102  85634.
## 6 Marketing                  7  81196.
## 7 Production                 5  71665.
```

```r
current_employee_dept_hourly <- filter(df, pay_rate_type == 'Hourly') %>% group_by(dept)
current_employee_dept_hourly <- current_employee_dept_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_employee_dept_hourly)
```

```
## # A tibble: 2 x 3
##   dept       count median
##   <chr>      <int>  <dbl>
## 1 News          96   33.0
## 2 Commercial   147   26.3
```

```r
current_employee_department_hourly <- filter(df, pay_rate_type == 'Hourly') %>% group_by(department)
current_employee_department_hourly <- current_employee_department_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_employee_department_hourly)
```

```
## # A tibble: 6 x 3
##   department       count median
##   <chr>            <int>  <dbl>
## 1 Public Relations     5   35.0
## 2 News                91   33.1
## 3 Editorial            5   32.3
## 4 Client Solutions    62   29.4
## 5 Finance             23   29.2
## 6 Circulation         49   22.4
```

### Employee cost center


```r
current_employee_desk <- df %>% group_by(desk)
current_employee_desk <- current_employee_desk %>% summarise(
  count = length(current_base_pay)
)
suppress_count(current_employee_desk)
```

```
## # A tibble: 19 x 2
##    desk                                count
##    <chr>                               <int>
##  1 non-newsroom                          280
##  2 National                              118
##  3 Local                                  70
##  4 Style                                  54
##  5 Video                                  50
##  6 Sports                                 48
##  7 Design                                 46
##  8 Multiplatform                          42
##  9 Editorial                              38
## 10 Financial                              38
## 11 Emerging News Products                 31
## 12 Foreign                                27
## 13 Audience Development and Engagement    23
## 14 Universal Desk                         16
## 15 Graphics                               15
## 16 Audio                                  13
## 17 Investigative                          13
## 18 Operations                             13
## 19 Outlook                                 8
```

```r
current_employee_cost_center <- df %>% group_by(cost_center_current)
current_employee_cost_center <- current_employee_cost_center %>% summarise(
  count = length(current_base_pay)
)
suppress_count(current_employee_cost_center)
```

```
## # A tibble: 50 x 2
##    cost_center_current                            count
##    <chr>                                          <int>
##  1 112300 Local Politics and Government              70
##  2 113200 National Politics and Government           63
##  3 110652 News Video - General                       50
##  4 110015 Sports Main                                48
##  5 110601 Multiplatform Desk                         42
##  6 110300 Style                                      39
##  7 119065 Dispatch Operations (Night Circulation)    39
##  8 113210 Economy and Business                       38
##  9 115000 Editorial Administration                   38
## 10 110605 Presentation                               24
## # … with 40 more rows
```

```r
current_employee_desk_salary <- filter(df, pay_rate_type == 'Salaried') %>% group_by(desk)
current_employee_desk_salary <- current_employee_desk_salary %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_employee_desk_salary)
```

```
## # A tibble: 19 x 3
##    desk                                count  median
##    <chr>                               <int>   <dbl>
##  1 National                              106 149520.
##  2 Foreign                                25 135000 
##  3 Financial                              38 133510.
##  4 Investigative                          13 129780 
##  5 Style                                  45 107171.
##  6 Local                                  65 105780 
##  7 Editorial                              33 105000 
##  8 Graphics                               15 100780 
##  9 Universal Desk                          8 100444.
## 10 Sports                                 37 100000 
## 11 Outlook                                 6  99938.
## 12 Audio                                   7  92000 
## 13 Design                                 45  88065.
## 14 Operations                              6  87890 
## 15 non-newsroom                          133  86105.
## 16 Multiplatform                          26  86104 
## 17 Video                                  46  84250 
## 18 Audience Development and Engagement    16  83530 
## 19 Emerging News Products                 30  75000
```

```r
current_employee_cost_center_salary <- filter(df, pay_rate_type == 'Salaried') %>% group_by(cost_center_current)
current_employee_cost_center_salary <- current_employee_cost_center_salary %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_employee_cost_center_salary)
```

```
## # A tibble: 35 x 3
##    cost_center_current                     count  median
##    <chr>                                   <int>   <dbl>
##  1 113205 National Security                   17 172780 
##  2 117682 Global Sales                        21 164984.
##  3 113200 National Politics and Government    55 145980 
##  4 113235 National America                    12 137124.
##  5 113215 News National Health & Science      12 135595.
##  6 113210 Economy and Business                38 133510.
##  7 110450 Investigative                       13 129780 
##  8 117600 Leadership Executive                 5 127500 
##  9 113240 News National Environment            5 126080 
## 10 110300 Style                               36 115178.
## # … with 25 more rows
```

```r
current_employee_desk_hourly <- filter(df, pay_rate_type == 'Hourly') %>% group_by(desk)
current_employee_desk_hourly <- current_employee_desk_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_employee_desk_hourly)
```

```
## # A tibble: 11 x 3
##    desk                                count median
##    <chr>                               <int>  <dbl>
##  1 Audio                                   6   39.7
##  2 Universal Desk                          8   38.7
##  3 Audience Development and Engagement     7   37.6
##  4 Multiplatform                          16   34.1
##  5 Editorial                               5   32.3
##  6 National                               12   31.7
##  7 Local                                   5   26.5
##  8 non-newsroom                          147   26.3
##  9 Style                                   9   21.8
## 10 Sports                                 11   20.9
## 11 Operations                              7   15.6
```

```r
current_employee_cost_center_hourly <- filter(df, pay_rate_type == 'Hourly') %>% group_by(cost_center_current)
current_employee_cost_center_hourly <- current_employee_cost_center_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_employee_cost_center_hourly)
```

```
## # A tibble: 17 x 3
##    cost_center_current                            count median
##    <chr>                                          <int>  <dbl>
##  1 110620 News Audio                                  6   39.7
##  2 110600 Universal Desk                              8   38.7
##  3 110610 Audience Development and Engagement         7   37.6
##  4 129100 Community                                   5   35.0
##  5 110601 Multiplatform Desk                         16   34.1
##  6 115000 Editorial Administration                    5   32.3
##  7 126060 Circulation Accounting                      9   30.5
##  8 113200 National Politics and Government            8   30.5
##  9 126020 Revenue Administration                     14   28.8
## 10 117210 Production Creative                         5   28.1
## 11 112300 Local Politics and Government               5   26.5
## 12 117310 Consumer to Consumer Team I                 5   24.7
## 13 117405 Jobs Tactical                               5   24.3
## 14 119065 Dispatch Operations (Night Circulation)    39   22.4
## 15 110015 Sports Main                                11   20.9
## 16 119026 Customer Contact Center                     5   20.5
## 17 110000 News Operations                             7   15.6
```

### Employee years of service


```r
current_employee_yos <- df %>% group_by(years_of_service_grouped)
current_employee_yos <- current_employee_yos %>% summarise(
  count = length(current_base_pay)
)
suppress(current_employee_yos)
```

```
## # A tibble: 8 x 2
##   years_of_service_grouped count
##   <fct>                    <int>
## 1 0                          138
## 2 1-2                        223
## 3 3-5                        195
## 4 6-10                       109
## 5 11-15                       80
## 6 16-20                      102
## 7 21-25                       46
## 8 25+                         57
```

```r
terminated_employee_yos <- df2 %>% group_by(years_of_service_grouped)
terminated_employee_yos <- terminated_employee_yos %>% summarise(
  count = length(current_base_pay)
)
suppress(terminated_employee_yos)
```

```
## # A tibble: 8 x 2
##   years_of_service_grouped count
##   <fct>                    <int>
## 1 0                            8
## 2 1-2                         78
## 3 3-5                        197
## 4 6-10                       119
## 5 11-15                       52
## 6 16-20                       44
## 7 21-25                       12
## 8 25+                         29
```

```r
current_employee_yos_salary <- filter(df, pay_rate_type == 'Salaried') %>% group_by(years_of_service_grouped)
current_employee_yos_salary <- current_employee_yos_salary %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_employee_yos_salary)
```

```
## # A tibble: 8 x 3
##   years_of_service_grouped count  median
##   <fct>                    <int>   <dbl>
## 1 0                           96  85000 
## 2 1-2                        164  91777.
## 3 3-5                        172  92306.
## 4 6-10                        75 106603.
## 5 11-15                       56 107685.
## 6 16-20                       74 125301.
## 7 21-25                       32 128485.
## 8 25+                         38 131793.
```

```r
current_employee_yos_hourly <- filter(df, pay_rate_type == 'Hourly') %>% group_by(years_of_service_grouped)
current_employee_yos_hourly <- current_employee_yos_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_employee_yos_hourly)
```

```
## # A tibble: 8 x 3
##   years_of_service_grouped count median
##   <fct>                    <int>  <dbl>
## 1 0                           42   27.7
## 2 1-2                         59   31.7
## 3 3-5                         23   27.0
## 4 6-10                        34   29.2
## 5 11-15                       24   32.4
## 6 16-20                       28   27.8
## 7 21-25                       14   31.1
## 8 25+                         19   26.8
```

```r
current_employee_yos_gender <- df %>% group_by(years_of_service_grouped, gender)
current_employee_yos_gender <- current_employee_yos_gender %>% summarise(
  count = length(current_base_pay)
)
suppress(current_employee_yos_gender)
```

```
## # A tibble: 16 x 3
## # Groups:   years_of_service_grouped [8]
##    years_of_service_grouped gender count
##    <fct>                    <chr>  <int>
##  1 0                        Female    82
##  2 0                        Male      56
##  3 1-2                      Female   132
##  4 1-2                      Male      91
##  5 3-5                      Female    96
##  6 3-5                      Male      99
##  7 6-10                     Female    51
##  8 6-10                     Male      58
##  9 11-15                    Female    41
## 10 11-15                    Male      39
## 11 16-20                    Female    48
## 12 16-20                    Male      54
## 13 21-25                    Female    25
## 14 21-25                    Male      21
## 15 25+                      Female    32
## 16 25+                      Male      25
```

```r
current_employee_yos_gender_salary <- filter(df, pay_rate_type == 'Salaried') %>% group_by(years_of_service_grouped, gender)
current_employee_yos_gender_salary <- current_employee_yos_gender_salary %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_employee_yos_gender_salary)
```

```
## # A tibble: 16 x 4
## # Groups:   years_of_service_grouped [8]
##    years_of_service_grouped gender count  median
##    <fct>                    <chr>  <int>   <dbl>
##  1 0                        Female    61  80000 
##  2 0                        Male      35 100000 
##  3 1-2                      Female    96  85780 
##  4 1-2                      Male      68  96738.
##  5 3-5                      Female    88  89725.
##  6 3-5                      Male      84  95265.
##  7 6-10                     Female    38  99500.
##  8 6-10                     Male      37 117844.
##  9 11-15                    Female    28  98142.
## 10 11-15                    Male      28 126911.
## 11 16-20                    Female    31 121140 
## 12 16-20                    Male      43 127059.
## 13 21-25                    Female    13 134780 
## 14 21-25                    Male      19  99012.
## 15 25+                      Female    15 139831.
## 16 25+                      Male      23 127476.
```

```r
current_employee_yos_gender_hourly <- filter(df, pay_rate_type == 'Hourly') %>% group_by(years_of_service_grouped, gender)
current_employee_yos_gender_hourly <- current_employee_yos_gender_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_employee_yos_gender_hourly)
```

```
## # A tibble: 14 x 4
## # Groups:   years_of_service_grouped [8]
##    years_of_service_grouped gender count median
##    <fct>                    <chr>  <int>  <dbl>
##  1 0                        Female    21   29.2
##  2 0                        Male      21   22.0
##  3 1-2                      Female    36   31.9
##  4 1-2                      Male      23   26.0
##  5 3-5                      Female     8   34.8
##  6 3-5                      Male      15   23.0
##  7 6-10                     Female    13   30.8
##  8 6-10                     Male      21   25.2
##  9 11-15                    Female    13   34.7
## 10 11-15                    Male      11   29.9
## 11 16-20                    Female    17   25.1
## 12 16-20                    Male      11   30.2
## 13 21-25                    Female    12   30.3
## 14 25+                      Female    17   27.7
```

```r
current_employee_yos_race <- df %>% group_by(years_of_service_grouped, race_ethnicity)
current_employee_yos_race <- current_employee_yos_race %>% summarise(
  count = length(current_base_pay)
)
suppress(current_employee_yos_race)
```

```
## # A tibble: 31 x 3
## # Groups:   years_of_service_grouped [8]
##    years_of_service_group… race_ethnicity                             count
##    <fct>                   <chr>                                      <int>
##  1 0                       Asian (United States of America)              15
##  2 0                       Black or African American (United States …    20
##  3 0                       Hispanic or Latino (United States of Amer…    10
##  4 0                       Prefer Not to Disclose (United States of …     8
##  5 0                       Two or More Races (United States of Ameri…     6
##  6 0                       White (United States of America)              77
##  7 1-2                     Asian (United States of America)              20
##  8 1-2                     Black or African American (United States …    30
##  9 1-2                     Hispanic or Latino (United States of Amer…    12
## 10 1-2                     Two or More Races (United States of Ameri…     6
## # … with 21 more rows
```

```r
current_employee_yos_race_salary <- filter(df, pay_rate_type == 'Salaried') %>% group_by(years_of_service_grouped, race_ethnicity)
current_employee_yos_race_salary <- current_employee_yos_race_salary %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_employee_yos_race_salary)
```

```
## # A tibble: 25 x 4
## # Groups:   years_of_service_grouped [8]
##    years_of_service_gro… race_ethnicity                        count median
##    <fct>                 <chr>                                 <int>  <dbl>
##  1 0                     Asian (United States of America)         11  77000
##  2 0                     Black or African American (United St…     5  87000
##  3 0                     Hispanic or Latino (United States of…     5  75000
##  4 0                     White (United States of America)         65  90000
##  5 1-2                   Asian (United States of America)         16  87780
##  6 1-2                   Black or African American (United St…    12  89780
##  7 1-2                   Hispanic or Latino (United States of…     7  82000
##  8 1-2                   Two or More Races (United States of …     5  68000
##  9 1-2                   White (United States of America)        115  92780
## 10 1-2                   <NA>                                      5 140280
## # … with 15 more rows
```

```r
current_employee_yos_race_hourly <- filter(df, pay_rate_type == 'Hourly') %>% group_by(years_of_service_grouped, race_ethnicity)
current_employee_yos_race_hourly <- current_employee_yos_race_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_employee_yos_race_hourly)
```

```
## # A tibble: 18 x 4
## # Groups:   years_of_service_grouped [8]
##    years_of_service_gro… race_ethnicity                        count median
##    <fct>                 <chr>                                 <int>  <dbl>
##  1 0                     Black or African American (United St…    15   25.6
##  2 0                     Hispanic or Latino (United States of…     5   28.2
##  3 0                     White (United States of America)         12   29.5
##  4 1-2                   Black or African American (United St…    18   25.8
##  5 1-2                   Hispanic or Latino (United States of…     5   21.8
##  6 1-2                   White (United States of America)         31   33.5
##  7 3-5                   Black or African American (United St…     6   21.8
##  8 3-5                   White (United States of America)         11   29.2
##  9 6-10                  Black or African American (United St…    15   24.4
## 10 6-10                  White (United States of America)         15   31.9
## 11 11-15                 Black or African American (United St…     8   30.2
## 12 11-15                 White (United States of America)         14   34.0
## 13 16-20                 Black or African American (United St…    13   24.0
## 14 16-20                 White (United States of America)         12   34.9
## 15 21-25                 Black or African American (United St…     9   29.7
## 16 21-25                 White (United States of America)          5   38.9
## 17 25+                   Black or African American (United St…    11   24.7
## 18 25+                   White (United States of America)          7   32.7
```

### Employee performance evaluations


```r
fifteen <- rbind(fifteen1,fifteen2)
fifteenrating_gender <- fifteen %>% group_by(gender)
fifteenrating_gender %>% summarise(
  median = median(performance_rating, na.rm = TRUE)
)
```

```
## # A tibble: 2 x 2
##   gender median
##   <chr>   <dbl>
## 1 Female    3.4
## 2 Male      3.4
```

```r
sixteen <- rbind(sixteen1,sixteen2)
sixteenrating_gender <- sixteen %>% group_by(gender)
sixteenrating_gender %>% summarise(
  median = median(performance_rating, na.rm = TRUE)
)
```

```
## # A tibble: 2 x 2
##   gender median
##   <chr>   <dbl>
## 1 Female    3.3
## 2 Male      3.3
```

```r
seventeen <- rbind(seventeen1,seventeen2)
seventeenrating_gender <- seventeen %>% group_by(gender)
seventeenrating_gender %>% summarise(
  median = median(performance_rating, na.rm = TRUE)
)
```

```
## # A tibble: 2 x 2
##   gender median
##   <chr>   <dbl>
## 1 Female    3.4
## 2 Male      3.4
```

```r
eighteen <- rbind(eighteen1,eighteen2)
eighteenrating_gender <- eighteen %>% group_by(gender)
eighteenrating_gender %>% summarise(
  median = median(performance_rating, na.rm = TRUE)
)
```

```
## # A tibble: 2 x 2
##   gender median
##   <chr>   <dbl>
## 1 Female    3.4
## 2 Male      3.4
```

```r
fifteen <- rbind(fifteen1,fifteen2)
fifteenrating_race_ethnicity <- fifteen %>% group_by(race_ethnicity)
fifteenrating_race_ethnicity %>% summarise(
  median = median(performance_rating, na.rm = TRUE)
)
```

```
## # A tibble: 9 x 2
##   race_ethnicity                                                     median
##   <chr>                                                               <dbl>
## 1 American Indian or Alaska Native (United States of America)          3.5 
## 2 Asian (United States of America)                                     3.4 
## 3 Black or African American (United States of America)                 3.2 
## 4 Hispanic or Latino (United States of America)                        3.2 
## 5 Native Hawaiian or Other Pacific Islander (United States of Ameri…   3.25
## 6 Prefer Not to Disclose (United States of America)                    3.3 
## 7 Two or More Races (United States of America)                         3.3 
## 8 White (United States of America)                                     3.4 
## 9 <NA>                                                                 3.7
```

```r
sixteen <- rbind(sixteen1,sixteen2)
sixteenrating_race_ethnicity <- sixteen %>% group_by(race_ethnicity)
sixteenrating_race_ethnicity %>% summarise(
  median = median(performance_rating, na.rm = TRUE)
)
```

```
## # A tibble: 9 x 2
##   race_ethnicity                                                     median
##   <chr>                                                               <dbl>
## 1 American Indian or Alaska Native (United States of America)          3.25
## 2 Asian (United States of America)                                     3.35
## 3 Black or African American (United States of America)                 3.2 
## 4 Hispanic or Latino (United States of America)                        3.1 
## 5 Native Hawaiian or Other Pacific Islander (United States of Ameri…   3.7 
## 6 Prefer Not to Disclose (United States of America)                    3.3 
## 7 Two or More Races (United States of America)                         3.2 
## 8 White (United States of America)                                     3.4 
## 9 <NA>                                                                 3.75
```

```r
seventeen <- rbind(seventeen1,seventeen2)
seventeenrating_race_ethnicity <- seventeen %>% group_by(race_ethnicity)
seventeenrating_race_ethnicity %>% summarise(
  median = median(performance_rating, na.rm = TRUE)
)
```

```
## # A tibble: 9 x 2
##   race_ethnicity                                                     median
##   <chr>                                                               <dbl>
## 1 American Indian or Alaska Native (United States of America)          3.55
## 2 Asian (United States of America)                                     3.4 
## 3 Black or African American (United States of America)                 3.2 
## 4 Hispanic or Latino (United States of America)                        3.3 
## 5 Native Hawaiian or Other Pacific Islander (United States of Ameri…   3.5 
## 6 Prefer Not to Disclose (United States of America)                    3.4 
## 7 Two or More Races (United States of America)                         3.3 
## 8 White (United States of America)                                     3.4 
## 9 <NA>                                                                 3.6
```

```r
eighteen <- rbind(eighteen1,eighteen2)
eighteenrating_race_ethnicity <- eighteen %>% group_by(race_ethnicity)
eighteenrating_race_ethnicity %>% summarise(
  median = median(performance_rating, na.rm = TRUE)
)
```

```
## # A tibble: 9 x 2
##   race_ethnicity                                                     median
##   <chr>                                                               <dbl>
## 1 American Indian or Alaska Native (United States of America)          3.55
## 2 Asian (United States of America)                                     3.4 
## 3 Black or African American (United States of America)                 3.3 
## 4 Hispanic or Latino (United States of America)                        3.3 
## 5 Native Hawaiian or Other Pacific Islander (United States of Ameri…   3.4 
## 6 Prefer Not to Disclose (United States of America)                    3.35
## 7 Two or More Races (United States of America)                         3.3 
## 8 White (United States of America)                                     3.5 
## 9 <NA>                                                                 3.5
```

```r
fifteen <- rbind(fifteen1,fifteen2)
fifteenrating_gender_race <- fifteen %>% group_by(race_ethnicity, gender)
fifteenrating_gender_race %>% summarise(
  median = median(performance_rating, na.rm = TRUE)
)
```

```
## # A tibble: 18 x 3
## # Groups:   race_ethnicity [9]
##    race_ethnicity                                             gender median
##    <chr>                                                      <chr>   <dbl>
##  1 American Indian or Alaska Native (United States of Americ… Female   3.5 
##  2 American Indian or Alaska Native (United States of Americ… Male     3.4 
##  3 Asian (United States of America)                           Female   3.4 
##  4 Asian (United States of America)                           Male     3.5 
##  5 Black or African American (United States of America)       Female   3.2 
##  6 Black or African American (United States of America)       Male     3   
##  7 Hispanic or Latino (United States of America)              Female   3.3 
##  8 Hispanic or Latino (United States of America)              Male     3.2 
##  9 Native Hawaiian or Other Pacific Islander (United States … Female   3.2 
## 10 Native Hawaiian or Other Pacific Islander (United States … Male     3.3 
## 11 Prefer Not to Disclose (United States of America)          Female   3.3 
## 12 Prefer Not to Disclose (United States of America)          Male    NA   
## 13 Two or More Races (United States of America)               Female   3.3 
## 14 Two or More Races (United States of America)               Male     2.75
## 15 White (United States of America)                           Female   3.4 
## 16 White (United States of America)                           Male     3.5 
## 17 <NA>                                                       Female   3.65
## 18 <NA>                                                       Male     3.8
```

```r
sixteen <- rbind(sixteen1,sixteen2)
sixteenrating_gender_race <- sixteen %>% group_by(race_ethnicity, gender)
sixteenrating_gender_race %>% summarise(
  median = median(performance_rating, na.rm = TRUE)
)
```

```
## # A tibble: 18 x 3
## # Groups:   race_ethnicity [9]
##    race_ethnicity                                             gender median
##    <chr>                                                      <chr>   <dbl>
##  1 American Indian or Alaska Native (United States of Americ… Female   3.3 
##  2 American Indian or Alaska Native (United States of Americ… Male     3.2 
##  3 Asian (United States of America)                           Female   3.4 
##  4 Asian (United States of America)                           Male     3.3 
##  5 Black or African American (United States of America)       Female   3.25
##  6 Black or African American (United States of America)       Male     3.15
##  7 Hispanic or Latino (United States of America)              Female   3.15
##  8 Hispanic or Latino (United States of America)              Male     3.1 
##  9 Native Hawaiian or Other Pacific Islander (United States … Female   4.1 
## 10 Native Hawaiian or Other Pacific Islander (United States … Male     3.3 
## 11 Prefer Not to Disclose (United States of America)          Female   3.3 
## 12 Prefer Not to Disclose (United States of America)          Male    NA   
## 13 Two or More Races (United States of America)               Female   3.2 
## 14 Two or More Races (United States of America)               Male     2.7 
## 15 White (United States of America)                           Female   3.4 
## 16 White (United States of America)                           Male     3.4 
## 17 <NA>                                                       Female   3.8 
## 18 <NA>                                                       Male     3.6
```

```r
seventeen <- rbind(seventeen1,seventeen2)
seventeenrating_gender_race <- seventeen %>% group_by(race_ethnicity, gender)
seventeenrating_gender_race %>% summarise(
  median = median(performance_rating, na.rm = TRUE)
)
```

```
## # A tibble: 18 x 3
## # Groups:   race_ethnicity [9]
##    race_ethnicity                                             gender median
##    <chr>                                                      <chr>   <dbl>
##  1 American Indian or Alaska Native (United States of Americ… Female   3.7 
##  2 American Indian or Alaska Native (United States of Americ… Male     3.1 
##  3 Asian (United States of America)                           Female   3.4 
##  4 Asian (United States of America)                           Male     3.3 
##  5 Black or African American (United States of America)       Female   3.2 
##  6 Black or African American (United States of America)       Male     3.1 
##  7 Hispanic or Latino (United States of America)              Female   3.3 
##  8 Hispanic or Latino (United States of America)              Male     3.3 
##  9 Native Hawaiian or Other Pacific Islander (United States … Female   4   
## 10 Native Hawaiian or Other Pacific Islander (United States … Male     3   
## 11 Prefer Not to Disclose (United States of America)          Female   3.5 
## 12 Prefer Not to Disclose (United States of America)          Male     3.2 
## 13 Two or More Races (United States of America)               Female   3.25
## 14 Two or More Races (United States of America)               Male     3.5 
## 15 White (United States of America)                           Female   3.4 
## 16 White (United States of America)                           Male     3.4 
## 17 <NA>                                                       Female   3.65
## 18 <NA>                                                       Male     3.5
```

```r
eighteen <- rbind(eighteen1,eighteen2)
eighteenrating_gender_race <- eighteen %>% group_by(race_ethnicity, gender)
eighteenrating_gender_race %>% summarise(
  median = median(performance_rating, na.rm = TRUE)
)
```

```
## # A tibble: 18 x 3
## # Groups:   race_ethnicity [9]
##    race_ethnicity                                             gender median
##    <chr>                                                      <chr>   <dbl>
##  1 American Indian or Alaska Native (United States of Americ… Female   3.7 
##  2 American Indian or Alaska Native (United States of Americ… Male     3.2 
##  3 Asian (United States of America)                           Female   3.4 
##  4 Asian (United States of America)                           Male     3.4 
##  5 Black or African American (United States of America)       Female   3.3 
##  6 Black or African American (United States of America)       Male     3.3 
##  7 Hispanic or Latino (United States of America)              Female   3.3 
##  8 Hispanic or Latino (United States of America)              Male     3.3 
##  9 Native Hawaiian or Other Pacific Islander (United States … Female  NA   
## 10 Native Hawaiian or Other Pacific Islander (United States … Male     3.4 
## 11 Prefer Not to Disclose (United States of America)          Female   3.55
## 12 Prefer Not to Disclose (United States of America)          Male     3.3 
## 13 Two or More Races (United States of America)               Female   3.3 
## 14 Two or More Races (United States of America)               Male     3.35
## 15 White (United States of America)                           Female   3.4 
## 16 White (United States of America)                           Male     3.5 
## 17 <NA>                                                       Female   3.6 
## 18 <NA>                                                       Male     3.4
```

### Employee pay changes


```r
reason_for_change <- reason_for_change_combined %>% group_by(business_process_reason)
reason_for_change <- reason_for_change %>% summarise(
  count = length(business_process_reason)
)
suppress_count(reason_for_change)
```

```
## # A tibble: 19 x 2
##    business_process_reason                                            count
##    <chr>                                                              <int>
##  1 <NA>                                                               16810
##  2 Request Compensation Change > Adjustment > Contract Increase        2451
##  3 Merit > Performance > Annual Performance Appraisal                  1729
##  4 Data Change > Data Change > Change Job Details                       673
##  5 Transfer > Transfer > Move to another Manager                        533
##  6 Request Compensation Change > Adjustment > Change Plan Assignment    435
##  7 Request Compensation Change > Adjustment > Market Adjustment         384
##  8 Promotion > Promotion > Promotion                                    359
##  9 Hire Employee > New Hire > Fill Vacancy                              253
## 10 Hire Employee > New Hire > New Position                              189
## 11 Request Compensation Change > Adjustment > Increased Job Responsi…    72
## 12 Request Compensation Change > Adjustment > Job Change                 60
## 13 Transfer > Transfer > Transfer between departments                    54
## 14 Request Compensation Change > Adjustment > Performance                38
## 15 Transfer > Transfer > Transfer between companies                      21
## 16 Hire Employee > Rehire > Fill Vacancy                                 16
## 17 Hire Employee > New Hire > Convert Contingent                         12
## 18 Hire Employee > New Hire > Conversion                                 11
## 19 Hire Employee > Rehire > New Position                                  7
```

```r
reason_for_change_gender <- reason_for_change_combined %>% group_by(business_process_reason, gender)
reason_for_change_gender <- reason_for_change_gender %>% summarise(
  count = length(business_process_reason)
)
suppress_count(reason_for_change_gender)
```

```
## # A tibble: 34 x 3
## # Groups:   business_process_reason [19]
##    business_process_reason                                     gender count
##    <chr>                                                       <chr>  <int>
##  1 <NA>                                                        Female  9012
##  2 <NA>                                                        Male    7798
##  3 Request Compensation Change > Adjustment > Contract Increa… Female  1284
##  4 Request Compensation Change > Adjustment > Contract Increa… Male    1167
##  5 Merit > Performance > Annual Performance Appraisal          Female   878
##  6 Merit > Performance > Annual Performance Appraisal          Male     851
##  7 Data Change > Data Change > Change Job Details              Female   367
##  8 Data Change > Data Change > Change Job Details              Male     306
##  9 Transfer > Transfer > Move to another Manager               Male     299
## 10 Request Compensation Change > Adjustment > Change Plan Ass… Female   288
## # … with 24 more rows
```

```r
reason_for_change_race <- reason_for_change_combined %>% group_by(business_process_reason, race_ethnicity)
reason_for_change_race <- reason_for_change_race %>% summarise(
  count = length(business_process_reason)
)
suppress_count(reason_for_change_race)
```

```
## # A tibble: 83 x 3
## # Groups:   business_process_reason [18]
##    business_process_reason             race_ethnicity                 count
##    <chr>                               <chr>                          <int>
##  1 <NA>                                White (United States of Ameri… 10227
##  2 <NA>                                Black or African American (Un…  3507
##  3 Request Compensation Change > Adju… White (United States of Ameri…  1556
##  4 <NA>                                Asian (United States of Ameri…  1366
##  5 Merit > Performance > Annual Perfo… White (United States of Ameri…  1109
##  6 <NA>                                Hispanic or Latino (United St…   756
##  7 Request Compensation Change > Adju… Black or African American (Un…   508
##  8 Data Change > Data Change > Change… White (United States of Ameri…   432
##  9 <NA>                                Two or More Races (United Sta…   382
## 10 Merit > Performance > Annual Perfo… Black or African American (Un…   347
## # … with 73 more rows
```

```r
reason_for_change_race_gender <- reason_for_change_combined %>% group_by(business_process_reason, race_ethnicity, gender)
reason_for_change_race_gender <- reason_for_change_race_gender %>% summarise(
  count = length(business_process_reason)
)
suppress_count(reason_for_change_race_gender)
```

```
## # A tibble: 122 x 4
## # Groups:   business_process_reason, race_ethnicity [70]
##    business_process_reason          race_ethnicity             gender count
##    <chr>                            <chr>                      <chr>  <int>
##  1 <NA>                             White (United States of A… Female  5391
##  2 <NA>                             White (United States of A… Male    4836
##  3 <NA>                             Black or African American… Male    1827
##  4 <NA>                             Black or African American… Female  1680
##  5 <NA>                             Asian (United States of A… Female  1022
##  6 Request Compensation Change > A… White (United States of A… Female   794
##  7 Request Compensation Change > A… White (United States of A… Male     762
##  8 Merit > Performance > Annual Pe… White (United States of A… Male     564
##  9 Merit > Performance > Annual Pe… White (United States of A… Female   545
## 10 <NA>                             Hispanic or Latino (Unite… Female   414
## # … with 112 more rows
```

## News

### Gender


```r
current_news_gender_salaried <- news_salaried %>% group_by(gender)
current_news_gender_salaried <- current_news_gender_salaried %>% summarise(
  count = length(current_base_pay)
)
suppress(current_news_gender_salaried)
```

```
## # A tibble: 2 x 2
##   gender count
##   <chr>  <int>
## 1 Female   284
## 2 Male     290
```

```r
current_news_gender_hourly <- news_hourly %>% group_by(gender)
current_news_gender_hourly <- current_news_gender_hourly %>% summarise(
  count = length(current_base_pay)
)
suppress(current_news_gender_hourly)
```

```
## # A tibble: 2 x 2
##   gender count
##   <chr>  <int>
## 1 Female    63
## 2 Male      33
```

```r
current_news_gender_salaried_median <- news_salaried %>% group_by(gender)
current_news_gender_salaried_median <- current_news_gender_salaried_median %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_gender_salaried_median)
```

```
## # A tibble: 2 x 3
##   gender count  median
##   <chr>  <int>   <dbl>
## 1 Female   284  95595.
## 2 Male     290 116065.
```

```r
current_news_gender_hourly_median <- news_hourly %>% group_by(gender)
current_news_gender_hourly_median <- current_news_gender_hourly_median %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_gender_hourly_median)
```

```
## # A tibble: 2 x 3
##   gender count median
##   <chr>  <int>  <dbl>
## 1 Female    63   32.8
## 2 Male      33   33.3
```

```r
current_news_gender_age_salaried <- news_salaried %>% group_by(gender)
current_news_gender_age_salaried %>% summarise(
  median_age = median(age)
)
```

```
## # A tibble: 2 x 2
##   gender median_age
##   <chr>       <dbl>
## 1 Female         35
## 2 Male           41
```

```r
current_news_gender_age_hourly <- news_hourly %>% group_by(gender)
current_news_gender_age_hourly %>% summarise(
  median_age = median(age)
)
```

```
## # A tibble: 2 x 2
##   gender median_age
##   <chr>       <dbl>
## 1 Female         31
## 2 Male           36
```

```r
current_news_gender_age_5_salary <- news_salaried %>% group_by(age_group_5, gender)
current_news_gender_age_5_salary <- current_news_gender_age_5_salary %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_gender_age_5_salary)
```

```
## # A tibble: 20 x 4
## # Groups:   age_group_5 [10]
##    age_group_5 gender count  median
##    <fct>       <chr>  <int>   <dbl>
##  1 <25         Female    19  64280 
##  2 <25         Male       5  72000 
##  3 25-29       Female    60  80000 
##  4 25-29       Male      31  85500 
##  5 30-34       Female    57  87000 
##  6 30-34       Male      46  97828.
##  7 35-39       Female    38  98892.
##  8 35-39       Male      48 116030 
##  9 40-44       Female    22 133200.
## 10 40-44       Male      41 125000 
## 11 45-49       Female    20 117295.
## 12 45-49       Male      23  99725 
## 13 50-54       Female    29 108864.
## 14 50-54       Male      41 126280.
## 15 55-59       Female    22 145655.
## 16 55-59       Male      29 147780 
## 17 60-64       Female    12 129325.
## 18 60-64       Male      16 131217.
## 19 65+         Female     5 157095.
## 20 65+         Male      10 156260.
```

```r
current_news_gender_age_5_hourly <- news_hourly %>% group_by(age_group_5, gender)
current_news_gender_age_5_hourly <- current_news_gender_age_5_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_gender_age_5_hourly)
```

```
## # A tibble: 9 x 4
## # Groups:   age_group_5 [8]
##   age_group_5 gender count median
##   <fct>       <chr>  <int>  <dbl>
## 1 <25         Female    12   31.4
## 2 25-29       Female    17   31.2
## 3 25-29       Male       6   21.0
## 4 30-34       Male       7   33.7
## 5 35-39       Female     5   31.9
## 6 40-44       Female     5   41.4
## 7 45-49       Female     5   44.5
## 8 50-54       Female     6   40.2
## 9 55-59       Male       5   34.9
```

```r
current_news_gender_age_10_salary <- news_salaried %>% group_by(age_group_10, gender)
current_news_gender_age_10_salary <- current_news_gender_age_10_salary %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_gender_age_10_salary)
```

```
## # A tibble: 12 x 4
## # Groups:   age_group_10 [6]
##    age_group_10 gender count  median
##    <fct>        <chr>  <int>   <dbl>
##  1 <25          Female    19  64280 
##  2 <25          Male       5  72000 
##  3 25-34        Female   117  83147.
##  4 25-34        Male      77  92500 
##  5 35-44        Female    60 105691.
##  6 35-44        Male      89 118785 
##  7 45-54        Female    49 108864.
##  8 45-54        Male      64 117982.
##  9 55-64        Female    34 140424.
## 10 55-64        Male      45 146542.
## 11 65+          Female     5 157095.
## 12 65+          Male      10 156260.
```

```r
current_news_gender_age_10_hourly <- news_hourly %>% group_by(age_group_10, gender)
current_news_gender_age_10_hourly <- current_news_gender_age_10_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_gender_age_10_hourly)
```

```
## # A tibble: 8 x 4
## # Groups:   age_group_10 [5]
##   age_group_10 gender count median
##   <fct>        <chr>  <int>  <dbl>
## 1 <25          Female    12   31.4
## 2 25-34        Female    21   31.2
## 3 25-34        Male      13   30.8
## 4 35-44        Female    10   33.1
## 5 35-44        Male       7   35.9
## 6 45-54        Female    11   41.4
## 7 55-64        Female     5   42.1
## 8 55-64        Male       7   33.4
```

```r
current_news_gender_salaried_under_40 <- filter(news_salaried, age < 40) %>% group_by(gender)
current_news_gender_salaried_under_40 <- current_news_gender_salaried_under_40 %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_gender_salaried_under_40)
```

```
## # A tibble: 2 x 3
##   gender count median
##   <chr>  <int>  <dbl>
## 1 Female   174  84030
## 2 Male     130  95890
```

```r
current_news_gender_salaried_over_40 <- filter(news_salaried, age > 39) %>% group_by(gender)
current_news_gender_salaried_over_40 <- current_news_gender_salaried_over_40 %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_gender_salaried_over_40)
```

```
## # A tibble: 2 x 3
##   gender count  median
##   <chr>  <int>   <dbl>
## 1 Female   110 126000 
## 2 Male     160 127765.
```

```r
current_news_gender_hourly_under_40 <- filter(news_hourly, age < 40) %>% group_by(gender)
current_news_gender_hourly_under_40 <- current_news_gender_hourly_under_40 %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_gender_hourly_under_40)
```

```
## # A tibble: 2 x 3
##   gender count median
##   <chr>  <int>  <dbl>
## 1 Female    38   31.4
## 2 Male      18   32.0
```

```r
current_news_gender_hourly_over_40 <- filter(news_hourly, age > 39) %>% group_by(gender)
current_news_gender_hourly_over_40 <- current_news_gender_hourly_over_40 %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_gender_hourly_over_40)
```

```
## # A tibble: 2 x 3
##   gender count median
##   <chr>  <int>  <dbl>
## 1 Female    25   41.4
## 2 Male      15   33.4
```

### Race and ethnicity


```r
current_news_race_salaried <- news_salaried %>% group_by(race_ethnicity)
current_news_race_salaried <- current_news_race_salaried %>% summarise(
  count = length(current_base_pay)
)
suppress_count(current_news_race_salaried)
```

```
## # A tibble: 7 x 2
##   race_ethnicity                                       count
##   <chr>                                                <int>
## 1 White (United States of America)                       406
## 2 Black or African American (United States of America)    48
## 3 Asian (United States of America)                        46
## 4 Hispanic or Latino (United States of America)           28
## 5 <NA>                                                    21
## 6 Two or More Races (United States of America)            14
## 7 Prefer Not to Disclose (United States of America)        8
```

```r
current_news_race_hourly <- news_hourly %>% group_by(race_ethnicity)
current_news_race_hourly <- current_news_race_hourly %>% summarise(
  count = length(current_base_pay)
)
suppress_count(current_news_race_hourly)
```

```
## # A tibble: 3 x 2
##   race_ethnicity                                       count
##   <chr>                                                <int>
## 1 White (United States of America)                        64
## 2 Black or African American (United States of America)    13
## 3 Asian (United States of America)                        11
```

```r
current_news_race_group_salaried <- news_salaried %>% group_by(race_grouping)
current_news_race_group_salaried <- current_news_race_group_salaried %>% summarise(
  count = length(current_base_pay)
)
suppress_count(current_news_race_group_salaried)
```

```
## # A tibble: 3 x 2
##   race_grouping   count
##   <chr>           <int>
## 1 white             406
## 2 person of color   139
## 3 unknown            29
```

```r
current_news_race_group_hourly <- news_hourly %>% group_by(race_grouping)
current_news_race_group_hourly <- current_news_race_group_hourly %>% summarise(
  count = length(current_base_pay)
)
suppress_count(current_news_race_group_hourly)
```

```
## # A tibble: 2 x 2
##   race_grouping   count
##   <chr>           <int>
## 1 white              64
## 2 person of color    30
```

```r
current_news_race_salaried_median <- news_salaried %>% group_by(race_ethnicity)
current_news_race_salaried_median <- current_news_race_salaried_median %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_race_salaried_median)
```

```
## # A tibble: 7 x 3
##   race_ethnicity                                       count  median
##   <chr>                                                <int>   <dbl>
## 1 <NA>                                                    21 140000 
## 2 White (United States of America)                       406 106212.
## 3 Black or African American (United States of America)    48  97276.
## 4 Asian (United States of America)                        46  95205.
## 5 Hispanic or Latino (United States of America)           28  82890 
## 6 Prefer Not to Disclose (United States of America)        8  82140 
## 7 Two or More Races (United States of America)            14  79860
```

```r
current_news_race_hourly_median <- news_hourly %>% group_by(race_ethnicity)
current_news_race_hourly_median <- current_news_race_hourly_median %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_race_hourly_median)
```

```
## # A tibble: 3 x 3
##   race_ethnicity                                       count median
##   <chr>                                                <int>  <dbl>
## 1 White (United States of America)                        64   33.6
## 2 Asian (United States of America)                        11   31.7
## 3 Black or African American (United States of America)    13   29.4
```

```r
current_news_race_group_salaried_median <- news_salaried %>% group_by(race_grouping)
current_news_race_group_salaried_median <- current_news_race_group_salaried_median %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_race_group_salaried_median)
```

```
## # A tibble: 3 x 3
##   race_grouping   count  median
##   <chr>           <int>   <dbl>
## 1 unknown            29 134780 
## 2 white             406 106212.
## 3 person of color   139  92080
```

```r
current_news_race_group_hourly_median <- news_hourly %>% group_by(race_grouping)
current_news_race_group_hourly_median <- current_news_race_group_hourly_median %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_race_group_hourly_median)
```

```
## # A tibble: 2 x 3
##   race_grouping   count median
##   <chr>           <int>  <dbl>
## 1 white              64   33.6
## 2 person of color    30   30.1
```

```r
current_news_race_age_salaried <- news_salaried %>% group_by(race_ethnicity)
current_news_race_age_salaried %>% summarise(
  median_age = median(age)
)
```

```
## # A tibble: 9 x 2
##   race_ethnicity                                                 median_age
##   <chr>                                                               <dbl>
## 1 American Indian or Alaska Native (United States of America)          49.5
## 2 Asian (United States of America)                                     33  
## 3 Black or African American (United States of America)                 39.5
## 4 Hispanic or Latino (United States of America)                        37  
## 5 Native Hawaiian or Other Pacific Islander (United States of A…       43  
## 6 Prefer Not to Disclose (United States of America)                    30.5
## 7 Two or More Races (United States of America)                         28  
## 8 White (United States of America)                                     40  
## 9 <NA>                                                                 36
```

```r
current_news_race_age_hourly <- news_hourly %>% group_by(race_ethnicity)
current_news_race_age_hourly %>% summarise(
  median_age = median(age)
)
```

```
## # A tibble: 8 x 2
##   race_ethnicity                                              median_age
##   <chr>                                                            <dbl>
## 1 American Indian or Alaska Native (United States of America)       69  
## 2 Asian (United States of America)                                  36  
## 3 Black or African American (United States of America)              28  
## 4 Hispanic or Latino (United States of America)                     26  
## 5 Prefer Not to Disclose (United States of America)                 23  
## 6 Two or More Races (United States of America)                      22.5
## 7 White (United States of America)                                  39.5
## 8 <NA>                                                              31
```

```r
current_news_race_age_5_salary <- news_salaried %>% group_by(age_group_5, race_ethnicity)
current_news_race_age_5_salary <- current_news_race_age_5_salary %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_race_age_5_salary)
```

```
## # A tibble: 25 x 4
## # Groups:   age_group_5 [10]
##    age_group_5 race_ethnicity                                  count median
##    <fct>       <chr>                                           <int>  <dbl>
##  1 <25         Asian (United States of America)                    5 65780 
##  2 <25         White (United States of America)                   12 65140 
##  3 25-29       Asian (United States of America)                   11 77000 
##  4 25-29       Black or African American (United States of Am…     6 81000 
##  5 25-29       Two or More Races (United States of America)        6 75690 
##  6 25-29       White (United States of America)                   59 81757.
##  7 30-34       Asian (United States of America)                   10 95780 
##  8 30-34       Black or African American (United States of Am…     9 88133.
##  9 30-34       Hispanic or Latino (United States of America)       6 80596.
## 10 30-34       White (United States of America)                   66 92640 
## # … with 15 more rows
```

```r
current_news_race_age_5_hourly <- news_hourly %>% group_by(age_group_5, race_ethnicity)
current_news_race_age_5_hourly <- current_news_race_age_5_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_race_age_5_hourly)
```

```
## # A tibble: 10 x 4
## # Groups:   age_group_5 [9]
##    age_group_5 race_ethnicity                                  count median
##    <fct>       <chr>                                           <int>  <dbl>
##  1 <25         White (United States of America)                    7   18.5
##  2 25-29       Black or African American (United States of Am…     8   30.1
##  3 25-29       White (United States of America)                   11   30.8
##  4 30-34       White (United States of America)                    9   33.7
##  5 35-39       White (United States of America)                    5   34.7
##  6 40-44       White (United States of America)                    7   41.4
##  7 45-49       White (United States of America)                    5   44.5
##  8 50-54       White (United States of America)                    6   40.2
##  9 55-59       White (United States of America)                    6   33.9
## 10 60-64       White (United States of America)                    5   38.8
```

```r
current_news_race_age_10_salary <- news_salaried %>% group_by(age_group_10, race_ethnicity)
current_news_race_age_10_salary <- current_news_race_age_10_salary %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_race_age_10_salary)
```

```
## # A tibble: 21 x 4
## # Groups:   age_group_10 [6]
##    age_group_10 race_ethnicity                                 count median
##    <fct>        <chr>                                          <int>  <dbl>
##  1 <25          Asian (United States of America)                   5 6.58e4
##  2 <25          White (United States of America)                  12 6.51e4
##  3 25-34        Asian (United States of America)                  21 8.60e4
##  4 25-34        Black or African American (United States of A…    15 8.70e4
##  5 25-34        Hispanic or Latino (United States of America)     10 8.12e4
##  6 25-34        Prefer Not to Disclose (United States of Amer…     5 7.85e4
##  7 25-34        Two or More Races (United States of America)       9 7.64e4
##  8 25-34        White (United States of America)                 125 8.60e4
##  9 25-34        <NA>                                               9 1.16e5
## 10 35-44        Asian (United States of America)                  11 1.08e5
## # … with 11 more rows
```

```r
current_news_race_age_10_hourly <- news_hourly %>% group_by(age_group_10, race_ethnicity)
current_news_race_age_10_hourly <- current_news_race_age_10_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_race_age_10_hourly)
```

```
## # A tibble: 6 x 4
## # Groups:   age_group_10 [5]
##   age_group_10 race_ethnicity                                  count median
##   <fct>        <chr>                                           <int>  <dbl>
## 1 <25          White (United States of America)                    7   18.5
## 2 25-34        Black or African American (United States of Am…     8   30.1
## 3 25-34        White (United States of America)                   20   31.3
## 4 35-44        White (United States of America)                   12   35.3
## 5 45-54        White (United States of America)                   11   41.4
## 6 55-64        White (United States of America)                   11   34.9
```

```r
current_news_race_group_age_5_salary <- news_salaried %>% group_by(age_group_5, race_grouping)
current_news_race_group_age_5_salary <- current_news_race_group_age_5_salary %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_race_group_age_5_salary)
```

```
## # A tibble: 21 x 4
## # Groups:   age_group_5 [10]
##    age_group_5 race_grouping   count  median
##    <fct>       <chr>           <int>   <dbl>
##  1 <25         person of color    11  63780 
##  2 <25         white              12  65140 
##  3 25-29       person of color    27  80000 
##  4 25-29       unknown             5  88280 
##  5 25-29       white              59  81757.
##  6 30-34       person of color    28  86983.
##  7 30-34       unknown             9 108000 
##  8 30-34       white              66  92640 
##  9 35-39       person of color    23  99238.
## 10 35-39       white              61 105780 
## # … with 11 more rows
```

```r
current_news_race_group_age_5_hourly <- news_hourly %>% group_by(age_group_5, race_grouping)
current_news_race_group_age_5_hourly <- current_news_race_group_age_5_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_race_group_age_5_hourly)
```

```
## # A tibble: 11 x 4
## # Groups:   age_group_5 [9]
##    age_group_5 race_grouping   count median
##    <fct>       <chr>           <int>  <dbl>
##  1 <25         person of color     6   29.5
##  2 <25         white               7   18.5
##  3 25-29       person of color    12   27.1
##  4 25-29       white              11   30.8
##  5 30-34       white               9   33.7
##  6 35-39       white               5   34.7
##  7 40-44       white               7   41.4
##  8 45-49       white               5   44.5
##  9 50-54       white               6   40.2
## 10 55-59       white               6   33.9
## 11 60-64       white               5   38.8
```

```r
current_news_race_group_age_10_salary <- news_salaried %>% group_by(age_group_10, race_grouping)
current_news_race_group_age_10_salary <- current_news_race_group_age_10_salary %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_race_group_age_10_salary)
```

```
## # A tibble: 13 x 4
## # Groups:   age_group_10 [6]
##    age_group_10 race_grouping   count  median
##    <fct>        <chr>           <int>   <dbl>
##  1 <25          person of color    11  63780 
##  2 <25          white              12  65140 
##  3 25-34        person of color    55  83340 
##  4 25-34        unknown            14 106890 
##  5 25-34        white             125  86000 
##  6 35-44        person of color    38 102890 
##  7 35-44        unknown             7 140280 
##  8 35-44        white             104 115258.
##  9 45-54        person of color    26 106932.
## 10 45-54        white              84 116687.
## 11 55-64        person of color     8 140424.
## 12 55-64        white              68 140052.
## 13 65+          white              13 159300
```

```r
current_news_race_group_age_10_hourly <- news_hourly %>% group_by(age_group_10, race_grouping)
current_news_race_group_age_10_hourly <- current_news_race_group_age_10_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_race_group_age_10_hourly)
```

```
## # A tibble: 8 x 4
## # Groups:   age_group_10 [5]
##   age_group_10 race_grouping   count median
##   <fct>        <chr>           <int>  <dbl>
## 1 <25          person of color     6   29.5
## 2 <25          white               7   18.5
## 3 25-34        person of color    13   29.1
## 4 25-34        white              20   31.3
## 5 35-44        person of color     5   23.9
## 6 35-44        white              12   35.3
## 7 45-54        white              11   41.4
## 8 55-64        white              11   34.9
```

```r
current_news_race_salaried_under_40 <- filter(news_salaried, age < 40) %>% group_by(race_ethnicity)
current_news_race_salaried_under_40 <- current_news_race_salaried_under_40 %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_race_salaried_under_40)
```

```
## # A tibble: 7 x 3
##   race_ethnicity                                       count  median
##   <chr>                                                <int>   <dbl>
## 1 <NA>                                                    11 125000 
## 2 White (United States of America)                       198  90780 
## 3 Black or African American (United States of America)    24  87970.
## 4 Asian (United States of America)                        33  87000 
## 5 Hispanic or Latino (United States of America)           19  79618.
## 6 Prefer Not to Disclose (United States of America)        6  77750 
## 7 Two or More Races (United States of America)            13  76380
```

```r
current_news_race_salaried_over_40 <- filter(news_salaried, age > 39) %>% group_by(race_ethnicity)
current_news_race_salaried_over_40 <- current_news_race_salaried_over_40 %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_race_salaried_over_40)
```

```
## # A tibble: 5 x 3
##   race_ethnicity                                       count  median
##   <chr>                                                <int>   <dbl>
## 1 <NA>                                                    10 151408.
## 2 White (United States of America)                       208 128484.
## 3 Hispanic or Latino (United States of America)            9 126580 
## 4 Asian (United States of America)                        13 111761.
## 5 Black or African American (United States of America)    24 109396.
```

```r
current_news_race_hourly_under_40 <- filter(news_hourly, age < 40) %>% group_by(race_ethnicity)
current_news_race_hourly_under_40 <- current_news_race_hourly_under_40 %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_race_hourly_under_40)
```

```
## # A tibble: 3 x 3
##   race_ethnicity                                       count median
##   <chr>                                                <int>  <dbl>
## 1 White (United States of America)                        32   32.0
## 2 Black or African American (United States of America)    10   29.9
## 3 Asian (United States of America)                         7   25.0
```

```r
current_news_race_hourly_over_40 <- filter(news_hourly, age > 39) %>% group_by(race_ethnicity)
current_news_race_hourly_over_40 <- current_news_race_hourly_over_40 %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_race_hourly_over_40)
```

```
## # A tibble: 1 x 3
##   race_ethnicity                   count median
##   <chr>                            <int>  <dbl>
## 1 White (United States of America)    32   39.9
```

### Gender x race/ethnicity


```r
current_news_race_gender_salaried <- news_salaried %>% group_by(race_ethnicity, gender)
current_news_race_gender_salaried <- current_news_race_gender_salaried %>% summarise(
  count = length(current_base_pay)
)
suppress(current_news_race_gender_salaried)
```

```
## # A tibble: 13 x 3
## # Groups:   race_ethnicity [7]
##    race_ethnicity                                       gender count
##    <chr>                                                <chr>  <int>
##  1 Asian (United States of America)                     Female    34
##  2 Asian (United States of America)                     Male      12
##  3 Black or African American (United States of America) Female    24
##  4 Black or African American (United States of America) Male      24
##  5 Hispanic or Latino (United States of America)        Female    14
##  6 Hispanic or Latino (United States of America)        Male      14
##  7 Prefer Not to Disclose (United States of America)    Male       5
##  8 Two or More Races (United States of America)         Female     9
##  9 Two or More Races (United States of America)         Male       5
## 10 White (United States of America)                     Female   188
## 11 White (United States of America)                     Male     218
## 12 <NA>                                                 Female    10
## 13 <NA>                                                 Male      11
```

```r
current_news_race_gender_hourly <- news_hourly %>% group_by(race_ethnicity, gender)
current_news_race_gender_hourly <- current_news_race_gender_hourly %>% summarise(
  count = length(current_base_pay)
)
suppress(current_news_race_gender_hourly)
```

```
## # A tibble: 5 x 3
## # Groups:   race_ethnicity [3]
##   race_ethnicity                                       gender count
##   <chr>                                                <chr>  <int>
## 1 Asian (United States of America)                     Female     8
## 2 Black or African American (United States of America) Female     8
## 3 Black or African American (United States of America) Male       5
## 4 White (United States of America)                     Female    41
## 5 White (United States of America)                     Male      23
```

```r
current_news_race_gender_median_salaried <- news_salaried %>% group_by(race_ethnicity, gender)
current_news_race_gender_median_salaried <- current_news_race_gender_median_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_race_gender_median_salaried)
```

```
## # A tibble: 13 x 4
## # Groups:   race_ethnicity [7]
##    race_ethnicity                                       gender count median
##    <chr>                                                <chr>  <int>  <dbl>
##  1 Asian (United States of America)                     Female    34 9.19e4
##  2 Asian (United States of America)                     Male      12 1.07e5
##  3 Black or African American (United States of America) Female    24 8.72e4
##  4 Black or African American (United States of America) Male      24 1.20e5
##  5 Hispanic or Latino (United States of America)        Female    14 8.12e4
##  6 Hispanic or Latino (United States of America)        Male      14 9.14e4
##  7 Prefer Not to Disclose (United States of America)    Male       5 8.83e4
##  8 Two or More Races (United States of America)         Female     9 7.50e4
##  9 Two or More Races (United States of America)         Male       5 9.49e4
## 10 White (United States of America)                     Female   188 9.96e4
## 11 White (United States of America)                     Male     218 1.17e5
## 12 <NA>                                                 Female    10 1.38e5
## 13 <NA>                                                 Male      11 1.40e5
```

```r
current_news_race_gender_hourly_median <- news_hourly %>% group_by(race_ethnicity, gender)
current_news_race_gender_hourly_median <- current_news_race_gender_hourly_median %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_race_gender_hourly_median)
```

```
## # A tibble: 5 x 4
## # Groups:   race_ethnicity [3]
##   race_ethnicity                                       gender count median
##   <chr>                                                <chr>  <int>  <dbl>
## 1 Asian (United States of America)                     Female     8   30.0
## 2 Black or African American (United States of America) Female     8   31.0
## 3 Black or African American (United States of America) Male       5   20.9
## 4 White (United States of America)                     Female    41   34.7
## 5 White (United States of America)                     Male      23   33.4
```

```r
current_news_race_gender_salaried_under_40 <- filter(news_salaried, age < 40) %>% group_by(race_ethnicity, gender)
current_news_race_gender_salaried_under_40 <- current_news_race_gender_salaried_under_40 %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_race_gender_salaried_under_40)
```

```
## # A tibble: 10 x 4
## # Groups:   race_ethnicity [6]
##    race_ethnicity                                       gender count median
##    <chr>                                                <chr>  <int>  <dbl>
##  1 Asian (United States of America)                     Female    25 8.60e4
##  2 Asian (United States of America)                     Male       8 1.03e5
##  3 Black or African American (United States of America) Female    16 8.54e4
##  4 Black or African American (United States of America) Male       8 1.28e5
##  5 Hispanic or Latino (United States of America)        Female    12 8.01e4
##  6 Hispanic or Latino (United States of America)        Male       7 7.50e4
##  7 Two or More Races (United States of America)         Female     9 7.50e4
##  8 White (United States of America)                     Female   105 8.58e4
##  9 White (United States of America)                     Male      93 9.57e4
## 10 <NA>                                                 Male       7 1.35e5
```

```r
current_news_race_gender_salaried_over_40 <- filter(news_salaried, age > 39) %>% group_by(race_ethnicity, gender)
current_news_race_gender_salaried_over_40 <- current_news_race_gender_salaried_over_40 %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_race_gender_salaried_over_40)
```

```
## # A tibble: 7 x 4
## # Groups:   race_ethnicity [5]
##   race_ethnicity                                       gender count  median
##   <chr>                                                <chr>  <int>   <dbl>
## 1 Asian (United States of America)                     Female     9 111761.
## 2 Black or African American (United States of America) Female     8 115002.
## 3 Black or African American (United States of America) Male      16 107464.
## 4 Hispanic or Latino (United States of America)        Male       7 126580 
## 5 White (United States of America)                     Female    83 122917.
## 6 White (United States of America)                     Male     125 130000 
## 7 <NA>                                                 Female     6 148572.
```

```r
current_news_race_gender_hourly_under_40 <- filter(news_hourly, age < 40) %>% group_by(race_ethnicity, gender)
current_news_race_gender_hourly_under_40 <- current_news_race_gender_hourly_under_40 %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_race_gender_hourly_under_40)
```

```
## # A tibble: 4 x 4
## # Groups:   race_ethnicity [3]
##   race_ethnicity                                       gender count median
##   <chr>                                                <chr>  <int>  <dbl>
## 1 Asian (United States of America)                     Female     5   25.0
## 2 Black or African American (United States of America) Female     6   31.0
## 3 White (United States of America)                     Female    21   31.9
## 4 White (United States of America)                     Male      11   33.7
```

```r
current_news_race_gender_hourly_over_40 <- filter(news_hourly, age > 39) %>% group_by(race_ethnicity, gender)
current_news_race_gender_hourly_over_40 <- current_news_race_gender_hourly_over_40 %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_race_gender_hourly_over_40)
```

```
## # A tibble: 2 x 4
## # Groups:   race_ethnicity [1]
##   race_ethnicity                   gender count median
##   <chr>                            <chr>  <int>  <dbl>
## 1 White (United States of America) Female    20   42.4
## 2 White (United States of America) Male      12   33.2
```

### Years of service


```r
current_news_yos_salaried <- news_salaried %>% group_by(years_of_service_grouped)
current_news_yos_salaried <- current_news_yos_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_yos_salaried)
```

```
## # A tibble: 8 x 3
##   years_of_service_grouped count  median
##   <fct>                    <int>   <dbl>
## 1 0                           65  90000 
## 2 1-2                        128  93780 
## 3 3-5                        146  92170.
## 4 6-10                        60 112926.
## 5 11-15                       50 110823.
## 6 16-20                       68 127655.
## 7 21-25                       24 143198.
## 8 25+                         33 139831.
```

```r
current_news_yos_hourly <- news_hourly %>% group_by(years_of_service_grouped)
current_news_yos_hourly <- current_news_yos_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_yos_hourly)
```

```
## # A tibble: 7 x 3
##   years_of_service_grouped count median
##   <fct>                    <int>  <dbl>
## 1 0                           16   29.5
## 2 1-2                         26   32.7
## 3 3-5                          9   33.0
## 4 6-10                        15   35.9
## 5 11-15                       10   36.5
## 6 16-20                       11   32.3
## 7 21-25                        5   38.9
```

```r
current_news_yos_gender_salaried <- news_salaried %>% group_by(years_of_service_grouped, gender)
current_news_yos_gender_salaried <- current_news_yos_gender_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_yos_gender_salaried)
```

```
## # A tibble: 16 x 4
## # Groups:   years_of_service_grouped [8]
##    years_of_service_grouped gender count  median
##    <fct>                    <chr>  <int>   <dbl>
##  1 0                        Female    39  80000 
##  2 0                        Male      26 105000 
##  3 1-2                      Female    70  87390 
##  4 1-2                      Male      58 101788.
##  5 3-5                      Female    72  88530 
##  6 3-5                      Male      74  95265.
##  7 6-10                     Female    26 100640.
##  8 6-10                     Male      34 119562.
##  9 11-15                    Female    25  98545.
## 10 11-15                    Male      25 129780 
## 11 16-20                    Female    28 119826.
## 12 16-20                    Male      40 129745.
## 13 21-25                    Female    11 134780 
## 14 21-25                    Male      13 148417.
## 15 25+                      Female    13 142280 
## 16 25+                      Male      20 131793.
```

```r
current_news_yos_gender_hourly <- news_hourly %>% group_by(years_of_service_grouped, gender)
current_news_yos_gender_hourly <- current_news_yos_gender_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_yos_gender_hourly)
```

```
## # A tibble: 9 x 4
## # Groups:   years_of_service_grouped [6]
##   years_of_service_grouped gender count median
##   <fct>                    <chr>  <int>  <dbl>
## 1 0                        Female    11   28.2
## 2 0                        Male       5   30.8
## 3 1-2                      Female    18   32.4
## 4 1-2                      Male       8   33.3
## 5 3-5                      Male       6   32.5
## 6 6-10                     Female     8   31.4
## 7 6-10                     Male       7   36.7
## 8 11-15                    Female     9   38.4
## 9 16-20                    Female     7   42.1
```

```r
current_news_yos_race_salaried <- news_salaried %>% group_by(years_of_service_grouped, race_ethnicity)
current_news_yos_race_salaried <- current_news_yos_race_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_yos_race_salaried)
```

```
## # A tibble: 21 x 4
## # Groups:   years_of_service_grouped [8]
##    years_of_service_gro… race_ethnicity                        count median
##    <fct>                 <chr>                                 <int>  <dbl>
##  1 0                     Asian (United States of America)          7 7.70e4
##  2 0                     White (United States of America)         42 1.00e5
##  3 1-2                   Asian (United States of America)         13 8.48e4
##  4 1-2                   Black or African American (United St…    10 8.98e4
##  5 1-2                   Hispanic or Latino (United States of…     6 8.29e4
##  6 1-2                   Two or More Races (United States of …     5 6.80e4
##  7 1-2                   White (United States of America)         85 9.58e4
##  8 1-2                   <NA>                                      5 1.40e5
##  9 3-5                   Asian (United States of America)         12 9.36e4
## 10 3-5                   Black or African American (United St…    12 9.73e4
## # … with 11 more rows
```

```r
current_news_yos_race_hourly <- news_hourly %>% group_by(years_of_service_grouped, race_ethnicity)
current_news_yos_race_hourly <- current_news_yos_race_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_yos_race_hourly)
```

```
## # A tibble: 7 x 4
## # Groups:   years_of_service_grouped [7]
##   years_of_service_grouped race_ethnicity                   count median
##   <fct>                    <chr>                            <int>  <dbl>
## 1 0                        White (United States of America)     6   29.5
## 2 1-2                      White (United States of America)    18   32.8
## 3 3-5                      White (United States of America)     6   32.5
## 4 6-10                     White (United States of America)     9   35.9
## 5 11-15                    White (United States of America)     8   39.9
## 6 16-20                    White (United States of America)     9   42.1
## 7 21-25                    White (United States of America)     5   38.9
```

```r
current_news_yos_race_gender_salaried <- news_salaried %>% group_by(years_of_service_grouped, race_ethnicity, gender)
current_news_yos_race_gender_salaried <- current_news_yos_race_gender_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_yos_race_gender_salaried)
```

```
## # A tibble: 26 x 5
## # Groups:   years_of_service_grouped, race_ethnicity [16]
##    years_of_service_gr… race_ethnicity                  gender count median
##    <fct>                <chr>                           <chr>  <int>  <dbl>
##  1 0                    Asian (United States of Americ… Female     7 7.70e4
##  2 0                    White (United States of Americ… Female    25 8.50e4
##  3 0                    White (United States of Americ… Male      17 1.10e5
##  4 1-2                  Asian (United States of Americ… Female    11 7.70e4
##  5 1-2                  Black or African American (Uni… Female     6 8.58e4
##  6 1-2                  Hispanic or Latino (United Sta… Female     5 8.20e4
##  7 1-2                  White (United States of Americ… Female    41 9.08e4
##  8 1-2                  White (United States of Americ… Male      44 9.98e4
##  9 3-5                  Asian (United States of Americ… Female     8 9.36e4
## 10 3-5                  Black or African American (Uni… Female     7 9.61e4
## # … with 16 more rows
```

```r
current_news_yos_race_gender_hourly <- news_hourly %>% group_by(years_of_service_grouped, race_ethnicity, gender)
current_news_yos_race_gender_hourly <- current_news_yos_race_gender_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_yos_race_gender_hourly)
```

```
## # A tibble: 5 x 5
## # Groups:   years_of_service_grouped, race_ethnicity [4]
##   years_of_service_group… race_ethnicity                gender count median
##   <fct>                   <chr>                         <chr>  <int>  <dbl>
## 1 1-2                     White (United States of Amer… Female    12   32.7
## 2 1-2                     White (United States of Amer… Male       6   33.3
## 3 6-10                    White (United States of Amer… Male       5   35.9
## 4 11-15                   White (United States of Amer… Female     7   41.4
## 5 16-20                   White (United States of Amer… Female     6   42.4
```

### Age


```r
current_median_news_age_5_salaried <- news_salaried %>% group_by(age_group_5)
current_median_news_age_5_salaried <- current_median_news_age_5_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_news_age_5_salaried)
```

```
## # A tibble: 10 x 3
##    age_group_5 count  median
##    <fct>       <int>   <dbl>
##  1 <25            24  64640 
##  2 25-29          91  80500 
##  3 30-34         103  90780 
##  4 35-39          86 105691.
##  5 40-44          63 125769.
##  6 45-49          43 102796.
##  7 50-54          70 115770.
##  8 55-59          51 147780 
##  9 60-64          28 131217.
## 10 65+            15 157095.
```

```r
current_median_news_age_5_hourly <- news_hourly %>% group_by(age_group_5)
current_median_news_age_5_hourly <- current_median_news_age_5_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_news_age_5_hourly)
```

```
## # A tibble: 10 x 3
##    age_group_5 count median
##    <fct>       <int>  <dbl>
##  1 <25            14   29.5
##  2 25-29          23   30.8
##  3 30-34          11   33.7
##  4 35-39           8   33.9
##  5 40-44           9   33.1
##  6 45-49           6   47.4
##  7 50-54           8   36.2
##  8 55-59           7   34.9
##  9 60-64           5   38.8
## 10 65+             5   42.6
```

```r
current_median_news_age_10_salaried <- news_salaried %>% group_by(age_group_10)
current_median_news_age_10_salaried <- current_median_news_age_10_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_news_age_10_salaried)
```

```
## # A tibble: 6 x 3
##   age_group_10 count  median
##   <fct>        <int>   <dbl>
## 1 <25             24  64640 
## 2 25-34          194  85890 
## 3 35-44          149 115237.
## 4 45-54          113 114803 
## 5 55-64           79 141016.
## 6 65+             15 157095.
```

```r
current_median_news_age_10_hourly <- news_hourly %>% group_by(age_group_10)
current_median_news_age_10_hourly <- current_median_news_age_10_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_news_age_10_hourly)
```

```
## # A tibble: 6 x 3
##   age_group_10 count median
##   <fct>        <int>  <dbl>
## 1 <25             14   29.5
## 2 25-34           34   31.0
## 3 35-44           17   33.1
## 4 45-54           14   41.1
## 5 55-64           12   35.8
## 6 65+              5   42.6
```

```r
current_news_age_5_yos_salary <- news_salaried %>% group_by(age_group_5, years_of_service_grouped)
current_news_age_5_yos_salary <- current_news_age_5_yos_salary %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_age_5_yos_salary)
```

```
## # A tibble: 39 x 4
## # Groups:   age_group_5 [9]
##    age_group_5 years_of_service_grouped count  median
##    <fct>       <fct>                    <int>   <dbl>
##  1 <25         0                            9  66000 
##  2 <25         1-2                         13  63780 
##  3 25-29       0                           19  82000 
##  4 25-29       1-2                         30  78500 
##  5 25-29       3-5                         41  81757.
##  6 30-34       0                           13  87000 
##  7 30-34       1-2                         28  93528.
##  8 30-34       3-5                         43  88780 
##  9 30-34       6-10                        15  82312.
## 10 35-39       0                            9 110000 
## # … with 29 more rows
```

```r
current_news_age_5_yos_hourly <- news_hourly %>% group_by(age_group_5, years_of_service_grouped)
current_news_age_5_yos_hourly <- current_news_age_5_yos_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_age_5_yos_hourly)
```

```
## # A tibble: 4 x 4
## # Groups:   age_group_5 [2]
##   age_group_5 years_of_service_grouped count median
##   <fct>       <fct>                    <int>  <dbl>
## 1 <25         0                            6   24.1
## 2 <25         1-2                          8   32  
## 3 25-29       0                            8   29.5
## 4 25-29       1-2                         12   32.2
```

```r
current_news_age_10_yos_salary <- news_salaried %>% group_by(age_group_10, years_of_service_grouped)
current_news_age_10_yos_salary <- current_news_age_10_yos_salary %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_age_10_yos_salary)
```

```
## # A tibble: 26 x 4
## # Groups:   age_group_10 [5]
##    age_group_10 years_of_service_grouped count  median
##    <fct>        <fct>                    <int>   <dbl>
##  1 <25          0                            9  66000 
##  2 <25          1-2                         13  63780 
##  3 25-34        0                           32  85000 
##  4 25-34        1-2                         58  86280 
##  5 25-34        3-5                         84  85890 
##  6 25-34        6-10                        16  94676.
##  7 35-44        0                           16 125000 
##  8 35-44        1-2                         36 116530 
##  9 35-44        3-5                         38 110935.
## 10 35-44        6-10                        25 115237.
## # … with 16 more rows
```

```r
current_news_age_10_yos_hourly <- news_hourly %>% group_by(age_group_10, years_of_service_grouped)
current_news_age_10_yos_hourly <- current_news_age_10_yos_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_news_age_10_yos_hourly)
```

```
## # A tibble: 6 x 4
## # Groups:   age_group_10 [3]
##   age_group_10 years_of_service_grouped count median
##   <fct>        <fct>                    <int>  <dbl>
## 1 <25          0                            6   24.1
## 2 <25          1-2                          8   32  
## 3 25-34        0                            9   30.8
## 4 25-34        1-2                         16   32.7
## 5 25-34        3-5                          6   30.0
## 6 35-44        11-15                        6   33.9
```

```r
current_median_news_age_5_gender_salaried <- news_salaried %>% group_by(age_group_5, gender)
current_median_news_age_5_gender_salaried <- current_median_news_age_5_gender_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_news_age_5_gender_salaried)
```

```
## # A tibble: 20 x 4
## # Groups:   age_group_5 [10]
##    age_group_5 gender count  median
##    <fct>       <chr>  <int>   <dbl>
##  1 <25         Female    19  64280 
##  2 <25         Male       5  72000 
##  3 25-29       Female    60  80000 
##  4 25-29       Male      31  85500 
##  5 30-34       Female    57  87000 
##  6 30-34       Male      46  97828.
##  7 35-39       Female    38  98892.
##  8 35-39       Male      48 116030 
##  9 40-44       Female    22 133200.
## 10 40-44       Male      41 125000 
## 11 45-49       Female    20 117295.
## 12 45-49       Male      23  99725 
## 13 50-54       Female    29 108864.
## 14 50-54       Male      41 126280.
## 15 55-59       Female    22 145655.
## 16 55-59       Male      29 147780 
## 17 60-64       Female    12 129325.
## 18 60-64       Male      16 131217.
## 19 65+         Female     5 157095.
## 20 65+         Male      10 156260.
```

```r
current_median_news_age_5_gender_hourly <- news_hourly %>% group_by(age_group_5, gender)
current_median_news_age_5_gender_hourly <- current_median_news_age_5_gender_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_news_age_5_gender_hourly)
```

```
## # A tibble: 9 x 4
## # Groups:   age_group_5 [8]
##   age_group_5 gender count median
##   <fct>       <chr>  <int>  <dbl>
## 1 <25         Female    12   31.4
## 2 25-29       Female    17   31.2
## 3 25-29       Male       6   21.0
## 4 30-34       Male       7   33.7
## 5 35-39       Female     5   31.9
## 6 40-44       Female     5   41.4
## 7 45-49       Female     5   44.5
## 8 50-54       Female     6   40.2
## 9 55-59       Male       5   34.9
```

```r
current_median_news_age_10_gender_salaried <- news_salaried %>% group_by(age_group_10, gender)
current_median_news_age_10_gender_salaried <- current_median_news_age_10_gender_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_news_age_10_gender_salaried)
```

```
## # A tibble: 12 x 4
## # Groups:   age_group_10 [6]
##    age_group_10 gender count  median
##    <fct>        <chr>  <int>   <dbl>
##  1 <25          Female    19  64280 
##  2 <25          Male       5  72000 
##  3 25-34        Female   117  83147.
##  4 25-34        Male      77  92500 
##  5 35-44        Female    60 105691.
##  6 35-44        Male      89 118785 
##  7 45-54        Female    49 108864.
##  8 45-54        Male      64 117982.
##  9 55-64        Female    34 140424.
## 10 55-64        Male      45 146542.
## 11 65+          Female     5 157095.
## 12 65+          Male      10 156260.
```

```r
current_median_news_age_10_gender_hourly <- news_hourly %>% group_by(age_group_10, gender)
current_median_news_age_10_gender_hourly <- current_median_news_age_10_gender_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_news_age_10_gender_hourly)
```

```
## # A tibble: 8 x 4
## # Groups:   age_group_10 [5]
##   age_group_10 gender count median
##   <fct>        <chr>  <int>  <dbl>
## 1 <25          Female    12   31.4
## 2 25-34        Female    21   31.2
## 3 25-34        Male      13   30.8
## 4 35-44        Female    10   33.1
## 5 35-44        Male       7   35.9
## 6 45-54        Female    11   41.4
## 7 55-64        Female     5   42.1
## 8 55-64        Male       7   33.4
```

```r
current_median_news_age_5_race_salaried <- news_salaried %>% group_by(age_group_5, race_ethnicity)
current_median_news_age_5_race_salaried <- current_median_news_age_5_race_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_news_age_5_race_salaried)
```

```
## # A tibble: 25 x 4
## # Groups:   age_group_5 [10]
##    age_group_5 race_ethnicity                                  count median
##    <fct>       <chr>                                           <int>  <dbl>
##  1 <25         Asian (United States of America)                    5 65780 
##  2 <25         White (United States of America)                   12 65140 
##  3 25-29       Asian (United States of America)                   11 77000 
##  4 25-29       Black or African American (United States of Am…     6 81000 
##  5 25-29       Two or More Races (United States of America)        6 75690 
##  6 25-29       White (United States of America)                   59 81757.
##  7 30-34       Asian (United States of America)                   10 95780 
##  8 30-34       Black or African American (United States of Am…     9 88133.
##  9 30-34       Hispanic or Latino (United States of America)       6 80596.
## 10 30-34       White (United States of America)                   66 92640 
## # … with 15 more rows
```

```r
current_median_news_age_5_race_hourly <- news_hourly %>% group_by(age_group_5, race_ethnicity)
current_median_news_age_5_race_hourly <- current_median_news_age_5_race_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_news_age_5_race_hourly)
```

```
## # A tibble: 10 x 4
## # Groups:   age_group_5 [9]
##    age_group_5 race_ethnicity                                  count median
##    <fct>       <chr>                                           <int>  <dbl>
##  1 <25         White (United States of America)                    7   18.5
##  2 25-29       Black or African American (United States of Am…     8   30.1
##  3 25-29       White (United States of America)                   11   30.8
##  4 30-34       White (United States of America)                    9   33.7
##  5 35-39       White (United States of America)                    5   34.7
##  6 40-44       White (United States of America)                    7   41.4
##  7 45-49       White (United States of America)                    5   44.5
##  8 50-54       White (United States of America)                    6   40.2
##  9 55-59       White (United States of America)                    6   33.9
## 10 60-64       White (United States of America)                    5   38.8
```

```r
current_median_news_age_10_race_salaried <- news_salaried %>% group_by(age_group_10, race_ethnicity)
current_median_news_age_10_race_salaried <- current_median_news_age_10_race_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_news_age_10_race_salaried)
```

```
## # A tibble: 21 x 4
## # Groups:   age_group_10 [6]
##    age_group_10 race_ethnicity                                 count median
##    <fct>        <chr>                                          <int>  <dbl>
##  1 <25          Asian (United States of America)                   5 6.58e4
##  2 <25          White (United States of America)                  12 6.51e4
##  3 25-34        Asian (United States of America)                  21 8.60e4
##  4 25-34        Black or African American (United States of A…    15 8.70e4
##  5 25-34        Hispanic or Latino (United States of America)     10 8.12e4
##  6 25-34        Prefer Not to Disclose (United States of Amer…     5 7.85e4
##  7 25-34        Two or More Races (United States of America)       9 7.64e4
##  8 25-34        White (United States of America)                 125 8.60e4
##  9 25-34        <NA>                                               9 1.16e5
## 10 35-44        Asian (United States of America)                  11 1.08e5
## # … with 11 more rows
```

```r
current_median_news_age_10_race_hourly <- news_hourly %>% group_by(age_group_10, race_ethnicity)
current_median_news_age_10_race_hourly <- current_median_news_age_10_race_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_news_age_10_race_hourly)
```

```
## # A tibble: 6 x 4
## # Groups:   age_group_10 [5]
##   age_group_10 race_ethnicity                                  count median
##   <fct>        <chr>                                           <int>  <dbl>
## 1 <25          White (United States of America)                    7   18.5
## 2 25-34        Black or African American (United States of Am…     8   30.1
## 3 25-34        White (United States of America)                   20   31.3
## 4 35-44        White (United States of America)                   12   35.3
## 5 45-54        White (United States of America)                   11   41.4
## 6 55-64        White (United States of America)                   11   34.9
```

```r
current_median_news_age_5_race_group_salaried <- news_salaried %>% group_by(age_group_5, race_grouping)
current_median_news_age_5_race_group_salaried <- current_median_news_age_5_race_group_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_news_age_5_race_group_salaried)
```

```
## # A tibble: 21 x 4
## # Groups:   age_group_5 [10]
##    age_group_5 race_grouping   count  median
##    <fct>       <chr>           <int>   <dbl>
##  1 <25         person of color    11  63780 
##  2 <25         white              12  65140 
##  3 25-29       person of color    27  80000 
##  4 25-29       unknown             5  88280 
##  5 25-29       white              59  81757.
##  6 30-34       person of color    28  86983.
##  7 30-34       unknown             9 108000 
##  8 30-34       white              66  92640 
##  9 35-39       person of color    23  99238.
## 10 35-39       white              61 105780 
## # … with 11 more rows
```

```r
current_median_news_age_5_race_group_hourly <- news_hourly %>% group_by(age_group_5, race_grouping)
current_median_news_age_5_race_group_hourly <- current_median_news_age_5_race_group_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_news_age_5_race_group_hourly)
```

```
## # A tibble: 11 x 4
## # Groups:   age_group_5 [9]
##    age_group_5 race_grouping   count median
##    <fct>       <chr>           <int>  <dbl>
##  1 <25         person of color     6   29.5
##  2 <25         white               7   18.5
##  3 25-29       person of color    12   27.1
##  4 25-29       white              11   30.8
##  5 30-34       white               9   33.7
##  6 35-39       white               5   34.7
##  7 40-44       white               7   41.4
##  8 45-49       white               5   44.5
##  9 50-54       white               6   40.2
## 10 55-59       white               6   33.9
## 11 60-64       white               5   38.8
```

```r
current_median_news_age_10_race_group_salaried <- news_salaried %>% group_by(age_group_10, race_grouping)
current_median_news_age_10_race_group_salaried <- current_median_news_age_10_race_group_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_news_age_10_race_group_salaried)
```

```
## # A tibble: 13 x 4
## # Groups:   age_group_10 [6]
##    age_group_10 race_grouping   count  median
##    <fct>        <chr>           <int>   <dbl>
##  1 <25          person of color    11  63780 
##  2 <25          white              12  65140 
##  3 25-34        person of color    55  83340 
##  4 25-34        unknown            14 106890 
##  5 25-34        white             125  86000 
##  6 35-44        person of color    38 102890 
##  7 35-44        unknown             7 140280 
##  8 35-44        white             104 115258.
##  9 45-54        person of color    26 106932.
## 10 45-54        white              84 116687.
## 11 55-64        person of color     8 140424.
## 12 55-64        white              68 140052.
## 13 65+          white              13 159300
```

```r
current_median_news_age_10_race_group_hourly <- news_hourly %>% group_by(age_group_10, race_grouping)
current_median_news_age_10_race_group_hourly <- current_median_news_age_10_race_group_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_news_age_10_race_group_hourly)
```

```
## # A tibble: 8 x 4
## # Groups:   age_group_10 [5]
##   age_group_10 race_grouping   count median
##   <fct>        <chr>           <int>  <dbl>
## 1 <25          person of color     6   29.5
## 2 <25          white               7   18.5
## 3 25-34        person of color    13   29.1
## 4 25-34        white              20   31.3
## 5 35-44        person of color     5   23.9
## 6 35-44        white              12   35.3
## 7 45-54        white              11   41.4
## 8 55-64        white              11   34.9
```

```r
current_median_news_age_5_race_gender_salaried <- news_salaried %>% group_by(age_group_5, race_ethnicity, gender)
current_median_news_age_5_race_gender_salaried <- current_median_news_age_5_race_gender_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_news_age_5_race_gender_salaried)
```

```
## # A tibble: 30 x 5
## # Groups:   age_group_5, race_ethnicity [22]
##    age_group_5 race_ethnicity                           gender count median
##    <fct>       <chr>                                    <chr>  <int>  <dbl>
##  1 <25         Asian (United States of America)         Female     5 6.58e4
##  2 <25         White (United States of America)         Female     9 6.43e4
##  3 25-29       Asian (United States of America)         Female     9 7.70e4
##  4 25-29       Black or African American (United State… Female     5 8.00e4
##  5 25-29       White (United States of America)         Female    38 8.19e4
##  6 25-29       White (United States of America)         Male      21 7.68e4
##  7 30-34       Asian (United States of America)         Female     8 1.01e5
##  8 30-34       Black or African American (United State… Female     5 8.58e4
##  9 30-34       Hispanic or Latino (United States of Am… Female     6 8.06e4
## 10 30-34       White (United States of America)         Female    32 8.77e4
## # … with 20 more rows
```

```r
current_median_news_age_5_race_gender_hourly <- news_hourly %>% group_by(age_group_5, race_ethnicity, gender)
current_median_news_age_5_race_gender_hourly <- current_median_news_age_5_race_gender_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_news_age_5_race_gender_hourly)
```

```
## # A tibble: 5 x 5
## # Groups:   age_group_5, race_ethnicity [5]
##   age_group_5 race_ethnicity                   gender count median
##   <fct>       <chr>                            <chr>  <int>  <dbl>
## 1 <25         White (United States of America) Female     5   32  
## 2 25-29       White (United States of America) Female    10   31.2
## 3 30-34       White (United States of America) Male       6   34.4
## 4 45-49       White (United States of America) Female     5   44.5
## 5 55-59       White (United States of America) Male       5   34.9
```

```r
current_median_news_age_10_race_gender_salaried <- news_salaried %>% group_by(age_group_10, race_ethnicity, gender)
current_median_news_age_10_race_gender_salaried <- current_median_news_age_10_race_gender_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_news_age_10_race_gender_salaried)
```

```
## # A tibble: 23 x 5
## # Groups:   age_group_10, race_ethnicity [17]
##    age_group_10 race_ethnicity                          gender count median
##    <fct>        <chr>                                   <chr>  <int>  <dbl>
##  1 <25          Asian (United States of America)        Female     5 6.58e4
##  2 <25          White (United States of America)        Female     9 6.43e4
##  3 25-34        Asian (United States of America)        Female    17 8.70e4
##  4 25-34        Black or African American (United Stat… Female    10 8.10e4
##  5 25-34        Black or African American (United Stat… Male       5 1.40e5
##  6 25-34        Hispanic or Latino (United States of A… Female     8 8.12e4
##  7 25-34        Two or More Races (United States of Am… Female     6 7.57e4
##  8 25-34        White (United States of America)        Female    70 8.46e4
##  9 25-34        White (United States of America)        Male      55 9.08e4
## 10 25-34        <NA>                                    Male       6 1.32e5
## # … with 13 more rows
```

```r
current_median_news_age_10_race_gender_hourly <- news_hourly %>% group_by(age_group_10, race_ethnicity, gender)
current_median_news_age_10_race_gender_hourly <- current_median_news_age_10_race_gender_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_news_age_10_race_gender_hourly)
```

```
## # A tibble: 7 x 5
## # Groups:   age_group_10, race_ethnicity [5]
##   age_group_10 race_ethnicity                   gender count median
##   <fct>        <chr>                            <chr>  <int>  <dbl>
## 1 <25          White (United States of America) Female     5   32  
## 2 25-34        White (United States of America) Female    13   30.8
## 3 25-34        White (United States of America) Male       7   33.7
## 4 35-44        White (United States of America) Female     7   34.7
## 5 35-44        White (United States of America) Male       5   35.9
## 6 45-54        White (United States of America) Female     9   44.5
## 7 55-64        White (United States of America) Male       7   33.4
```

```r
current_median_news_age_5_race_group_gender_salaried <- news_salaried %>% group_by(age_group_5, race_grouping, gender)
current_median_news_age_5_race_group_gender_salaried <- current_median_news_age_5_race_group_gender_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_news_age_5_race_group_gender_salaried)
```

```
## # A tibble: 31 x 5
## # Groups:   age_group_5, race_grouping [18]
##    age_group_5 race_grouping   gender count  median
##    <fct>       <chr>           <chr>  <int>   <dbl>
##  1 <25         person of color Female    10  64390 
##  2 <25         white           Female     9  64280 
##  3 25-29       person of color Female    19  77000 
##  4 25-29       person of color Male       8  88540 
##  5 25-29       white           Female    38  81878.
##  6 25-29       white           Male      21  76780 
##  7 30-34       person of color Female    22  86373.
##  8 30-34       person of color Male       6 106000 
##  9 30-34       unknown         Male       6 120390 
## 10 30-34       white           Female    32  87660 
## # … with 21 more rows
```

```r
current_median_news_age_5_race_group_gender_hourly <- news_hourly %>% group_by(age_group_5, race_grouping, gender)
current_median_news_age_5_race_group_gender_hourly <- current_median_news_age_5_race_group_gender_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_news_age_5_race_group_gender_hourly)
```

```
## # A tibble: 8 x 5
## # Groups:   age_group_5, race_grouping [7]
##   age_group_5 race_grouping   gender count median
##   <fct>       <chr>           <chr>  <int>  <dbl>
## 1 <25         person of color Female     6   29.5
## 2 <25         white           Female     5   32  
## 3 25-29       person of color Female     7   31.2
## 4 25-29       person of color Male       5   20.9
## 5 25-29       white           Female    10   31.2
## 6 30-34       white           Male       6   34.4
## 7 45-49       white           Female     5   44.5
## 8 55-59       white           Male       5   34.9
```

```r
current_median_news_age_10_race_group_gender_salaried <- news_salaried %>% group_by(age_group_10, race_grouping, gender)
current_median_news_age_10_race_group_gender_salaried <- current_median_news_age_10_race_group_gender_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_news_age_10_race_group_gender_salaried)
```

```
## # A tibble: 20 x 5
## # Groups:   age_group_10, race_grouping [12]
##    age_group_10 race_grouping   gender count  median
##    <fct>        <chr>           <chr>  <int>   <dbl>
##  1 <25          person of color Female    10  64390 
##  2 <25          white           Female     9  64280 
##  3 25-34        person of color Female    41  82000.
##  4 25-34        person of color Male      14  89540 
##  5 25-34        unknown         Female     6  92140 
##  6 25-34        unknown         Male       8 120390 
##  7 25-34        white           Female    70  84640 
##  8 25-34        white           Male      55  90780 
##  9 35-44        person of color Female    19 100000 
## 10 35-44        person of color Male      19 113280 
## 11 35-44        white           Female    37 105000 
## 12 35-44        white           Male      67 120780 
## 13 45-54        person of color Female     7 108864.
## 14 45-54        person of color Male      19 105000 
## 15 45-54        white           Female    42 111589.
## 16 45-54        white           Male      42 123530.
## 17 55-64        person of color Female     6 142688.
## 18 55-64        white           Female    26 130924.
## 19 55-64        white           Male      42 147161.
## 20 65+          white           Male       9 159458.
```

```r
current_median_news_age_10_race_group_gender_hourly <- news_hourly %>% group_by(age_group_10, race_grouping, gender)
current_median_news_age_10_race_group_gender_hourly <- current_median_news_age_10_race_group_gender_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_news_age_10_race_group_gender_hourly)
```

```
## # A tibble: 10 x 5
## # Groups:   age_group_10, race_grouping [7]
##    age_group_10 race_grouping   gender count median
##    <fct>        <chr>           <chr>  <int>  <dbl>
##  1 <25          person of color Female     6   29.5
##  2 <25          white           Female     5   32  
##  3 25-34        person of color Female     7   31.2
##  4 25-34        person of color Male       6   21.0
##  5 25-34        white           Female    13   30.8
##  6 25-34        white           Male       7   33.7
##  7 35-44        white           Female     7   34.7
##  8 35-44        white           Male       5   35.9
##  9 45-54        white           Female     9   44.5
## 10 55-64        white           Male       7   33.4
```

### Desks


```r
current_news_median_desk_salaried <- news_salaried %>% group_by(desk)
current_news_median_desk_salaried <- current_news_median_desk_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_desk_salaried)
```

```
## # A tibble: 18 x 3
##    desk                                count  median
##    <chr>                               <int>   <dbl>
##  1 National                              106 149520.
##  2 Foreign                                25 135000 
##  3 Financial                              38 133510.
##  4 Investigative                          13 129780 
##  5 Style                                  45 107171.
##  6 Local                                  65 105780 
##  7 Editorial                              33 105000 
##  8 Graphics                               15 100780 
##  9 Universal Desk                          8 100444.
## 10 Sports                                 37 100000 
## 11 Outlook                                 6  99938.
## 12 Audio                                   7  92000 
## 13 Design                                 45  88065.
## 14 Operations                              6  87890 
## 15 Multiplatform                          26  86104 
## 16 Video                                  46  84250 
## 17 Audience Development and Engagement    16  83530 
## 18 Emerging News Products                 30  75000
```

```r
current_news_median_desk_hourly <- news_hourly %>% group_by(desk)
current_news_median_desk_hourly <- current_news_median_desk_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_desk_hourly)
```

```
## # A tibble: 10 x 3
##    desk                                count median
##    <chr>                               <int>  <dbl>
##  1 Audio                                   6   39.7
##  2 Universal Desk                          8   38.7
##  3 Audience Development and Engagement     7   37.6
##  4 Multiplatform                          16   34.1
##  5 Editorial                               5   32.3
##  6 National                               12   31.7
##  7 Local                                   5   26.5
##  8 Style                                   9   21.8
##  9 Sports                                 11   20.9
## 10 Operations                              7   15.6
```

```r
current_news_median_desk_gender_salaried <- news_salaried %>% group_by(desk, gender)
current_news_median_desk_gender_salaried <- current_news_median_desk_gender_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_desk_gender_salaried)
```

```
## # A tibble: 31 x 4
## # Groups:   desk [17]
##    desk          gender count  median
##    <chr>         <chr>  <int>   <dbl>
##  1 National      Male      57 169780 
##  2 Foreign       Male      14 145390 
##  3 Editorial     Male      18 140271.
##  4 National      Female    49 139780 
##  5 Financial     Male      25 136468.
##  6 Investigative Male       8 135030 
##  7 Foreign       Female    11 129970.
##  8 Financial     Female    13 125000 
##  9 Investigative Female     5 125000 
## 10 Local         Male      31 118850 
## # … with 21 more rows
```

```r
current_news_median_desk_gender_hourly <- news_hourly %>% group_by(desk, gender)
current_news_median_desk_gender_hourly <- current_news_median_desk_gender_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_desk_gender_hourly)
```

```
## # A tibble: 6 x 4
## # Groups:   desk [6]
##   desk           gender count median
##   <chr>          <chr>  <int>  <dbl>
## 1 Audio          Female     5   41.0
## 2 Universal Desk Female     5   35.9
## 3 Multiplatform  Female    13   34.7
## 4 Sports         Male       8   33.0
## 5 National       Female     8   32.7
## 6 Style          Female     8   26.7
```

```r
current_news_median_desk_race_salaried <- news_salaried %>% group_by(desk, race_ethnicity)
current_news_median_desk_race_salaried <- current_news_median_desk_race_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_desk_race_salaried)
```

```
## # A tibble: 23 x 4
## # Groups:   desk [16]
##    desk          race_ethnicity                                count median
##    <chr>         <chr>                                         <int>  <dbl>
##  1 National      White (United States of America)                 84 1.69e5
##  2 Investigative White (United States of America)                 10 1.40e5
##  3 National      Black or African American (United States of …     9 1.40e5
##  4 Foreign       <NA>                                             20 1.38e5
##  5 Financial     White (United States of America)                 29 1.36e5
##  6 National      Asian (United States of America)                 11 1.26e5
##  7 Editorial     White (United States of America)                 27 1.20e5
##  8 Style         White (United States of America)                 38 1.12e5
##  9 Local         White (United States of America)                 46 1.08e5
## 10 Universal De… White (United States of America)                  5 1.04e5
## # … with 13 more rows
```

```r
current_news_median_desk_race_hourly <- news_hourly %>% group_by(desk, race_ethnicity)
current_news_median_desk_race_hourly <- current_news_median_desk_race_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_desk_race_hourly)
```

```
## # A tibble: 5 x 4
## # Groups:   desk [5]
##   desk           race_ethnicity                   count median
##   <chr>          <chr>                            <int>  <dbl>
## 1 Style          White (United States of America)     5   38.9
## 2 Universal Desk White (United States of America)     6   38.7
## 3 Multiplatform  White (United States of America)    12   36.5
## 4 Sports         White (United States of America)     9   33.0
## 5 National       White (United States of America)     9   32.7
```

```r
current_news_median_desk_race_gender_salaried <- news_salaried %>% group_by(desk, race_ethnicity, gender)
current_news_median_desk_race_gender_salaried <- current_news_median_desk_race_gender_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_desk_race_gender_salaried)
```

```
## # A tibble: 30 x 5
## # Groups:   desk, race_ethnicity [18]
##    desk        race_ethnicity                           gender count median
##    <chr>       <chr>                                    <chr>  <int>  <dbl>
##  1 National    White (United States of America)         Male      46 1.75e5
##  2 Investigat… White (United States of America)         Male       6 1.49e5
##  3 Financial   White (United States of America)         Male      21 1.40e5
##  4 Editorial   White (United States of America)         Male      16 1.40e5
##  5 Foreign     <NA>                                     Male      11 1.40e5
##  6 National    White (United States of America)         Female    38 1.40e5
##  7 National    Black or African American (United State… Male       8 1.35e5
##  8 Foreign     <NA>                                     Female     9 1.35e5
##  9 National    Asian (United States of America)         Female     8 1.33e5
## 10 Sports      White (United States of America)         Female     6 1.32e5
## # … with 20 more rows
```

```r
current_news_median_desk_race_gender_hourly <- news_hourly %>% group_by(desk, race_ethnicity, gender)
current_news_median_desk_race_gender_hourly <- current_news_median_desk_race_gender_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_desk_race_gender_hourly)
```

```
## # A tibble: 4 x 5
## # Groups:   desk, race_ethnicity [4]
##   desk          race_ethnicity                   gender count median
##   <chr>         <chr>                            <chr>  <int>  <dbl>
## 1 Style         White (United States of America) Female     5   38.9
## 2 Multiplatform White (United States of America) Female     9   38.4
## 3 Sports        White (United States of America) Male       7   33.0
## 4 National      White (United States of America) Female     6   32.7
```

```r
current_news_median_desk_race_group_gender_salaried <- news_salaried %>% group_by(desk, race_grouping, gender)
current_news_median_desk_race_group_gender_salaried <- current_news_median_desk_race_group_gender_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_desk_race_group_gender_salaried)
```

```
## # A tibble: 36 x 5
## # Groups:   desk, race_grouping [21]
##    desk          race_grouping   gender count  median
##    <chr>         <chr>           <chr>  <int>   <dbl>
##  1 National      white           Male      46 175374.
##  2 Investigative white           Male       6 149422.
##  3 Financial     white           Male      21 140387.
##  4 Editorial     white           Male      16 140271.
##  5 Foreign       unknown         Male      11 140000 
##  6 National      white           Female    38 139734.
##  7 Foreign       unknown         Female     9 135000 
##  8 National      person of color Female    10 132780 
##  9 Sports        white           Female     6 132015.
## 10 National      person of color Male      11 130780 
## # … with 26 more rows
```

```r
current_news_median_desk_race_group_gender_hourly <- news_hourly %>% group_by(desk, race_grouping, gender)
current_news_median_desk_race_group_gender_hourly <- current_news_median_desk_race_group_gender_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_desk_race_group_gender_hourly)
```

```
## # A tibble: 4 x 5
## # Groups:   desk, race_grouping [4]
##   desk          race_grouping gender count median
##   <chr>         <chr>         <chr>  <int>  <dbl>
## 1 Style         white         Female     5   38.9
## 2 Multiplatform white         Female     9   38.4
## 3 Sports        white         Male       7   33.0
## 4 National      white         Female     6   32.7
```

```r
current_news_median_desk_race_gender_age5_salaried <- news_salaried %>% group_by(desk, race_ethnicity, gender, age_group_5)
current_news_median_desk_race_gender_age5_salaried <- current_news_median_desk_race_gender_age5_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_desk_race_gender_age5_salaried)
```

```
## # A tibble: 15 x 6
## # Groups:   desk, race_ethnicity, gender [8]
##    desk            race_ethnicity           gender age_group_5 count median
##    <chr>           <chr>                    <chr>  <fct>       <int>  <dbl>
##  1 National        White (United States of… Male   40-44           9 1.70e5
##  2 National        White (United States of… Male   30-34           9 1.70e5
##  3 National        White (United States of… Female 50-54           5 1.68e5
##  4 National        White (United States of… Female 55-59           6 1.63e5
##  5 National        White (United States of… Female 40-44           5 1.60e5
##  6 National        White (United States of… Male   35-39          10 1.49e5
##  7 Sports          White (United States of… Male   35-39           7 1.47e5
##  8 Financial       White (United States of… Male   35-39           5 1.45e5
##  9 Local           White (United States of… Male   55-59           6 1.28e5
## 10 Foreign         <NA>                     Male   30-34           5 1.25e5
## 11 National        White (United States of… Female 25-29           5 1.25e5
## 12 National        White (United States of… Female 35-39           6 1.09e5
## 13 Video           White (United States of… Female 30-34           5 8.80e4
## 14 Sports          White (United States of… Male   45-49           5 8.73e4
## 15 Emerging News … White (United States of… Female 25-29           7 7.00e4
```

```r
current_news_median_desk_race_gender_age5_hourly <- news_hourly %>% group_by(desk, race_ethnicity, gender, age_group_5)
current_news_median_desk_race_gender_age5_hourly <- current_news_median_desk_race_gender_age5_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_desk_race_gender_age5_hourly)
```

```
## # A tibble: 0 x 6
## # Groups:   desk, race_ethnicity, gender [0]
## # … with 6 variables: desk <chr>, race_ethnicity <chr>, gender <chr>,
## #   age_group_5 <fct>, count <int>, median <dbl>
```

```r
current_news_median_desk_race_group_gender_age5_salaried <- news_salaried %>% group_by(desk, race_grouping, gender, age_group_5)
current_news_median_desk_race_group_gender_age5_salaried <- current_news_median_desk_race_group_gender_age5_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_desk_race_group_gender_age5_salaried)
```

```
## # A tibble: 16 x 6
## # Groups:   desk, race_grouping, gender [9]
##    desk                   race_grouping   gender age_group_5 count  median
##    <chr>                  <chr>           <chr>  <fct>       <int>   <dbl>
##  1 National               white           Male   40-44           9 170000 
##  2 National               white           Male   30-34           9 169780 
##  3 National               white           Female 50-54           5 167780 
##  4 National               white           Female 55-59           6 162854.
##  5 National               white           Female 40-44           5 160000 
##  6 National               white           Male   35-39          10 148640 
##  7 Sports                 white           Male   35-39           7 147300 
##  8 Financial              white           Male   35-39           5 144755 
##  9 Local                  white           Male   55-59           6 127655.
## 10 Foreign                unknown         Male   30-34           5 125000 
## 11 National               white           Female 25-29           5 125000 
## 12 National               white           Female 35-39           6 109390 
## 13 Video                  white           Female 30-34           5  88000 
## 14 Sports                 white           Male   45-49           5  87278.
## 15 Video                  person of color Female 25-29           8  76390 
## 16 Emerging News Products white           Female 25-29           7  70000
```

```r
current_news_median_desk_race_group_gender_age5_hourly <- news_hourly %>% group_by(desk, race_grouping, gender, age_group_5)
current_news_median_desk_race_group_gender_age5_hourly <- current_news_median_desk_race_group_gender_age5_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_desk_race_group_gender_age5_hourly)
```

```
## # A tibble: 0 x 6
## # Groups:   desk, race_grouping, gender [0]
## # … with 6 variables: desk <chr>, race_grouping <chr>, gender <chr>,
## #   age_group_5 <fct>, count <int>, median <dbl>
```

```r
current_news_median_desk_tier_salaried <- news_salaried %>% group_by(tier)
current_news_median_desk_tier_salaried <- current_news_median_desk_tier_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_desk_tier_salaried)
```

```
## # A tibble: 4 x 3
##   tier   count median
##   <chr>  <int>  <dbl>
## 1 Tier 1   182 140140
## 2 Tier 2   209 105000
## 3 Tier 3   147  85780
## 4 Tier 4    36  75000
```

```r
current_news_median_desk_tier_gender_salaried <- news_salaried %>% group_by(tier, gender)
current_news_median_desk_tier_gender_salaried <- current_news_median_desk_tier_gender_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_desk_tier_gender_salaried)
```

```
## # A tibble: 8 x 4
## # Groups:   tier [4]
##   tier   gender count  median
##   <chr>  <chr>  <int>   <dbl>
## 1 Tier 1 Male     104 150975.
## 2 Tier 1 Female    78 135160.
## 3 Tier 2 Male     112 112755.
## 4 Tier 2 Female    97  99252.
## 5 Tier 3 Male      64  90660.
## 6 Tier 3 Female    83  82010.
## 7 Tier 4 Female    26  75000 
## 8 Tier 4 Male      10  74086.
```

```r
current_news_median_desk_tier_race_salaried <- news_salaried %>% group_by(tier, race_ethnicity)
current_news_median_desk_tier_race_salaried <- current_news_median_desk_tier_race_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_desk_tier_race_salaried)
```

```
## # A tibble: 14 x 4
## # Groups:   tier [4]
##    tier   race_ethnicity                                       count median
##    <chr>  <chr>                                                <int>  <dbl>
##  1 Tier 1 White (United States of America)                       126 1.58e5
##  2 Tier 1 <NA>                                                    21 1.40e5
##  3 Tier 1 Black or African American (United States of America)    12 1.35e5
##  4 Tier 1 Asian (United States of America)                        17 1.25e5
##  5 Tier 2 White (United States of America)                       159 1.07e5
##  6 Tier 2 Black or African American (United States of America)    16 1.02e5
##  7 Tier 2 Asian (United States of America)                        14 9.38e4
##  8 Tier 2 Hispanic or Latino (United States of America)           11 9.21e4
##  9 Tier 2 Two or More Races (United States of America)             6 8.91e4
## 10 Tier 3 White (United States of America)                        98 8.80e4
## 11 Tier 3 Black or African American (United States of America)    17 8.57e4
## 12 Tier 3 Hispanic or Latino (United States of America)           12 8.12e4
## 13 Tier 3 Asian (United States of America)                        13 7.70e4
## 14 Tier 4 White (United States of America)                        23 7.50e4
```

```r
current_news_median_desk_tier_race_gender_salaried <- news_salaried %>% group_by(tier, race_ethnicity, gender)
current_news_median_desk_tier_race_gender_salaried <- current_news_median_desk_tier_race_gender_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_desk_tier_race_gender_salaried)
```

```
## # A tibble: 23 x 5
## # Groups:   tier, race_ethnicity [13]
##    tier   race_ethnicity                                gender count median
##    <chr>  <chr>                                         <chr>  <int>  <dbl>
##  1 Tier 1 White (United States of America)              Male      74 1.66e5
##  2 Tier 1 <NA>                                          Male      11 1.40e5
##  3 Tier 1 <NA>                                          Female    10 1.38e5
##  4 Tier 1 White (United States of America)              Female    52 1.36e5
##  5 Tier 1 Black or African American (United States of … Male       9 1.31e5
##  6 Tier 1 Asian (United States of America)              Female    11 1.26e5
##  7 Tier 1 Asian (United States of America)              Male       6 1.22e5
##  8 Tier 2 White (United States of America)              Male      93 1.18e5
##  9 Tier 2 Hispanic or Latino (United States of America) Male       5 1.18e5
## 10 Tier 2 Black or African American (United States of … Male       7 1.16e5
## # … with 13 more rows
```

```r
current_news_median_desk_tier_race_group_gender_salaried <- news_salaried %>% group_by(tier, race_grouping, gender)
current_news_median_desk_tier_race_group_gender_salaried <- current_news_median_desk_tier_race_group_gender_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_desk_tier_race_group_gender_salaried)
```

```
## # A tibble: 17 x 5
## # Groups:   tier, race_grouping [9]
##    tier   race_grouping   gender count  median
##    <chr>  <chr>           <chr>  <int>   <dbl>
##  1 Tier 1 white           Male      74 166149.
##  2 Tier 1 unknown         Male      14 137890 
##  3 Tier 1 unknown         Female    10 137640 
##  4 Tier 1 white           Female    52 135825.
##  5 Tier 1 person of color Male      16 127890 
##  6 Tier 1 person of color Female    16 125390 
##  7 Tier 2 white           Male      93 117844.
##  8 Tier 2 person of color Male      19 105000 
##  9 Tier 2 white           Female    66 102424.
## 10 Tier 2 person of color Female    30  93020.
## 11 Tier 3 white           Male      43  92500 
## 12 Tier 3 person of color Male      19  85692.
## 13 Tier 3 white           Female    55  84780 
## 14 Tier 3 person of color Female    27  79161.
## 15 Tier 4 person of color Female    10  78500 
## 16 Tier 4 white           Male       8  75500 
## 17 Tier 4 white           Female    15  75000
```

```r
current_news_median_desk_tier_race_gender_age5_salaried <- news_salaried %>% group_by(tier, race_ethnicity, gender, age_group_5)
current_news_median_desk_tier_race_gender_age5_salaried <- current_news_median_desk_tier_race_gender_age5_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_desk_tier_race_gender_age5_salaried)
```

```
## # A tibble: 42 x 6
## # Groups:   tier, race_ethnicity, gender [9]
##    tier   race_ethnicity                   gender age_group_5 count  median
##    <chr>  <chr>                            <chr>  <fct>       <int>   <dbl>
##  1 Tier 1 White (United States of America) Male   50-54           5 180040 
##  2 Tier 1 White (United States of America) Male   60-64           6 170790.
##  3 Tier 1 White (United States of America) Male   40-44          15 166999.
##  4 Tier 1 White (United States of America) Female 45-49           5 165000 
##  5 Tier 1 White (United States of America) Female 55-59           6 162854.
##  6 Tier 1 White (United States of America) Male   55-59           9 160780 
##  7 Tier 2 White (United States of America) Female 55-59           5 149030.
##  8 Tier 2 White (United States of America) Male   65+             6 147473.
##  9 Tier 2 White (United States of America) Male   55-59          16 147161.
## 10 Tier 1 White (United States of America) Female 50-54           8 146280 
## # … with 32 more rows
```

```r
current_news_median_desk_tier_race_group_gender_age5_salaried <- news_salaried %>% group_by(tier, race_grouping, gender, age_group_5)
current_news_median_desk_tier_race_group_gender_age5_salaried <- current_news_median_desk_tier_race_group_gender_age5_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_desk_tier_race_group_gender_age5_salaried)
```

```
## # A tibble: 49 x 6
## # Groups:   tier, race_grouping, gender [14]
##    tier   race_grouping gender age_group_5 count  median
##    <chr>  <chr>         <chr>  <fct>       <int>   <dbl>
##  1 Tier 1 white         Male   50-54           5 180040 
##  2 Tier 1 white         Male   60-64           6 170790.
##  3 Tier 1 white         Male   40-44          15 166999.
##  4 Tier 1 white         Female 45-49           5 165000 
##  5 Tier 1 white         Female 55-59           6 162854.
##  6 Tier 1 white         Male   55-59           9 160780 
##  7 Tier 2 white         Female 55-59           5 149030.
##  8 Tier 2 white         Male   65+             6 147473.
##  9 Tier 2 white         Male   55-59          16 147161.
## 10 Tier 1 white         Female 50-54           8 146280 
## # … with 39 more rows
```

### Job profiles


```r
current_news_median_job_salaried <- news_salaried %>% group_by(job_profile_current)
current_news_median_job_salaried <- current_news_median_job_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_job_salaried)
```

```
## # A tibble: 18 x 3
##    job_profile_current            count  median
##    <chr>                          <int>   <dbl>
##  1 300113 - Columnist                19 170497.
##  2 300313 - Columnist - Editorial     7 151896.
##  3 320113 - Critic                    9 150962.
##  4 330113 - Editorial Writer          7 129236.
##  5 280212 - Staff Writer            306 124040 
##  6 390510 - Graphics Editor           7 111071 
##  7 360114 - Photographer             16 106015.
##  8 126902 - Topic Editor              6 103772.
##  9 390610 - Graphics Reporter         8  97280 
## 10 120602 - Operations Editor         7  90780 
## 11 280226 - Video Journalist         20  89240 
## 12 390310 - Video Graphics Editor     8  87280 
## 13 120202 - Assistant Editor         23  87000 
## 14 390110 - Multiplatform Editor     53  83147.
## 15 280228 - Designer                 29  76000 
## 16 126202 - Photo Editor              8  74962.
## 17 390410 - Digital Video Editor     22  74500 
## 18 289711 - News Intern - 2 Year      5  65780
```

```r
current_news_median_job_hourly <- news_hourly %>% group_by(job_profile_current)
current_news_median_job_hourly <- current_news_median_job_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_job_hourly)
```

```
## # A tibble: 7 x 3
##   job_profile_current                     count median
##   <chr>                                   <int>  <dbl>
## 1 280225 - Producer                          18   36.7
## 2 400151 - Administrative Aide                6   35.3
## 3 397110 - Multiplatform Editor (PT/PTOC)    23   34.7
## 4 380117 - Research Assistant                 6   31.2
## 5 410251 - Editorial Aide                    12   21.4
## 6 430117 - News Aide                          8   17.1
## 7 440116 - Copy Aide                          5   15.2
```

```r
current_news_median_job_gender_salaried <- news_salaried %>% group_by(job_profile_current, gender)
current_news_median_job_gender_salaried <- current_news_median_job_gender_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_job_gender_salaried)
```

```
## # A tibble: 23 x 4
## # Groups:   job_profile_current [15]
##    job_profile_current            gender count  median
##    <chr>                          <chr>  <int>   <dbl>
##  1 300113 - Columnist             Male       8 175984.
##  2 330113 - Editorial Writer      Male       5 164900.
##  3 320113 - Critic                Male       5 160780 
##  4 300113 - Columnist             Female    11 154780 
##  5 300313 - Columnist - Editorial Male       5 151896.
##  6 280212 - Staff Writer          Male     170 128440.
##  7 280212 - Staff Writer          Female   136 113474.
##  8 390510 - Graphics Editor       Male       5 111071 
##  9 360114 - Photographer          Male      11 109928.
## 10 280226 - Video Journalist      Male       8  98555 
## # … with 13 more rows
```

```r
current_news_median_job_gender_hourly <- news_hourly %>% group_by(job_profile_current, gender)
current_news_median_job_gender_hourly <- current_news_median_job_gender_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_job_gender_hourly)
```

```
## # A tibble: 7 x 4
## # Groups:   job_profile_current [5]
##   job_profile_current                     gender count median
##   <chr>                                   <chr>  <int>  <dbl>
## 1 280225 - Producer                       Male       6   36.7
## 2 397110 - Multiplatform Editor (PT/PTOC) Female    14   36.5
## 3 280225 - Producer                       Female    12   36.4
## 4 400151 - Administrative Aide            Female     6   35.3
## 5 397110 - Multiplatform Editor (PT/PTOC) Male       9   33.4
## 6 380117 - Research Assistant             Female     5   31.7
## 7 410251 - Editorial Aide                 Female     8   21.4
```

```r
current_news_median_job_race_salaried <- news_salaried %>% group_by(job_profile_current, race_ethnicity)
current_news_median_job_race_salaried <- current_news_median_job_race_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_job_race_salaried)
```

```
## # A tibble: 21 x 4
## # Groups:   job_profile_current [14]
##    job_profile_current      race_ethnicity                     count median
##    <chr>                    <chr>                              <int>  <dbl>
##  1 300313 - Columnist - Ed… White (United States of America)       6 1.91e5
##  2 300113 - Columnist       White (United States of America)      13 1.77e5
##  3 300113 - Columnist       Black or African American (United…     5 1.53e5
##  4 320113 - Critic          White (United States of America)       8 1.49e5
##  5 280212 - Staff Writer    <NA>                                  21 1.40e5
##  6 330113 - Editorial Writ… White (United States of America)       6 1.27e5
##  7 280212 - Staff Writer    White (United States of America)     223 1.25e5
##  8 280212 - Staff Writer    Black or African American (United…    18 1.22e5
##  9 280212 - Staff Writer    Asian (United States of America)      24 1.17e5
## 10 390510 - Graphics Editor White (United States of America)       5 1.11e5
## # … with 11 more rows
```

```r
current_news_median_job_race_hourly <- news_hourly %>% group_by(job_profile_current, race_ethnicity)
current_news_median_job_race_hourly <- current_news_median_job_race_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_job_race_hourly)
```

```
## # A tibble: 6 x 4
## # Groups:   job_profile_current [5]
##   job_profile_current         race_ethnicity                   count median
##   <chr>                       <chr>                            <int>  <dbl>
## 1 280225 - Producer           Black or African American (Unit…     5   37.6
## 2 280225 - Producer           White (United States of America)     8   35.9
## 3 397110 - Multiplatform Edi… White (United States of America)    18   34.8
## 4 380117 - Research Assistant White (United States of America)     5   31.7
## 5 410251 - Editorial Aide     White (United States of America)     7   21.1
## 6 430117 - News Aide          White (United States of America)     5   16.5
```

```r
current_news_median_job_race_gender_salaried <- news_salaried %>% group_by(job_profile_current, race_ethnicity, gender)
current_news_median_job_race_gender_salaried <- current_news_median_job_race_gender_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_job_race_gender_salaried)
```

```
## # A tibble: 23 x 5
## # Groups:   job_profile_current, race_ethnicity [13]
##    job_profile_current race_ethnicity                   gender count median
##    <chr>               <chr>                            <chr>  <int>  <dbl>
##  1 300113 - Columnist  White (United States of America) Female     7 2.24e5
##  2 300113 - Columnist  White (United States of America) Male       6 1.76e5
##  3 320113 - Critic     White (United States of America) Male       5 1.61e5
##  4 280212 - Staff Wri… <NA>                             Male      11 1.40e5
##  5 280212 - Staff Wri… <NA>                             Female    10 1.38e5
##  6 280212 - Staff Wri… White (United States of America) Male     130 1.29e5
##  7 280212 - Staff Wri… Black or African American (Unit… Male      13 1.25e5
##  8 280212 - Staff Wri… Asian (United States of America) Male       9 1.19e5
##  9 280212 - Staff Wri… Asian (United States of America) Female    15 1.15e5
## 10 280212 - Staff Wri… White (United States of America) Female    93 1.15e5
## # … with 13 more rows
```

```r
current_news_median_job_race_gender_hourly <- news_hourly %>% group_by(job_profile_current, race_ethnicity, gender)
current_news_median_job_race_gender_hourly <- current_news_median_job_race_gender_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_job_race_gender_hourly)
```

```
## # A tibble: 4 x 5
## # Groups:   job_profile_current, race_ethnicity [3]
##   job_profile_current           race_ethnicity          gender count median
##   <chr>                         <chr>                   <chr>  <int>  <dbl>
## 1 397110 - Multiplatform Edito… White (United States o… Female    10   39.9
## 2 280225 - Producer             White (United States o… Female     5   34.2
## 3 397110 - Multiplatform Edito… White (United States o… Male       8   33.4
## 4 410251 - Editorial Aide       White (United States o… Female     5   21.1
```

```r
current_news_median_job_race_group_gender_salaried <- news_salaried %>% group_by(job_profile_current, race_grouping, gender)
current_news_median_job_race_group_gender_salaried <- current_news_median_job_race_group_gender_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_job_race_group_gender_salaried)
```

```
## # A tibble: 27 x 5
## # Groups:   job_profile_current, race_grouping [16]
##    job_profile_current       race_grouping   gender count  median
##    <chr>                     <chr>           <chr>  <int>   <dbl>
##  1 300113 - Columnist        white           Female     7 224461.
##  2 300113 - Columnist        white           Male       6 175984.
##  3 320113 - Critic           white           Male       5 160780 
##  4 280212 - Staff Writer     unknown         Male      14 137890 
##  5 280212 - Staff Writer     unknown         Female    11 135000 
##  6 280212 - Staff Writer     white           Male     130 129280 
##  7 280212 - Staff Writer     person of color Male      26 124540 
##  8 280212 - Staff Writer     white           Female    93 115000 
##  9 360114 - Photographer     white           Male       7 113757.
## 10 280226 - Video Journalist white           Male       6 106500 
## # … with 17 more rows
```

```r
current_news_median_job_race_group_gender_hourly <- news_hourly %>% group_by(job_profile_current, race_grouping, gender)
current_news_median_job_race_group_gender_hourly <- current_news_median_job_race_group_gender_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_job_race_group_gender_hourly)
```

```
## # A tibble: 5 x 5
## # Groups:   job_profile_current, race_grouping [4]
##   job_profile_current                    race_grouping  gender count median
##   <chr>                                  <chr>          <chr>  <int>  <dbl>
## 1 397110 - Multiplatform Editor (PT/PTO… white          Female    10   39.9
## 2 280225 - Producer                      person of col… Female     6   35.9
## 3 280225 - Producer                      white          Female     5   34.2
## 4 397110 - Multiplatform Editor (PT/PTO… white          Male       8   33.4
## 5 410251 - Editorial Aide                white          Female     5   21.1
```

```r
current_news_median_job_race_gender_age5_salaried <- news_salaried %>% group_by(job_profile_current, race_ethnicity, gender, age_group_5)
current_news_median_job_race_gender_age5_salaried <- current_news_median_job_race_gender_age5_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_job_race_gender_age5_salaried)
```

```
## # A tibble: 25 x 6
## # Groups:   job_profile_current, race_ethnicity, gender [7]
##    job_profile_curre… race_ethnicity        gender age_group_5 count median
##    <chr>              <chr>                 <chr>  <fct>       <int>  <dbl>
##  1 280212 - Staff Wr… White (United States… Male   65+             5 1.59e5
##  2 280212 - Staff Wr… White (United States… Male   55-59          17 1.54e5
##  3 280212 - Staff Wr… White (United States… Female 55-59           7 1.54e5
##  4 280212 - Staff Wr… White (United States… Female 45-49          10 1.45e5
##  5 280212 - Staff Wr… White (United States… Female 40-44           9 1.40e5
##  6 280212 - Staff Wr… White (United States… Male   60-64          11 1.35e5
##  7 280212 - Staff Wr… White (United States… Male   40-44          20 1.33e5
##  8 280212 - Staff Wr… White (United States… Male   50-54          14 1.32e5
##  9 280212 - Staff Wr… White (United States… Male   45-49           9 1.31e5
## 10 280212 - Staff Wr… White (United States… Female 60-64           6 1.28e5
## # … with 15 more rows
```

```r
current_news_median_job_race_gender_age5_hourly <- news_hourly %>% group_by(job_profile_current, race_ethnicity, gender, age_group_5)
current_news_median_job_race_gender_age5_hourly <- current_news_median_job_race_gender_age5_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_job_race_gender_age5_hourly)
```

```
## # A tibble: 0 x 6
## # Groups:   job_profile_current, race_ethnicity, gender [0]
## # … with 6 variables: job_profile_current <chr>, race_ethnicity <chr>,
## #   gender <chr>, age_group_5 <fct>, count <int>, median <dbl>
```

```r
current_news_median_job_race_group_gender_age5_salaried <- news_salaried %>% group_by(job_profile_current, race_grouping, gender, age_group_5)
current_news_median_job_race_group_gender_age5_salaried <- current_news_median_job_race_group_gender_age5_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_job_race_group_gender_age5_salaried)
```

```
## # A tibble: 29 x 6
## # Groups:   job_profile_current, race_grouping, gender [9]
##    job_profile_current   race_grouping gender age_group_5 count  median
##    <chr>                 <chr>         <chr>  <fct>       <int>   <dbl>
##  1 280212 - Staff Writer white         Male   65+             5 159458.
##  2 280212 - Staff Writer white         Male   55-59          17 153923.
##  3 280212 - Staff Writer white         Female 55-59           7 153780 
##  4 280212 - Staff Writer white         Female 45-49          10 144560.
##  5 280212 - Staff Writer white         Female 40-44           9 140000 
##  6 280212 - Staff Writer white         Male   60-64          11 134957.
##  7 280212 - Staff Writer white         Male   40-44          20 132980.
##  8 280212 - Staff Writer white         Male   50-54          14 132273.
##  9 280212 - Staff Writer white         Male   45-49           9 130845 
## 10 280212 - Staff Writer white         Female 60-64           6 128441.
## # … with 19 more rows
```

```r
current_news_median_job_race_group_gender_age5_hourly <- news_hourly %>% group_by(job_profile_current, race_grouping, gender, age_group_5)
current_news_median_job_race_group_gender_age5_hourly <- current_news_median_job_race_group_gender_age5_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_news_median_job_race_group_gender_age5_hourly)
```

```
## # A tibble: 0 x 6
## # Groups:   job_profile_current, race_grouping, gender [0]
## # … with 6 variables: job_profile_current <chr>, race_grouping <chr>,
## #   gender <chr>, age_group_5 <fct>, count <int>, median <dbl>
```

### Performance evaluations


```r
news_ratings <- filter(ratings_combined, dept == 'News')

news_ratings_gender <- news_ratings %>% group_by(gender)
news_ratings_gender <- news_ratings_gender %>% summarise(
  count = length(performance_rating),
  median = median(performance_rating)
)
suppress_median(news_ratings_gender)
```

```
## # A tibble: 2 x 3
##   gender count median
##   <chr>  <int>  <dbl>
## 1 Female  1892     NA
## 2 Male    1772     NA
```

```r
news_ratings_race <- news_ratings %>% group_by(race_ethnicity)
news_ratings_race <- news_ratings_race %>% summarise(
  count = length(performance_rating),
  median = median(performance_rating, na.rm = TRUE)
)
suppress_median(news_ratings_race)
```

```
## # A tibble: 9 x 3
##   race_ethnicity                                               count median
##   <chr>                                                        <int>  <dbl>
## 1 American Indian or Alaska Native (United States of America)     12    3.6
## 2 <NA>                                                            88    3.6
## 3 White (United States of America)                              2516    3.5
## 4 Asian (United States of America)                               324    3.4
## 5 Prefer Not to Disclose (United States of America)               56    3.4
## 6 Black or African American (United States of America)           416    3.3
## 7 Hispanic or Latino (United States of America)                  164    3.3
## 8 Native Hawaiian or Other Pacific Islander (United States of…     8    3.3
## 9 Two or More Races (United States of America)                    80    3.2
```

```r
news_ratings_race_gender <- news_ratings %>% group_by(race_ethnicity, gender)
news_ratings_race_gender <- news_ratings_race_gender %>% summarise(
  count = length(performance_rating),
  median = median(performance_rating, na.rm = TRUE)
)
suppress(news_ratings_race_gender)
```

```
## # A tibble: 16 x 4
## # Groups:   race_ethnicity [9]
##    race_ethnicity                                       gender count median
##    <chr>                                                <chr>  <int>  <dbl>
##  1 American Indian or Alaska Native (United States of … Female     8   3.7 
##  2 Asian (United States of America)                     Female   232   3.4 
##  3 Asian (United States of America)                     Male      92   3.4 
##  4 Black or African American (United States of America) Female   224   3.25
##  5 Black or African American (United States of America) Male     192   3.3 
##  6 Hispanic or Latino (United States of America)        Female    80   3.3 
##  7 Hispanic or Latino (United States of America)        Male      84   3.3 
##  8 Native Hawaiian or Other Pacific Islander (United S… Male       8   3.3 
##  9 Prefer Not to Disclose (United States of America)    Female    24   3.5 
## 10 Prefer Not to Disclose (United States of America)    Male      32   3.3 
## 11 Two or More Races (United States of America)         Female    52   3.2 
## 12 Two or More Races (United States of America)         Male      28   3.2 
## 13 White (United States of America)                     Female  1228   3.4 
## 14 White (United States of America)                     Male    1288   3.5 
## 15 <NA>                                                 Female    44   3.7 
## 16 <NA>                                                 Male      44   3.55
```

```r
news_ratings_race_gender_under3 <- filter(news_ratings, performance_rating < 3.1) %>% group_by(race_grouping, gender)
news_ratings_race_gender_under3 <- news_ratings_race_gender_under3 %>% summarise(
  count = length(performance_rating),
  median = median(performance_rating, na.rm = TRUE)
)
suppress(news_ratings_race_gender_under3)
```

```
## # A tibble: 4 x 4
## # Groups:   race_grouping [2]
##   race_grouping   gender count median
##   <chr>           <chr>  <int>  <dbl>
## 1 person of color Female    57      3
## 2 person of color Male      49      3
## 3 white           Female    92      3
## 4 white           Male      80      3
```

```r
news_ratings_race_gender_over4 <- filter(news_ratings, performance_rating > 3.9) %>% group_by(race_grouping, gender)
news_ratings_race_gender_over4 <- news_ratings_race_gender_over4 %>% summarise(
  count = length(performance_rating),
  median = median(performance_rating, na.rm = TRUE)
)
suppress(news_ratings_race_gender_over4)
```

```
## # A tibble: 6 x 4
## # Groups:   race_grouping [3]
##   race_grouping   gender count median
##   <chr>           <chr>  <int>  <dbl>
## 1 person of color Female    13   4.1 
## 2 person of color Male       5   4.1 
## 3 unknown         Female     5   4.1 
## 4 unknown         Male      10   4.05
## 5 white           Female    67   4.1 
## 6 white           Male     114   4.2
```

### Pay changes


```r
news_change <- filter(reason_for_change_combined, dept == 'News')

news_change_gender <- news_change %>% group_by(business_process_reason, gender)
news_change_gender %>% summarise(
  count = length(business_process_reason)
)
```

```
## # A tibble: 37 x 3
## # Groups:   business_process_reason [19]
##    business_process_reason                        gender count
##    <chr>                                          <chr>  <int>
##  1 Data Change > Data Change > Change Job Details Female   282
##  2 Data Change > Data Change > Change Job Details Male     245
##  3 Hire Employee > New Hire > Conversion          Female     1
##  4 Hire Employee > New Hire > Conversion          Male       1
##  5 Hire Employee > New Hire > Convert Contingent  Female     4
##  6 Hire Employee > New Hire > Convert Contingent  Male       1
##  7 Hire Employee > New Hire > Fill Vacancy        Female    70
##  8 Hire Employee > New Hire > Fill Vacancy        Male      55
##  9 Hire Employee > New Hire > New Position        Female    78
## 10 Hire Employee > New Hire > New Position        Male      58
## # … with 27 more rows
```

```r
news_change_race <- news_change %>% group_by(business_process_reason, race_ethnicity)
news_change_race <- news_change_race %>% summarise(
  count = length(business_process_reason)
)
suppress_count(news_change_race)
```

```
## # A tibble: 70 x 3
## # Groups:   business_process_reason [14]
##    business_process_reason             race_ethnicity                 count
##    <chr>                               <chr>                          <int>
##  1 <NA>                                White (United States of Ameri…  7232
##  2 <NA>                                Black or African American (Un…  1167
##  3 Request Compensation Change > Adju… White (United States of Ameri…  1164
##  4 <NA>                                Asian (United States of Ameri…   918
##  5 Merit > Performance > Annual Perfo… White (United States of Ameri…   889
##  6 <NA>                                Hispanic or Latino (United St…   484
##  7 Data Change > Data Change > Change… White (United States of Ameri…   345
##  8 <NA>                                Two or More Races (United Sta…   274
##  9 <NA>                                <NA>                             207
## 10 Transfer > Transfer > Move to anot… White (United States of Ameri…   201
## # … with 60 more rows
```

```r
news_change_race_gender <- news_change %>% group_by(business_process_reason, race_ethnicity, gender)
news_change_race_gender <- news_change_race_gender %>% summarise(
  count = length(business_process_reason)
)
suppress_count(news_change_race_gender)
```

```
## # A tibble: 107 x 4
## # Groups:   business_process_reason, race_ethnicity [62]
##    business_process_reason          race_ethnicity             gender count
##    <chr>                            <chr>                      <chr>  <int>
##  1 <NA>                             White (United States of A… Male    3680
##  2 <NA>                             White (United States of A… Female  3552
##  3 <NA>                             Asian (United States of A… Female   702
##  4 <NA>                             Black or African American… Female   612
##  5 Request Compensation Change > A… White (United States of A… Male     606
##  6 Request Compensation Change > A… White (United States of A… Female   558
##  7 <NA>                             Black or African American… Male     555
##  8 Merit > Performance > Annual Pe… White (United States of A… Male     476
##  9 Merit > Performance > Annual Pe… White (United States of A… Female   413
## 10 <NA>                             Hispanic or Latino (Unite… Female   250
## # … with 97 more rows
```

### Performance evaluations x merit raises


```r
reason_for_change_combined <- reason_for_change_combined %>% mutate(merit_raises = grepl('*Merit*', business_process_reason))
twenty14 = as.Date('2016-04-01')
twenty15 = as.Date('2017-04-01')
twenty16 = as.Date('2018-04-01')
twenty17 = as.Date('2019-04-01')
twenty18 = as.Date('2020-04-01')
  
reason_for_change_combined <- reason_for_change_combined %>%
    mutate(raise_after=case_when(
    effective_date < twenty14 ~ 'before 2015',
    effective_date < twenty15 ~ '2015',
    effective_date < twenty16 ~ '2016',
    effective_date < twenty17 ~ '2017',
    effective_date < twenty18 ~ '2018',
    TRUE ~ 'Other'))

merit_raises_news_gender_salaried <- filter(reason_for_change_combined, merit_raises == TRUE, dept == 'News', pay_rate_type == 'Salaried') %>% group_by(gender)
merit_raises_news_gender_salaried <- merit_raises_news_gender_salaried %>% summarise(
  count = length(base_pay_change),
  median = median(base_pay_change, na.rm = TRUE)
)
suppress(merit_raises_news_gender_salaried)
```

```
## # A tibble: 2 x 3
##   gender count median
##   <chr>  <int>  <dbl>
## 1 Female   431   3000
## 2 Male     494   3000
```

```r
merit_raises_news_gender_hourly <- filter(reason_for_change_combined, merit_raises == TRUE, dept == 'News', pay_rate_type == 'Hourly') %>% group_by(gender)
merit_raises_news_gender_hourly <- merit_raises_news_gender_hourly %>% summarise(
  count = length(base_pay_change),
  median = median(base_pay_change, na.rm = TRUE)
)
suppress(merit_raises_news_gender_hourly)
```

```
## # A tibble: 2 x 3
##   gender count median
##   <chr>  <int>  <dbl>
## 1 Female    78   1.27
## 2 Male      51   1.03
```

```r
merit_raises_news_race_salaried <- filter(reason_for_change_combined, merit_raises == TRUE, dept == 'News', pay_rate_type == 'Salaried') %>% group_by(race_ethnicity)
merit_raises_news_race_salaried <- merit_raises_news_race_salaried %>% summarise(
  count = length(base_pay_change),
  median = median(base_pay_change, na.rm = TRUE)
)
suppress_median(merit_raises_news_race_salaried)
```

```
## # A tibble: 7 x 3
##   race_ethnicity                                              count median
##   <chr>                                                       <int>  <dbl>
## 1 American Indian or Alaska Native (United States of America)     5   3500
## 2 Two or More Races (United States of America)                    7   3500
## 3 <NA>                                                           14   3500
## 4 Asian (United States of America)                               69   3000
## 5 Black or African American (United States of America)           82   3000
## 6 White (United States of America)                              707   3000
## 7 Hispanic or Latino (United States of America)                  36   2500
```

```r
merit_raises_news_race_hourly <- filter(reason_for_change_combined, merit_raises == TRUE, dept == 'News', pay_rate_type == 'Hourly') %>% group_by(race_ethnicity)
merit_raises_news_race_hourly <- merit_raises_news_race_hourly %>% summarise(
  count = length(base_pay_change),
  median = median(base_pay_change, na.rm = TRUE)
)
suppress_median(merit_raises_news_race_hourly)
```

```
## # A tibble: 3 x 3
##   race_ethnicity                                       count median
##   <chr>                                                <int>  <dbl>
## 1 White (United States of America)                        91   1.28
## 2 Black or African American (United States of America)    16   1.25
## 3 Asian (United States of America)                        18   1.03
```

```r
merit_raises_news_race_group_salaried <- filter(reason_for_change_combined, merit_raises == TRUE, dept == 'News', pay_rate_type == 'Salaried') %>% group_by(race_grouping)
merit_raises_news_race_group_salaried <- merit_raises_news_race_group_salaried %>% summarise(
  count = length(base_pay_change),
  median = median(base_pay_change, na.rm = TRUE)
)
suppress_median(merit_raises_news_race_group_salaried)
```

```
## # A tibble: 3 x 3
##   race_grouping   count median
##   <chr>           <int>  <dbl>
## 1 person of color   200   3000
## 2 white             707   3000
## 3 unknown            18   2860
```

```r
merit_raises_news_race_group_hourly <- filter(reason_for_change_combined, merit_raises == TRUE, dept == 'News', pay_rate_type == 'Hourly') %>% group_by(race_grouping)
merit_raises_news_race_group_hourly <- merit_raises_news_race_group_hourly %>% summarise(
  count = length(base_pay_change),
  median = median(base_pay_change, na.rm = TRUE)
)
suppress_median(merit_raises_news_race_group_hourly)
```

```
## # A tibble: 2 x 3
##   race_grouping   count median
##   <chr>           <int>  <dbl>
## 1 white              91   1.28
## 2 person of color    38   1.03
```

```r
merit_raises_news_gender_race_group_salaried <- filter(reason_for_change_combined, merit_raises == TRUE, dept == 'News', pay_rate_type == 'Salaried') %>% group_by(race_grouping, gender)
merit_raises_news_gender_race_group_salaried <- merit_raises_news_gender_race_group_salaried %>% summarise(
  count = length(base_pay_change),
  median = median(base_pay_change, na.rm = TRUE)
)
suppress_median(merit_raises_news_gender_race_group_salaried)
```

```
## # A tibble: 6 x 4
## # Groups:   race_grouping [3]
##   race_grouping   gender count median
##   <chr>           <chr>  <int>  <dbl>
## 1 unknown         Female    10  3500 
## 2 person of color Female   112  3000 
## 3 white           Female   309  3000 
## 4 white           Male     398  3000 
## 5 person of color Male      88  2900 
## 6 unknown         Male       8  2458.
```

```r
merit_raises_news_gender_race_group_hourly <- filter(reason_for_change_combined, merit_raises == TRUE, dept == 'News', pay_rate_type == 'Hourly') %>% group_by(race_grouping, gender)
merit_raises_news_gender_race_group_hourly <- merit_raises_news_gender_race_group_hourly %>% summarise(
  count = length(base_pay_change),
  median = median(base_pay_change, na.rm = TRUE)
)
suppress_median(merit_raises_news_gender_race_group_hourly)
```

```
## # A tibble: 4 x 4
## # Groups:   race_grouping [2]
##   race_grouping   gender count median
##   <chr>           <chr>  <int>  <dbl>
## 1 white           Female    59   1.28
## 2 person of color Female    19   1.26
## 3 person of color Male      19   1.03
## 4 white           Male      32   1.02
```

```r
fifteen_raises_amount <- filter(reason_for_change_combined, merit_raises == TRUE, dept == 'News', pay_rate_type == 'Salaried', raise_after == '2015') %>% group_by(race_grouping, gender)
fifteen_raises_amount <- fifteen_raises_amount %>% summarise(
  count = length(base_pay_change),
  median_raise = median(base_pay_change, na.rm = TRUE)
)
suppress(fifteen_raises_amount)
```

```
## # A tibble: 4 x 4
## # Groups:   race_grouping [2]
##   race_grouping   gender count median_raise
##   <chr>           <chr>  <int>        <dbl>
## 1 person of color Female    17        2888 
## 2 person of color Male      10        2162.
## 3 white           Female    44        2500 
## 4 white           Male      64        3000
```

```r
fifteen_raises_score <- filter(reason_for_change_combined, merit_raises == TRUE, dept == 'News', pay_rate_type == 'Salaried', raise_after == '2015') %>% group_by(race_grouping, gender)
fifteen_raises_score <- fifteen_raises_score %>% summarise(
  count = length('2015_annual_performance_rating'),
  median = median('2015_annual_performance_rating', na.rm = TRUE)
)
suppress(fifteen_raises_score)
```

```
## # A tibble: 0 x 4
## # Groups:   race_grouping [0]
## # … with 4 variables: race_grouping <chr>, gender <chr>, count <int>,
## #   median <chr>
```

```r
sixteen_raises_amount <- filter(reason_for_change_combined, merit_raises == TRUE, dept == 'News', pay_rate_type == 'Salaried', raise_after == '2016') %>% group_by(race_grouping, gender)
sixteen_raises_amount <- sixteen_raises_amount %>% summarise(
  count = length(base_pay_change),
  median_raise = median(base_pay_change, na.rm = TRUE)
)
suppress(sixteen_raises_amount)
```

```
## # A tibble: 4 x 4
## # Groups:   race_grouping [2]
##   race_grouping   gender count median_raise
##   <chr>           <chr>  <int>        <dbl>
## 1 person of color Female    26         3000
## 2 person of color Male      17         3000
## 3 white           Female    60         3000
## 4 white           Male      81         3000
```

```r
sixteen_raises_score <- filter(reason_for_change_combined, merit_raises == TRUE, dept == 'News', pay_rate_type == 'Salaried', raise_after == '2016') %>% group_by(race_grouping, gender)
sixteen_raises_score <- sixteen_raises_score %>% summarise(
  count = length('2016_annual_performance_rating'),
  median = median('2016_annual_performance_rating', na.rm = TRUE)
)
suppress(sixteen_raises_score)
```

```
## # A tibble: 0 x 4
## # Groups:   race_grouping [0]
## # … with 4 variables: race_grouping <chr>, gender <chr>, count <int>,
## #   median <chr>
```

```r
seventeen_raises_amount <- filter(reason_for_change_combined, merit_raises == TRUE, dept == 'News', pay_rate_type == 'Salaried', raise_after == '2017') %>% group_by(race_grouping, gender)
seventeen_raises_amount <- seventeen_raises_amount %>% summarise(
  count = length(base_pay_change),
  median_raise = median(base_pay_change, na.rm = TRUE)
)
suppress(seventeen_raises_amount)
```

```
## # A tibble: 4 x 4
## # Groups:   race_grouping [2]
##   race_grouping   gender count median_raise
##   <chr>           <chr>  <int>        <dbl>
## 1 person of color Female    25         3000
## 2 person of color Male      25         3000
## 3 white           Female    59         2500
## 4 white           Male      89         3000
```

```r
seventeen_raises_score <- filter(reason_for_change_combined, merit_raises == TRUE, dept == 'News', pay_rate_type == 'Salaried', raise_after == '2017') %>% group_by(race_grouping, gender)
seventeen_raises_score <- seventeen_raises_score %>% summarise(
  count = length('2017_annual_performance_rating'),
  median = median('2017_annual_performance_rating', na.rm = TRUE)
)
suppress(seventeen_raises_score)
```

```
## # A tibble: 0 x 4
## # Groups:   race_grouping [0]
## # … with 4 variables: race_grouping <chr>, gender <chr>, count <int>,
## #   median <chr>
```

```r
eighteen_raises_amount <- filter(reason_for_change_combined, merit_raises == TRUE, dept == 'News', pay_rate_type == 'Salaried', raise_after == '2018') %>% group_by(race_grouping, gender)
eighteen_raises_amount <- eighteen_raises_amount %>% summarise(
  count = length(base_pay_change),
  median_raise = median(base_pay_change, na.rm = TRUE)
)
suppress(eighteen_raises_amount)
```

```
## # A tibble: 4 x 4
## # Groups:   race_grouping [2]
##   race_grouping   gender count median_raise
##   <chr>           <chr>  <int>        <dbl>
## 1 person of color Female    28         3000
## 2 person of color Male      26         2500
## 3 white           Female   104         3000
## 4 white           Male     120         3000
```

```r
eighteen_raises_score <- filter(reason_for_change_combined, merit_raises == TRUE, dept == 'News', pay_rate_type == 'Salaried', raise_after == '2018') %>% group_by(race_grouping, gender)
eighteen_raises_score <- eighteen_raises_score %>% summarise(
  count = length('2018_annual_performance_rating'),
  median = median('2018_annual_performance_rating', na.rm = TRUE)
)
suppress(eighteen_raises_score)
```

```
## # A tibble: 0 x 4
## # Groups:   race_grouping [0]
## # … with 4 variables: race_grouping <chr>, gender <chr>, count <int>,
## #   median <chr>
```

```r
merit_raises_15 <- filter(reason_for_change_combined, raise_after == '2015', merit_raises == TRUE)
merit_raises_16 <- filter(reason_for_change_combined, raise_after == '2016', merit_raises == TRUE)
merit_raises_17 <- filter(reason_for_change_combined, raise_after == '2017', merit_raises == TRUE)
merit_raises_18 <- filter(reason_for_change_combined, raise_after == '2018', merit_raises == TRUE)

merit_raises_15 <- merit_raises_15[,c('base_pay_change','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating')]
merit_raises_16 <- merit_raises_16[,c('base_pay_change','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2016_annual_performance_rating')]
merit_raises_17 <- merit_raises_17[,c('base_pay_change','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2017_annual_performance_rating')]
merit_raises_18 <- merit_raises_18[,c('base_pay_change','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2018_annual_performance_rating')]

names(merit_raises_15) <- c('base_pay_change','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','performance_rating')
names(merit_raises_16) <- c('base_pay_change','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','performance_rating')
names(merit_raises_17) <- c('base_pay_change','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','performance_rating')
names(merit_raises_18) <- c('base_pay_change','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','performance_rating')

merit_raises_combined <- rbind(merit_raises_15, merit_raises_16, merit_raises_17, merit_raises_18)

news_salaried_raises <- filter(merit_raises_combined, pay_rate_type == 'Salaried', dept == 'News') %>% group_by(race_grouping, gender)
news_salaried_raises <- news_salaried_raises %>% summarise(
  count = length(base_pay_change),
  median = median(base_pay_change, na.rm = TRUE)
)
suppress(news_salaried_raises)
```

```
## # A tibble: 6 x 4
## # Groups:   race_grouping [3]
##   race_grouping   gender count median
##   <chr>           <chr>  <int>  <dbl>
## 1 person of color Female    96  3000 
## 2 person of color Male      78  2659.
## 3 unknown         Female     9  3000 
## 4 unknown         Male       7  2500 
## 5 white           Female   267  3000 
## 6 white           Male     354  3000
```

```r
news_salaried_raises_scores <- filter(merit_raises_combined, pay_rate_type == 'Salaried', dept == 'News') %>% group_by(race_grouping, gender)
news_salaried_raises_scores <- news_salaried_raises_scores %>% summarise(
  count = length(performance_rating),
  median = median(performance_rating, na.rm = TRUE)
)
suppress(news_salaried_raises_scores)
```

```
## # A tibble: 6 x 4
## # Groups:   race_grouping [3]
##   race_grouping   gender count median
##   <chr>           <chr>  <int>  <dbl>
## 1 person of color Female    96    3.4
## 2 person of color Male      78    3.4
## 3 unknown         Female     9    3.9
## 4 unknown         Male       7    3.7
## 5 white           Female   267    3.5
## 6 white           Male     354    3.6
```

```r
news_hourly_raises <- filter(merit_raises_combined, pay_rate_type == 'Hourly', dept == 'News') %>% group_by(race_grouping, gender)
news_hourly_raises <- news_hourly_raises %>% summarise(
  count = length(base_pay_change),
  median = median(base_pay_change, na.rm = TRUE)
)
suppress(news_hourly_raises)
```

```
## # A tibble: 4 x 4
## # Groups:   race_grouping [2]
##   race_grouping   gender count median
##   <chr>           <chr>  <int>  <dbl>
## 1 person of color Female    18   1.27
## 2 person of color Male      19   1.03
## 3 white           Female    54   1.46
## 4 white           Male      28   1.16
```

```r
news_hourly_raises_scores <- filter(merit_raises_combined, pay_rate_type == 'Hourly', dept == 'News') %>% group_by(race_grouping, gender)
news_hourly_raises_scores <- news_hourly_raises_scores %>% summarise(
  count = length(performance_rating),
  median = median(performance_rating, na.rm = TRUE)
)
suppress(news_hourly_raises_scores)
```

```
## # A tibble: 4 x 4
## # Groups:   race_grouping [2]
##   race_grouping   gender count median
##   <chr>           <chr>  <int>  <dbl>
## 1 person of color Female    18    3.4
## 2 person of color Male      19    3.4
## 3 white           Female    54    3.5
## 4 white           Male      28    3.6
```

### Era


```r
bezos <- filter(news_salaried, hire_date > '2013-10-04')
graham <- filter(news_salaried, hire_date < '2013-10-05')

bezos_gender <- bezos %>% group_by(gender)
bezos_gender <- bezos_gender %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(bezos_gender)
```

```
## # A tibble: 2 x 3
##   gender count median
##   <chr>  <int>  <dbl>
## 1 Male     157 100780
## 2 Female   180  87160
```

```r
graham_gender <- graham %>% group_by(gender)
graham_gender <- graham_gender %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(graham_gender)
```

```
## # A tibble: 2 x 3
##   gender count  median
##   <chr>  <int>   <dbl>
## 1 Male     133 127059.
## 2 Female   104 112136.
```

```r
bezos_race <- bezos %>% group_by(race_ethnicity)
bezos_race <- bezos_race %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(bezos_race)
```

```
## # A tibble: 7 x 3
##   race_ethnicity                                       count  median
##   <chr>                                                <int>   <dbl>
## 1 <NA>                                                    12 130000 
## 2 Black or African American (United States of America)    26  94964.
## 3 White (United States of America)                       224  94519.
## 4 Asian (United States of America)                        31  87000 
## 5 Prefer Not to Disclose (United States of America)        8  82140 
## 6 Hispanic or Latino (United States of America)           22  81250.
## 7 Two or More Races (United States of America)            14  79860
```

```r
graham_race <- graham %>% group_by(race_ethnicity)
graham_race <- graham_race %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(graham_race)
```

```
## # A tibble: 5 x 3
##   race_ethnicity                                       count  median
##   <chr>                                                <int>   <dbl>
## 1 <NA>                                                     9 151171.
## 2 Hispanic or Latino (United States of America)            6 135272.
## 3 White (United States of America)                       182 124500 
## 4 Asian (United States of America)                        15 111761.
## 5 Black or African American (United States of America)    22 104398.
```

```r
bezos_race_group <- bezos %>% group_by(race_grouping)
bezos_race_group <- bezos_race_group %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(bezos_race_group)
```

```
## # A tibble: 3 x 3
##   race_grouping   count  median
##   <chr>           <int>   <dbl>
## 1 unknown            20 113890 
## 2 white             224  94519.
## 3 person of color    93  86000
```

```r
graham_race_group <- graham %>% group_by(race_grouping)
graham_race_group <- graham_race_group %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(graham_race_group)
```

```
## # A tibble: 3 x 3
##   race_grouping   count  median
##   <chr>           <int>   <dbl>
## 1 unknown             9 151171.
## 2 white             182 124500 
## 3 person of color    46 110845.
```

```r
bezos_gender_race_group <- bezos %>% group_by(race_grouping, gender)
bezos_gender_race_group <- bezos_gender_race_group %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(bezos_gender_race_group)
```

```
## # A tibble: 6 x 4
## # Groups:   race_grouping [3]
##   race_grouping   gender count  median
##   <chr>           <chr>  <int>   <dbl>
## 1 unknown         Male      10 121390 
## 2 unknown         Female    10 109000 
## 3 white           Male     115 102780 
## 4 person of color Male      32  94026.
## 5 white           Female   109  88780 
## 6 person of color Female    61  82000
```

```r
graham_gender_race_group <- graham %>% group_by(race_grouping, gender)
graham_gender_race_group <- graham_gender_race_group %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(graham_gender_race_group)
```

```
## # A tibble: 5 x 4
## # Groups:   race_grouping [3]
##   race_grouping   gender count  median
##   <chr>           <chr>  <int>   <dbl>
## 1 unknown         Male       6 150975.
## 2 white           Male     103 128629.
## 3 person of color Male      24 117567.
## 4 white           Female    79 112512.
## 5 person of color Female    22 108594.
```

```r
bezos_gender_race_group_age5 <- bezos %>% group_by(race_grouping, gender, age_group_5)
bezos_gender_race_group_age5 <- bezos_gender_race_group_age5 %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(bezos_gender_race_group_age5)
```

```
## # A tibble: 20 x 5
## # Groups:   race_grouping, gender [4]
##    race_grouping   gender age_group_5 count  median
##    <chr>           <chr>  <fct>       <int>   <dbl>
##  1 white           Female 45-49           7 160780 
##  2 white           Male   55-59           8 156807.
##  3 white           Female 40-44           6 143750 
##  4 white           Male   40-44          15 136468.
##  5 person of color Male   35-39           8 115530 
##  6 white           Female 50-54           8 114975.
##  7 white           Male   35-39          24 107880 
##  8 white           Female 35-39          15 105000 
##  9 white           Male   45-49           9 102796.
## 10 person of color Female 35-39           8  99619.
## 11 white           Male   30-34          29  94780 
## 12 person of color Male   25-29           8  88540 
## 13 white           Female 30-34          24  87050 
## 14 person of color Female 30-34          19  87000 
## 15 person of color Male   30-34           5  87000 
## 16 white           Female 25-29          37  81757.
## 17 person of color Female 25-29          19  77000 
## 18 white           Male   25-29          21  76780 
## 19 person of color Female <25            10  64390 
## 20 white           Female <25             9  64280
```

```r
graham_gender_race_group_age5 <- graham %>% group_by(race_grouping, gender, age_group_5)
graham_gender_race_group_age5 <- graham_gender_race_group_age5 %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(graham_gender_race_group_age5)
```

```
## # A tibble: 18 x 5
## # Groups:   race_grouping, gender [4]
##    race_grouping   gender age_group_5 count  median
##    <chr>           <chr>  <fct>       <int>   <dbl>
##  1 white           Male   65+             8 153937.
##  2 white           Male   35-39          11 147300 
##  3 white           Male   55-59          19 146542.
##  4 white           Female 55-59          16 138564.
##  5 white           Male   50-54          21 134547.
##  6 white           Male   60-64          14 123515.
##  7 white           Female 40-44           5 120780 
##  8 person of color Female 40-44           5 118512.
##  9 person of color Male   50-54          11 116349.
## 10 white           Male   40-44          17 115237.
## 11 white           Female 50-54          15 114803 
## 12 white           Female 60-64           7 112512.
## 13 white           Male   45-49           8 111473.
## 14 white           Female 45-49          12 100910.
## 15 white           Female 30-34           8 100788.
## 16 person of color Female 50-54           5  96944.
## 17 white           Female 35-39          11  88000 
## 18 white           Male   30-34           5  83650.
```

```r
bezos_gender_race_group_age5_tier <- bezos %>% group_by(race_grouping, gender, age_group_5, tier)
bezos_gender_race_group_age5_tier <- bezos_gender_race_group_age5_tier %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(bezos_gender_race_group_age5_tier)
```

```
## # A tibble: 20 x 6
## # Groups:   race_grouping, gender, age_group_5 [10]
##    race_grouping   gender age_group_5 tier   count  median
##    <chr>           <chr>  <fct>       <chr>  <int>   <dbl>
##  1 white           Male   40-44       Tier 1     8 191530 
##  2 white           Male   35-39       Tier 1    10 130018.
##  3 white           Female 35-39       Tier 1     8 128330 
##  4 white           Male   30-34       Tier 1    12 125233.
##  5 white           Male   45-49       Tier 2     5 120780 
##  6 white           Female 25-29       Tier 1     5 100000 
##  7 white           Male   30-34       Tier 2     5 100000 
##  8 white           Male   35-39       Tier 2     8  98890 
##  9 white           Female 30-34       Tier 2     6  93780 
## 10 white           Male   35-39       Tier 3     6  93030 
## 11 white           Male   25-29       Tier 2     6  91282.
## 12 white           Female 25-29       Tier 2     9  91000 
## 13 white           Male   30-34       Tier 3    10  88240 
## 14 person of color Female 30-34       Tier 2     7  88133.
## 15 white           Female 30-34       Tier 3    11  86000 
## 16 person of color Female 30-34       Tier 3     6  83890.
## 17 white           Female 25-29       Tier 3    15  79140 
## 18 person of color Female 25-29       Tier 3    12  77000 
## 19 white           Male   25-29       Tier 3     8  73890 
## 20 white           Female 25-29       Tier 4     8  69890
```

```r
graham_gender_race_group_age5_tier <- graham %>% group_by(race_grouping, gender, age_group_5, tier)
graham_gender_race_group_age5_tier <- graham_gender_race_group_age5_tier %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(graham_gender_race_group_age5_tier)
```

```
## # A tibble: 21 x 6
## # Groups:   race_grouping, gender, age_group_5 [13]
##    race_grouping gender age_group_5 tier   count  median
##    <chr>         <chr>  <fct>       <chr>  <int>   <dbl>
##  1 white         Male   50-54       Tier 1     5 180040 
##  2 white         Male   35-39       Tier 1     5 173280 
##  3 white         Female 50-54       Tier 1     5 167780 
##  4 white         Male   55-59       Tier 1     6 167172.
##  5 white         Male   60-64       Tier 1     5 166612.
##  6 white         Female 55-59       Tier 1     6 162854.
##  7 white         Female 55-59       Tier 2     5 149030.
##  8 white         Male   65+         Tier 2     6 147473.
##  9 white         Male   35-39       Tier 2     5 147300 
## 10 white         Male   55-59       Tier 2    12 143129.
## # … with 11 more rows
```

### Overall disparity calculations


```r
news_groups <- news_salaried %>% group_by(age_group_5, tier)
news_groups <- news_groups %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(news_groups)
```

```
## # A tibble: 30 x 4
## # Groups:   age_group_5 [10]
##    age_group_5 tier   count median
##    <fct>       <chr>  <int>  <dbl>
##  1 <25         Tier 2    10  65140
##  2 <25         Tier 3     8  66250
##  3 25-29       Tier 1    13 110000
##  4 25-29       Tier 2    23  90000
##  5 25-29       Tier 3    38  77000
##  6 25-29       Tier 4    17  75000
##  7 30-34       Tier 1    35 121280
##  8 30-34       Tier 2    27  94535
##  9 30-34       Tier 3    32  83140
## 10 30-34       Tier 4     9  77000
## # … with 20 more rows
```

```r
expected_medians <- merge(news_salaried, news_groups, by=c('age_group_5', 'tier'), all.x = TRUE)

below_expected_medians <- filter(expected_medians, current_base_pay < median) %>% group_by(race_grouping, gender)
below_expected_medians <- below_expected_medians %>% summarise(
  count = length(current_base_pay)
)
suppress(below_expected_medians)
```

```
## # A tibble: 6 x 3
## # Groups:   race_grouping [3]
##   race_grouping   gender count
##   <chr>           <chr>  <int>
## 1 person of color Female    48
## 2 person of color Male      30
## 3 unknown         Female     7
## 4 unknown         Male       8
## 5 white           Female    94
## 6 white           Male      89
```

```r
above_expected_medians <- filter(expected_medians, current_base_pay > median) %>% group_by(race_grouping, gender)
above_expected_medians <- above_expected_medians %>% summarise(
  count = length(current_base_pay)
)
suppress(above_expected_medians)
```

```
## # A tibble: 5 x 3
## # Groups:   race_grouping [3]
##   race_grouping   gender count
##   <chr>           <chr>  <int>
## 1 person of color Female    30
## 2 person of color Male      21
## 3 unknown         Male       8
## 4 white           Female    90
## 5 white           Male     121
```

```r
expected_medians <- expected_medians %>% mutate(disparity = current_base_pay - median,
                    disparity_pct = (current_base_pay - median)/median)

disparity <- expected_medians %>% group_by(race_grouping, gender)
disparity <- disparity %>% summarise(
  count = length(disparity),
  median_disparity = median(disparity, na.rm = TRUE)
)
suppress(disparity)
```

```
## # A tibble: 6 x 4
## # Groups:   race_grouping [3]
##   race_grouping   gender count median_disparity
##   <chr>           <chr>  <int>            <dbl>
## 1 person of color Female    83          -1360  
## 2 person of color Male      56           -407. 
## 3 unknown         Female    13          -1300  
## 4 unknown         Male      16           1537. 
## 5 white           Female   188            -14.2
## 6 white           Male     218           2448.
```

```r
disparity_pct_above <- filter(expected_medians, disparity_pct > .05) %>% group_by(race_grouping, gender)
disparity_pct_above <- disparity_pct_above %>% summarise(
  count = length(disparity),
  median_disparity = median(disparity, na.rm = TRUE)
)
suppress(disparity_pct_above)
```

```
## # A tibble: 5 x 4
## # Groups:   race_grouping [3]
##   race_grouping   gender count median_disparity
##   <chr>           <chr>  <int>            <dbl>
## 1 person of color Female    20            9360 
## 2 person of color Male      15           24700 
## 3 unknown         Male       7           29500 
## 4 white           Female    65           19211.
## 5 white           Male     101           28780
```

```r
disparity_pct_below <- filter(expected_medians, disparity_pct < -.05) %>% group_by(race_grouping, gender)
disparity_pct_below <- disparity_pct_below %>% summarise(
  count = length(disparity),
  median_disparity = median(disparity, na.rm = TRUE)
)
suppress(disparity_pct_below)
```

```
## # A tibble: 5 x 4
## # Groups:   race_grouping [3]
##   race_grouping   gender count median_disparity
##   <chr>           <chr>  <int>            <dbl>
## 1 person of color Female    36          -10140 
## 2 person of color Male      21          -15435 
## 3 unknown         Female     6          -14390 
## 4 white           Female    70          -14589.
## 5 white           Male      68          -18102.
```

```r
bezos_news_groups <- bezos %>% group_by(age_group_5, tier)
bezos_news_groups <- bezos_news_groups %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(bezos_news_groups)
```

```
## # A tibble: 20 x 4
## # Groups:   age_group_5 [7]
##    age_group_5 tier   count  median
##    <fct>       <chr>  <int>   <dbl>
##  1 <25         Tier 2    10  65140 
##  2 <25         Tier 3     8  66250 
##  3 25-29       Tier 1    12 110000 
##  4 25-29       Tier 2    23  90000 
##  5 25-29       Tier 3    38  77000 
##  6 25-29       Tier 4    17  75000 
##  7 30-34       Tier 1    28 120843.
##  8 30-34       Tier 2    19  95656.
##  9 30-34       Tier 3    30  84640 
## 10 30-34       Tier 4     7  77000 
## 11 35-39       Tier 1    26 122940 
## 12 35-39       Tier 2    16 102801.
## 13 35-39       Tier 3    13  90780 
## 14 40-44       Tier 1    18 148572.
## 15 40-44       Tier 2     6 128713.
## 16 40-44       Tier 3     7 103000 
## 17 45-49       Tier 2     9 120780 
## 18 45-49       Tier 3     6  91234.
## 19 50-54       Tier 2     7 107171.
## 20 50-54       Tier 3     5  92352.
```

```r
bezos_expected_medians <- merge(bezos, bezos_news_groups, by=c('age_group_5', 'tier'), all.x = TRUE)

graham_news_groups <- graham %>% group_by(age_group_5, tier)
graham_news_groups <- graham_news_groups %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(graham_news_groups)
```

```
## # A tibble: 20 x 4
## # Groups:   age_group_5 [8]
##    age_group_5 tier   count  median
##    <fct>       <chr>  <int>   <dbl>
##  1 30-34       Tier 1     7 121280 
##  2 30-34       Tier 2     8  89092.
##  3 35-39       Tier 1     9 125000 
##  4 35-39       Tier 2    14  95500 
##  5 35-39       Tier 3     6  89000.
##  6 40-44       Tier 1    13 129780 
##  7 40-44       Tier 2    13 104560.
##  8 45-49       Tier 1     8 158458.
##  9 45-49       Tier 2    12  94653.
## 10 50-54       Tier 1    13 165685.
## 11 50-54       Tier 2    30 117266.
## 12 50-54       Tier 3    10 100406.
## 13 55-59       Tier 1    17 170497.
## 14 55-59       Tier 2    18 143186.
## 15 55-59       Tier 3     6  92226.
## 16 60-64       Tier 1    10 158690.
## 17 60-64       Tier 2     9 112512.
## 18 60-64       Tier 3     5 107212.
## 19 65+         Tier 1     6 172067.
## 20 65+         Tier 2     8 147473.
```

```r
graham_expected_medians <- merge(graham, graham_news_groups, by=c('age_group_5', 'tier'), all.x = TRUE)

bezos_expected_medians <- bezos_expected_medians %>% mutate(disparity = current_base_pay - median,
                    disparity_pct = (current_base_pay - median)/median)

graham_expected_medians <- graham_expected_medians %>% mutate(disparity = current_base_pay - median,
                    disparity_pct = (current_base_pay - median)/median)

bezos_disparity_gender <- bezos_expected_medians %>% group_by(gender)
bezos_disparity_gender <- bezos_disparity_gender %>% summarise(
  count = length(disparity),
  median_disparity = median(disparity, na.rm = TRUE)
)
suppress(bezos_disparity_gender)
```

```
## # A tibble: 2 x 3
##   gender count median_disparity
##   <chr>  <int>            <dbl>
## 1 Female   180           -352. 
## 2 Male     157             66.9
```

```r
bezos_disparity_race_group <- bezos_expected_medians %>% group_by(race_grouping)
bezos_disparity_race_group <- bezos_disparity_race_group %>% summarise(
  count = length(disparity),
  median_disparity = median(disparity, na.rm = TRUE)
)
suppress(bezos_disparity_race_group)
```

```
## # A tibble: 3 x 3
##   race_grouping   count median_disparity
##   <chr>           <int>            <dbl>
## 1 person of color    93               0 
## 2 unknown            20           -4536.
## 3 white             224               0
```

```r
bezos_disparity_gender_race_group <- bezos_expected_medians %>% group_by(race_grouping, gender)
bezos_disparity_gender_race_group <- bezos_disparity_gender_race_group %>% summarise(
  count = length(disparity),
  median_disparity = median(disparity, na.rm = TRUE)
)
suppress(bezos_disparity_gender_race_group)
```

```
## # A tibble: 6 x 4
## # Groups:   race_grouping [3]
##   race_grouping   gender count median_disparity
##   <chr>           <chr>  <int>            <dbl>
## 1 person of color Female    61            -590 
## 2 person of color Male      32               0 
## 3 unknown         Female    10           -6070 
## 4 unknown         Male      10            7453.
## 5 white           Female   109               0 
## 6 white           Male     115             454.
```

```r
graham_disparity_gender <- graham_expected_medians %>% group_by(gender)
graham_disparity_gender <- graham_disparity_gender %>% summarise(
  count = length(disparity),
  median_disparity = median(disparity, na.rm = TRUE)
)
suppress(graham_disparity_gender)
```

```
## # A tibble: 2 x 3
##   gender count median_disparity
##   <chr>  <int>            <dbl>
## 1 Female   104            -905.
## 2 Male     133             475.
```

```r
graham_disparity_race_group <- graham_expected_medians %>% group_by(race_grouping)
graham_disparity_race_group <- graham_disparity_race_group %>% summarise(
  count = length(disparity),
  median_disparity = median(disparity, na.rm = TRUE)
)
suppress(graham_disparity_race_group)
```

```
## # A tibble: 3 x 3
##   race_grouping   count median_disparity
##   <chr>           <int>            <dbl>
## 1 person of color    46           -5439.
## 2 unknown             9           -3191.
## 3 white             182            2069.
```

```r
graham_disparity_gender_race_group <- graham_expected_medians %>% group_by(race_grouping, gender)
graham_disparity_gender_race_group <- graham_disparity_gender_race_group %>% summarise(
  count = length(disparity),
  median_disparity = median(disparity, na.rm = TRUE)
)
suppress(graham_disparity_gender_race_group)
```

```
## # A tibble: 5 x 4
## # Groups:   race_grouping [3]
##   race_grouping   gender count median_disparity
##   <chr>           <chr>  <int>            <dbl>
## 1 person of color Female    22           -6599.
## 2 person of color Male      24           -1409.
## 3 unknown         Male       6           -2850.
## 4 white           Female    79             810.
## 5 white           Male     103            3355.
```

### Regression


```r
news_salaried_regression <- news_salaried[,c('department','gender','race_ethnicity','current_base_pay','job_profile_current','cost_center_current','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating','age','years_of_service','age_group_5','years_of_service_grouped','dept','desk','tier','race_grouping')]
news_salaried_regression <- fastDummies::dummy_cols(news_salaried_regression, select_columns = c('gender','race_ethnicity','age_group_5','years_of_service_grouped','dept','desk','tier','race_grouping'))
names(news_salaried_regression) <- gsub(' ', '_', names(news_salaried_regression))
names(news_salaried_regression) <- gsub('-', 'to', names(news_salaried_regression))
names(news_salaried_regression) <- gsub('\\+', '_over', names(news_salaried_regression))
names(news_salaried_regression) <- gsub('<', 'under_', names(news_salaried_regression))

linearMod1 <- lm(formula = current_base_pay ~ gender_Female + gender_Male, data=news_salaried_regression)
summary(linearMod1)
```

```
## 
## Call:
## lm(formula = current_base_pay ~ gender_Female + gender_Male, 
##     data = news_salaried_regression)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -66717 -30572 -10009  22943 207383 
## 
## Coefficients: (1 not defined because of singularities)
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     124717       2500  49.895  < 2e-16 ***
## gender_Female   -17250       3554  -4.854 1.56e-06 ***
## gender_Male         NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 42570 on 572 degrees of freedom
## Multiple R-squared:  0.03957,	Adjusted R-squared:  0.03789 
## F-statistic: 23.56 on 1 and 572 DF,  p-value: 1.561e-06
```

```r
linearMod2 <- lm(formula = current_base_pay ~ race_grouping_white + race_grouping_person_of_color, data=news_salaried_regression)
summary(linearMod2)
```

```
## 
## Call:
## lm(formula = current_base_pay ~ race_grouping_white + race_grouping_person_of_color, 
##     data = news_salaried_regression)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -62782 -30287 -11247  24529 211317 
## 
## Coefficients:
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     127084       7897  16.092  < 2e-16 ***
## race_grouping_white              -6302       8175  -0.771  0.44107    
## race_grouping_person_of_color   -26614       8682  -3.065  0.00228 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 42530 on 571 degrees of freedom
## Multiple R-squared:  0.04295,	Adjusted R-squared:  0.0396 
## F-statistic: 12.81 on 2 and 571 DF,  p-value: 3.602e-06
```

```r
linearMod3 <- lm(formula = current_base_pay ~ gender_Female + gender_Male + race_grouping_white + race_grouping_person_of_color, data=news_salaried_regression)
summary(linearMod3)
```

```
## 
## Call:
## lm(formula = current_base_pay ~ gender_Female + gender_Male + 
##     race_grouping_white + race_grouping_person_of_color, data = news_salaried_regression)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -69906 -30002  -9689  22094 204194 
## 
## Coefficients: (1 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     133980       7934  16.888  < 2e-16 ***
## gender_Female                   -15384       3519  -4.371 1.47e-05 ***
## gender_Male                         NA         NA      NA       NA    
## race_grouping_white              -6075       8048  -0.755  0.45069    
## race_grouping_person_of_color   -24324       8564  -2.840  0.00467 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 41870 on 570 degrees of freedom
## Multiple R-squared:  0.074,	Adjusted R-squared:  0.06912 
## F-statistic: 15.18 on 3 and 570 DF,  p-value: 1.617e-09
```

```r
linearMod4 <- lm(formula = current_base_pay ~ gender_Female + gender_Male + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=news_salaried_regression)
summary(linearMod4)
```

```
## 
## Call:
## lm(formula = current_base_pay ~ gender_Female + gender_Male + 
##     age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + 
##     age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + 
##     age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + 
##     age_group_5_65_over, data = news_salaried_regression)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -86172 -22779  -7300  13780 181520 
## 
## Coefficients: (2 not defined because of singularities)
##                      Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            167625       9729  17.229  < 2e-16 ***
## gender_Female           -8165       3214  -2.540 0.011345 *  
## gender_Male                NA         NA      NA       NA    
## age_group_5_under_25   -94875      12415  -7.642 9.25e-14 ***
## age_group_5_25to29     -75435      10489  -7.192 2.05e-12 ***
## age_group_5_30to34     -59320      10374  -5.718 1.75e-08 ***
## age_group_5_35to39     -48805      10485  -4.655 4.05e-06 ***
## age_group_5_40to44     -30359      10760  -2.821 0.004949 ** 
## age_group_5_45to49     -38200      11239  -3.399 0.000724 ***
## age_group_5_50to54     -35503      10659  -3.331 0.000923 ***
## age_group_5_55to59     -19524      11005  -1.774 0.076595 .  
## age_group_5_60to64     -25877      11987  -2.159 0.031299 *  
## age_group_5_65_over        NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 37450 on 563 degrees of freedom
## Multiple R-squared:  0.2682,	Adjusted R-squared:  0.2552 
## F-statistic: 20.63 on 10 and 563 DF,  p-value: < 2.2e-16
```

```r
linearMod5 <- lm(formula = current_base_pay ~ race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=news_salaried_regression)
summary(linearMod5)
```

```
## 
## Call:
## lm(formula = current_base_pay ~ race_grouping_white + race_grouping_person_of_color + 
##     age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + 
##     age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + 
##     age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + 
##     age_group_5_65_over, data = news_salaried_regression)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -83570 -24373  -6835  13690 175683 
## 
## Coefficients: (1 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     175496      11714  14.982  < 2e-16 ***
## race_grouping_white             -10472       7206  -1.453 0.146732    
## race_grouping_person_of_color   -22748       7649  -2.974 0.003064 ** 
## age_group_5_under_25            -93548      12337  -7.583 1.41e-13 ***
## age_group_5_25to29              -75151      10408  -7.221 1.69e-12 ***
## age_group_5_30to34              -58814      10318  -5.700 1.93e-08 ***
## age_group_5_35to39              -46772      10444  -4.478 9.11e-06 ***
## age_group_5_40to44              -28517      10714  -2.662 0.007999 ** 
## age_group_5_45to49              -37927      11169  -3.396 0.000733 ***
## age_group_5_50to54              -33076      10623  -3.114 0.001942 ** 
## age_group_5_55to59              -19411      10936  -1.775 0.076454 .  
## age_group_5_60to64              -26272      11912  -2.205 0.027827 *  
## age_group_5_65_over                 NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 37220 on 562 degrees of freedom
## Multiple R-squared:  0.2784,	Adjusted R-squared:  0.2643 
## F-statistic: 19.71 on 11 and 562 DF,  p-value: < 2.2e-16
```

```r
linearMod6 <- lm(formula = current_base_pay ~ gender_Female + gender_Male + race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=news_salaried_regression)
summary(linearMod6)
```

```
## 
## Call:
## lm(formula = current_base_pay ~ gender_Female + gender_Male + 
##     race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + 
##     age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + 
##     age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + 
##     age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, 
##     data = news_salaried_regression)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -85913 -23282  -6439  12503 179595 
## 
## Coefficients: (2 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     177610      11711  15.166  < 2e-16 ***
## gender_Female                    -7131       3197  -2.230 0.026121 *  
## gender_Male                         NA         NA      NA       NA    
## race_grouping_white             -10244       7182  -1.426 0.154303    
## race_grouping_person_of_color   -21779       7634  -2.853 0.004492 ** 
## age_group_5_under_25            -90575      12366  -7.325 8.37e-13 ***
## age_group_5_25to29              -72999      10416  -7.008 6.94e-12 ***
## age_group_5_30to34              -57392      10302  -5.571 3.93e-08 ***
## age_group_5_35to39              -46156      10411  -4.433 1.12e-05 ***
## age_group_5_40to44              -28528      10677  -2.672 0.007759 ** 
## age_group_5_45to49              -37051      11136  -3.327 0.000935 ***
## age_group_5_50to54              -32670      10587  -3.086 0.002130 ** 
## age_group_5_55to59              -18756      10902  -1.720 0.085904 .  
## age_group_5_60to64              -25603      11874  -2.156 0.031490 *  
## age_group_5_65_over                 NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 37090 on 561 degrees of freedom
## Multiple R-squared:  0.2848,	Adjusted R-squared:  0.2695 
## F-statistic: 18.61 on 12 and 561 DF,  p-value: < 2.2e-16
```

```r
linearMod7 <- lm(formula = current_base_pay ~ gender_Female + gender_Male + race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over + tier_Tier_1 + tier_Tier_2 + tier_Tier_3 + tier_Tier_4, data=news_salaried_regression)
summary(linearMod7)
```

```
## 
## Call:
## lm(formula = current_base_pay ~ gender_Female + gender_Male + 
##     race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + 
##     age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + 
##     age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + 
##     age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over + 
##     tier_Tier_1 + tier_Tier_2 + tier_Tier_3 + tier_Tier_4, data = news_salaried_regression)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -73755 -19471  -4221  11237 181914 
## 
## Coefficients: (3 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   120967.5    11990.4  10.089  < 2e-16 ***
## gender_Female                  -4876.0     2760.7  -1.766 0.077907 .  
## gender_Male                         NA         NA      NA       NA    
## race_grouping_white             9406.8     6384.3   1.473 0.141200    
## race_grouping_person_of_color    651.1     6797.4   0.096 0.923721    
## age_group_5_under_25          -70967.7    10751.7  -6.601 9.54e-11 ***
## age_group_5_25to29            -51967.8     9147.7  -5.681 2.16e-08 ***
## age_group_5_30to34            -45835.5     8943.2  -5.125 4.10e-07 ***
## age_group_5_35to39            -40948.5     8989.4  -4.555 6.43e-06 ***
## age_group_5_40to44            -25228.7     9217.5  -2.737 0.006397 ** 
## age_group_5_45to49            -28012.2     9613.5  -2.914 0.003713 ** 
## age_group_5_50to54            -22011.8     9145.4  -2.407 0.016413 *  
## age_group_5_55to59            -13805.7     9398.9  -1.469 0.142435    
## age_group_5_60to64            -20565.1    10235.5  -2.009 0.044997 *  
## age_group_5_65_over                 NA         NA      NA       NA    
## tier_Tier_1                    53348.3     6214.7   8.584  < 2e-16 ***
## tier_Tier_2                    23380.1     6049.1   3.865 0.000124 ***
## tier_Tier_3                     2870.8     6037.4   0.475 0.634619    
## tier_Tier_4                         NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 31930 on 558 degrees of freedom
## Multiple R-squared:  0.4729,	Adjusted R-squared:  0.4587 
## F-statistic: 33.38 on 15 and 558 DF,  p-value: < 2.2e-16
```

```r
linearMod8 <- lm(formula = current_base_pay ~ gender_Female + gender_Male + race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over + tier_Tier_1 + tier_Tier_2 + tier_Tier_3 + tier_Tier_4 + years_of_service_grouped_0 + years_of_service_grouped_1to2 + years_of_service_grouped_3to5 + years_of_service_grouped_6to10 + years_of_service_grouped_11to15 + years_of_service_grouped_16to20 + years_of_service_grouped_21to25 + years_of_service_grouped_25_over, data=news_salaried_regression)
summary(linearMod8)
```

```
## 
## Call:
## lm(formula = current_base_pay ~ gender_Female + gender_Male + 
##     race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + 
##     age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + 
##     age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + 
##     age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over + 
##     tier_Tier_1 + tier_Tier_2 + tier_Tier_3 + tier_Tier_4 + years_of_service_grouped_0 + 
##     years_of_service_grouped_1to2 + years_of_service_grouped_3to5 + 
##     years_of_service_grouped_6to10 + years_of_service_grouped_11to15 + 
##     years_of_service_grouped_16to20 + years_of_service_grouped_21to25 + 
##     years_of_service_grouped_25_over, data = news_salaried_regression)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -78346 -19056  -3790  11052 174977 
## 
## Coefficients: (4 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                        125096      13133   9.525  < 2e-16 ***
## gender_Female                       -4750       2766  -1.717  0.08653 .  
## gender_Male                            NA         NA      NA       NA    
## race_grouping_white                  9871       6401   1.542  0.12364    
## race_grouping_person_of_color        1117       6822   0.164  0.86996    
## age_group_5_under_25               -78083      11460  -6.814 2.50e-11 ***
## age_group_5_25to29                 -57223       9842  -5.814 1.03e-08 ***
## age_group_5_30to34                 -49792       9556  -5.211 2.66e-07 ***
## age_group_5_35to39                 -44320       9550  -4.641 4.35e-06 ***
## age_group_5_40to44                 -27729       9612  -2.885  0.00407 ** 
## age_group_5_45to49                 -29546       9874  -2.992  0.00289 ** 
## age_group_5_50to54                 -22921       9304  -2.464  0.01406 *  
## age_group_5_55to59                 -14698       9472  -1.552  0.12129    
## age_group_5_60to64                 -23419      10417  -2.248  0.02495 *  
## age_group_5_65_over                    NA         NA      NA       NA    
## tier_Tier_1                         54494       6295   8.657  < 2e-16 ***
## tier_Tier_2                         24832       6191   4.011 6.88e-05 ***
## tier_Tier_3                          3350       6125   0.547  0.58466    
## tier_Tier_4                            NA         NA      NA       NA    
## years_of_service_grouped_0           1437       8374   0.172  0.86380    
## years_of_service_grouped_1to2        2300       7767   0.296  0.76728    
## years_of_service_grouped_3to5       -2442       7660  -0.319  0.75001    
## years_of_service_grouped_6to10      -7719       8036  -0.961  0.33722    
## years_of_service_grouped_11to15     -6384       8152  -0.783  0.43394    
## years_of_service_grouped_16to20     -6308       7463  -0.845  0.39831    
## years_of_service_grouped_21to25    -12596       8953  -1.407  0.16003    
## years_of_service_grouped_25_over       NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 31910 on 551 degrees of freedom
## Multiple R-squared:  0.4802,	Adjusted R-squared:  0.4594 
## F-statistic: 23.14 on 22 and 551 DF,  p-value: < 2.2e-16
```

```r
merit_raises_combined_salaried_regression <- filter(merit_raises_combined, dept == 'News', pay_rate_type == 'Salaried')

merit_raises_combined_salaried_regression <- fastDummies::dummy_cols(merit_raises_combined_salaried_regression, select_columns = c('gender','race_grouping','age_group_5'))
names(merit_raises_combined_salaried_regression) <- gsub(' ', '_', names(merit_raises_combined_salaried_regression))
names(merit_raises_combined_salaried_regression) <- gsub('-', 'to', names(merit_raises_combined_salaried_regression))
names(merit_raises_combined_salaried_regression) <- gsub('\\+', '_over', names(merit_raises_combined_salaried_regression))
names(merit_raises_combined_salaried_regression) <- gsub('<', 'under_', names(merit_raises_combined_salaried_regression))

linearMod9 <- lm(formula = base_pay_change ~ gender_Female + gender_Male, data=merit_raises_combined_salaried_regression)
summary(linearMod9)
```

```
## 
## Call:
## lm(formula = base_pay_change ~ gender_Female + gender_Male, data = merit_raises_combined_salaried_regression)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2775.8 -1074.6  -275.8   724.2 16724.0 
## 
## Coefficients: (1 not defined because of singularities)
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    3275.85      75.31   43.49   <2e-16 ***
## gender_Female  -201.24     111.20   -1.81   0.0707 .  
## gender_Male         NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1578 on 809 degrees of freedom
## Multiple R-squared:  0.004032,	Adjusted R-squared:  0.0028 
## F-statistic: 3.275 on 1 and 809 DF,  p-value: 0.07072
```

```r
linearMod10 <- lm(formula = base_pay_change ~ race_grouping_white + race_grouping_person_of_color, data=merit_raises_combined_salaried_regression)
summary(linearMod10)
```

```
## 
## Call:
## lm(formula = base_pay_change ~ race_grouping_white + race_grouping_person_of_color, 
##     data = merit_raises_combined_salaried_regression)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2747.6  -997.6  -247.6   752.4 16752.3 
## 
## Coefficients:
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     3426.8      394.1   8.694   <2e-16 ***
## race_grouping_white             -179.2      399.2  -0.449    0.654    
## race_grouping_person_of_color   -494.1      411.9  -1.200    0.231    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1577 on 808 degrees of freedom
## Multiple R-squared:  0.00714,	Adjusted R-squared:  0.004682 
## F-statistic: 2.905 on 2 and 808 DF,  p-value: 0.05531
```

```r
linearMod11 <- lm(formula = base_pay_change ~ gender_Female + gender_Male + race_grouping_white + race_grouping_person_of_color, data=merit_raises_combined_salaried_regression)
summary(linearMod11)
```

```
## 
## Call:
## lm(formula = base_pay_change ~ gender_Female + gender_Male + 
##     race_grouping_white + race_grouping_person_of_color, data = merit_raises_combined_salaried_regression)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2824.7 -1031.6  -324.7   675.3 16675.2 
## 
## Coefficients: (1 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     3527.6      398.7   8.847   <2e-16 ***
## gender_Female                   -179.4      111.6  -1.607    0.108    
## gender_Male                         NA         NA      NA       NA    
## race_grouping_white             -203.0      399.1  -0.509    0.611    
## race_grouping_person_of_color   -496.0      411.5  -1.205    0.228    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1575 on 807 degrees of freedom
## Multiple R-squared:  0.01031,	Adjusted R-squared:  0.006628 
## F-statistic: 2.802 on 3 and 807 DF,  p-value: 0.03901
```

```r
linearMod12 <- lm(formula = base_pay_change ~ gender_Female + gender_Male + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=merit_raises_combined_salaried_regression)
summary(linearMod12)
```

```
## 
## Call:
## lm(formula = base_pay_change ~ gender_Female + gender_Male + 
##     age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + 
##     age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + 
##     age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + 
##     age_group_5_65_over, data = merit_raises_combined_salaried_regression)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2811.4  -919.2  -269.6   580.8 16528.8 
## 
## Coefficients: (2 not defined because of singularities)
##                      Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            2650.4      254.7  10.408  < 2e-16 ***
## gender_Female          -225.4      110.8  -2.034 0.042296 *  
## gender_Male                NA         NA      NA       NA    
## age_group_5_under_25   -312.6      684.1  -0.457 0.647863    
## age_group_5_25to29      661.0      315.0   2.098 0.036175 *  
## age_group_5_30to34      820.6      285.2   2.877 0.004117 ** 
## age_group_5_35to39      994.2      289.1   3.439 0.000614 ***
## age_group_5_40to44      942.4      297.5   3.168 0.001596 ** 
## age_group_5_45to49      768.5      309.3   2.484 0.013191 *  
## age_group_5_50to54      113.3      288.2   0.393 0.694409    
## age_group_5_55to59      561.6      299.4   1.875 0.061104 .  
## age_group_5_60to64      476.2      330.1   1.442 0.149582    
## age_group_5_65_over        NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1552 on 800 degrees of freedom
## Multiple R-squared:  0.04691,	Adjusted R-squared:  0.03499 
## F-statistic: 3.937 on 10 and 800 DF,  p-value: 2.953e-05
```

```r
linearMod13 <- lm(formula = base_pay_change ~ race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=merit_raises_combined_salaried_regression)
summary(linearMod13)
```

```
## 
## Call:
## lm(formula = base_pay_change ~ race_grouping_white + race_grouping_person_of_color + 
##     age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + 
##     age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + 
##     age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + 
##     age_group_5_65_over, data = merit_raises_combined_salaried_regression)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2764.1  -940.3  -289.7   602.3 16548.0 
## 
## Coefficients: (1 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     2607.1      469.0   5.559 3.69e-08 ***
## race_grouping_white              -33.8      395.9  -0.085 0.931998    
## race_grouping_person_of_color   -425.7      407.7  -1.044 0.296638    
## age_group_5_under_25            -423.3      680.6  -0.622 0.534129    
## age_group_5_25to29               690.8      314.9   2.194 0.028528 *  
## age_group_5_30to34               878.6      286.2   3.070 0.002210 ** 
## age_group_5_35to39              1066.6      290.4   3.673 0.000256 ***
## age_group_5_40to44              1053.7      301.1   3.500 0.000492 ***
## age_group_5_45to49               790.0      309.0   2.557 0.010753 *  
## age_group_5_50to54               216.4      290.5   0.745 0.456589    
## age_group_5_55to59               583.6      299.1   1.951 0.051391 .  
## age_group_5_60to64               498.6      330.1   1.510 0.131317    
## age_group_5_65_over                 NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1549 on 799 degrees of freedom
## Multiple R-squared:  0.0519,	Adjusted R-squared:  0.03885 
## F-statistic: 3.976 on 11 and 799 DF,  p-value: 1.165e-05
```

```r
linearMod14 <- lm(formula = base_pay_change ~ gender_Female + gender_Male + race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=merit_raises_combined_salaried_regression)
summary(linearMod14)
```

```
## 
## Call:
## lm(formula = base_pay_change ~ gender_Female + gender_Male + 
##     race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + 
##     age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + 
##     age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + 
##     age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, 
##     data = merit_raises_combined_salaried_regression)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2877.3  -956.5  -288.3   589.4 16449.3 
## 
## Coefficients: (2 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    2704.77     471.57   5.736 1.38e-08 ***
## gender_Female                  -196.82     111.10  -1.772 0.076853 .  
## gender_Male                         NA         NA      NA       NA    
## race_grouping_white             -64.13     395.78  -0.162 0.871313    
## race_grouping_person_of_color  -431.55     407.13  -1.060 0.289475    
## age_group_5_under_25           -326.62     681.85  -0.479 0.632053    
## age_group_5_25to29              736.69     315.51   2.335 0.019794 *  
## age_group_5_30to34              909.99     286.33   3.178 0.001539 ** 
## age_group_5_35to39             1086.86     290.23   3.745 0.000194 ***
## age_group_5_40to44             1048.77     300.69   3.488 0.000513 ***
## age_group_5_45to49              808.51     308.75   2.619 0.008996 ** 
## age_group_5_50to54              221.43     290.13   0.763 0.445568    
## age_group_5_55to59              594.80     298.76   1.991 0.046834 *  
## age_group_5_60to64              512.72     329.73   1.555 0.120345    
## age_group_5_65_over                 NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1547 on 798 degrees of freedom
## Multiple R-squared:  0.05562,	Adjusted R-squared:  0.04142 
## F-statistic: 3.916 on 12 and 798 DF,  p-value: 7.08e-06
```

```r
linearMod15 <- lm(formula = performance_rating ~ gender_Female + gender_Male, data=merit_raises_combined_salaried_regression)
summary(linearMod15)
```

```
## 
## Call:
## lm(formula = performance_rating ~ gender_Female + gender_Male, 
##     data = merit_raises_combined_salaried_regression)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -0.8339 -0.2063 -0.0339  0.1937  1.0661 
## 
## Coefficients: (1 not defined because of singularities)
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    3.60631    0.01616 223.118  < 2e-16 ***
## gender_Female -0.07241    0.02383  -3.038  0.00246 ** 
## gender_Male         NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3281 on 761 degrees of freedom
##   (48 observations deleted due to missingness)
## Multiple R-squared:  0.01199,	Adjusted R-squared:  0.01069 
## F-statistic: 9.232 on 1 and 761 DF,  p-value: 0.00246
```

```r
linearMod16 <- lm(formula = performance_rating ~ race_grouping_white + race_grouping_person_of_color, data=merit_raises_combined_salaried_regression)
summary(linearMod16)
```

```
## 
## Call:
## lm(formula = performance_rating ~ race_grouping_white + race_grouping_person_of_color, 
##     data = merit_raises_combined_salaried_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.90017 -0.20017 -0.00017  0.19983  0.99983 
## 
## Coefficients:
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    3.72500    0.08115  45.900  < 2e-16 ***
## race_grouping_white           -0.12483    0.08226  -1.517  0.12957    
## race_grouping_person_of_color -0.26258    0.08500  -3.089  0.00208 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3246 on 760 degrees of freedom
##   (48 observations deleted due to missingness)
## Multiple R-squared:  0.03398,	Adjusted R-squared:  0.03143 
## F-statistic: 13.37 on 2 and 760 DF,  p-value: 1.974e-06
```

```r
linearMod17 <- lm(formula = performance_rating ~ gender_Female + gender_Male + race_grouping_white + race_grouping_person_of_color, data=merit_raises_combined_salaried_regression)
summary(linearMod17)
```

```
## 
## Call:
## lm(formula = performance_rating ~ gender_Female + gender_Male + 
##     race_grouping_white + race_grouping_person_of_color, data = merit_raises_combined_salaried_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.86450 -0.22704 -0.02704  0.17296  1.03550 
## 
## Coefficients: (1 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    3.76018    0.08192  45.900  < 2e-16 ***
## gender_Female                 -0.06254    0.02363  -2.647  0.00830 ** 
## gender_Male                         NA         NA      NA       NA    
## race_grouping_white           -0.13314    0.08200  -1.624  0.10486    
## race_grouping_person_of_color -0.26288    0.08466  -3.105  0.00197 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3233 on 759 degrees of freedom
##   (48 observations deleted due to missingness)
## Multiple R-squared:  0.04281,	Adjusted R-squared:  0.03903 
## F-statistic: 11.32 on 3 and 759 DF,  p-value: 2.878e-07
```

```r
linearMod18 <- lm(formula = performance_rating ~ gender_Female + gender_Male + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=merit_raises_combined_salaried_regression)
summary(linearMod18)
```

```
## 
## Call:
## lm(formula = performance_rating ~ gender_Female + gender_Male + 
##     age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + 
##     age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + 
##     age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + 
##     age_group_5_65_over, data = merit_raises_combined_salaried_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.79665 -0.22275 -0.04557  0.20335  1.04477 
## 
## Coefficients: (2 not defined because of singularities)
##                      Estimate Std. Error t value Pr(>|t|)    
## (Intercept)           3.71995    0.05474  67.951  < 2e-16 ***
## gender_Female        -0.05524    0.02383  -2.318 0.020734 *  
## gender_Male                NA         NA      NA       NA    
## age_group_5_under_25 -0.42391    0.14348  -2.954 0.003230 ** 
## age_group_5_25to29   -0.23365    0.06766  -3.453 0.000585 ***
## age_group_5_30to34   -0.16806    0.06119  -2.746 0.006170 ** 
## age_group_5_35to39   -0.11914    0.06202  -1.921 0.055133 .  
## age_group_5_40to44   -0.07345    0.06377  -1.152 0.249801    
## age_group_5_45to49   -0.14779    0.06609  -2.236 0.025631 *  
## age_group_5_50to54   -0.10948    0.06225  -1.759 0.079048 .  
## age_group_5_55to59   -0.05711    0.06463  -0.884 0.377127    
## age_group_5_60to64   -0.09720    0.07062  -1.376 0.169122    
## age_group_5_65_over        NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3244 on 752 degrees of freedom
##   (48 observations deleted due to missingness)
## Multiple R-squared:  0.04554,	Adjusted R-squared:  0.03284 
## F-statistic: 3.588 on 10 and 752 DF,  p-value: 0.0001139
```

```r
linearMod19 <- lm(formula = performance_rating ~ race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=merit_raises_combined_salaried_regression)
summary(linearMod19)
```

```
## 
## Call:
## lm(formula = performance_rating ~ race_grouping_white + race_grouping_person_of_color + 
##     age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + 
##     age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + 
##     age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + 
##     age_group_5_65_over, data = merit_raises_combined_salaried_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.85831 -0.21122 -0.02461  0.20166  0.99313 
## 
## Coefficients: (1 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    3.81831    0.09786  39.020  < 2e-16 ***
## race_grouping_white           -0.11831    0.08200  -1.443  0.14947    
## race_grouping_person_of_color -0.25310    0.08450  -2.995  0.00283 ** 
## age_group_5_under_25          -0.45000    0.14130  -3.185  0.00151 ** 
## age_group_5_25to29            -0.21815    0.06703  -3.255  0.00119 ** 
## age_group_5_30to34            -0.14169    0.06088  -2.327  0.02022 *  
## age_group_5_35to39            -0.09313    0.06165  -1.511  0.13129    
## age_group_5_40to44            -0.04061    0.06390  -0.635  0.52532    
## age_group_5_45to49            -0.13706    0.06537  -2.097  0.03637 *  
## age_group_5_50to54            -0.07541    0.06208  -1.215  0.22483    
## age_group_5_55to59            -0.04649    0.06392  -0.727  0.46726    
## age_group_5_60to64            -0.08878    0.06993  -1.270  0.20460    
## age_group_5_65_over                 NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3204 on 751 degrees of freedom
##   (48 observations deleted due to missingness)
## Multiple R-squared:  0.06981,	Adjusted R-squared:  0.05618 
## F-statistic: 5.124 on 11 and 751 DF,  p-value: 8.908e-08
```

```r
linearMod20 <- lm(formula = performance_rating ~ gender_Female + gender_Male + race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=merit_raises_combined_salaried_regression)
summary(linearMod20)
```

```
## 
## Call:
## lm(formula = performance_rating ~ gender_Female + gender_Male + 
##     race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + 
##     age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + 
##     age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + 
##     age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, 
##     data = merit_raises_combined_salaried_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.83393 -0.22779 -0.02923  0.20972  1.00255 
## 
## Coefficients: (2 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    3.84222    0.09844  39.031  < 2e-16 ***
## gender_Female                 -0.04611    0.02364  -1.950  0.05149 .  
## gender_Male                         NA         NA      NA       NA    
## race_grouping_white           -0.12557    0.08193  -1.533  0.12578    
## race_grouping_person_of_color -0.25452    0.08435  -3.017  0.00264 ** 
## age_group_5_under_25          -0.42822    0.14148  -3.027  0.00256 ** 
## age_group_5_25to29            -0.20809    0.06710  -3.101  0.00200 ** 
## age_group_5_30to34            -0.13661    0.06082  -2.246  0.02499 *  
## age_group_5_35to39            -0.08886    0.06157  -1.443  0.14937    
## age_group_5_40to44            -0.04247    0.06379  -0.666  0.50578    
## age_group_5_45to49            -0.13432    0.06527  -2.058  0.03994 *  
## age_group_5_50to54            -0.07309    0.06197  -1.179  0.23861    
## age_group_5_55to59            -0.04519    0.06381  -0.708  0.47902    
## age_group_5_60to64            -0.08742    0.06980  -1.252  0.21081    
## age_group_5_65_over                 NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3198 on 750 degrees of freedom
##   (48 observations deleted due to missingness)
## Multiple R-squared:  0.0745,	Adjusted R-squared:  0.0597 
## F-statistic: 5.031 on 12 and 750 DF,  p-value: 4.315e-08
```

```r
news_hourly_regression <- news_hourly[,c('department','gender','race_ethnicity','current_base_pay','job_profile_current','cost_center_current','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating','age','years_of_service','age_group_5','years_of_service_grouped','dept','desk','tier','race_grouping')]
news_hourly_regression <- fastDummies::dummy_cols(news_hourly_regression, select_columns = c('gender','race_ethnicity','age_group_5','years_of_service_grouped','dept','desk','tier','race_grouping'))
names(news_hourly_regression) <- gsub(' ', '_', names(news_hourly_regression))
names(news_hourly_regression) <- gsub('-', 'to', names(news_hourly_regression))
names(news_hourly_regression) <- gsub('\\+', '_over', names(news_hourly_regression))
names(news_hourly_regression) <- gsub('<', 'under_', names(news_hourly_regression))

linearMod21 <- lm(formula = current_base_pay ~ gender_Female + gender_Male, data=news_hourly_regression)
summary(linearMod21)
```

```
## 
## Call:
## lm(formula = current_base_pay ~ gender_Female + gender_Male, 
##     data = news_hourly_regression)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -20.179  -6.719  -0.449   6.101  34.131 
## 
## Coefficients: (1 not defined because of singularities)
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     32.031      2.029  15.786   <2e-16 ***
## gender_Female    2.898      2.505   1.157     0.25    
## gender_Male         NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 11.66 on 94 degrees of freedom
## Multiple R-squared:  0.01404,	Adjusted R-squared:  0.003553 
## F-statistic: 1.339 on 1 and 94 DF,  p-value: 0.2502
```

```r
linearMod22 <- lm(formula = current_base_pay ~ race_grouping_white + race_grouping_person_of_color, data=news_hourly_regression)
summary(linearMod22)
```

```
## 
## Call:
## lm(formula = current_base_pay ~ race_grouping_white + race_grouping_person_of_color, 
##     data = news_hourly_regression)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -20.799  -6.485  -0.920   5.844  33.511 
## 
## Coefficients:
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     39.230      8.131   4.825 5.47e-06 ***
## race_grouping_white             -3.681      8.257  -0.446    0.657    
## race_grouping_person_of_color   -9.099      8.397  -1.084    0.281    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 11.5 on 93 degrees of freedom
## Multiple R-squared:  0.05071,	Adjusted R-squared:  0.0303 
## F-statistic: 2.484 on 2 and 93 DF,  p-value: 0.08892
```

```r
linearMod23 <- lm(formula = current_base_pay ~ gender_Female + gender_Male + race_grouping_white + race_grouping_person_of_color, data=news_hourly_regression)
summary(linearMod23)
```

```
## 
## Call:
## lm(formula = current_base_pay ~ gender_Female + gender_Male + 
##     race_grouping_white + race_grouping_person_of_color, data = news_hourly_regression)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -21.839  -5.883  -0.707   5.165  32.471 
## 
## Coefficients: (1 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     36.336      8.486   4.282 4.54e-05 ***
## gender_Female                    2.894      2.481   1.166    0.246    
## gender_Male                         NA         NA      NA       NA    
## race_grouping_white             -2.641      8.289  -0.319    0.751    
## race_grouping_person_of_color   -8.134      8.422  -0.966    0.337    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 11.48 on 92 degrees of freedom
## Multiple R-squared:  0.06455,	Adjusted R-squared:  0.03404 
## F-statistic: 2.116 on 3 and 92 DF,  p-value: 0.1036
```

```r
linearMod24 <- lm(formula = current_base_pay ~ gender_Female + gender_Male + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=news_hourly_regression)
summary(linearMod24)
```

```
## 
## Call:
## lm(formula = current_base_pay ~ gender_Female + gender_Male + 
##     age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + 
##     age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + 
##     age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + 
##     age_group_5_65_over, data = news_hourly_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -21.9861  -6.8240   0.1867   6.5690  21.9307 
## 
## Coefficients: (2 not defined because of singularities)
##                      Estimate Std. Error t value Pr(>|t|)    
## (Intercept)           43.2529     4.9315   8.771 1.57e-13 ***
## gender_Female          3.8764     2.3643   1.640 0.104789    
## gender_Male                NA         NA      NA       NA    
## age_group_5_under_25 -20.4162     5.3074  -3.847 0.000231 ***
## age_group_5_25to29   -17.3859     5.0272  -3.458 0.000851 ***
## age_group_5_30to34   -12.0079     5.5888  -2.149 0.034514 *  
## age_group_5_35to39   -13.9194     5.8204  -2.391 0.018988 *  
## age_group_5_40to44    -7.8709     5.7096  -1.379 0.171659    
## age_group_5_45to49    -0.8132     6.1672  -0.132 0.895405    
## age_group_5_50to54    -6.2127     5.8069  -1.070 0.287704    
## age_group_5_55to59    -9.4947     6.0858  -1.560 0.122442    
## age_group_5_60to64    -3.5887     6.4582  -0.556 0.579887    
## age_group_5_65_over        NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.18 on 85 degrees of freedom
## Multiple R-squared:  0.3194,	Adjusted R-squared:  0.2394 
## F-statistic:  3.99 on 10 and 85 DF,  p-value: 0.000172
```

```r
linearMod25 <- lm(formula = current_base_pay ~ race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=news_hourly_regression)
summary(linearMod25)
```

```
## 
## Call:
## lm(formula = current_base_pay ~ race_grouping_white + race_grouping_person_of_color + 
##     age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + 
##     age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + 
##     age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + 
##     age_group_5_65_over, data = news_hourly_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -21.7944  -8.2916   0.2457   6.6045  23.3800 
## 
## Coefficients: (1 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     57.365      8.810   6.511 5.17e-09 ***
## race_grouping_white             -9.920      7.561  -1.312 0.193086    
## race_grouping_person_of_color  -12.646      7.728  -1.636 0.105485    
## age_group_5_under_25           -20.825      5.346  -3.895 0.000196 ***
## age_group_5_25to29             -17.290      5.045  -3.427 0.000948 ***
## age_group_5_30to34             -15.444      5.590  -2.763 0.007043 ** 
## age_group_5_35to39             -14.666      5.820  -2.520 0.013624 *  
## age_group_5_40to44              -9.303      5.710  -1.629 0.106989    
## age_group_5_45to49              -1.320      6.207  -0.213 0.832088    
## age_group_5_50to54              -6.815      5.831  -1.169 0.245745    
## age_group_5_55to59             -12.189      6.009  -2.028 0.045688 *  
## age_group_5_60to64              -5.454      6.528  -0.836 0.405793    
## age_group_5_65_over                 NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.21 on 84 degrees of freedom
## Multiple R-squared:  0.3243,	Adjusted R-squared:  0.2358 
## F-statistic: 3.665 on 11 and 84 DF,  p-value: 0.0002868
```

```r
linearMod26 <- lm(formula = current_base_pay ~ gender_Female + gender_Male + race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=news_hourly_regression)
summary(linearMod26)
```

```
## 
## Call:
## lm(formula = current_base_pay ~ gender_Female + gender_Male + 
##     race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + 
##     age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + 
##     age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + 
##     age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, 
##     data = news_hourly_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -22.3029  -7.4082  -0.4587   6.2195  22.0580 
## 
## Coefficients: (2 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     53.290      9.243   5.766 1.35e-07 ***
## gender_Female                    3.305      2.385   1.386 0.169551    
## gender_Male                         NA         NA      NA       NA    
## race_grouping_white             -8.590      7.581  -1.133 0.260405    
## race_grouping_person_of_color  -11.063      7.770  -1.424 0.158260    
## age_group_5_under_25           -20.927      5.318  -3.935 0.000172 ***
## age_group_5_25to29             -17.120      5.020  -3.411 0.001003 ** 
## age_group_5_30to34             -13.803      5.685  -2.428 0.017343 *  
## age_group_5_35to39             -14.081      5.803  -2.426 0.017414 *  
## age_group_5_40to44              -8.450      5.712  -1.479 0.142828    
## age_group_5_45to49              -1.371      6.173  -0.222 0.824773    
## age_group_5_50to54              -6.612      5.801  -1.140 0.257613    
## age_group_5_55to59             -10.424      6.111  -1.706 0.091769 .  
## age_group_5_60to64              -4.692      6.516  -0.720 0.473490    
## age_group_5_65_over                 NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.15 on 83 degrees of freedom
## Multiple R-squared:  0.3396,	Adjusted R-squared:  0.2441 
## F-statistic: 3.557 on 12 and 83 DF,  p-value: 0.0002782
```

```r
linearMod27 <- lm(formula = current_base_pay ~ gender_Female + gender_Male + race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over + tier_Tier_1 + tier_Tier_2 + tier_Tier_3 + tier_Tier_4, data=news_hourly_regression)
summary(linearMod27)
```

```
## 
## Call:
## lm(formula = current_base_pay ~ gender_Female + gender_Male + 
##     race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + 
##     age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + 
##     age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + 
##     age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over + 
##     tier_Tier_1 + tier_Tier_2 + tier_Tier_3 + tier_Tier_4, data = news_hourly_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -24.2238  -6.6641  -0.0323   5.5695  21.6317 
## 
## Coefficients: (3 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    53.6848    14.0087   3.832 0.000252 ***
## gender_Female                   3.3510     2.3687   1.415 0.161044    
## gender_Male                         NA         NA      NA       NA    
## race_grouping_white            -6.0059     7.6086  -0.789 0.432234    
## race_grouping_person_of_color  -8.6620     7.7760  -1.114 0.268644    
## age_group_5_under_25          -21.7546     5.2967  -4.107 9.61e-05 ***
## age_group_5_25to29            -19.2538     5.0763  -3.793 0.000288 ***
## age_group_5_30to34            -14.5598     5.6815  -2.563 0.012263 *  
## age_group_5_35to39            -14.7994     5.7933  -2.555 0.012528 *  
## age_group_5_40to44             -9.3640     5.6801  -1.649 0.103157    
## age_group_5_45to49             -1.7920     6.1190  -0.293 0.770392    
## age_group_5_50to54             -6.7158     5.7787  -1.162 0.248626    
## age_group_5_55to59             -9.7711     6.0652  -1.611 0.111116    
## age_group_5_60to64             -6.6075     6.5166  -1.014 0.313667    
## age_group_5_65_over                 NA         NA      NA       NA    
## tier_Tier_1                    -0.6841    10.6737  -0.064 0.949058    
## tier_Tier_2                    -4.7945    10.5300  -0.455 0.650114    
## tier_Tier_3                     0.3514    10.4066   0.034 0.973149    
## tier_Tier_4                         NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.04 on 80 degrees of freedom
## Multiple R-squared:  0.3771,	Adjusted R-squared:  0.2603 
## F-statistic: 3.229 on 15 and 80 DF,  p-value: 0.0003464
```

```r
linearMod28 <- lm(formula = current_base_pay ~ gender_Female + gender_Male + race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over + tier_Tier_1 + tier_Tier_2 + tier_Tier_3 + tier_Tier_4 + years_of_service_grouped_0 + years_of_service_grouped_1to2 + years_of_service_grouped_3to5 + years_of_service_grouped_6to10 + years_of_service_grouped_11to15 + years_of_service_grouped_16to20 + years_of_service_grouped_21to25 + years_of_service_grouped_25_over, data=news_hourly_regression)
summary(linearMod28)
```

```
## 
## Call:
## lm(formula = current_base_pay ~ gender_Female + gender_Male + 
##     race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + 
##     age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + 
##     age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + 
##     age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over + 
##     tier_Tier_1 + tier_Tier_2 + tier_Tier_3 + tier_Tier_4 + years_of_service_grouped_0 + 
##     years_of_service_grouped_1to2 + years_of_service_grouped_3to5 + 
##     years_of_service_grouped_6to10 + years_of_service_grouped_11to15 + 
##     years_of_service_grouped_16to20 + years_of_service_grouped_21to25 + 
##     years_of_service_grouped_25_over, data = news_hourly_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -25.3075  -6.0851  -0.2956   4.8862  21.3493 
## 
## Coefficients: (4 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                        53.931     16.201   3.329 0.001369 ** 
## gender_Female                       3.898      2.602   1.498 0.138366    
## gender_Male                            NA         NA      NA       NA    
## race_grouping_white                -6.516      8.151  -0.799 0.426690    
## race_grouping_person_of_color      -8.812      8.264  -1.066 0.289779    
## age_group_5_under_25              -26.059      7.469  -3.489 0.000825 ***
## age_group_5_25to29                -23.476      7.134  -3.291 0.001541 ** 
## age_group_5_30to34                -18.204      6.875  -2.648 0.009919 ** 
## age_group_5_35to39                -16.802      6.260  -2.684 0.008991 ** 
## age_group_5_40to44                -10.518      6.094  -1.726 0.088617 .  
## age_group_5_45to49                 -3.107      6.913  -0.449 0.654408    
## age_group_5_50to54                 -6.589      6.271  -1.051 0.296836    
## age_group_5_55to59                -10.051      6.443  -1.560 0.123107    
## age_group_5_60to64                 -4.929      7.126  -0.692 0.491289    
## age_group_5_65_over                    NA         NA      NA       NA    
## tier_Tier_1                        -2.363     11.910  -0.198 0.843253    
## tier_Tier_2                        -6.444     11.796  -0.546 0.586533    
## tier_Tier_3                        -1.568     11.794  -0.133 0.894608    
## tier_Tier_4                            NA         NA      NA       NA    
## years_of_service_grouped_0          4.690      8.300   0.565 0.573758    
## years_of_service_grouped_1to2       6.449      7.941   0.812 0.419322    
## years_of_service_grouped_3to5       5.510      8.094   0.681 0.498186    
## years_of_service_grouped_6to10      3.580      6.740   0.531 0.596937    
## years_of_service_grouped_11to15     2.551      7.223   0.353 0.724986    
## years_of_service_grouped_16to20    -1.724      6.531  -0.264 0.792586    
## years_of_service_grouped_21to25     3.795      8.071   0.470 0.639637    
## years_of_service_grouped_25_over       NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 10.33 on 73 degrees of freedom
## Multiple R-squared:  0.3984,	Adjusted R-squared:  0.2171 
## F-statistic: 2.197 on 22 and 73 DF,  p-value: 0.006537
```

```r
merit_raises_combined_hourly_regression <- filter(merit_raises_combined, dept == 'News', pay_rate_type == 'Hourly')

merit_raises_combined_hourly_regression <- fastDummies::dummy_cols(merit_raises_combined_hourly_regression, select_columns = c('gender','race_grouping','age_group_5'))
names(merit_raises_combined_hourly_regression) <- gsub(' ', '_', names(merit_raises_combined_hourly_regression))
names(merit_raises_combined_hourly_regression) <- gsub('-', 'to', names(merit_raises_combined_hourly_regression))
names(merit_raises_combined_hourly_regression) <- gsub('\\+', '_over', names(merit_raises_combined_hourly_regression))
names(merit_raises_combined_hourly_regression) <- gsub('<', 'under_', names(merit_raises_combined_hourly_regression))

linearMod29 <- lm(formula = base_pay_change ~ gender_Female + gender_Male, data=merit_raises_combined_hourly_regression)
summary(linearMod29)
```

```
## 
## Call:
## lm(formula = base_pay_change ~ gender_Female + gender_Male, data = merit_raises_combined_hourly_regression)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.4296 -0.8772 -0.3572  0.1916 11.2704 
## 
## Coefficients: (1 not defined because of singularities)
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     1.3872     0.2213   6.270 6.27e-09 ***
## gender_Female   0.3023     0.2845   1.063     0.29    
## gender_Male         NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.517 on 117 degrees of freedom
## Multiple R-squared:  0.009564,	Adjusted R-squared:  0.001098 
## F-statistic:  1.13 on 1 and 117 DF,  p-value: 0.29
```

```r
linearMod30 <- lm(formula = base_pay_change ~ race_grouping_white + race_grouping_person_of_color, data=merit_raises_combined_hourly_regression)
summary(linearMod30)
```

```
## 
## Call:
## lm(formula = base_pay_change ~ race_grouping_white + race_grouping_person_of_color, 
##     data = merit_raises_combined_hourly_regression)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.4851 -0.7451 -0.2051  0.2399 11.2149 
## 
## Coefficients: (1 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     1.1824     0.2468   4.790 4.92e-06 ***
## race_grouping_white             0.5627     0.2973   1.892   0.0609 .  
## race_grouping_person_of_color       NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.501 on 117 degrees of freedom
## Multiple R-squared:  0.0297,	Adjusted R-squared:  0.0214 
## F-statistic: 3.581 on 1 and 117 DF,  p-value: 0.06092
```

```r
linearMod31 <- lm(formula = base_pay_change ~ gender_Female + gender_Male + race_grouping_white + race_grouping_person_of_color, data=merit_raises_combined_hourly_regression)
summary(linearMod31)
```

```
## 
## Call:
## lm(formula = base_pay_change ~ gender_Female + gender_Male + 
##     race_grouping_white + race_grouping_person_of_color, data = merit_raises_combined_hourly_regression)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.5607 -0.7907 -0.2761  0.2293 11.1393 
## 
## Coefficients: (2 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     1.0747     0.2837   3.788 0.000242 ***
## gender_Female                   0.2214     0.2859   0.775 0.440140    
## gender_Male                         NA         NA      NA       NA    
## race_grouping_white             0.5246     0.3019   1.738 0.084920 .  
## race_grouping_person_of_color       NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.504 on 116 degrees of freedom
## Multiple R-squared:  0.03469,	Adjusted R-squared:  0.01805 
## F-statistic: 2.084 on 2 and 116 DF,  p-value: 0.129
```

```r
linearMod32 <- lm(formula = base_pay_change ~ gender_Female + gender_Male + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=merit_raises_combined_hourly_regression)
summary(linearMod32)
```

```
## 
## Call:
## lm(formula = base_pay_change ~ gender_Female + gender_Male + 
##     age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + 
##     age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + 
##     age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + 
##     age_group_5_65_over, data = merit_raises_combined_hourly_regression)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.1687 -0.7028 -0.2051  0.1201 11.2348 
## 
## Coefficients: (2 not defined because of singularities)
##                      Estimate Std. Error t value Pr(>|t|)  
## (Intercept)           1.37422    0.72535   1.895   0.0608 .
## gender_Female         0.25473    0.31068   0.820   0.4141  
## gender_Male                NA         NA      NA       NA  
## age_group_5_under_25 -0.94913    1.12219  -0.846   0.3995  
## age_group_5_25to29    0.09621    0.74907   0.128   0.8980  
## age_group_5_30to34    0.08219    0.78051   0.105   0.9163  
## age_group_5_35to39   -0.16741    0.81641  -0.205   0.8379  
## age_group_5_40to44    0.20675    0.81641   0.253   0.8006  
## age_group_5_45to49    0.13063    0.78795   0.166   0.8686  
## age_group_5_50to54   -0.24684    0.79405  -0.311   0.7565  
## age_group_5_55to59   -0.20909    0.82883  -0.252   0.8013  
## age_group_5_60to64    1.81974    1.02228   1.780   0.0779 .
## age_group_5_65_over        NA         NA      NA       NA  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.524 on 108 degrees of freedom
## Multiple R-squared:  0.0775,	Adjusted R-squared:  -0.007921 
## F-statistic: 0.9073 on 10 and 108 DF,  p-value: 0.5293
```

```r
linearMod33 <- lm(formula = base_pay_change ~ race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=merit_raises_combined_hourly_regression)
summary(linearMod33)
```

```
## 
## Call:
## lm(formula = base_pay_change ~ race_grouping_white + race_grouping_person_of_color + 
##     age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + 
##     age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + 
##     age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + 
##     age_group_5_65_over, data = merit_raises_combined_hourly_regression)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.1050 -0.7879 -0.2464  0.3048 10.9892 
## 
## Coefficients: (2 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)  
## (Intercept)                    1.33904    0.68508   1.955   0.0532 .
## race_grouping_white            0.59741    0.32158   1.858   0.0659 .
## race_grouping_person_of_color       NA         NA      NA       NA  
## age_group_5_under_25          -1.42644    1.11564  -1.279   0.2038  
## age_group_5_25to29             0.03434    0.74037   0.046   0.9631  
## age_group_5_30to34            -0.33672    0.77282  -0.436   0.6639  
## age_group_5_35to39            -0.30357    0.80155  -0.379   0.7056  
## age_group_5_40to44            -0.12854    0.81293  -0.158   0.8747  
## age_group_5_45to49            -0.06264    0.78171  -0.080   0.9363  
## age_group_5_50to54            -0.46271    0.79365  -0.583   0.5611  
## age_group_5_55to59            -0.43830    0.81509  -0.538   0.5919  
## age_group_5_60to64             1.44856    1.02761   1.410   0.1615  
## age_group_5_65_over                 NA         NA      NA       NA  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.505 on 108 degrees of freedom
## Multiple R-squared:  0.1005,	Adjusted R-squared:  0.01721 
## F-statistic: 1.207 on 10 and 108 DF,  p-value: 0.2949
```

```r
linearMod34 <- lm(formula = base_pay_change ~ gender_Female + gender_Male + race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=merit_raises_combined_hourly_regression)
summary(linearMod34)
```

```
## 
## Call:
## lm(formula = base_pay_change ~ gender_Female + gender_Male + 
##     race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + 
##     age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + 
##     age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + 
##     age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, 
##     data = merit_raises_combined_hourly_regression)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.1318 -0.7714 -0.2091  0.2843 10.9822 
## 
## Coefficients: (3 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)  
## (Intercept)                    1.26541    0.72209   1.752   0.0826 .
## gender_Female                  0.10738    0.32017   0.335   0.7380  
## gender_Male                         NA         NA      NA       NA  
## race_grouping_white            0.56673    0.33561   1.689   0.0942 .
## race_grouping_person_of_color       NA         NA      NA       NA  
## age_group_5_under_25          -1.35793    1.13873  -1.192   0.2357  
## age_group_5_25to29             0.03831    0.74352   0.052   0.9590  
## age_group_5_30to34            -0.26955    0.80144  -0.336   0.7373  
## age_group_5_35to39            -0.26829    0.81171  -0.331   0.7416  
## age_group_5_40to44            -0.08304    0.82749  -0.100   0.9203  
## age_group_5_45to49            -0.04014    0.78780  -0.051   0.9595  
## age_group_5_50to54            -0.45702    0.79711  -0.573   0.5676  
## age_group_5_55to59            -0.39395    0.82907  -0.475   0.6356  
## age_group_5_60to64             1.47233    1.03429   1.424   0.1575  
## age_group_5_65_over                 NA         NA      NA       NA  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.511 on 107 degrees of freedom
## Multiple R-squared:  0.1014,	Adjusted R-squared:  0.009067 
## F-statistic: 1.098 on 11 and 107 DF,  p-value: 0.3699
```

```r
linearMod35 <- lm(formula = performance_rating ~ gender_Female + gender_Male, data=merit_raises_combined_hourly_regression)
summary(linearMod35)
```

```
## 
## Call:
## lm(formula = performance_rating ~ gender_Female + gender_Male, 
##     data = merit_raises_combined_hourly_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.59767 -0.26943 -0.09767  0.25882  0.90233 
## 
## Coefficients: (1 not defined because of singularities)
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    3.49767    0.05346  65.428   <2e-16 ***
## gender_Female  0.04350    0.06830   0.637    0.526    
## gender_Male         NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3505 on 109 degrees of freedom
##   (8 observations deleted due to missingness)
## Multiple R-squared:  0.003708,	Adjusted R-squared:  -0.005432 
## F-statistic: 0.4057 on 1 and 109 DF,  p-value: 0.5255
```

```r
linearMod36 <- lm(formula = performance_rating ~ race_grouping_white + race_grouping_person_of_color, data=merit_raises_combined_hourly_regression)
summary(linearMod36)
```

```
## 
## Call:
## lm(formula = performance_rating ~ race_grouping_white + race_grouping_person_of_color, 
##     data = merit_raises_combined_hourly_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.67467 -0.27467 -0.01944  0.22533  0.82533 
## 
## Coefficients: (1 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    3.41944    0.05724  59.735   <2e-16 ***
## race_grouping_white            0.15522    0.06964   2.229   0.0279 *  
## race_grouping_person_of_color       NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3435 on 109 degrees of freedom
##   (8 observations deleted due to missingness)
## Multiple R-squared:  0.04359,	Adjusted R-squared:  0.03482 
## F-statistic: 4.968 on 1 and 109 DF,  p-value: 0.02787
```

```r
linearMod37 <- lm(formula = performance_rating ~ gender_Female + gender_Male + race_grouping_white + race_grouping_person_of_color, data=merit_raises_combined_hourly_regression)
summary(linearMod37)
```

```
## 
## Call:
## lm(formula = performance_rating ~ gender_Female + gender_Male + 
##     race_grouping_white + race_grouping_person_of_color, data = merit_raises_combined_hourly_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.66495 -0.27924 -0.02699  0.23505  0.83505 
## 
## Coefficients: (2 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    3.41270    0.06599  51.713   <2e-16 ***
## gender_Female                  0.01429    0.06860   0.208   0.8354    
## gender_Male                         NA         NA      NA       NA    
## race_grouping_white            0.15225    0.07138   2.133   0.0352 *  
## race_grouping_person_of_color       NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.345 on 108 degrees of freedom
##   (8 observations deleted due to missingness)
## Multiple R-squared:  0.04398,	Adjusted R-squared:  0.02627 
## F-statistic: 2.484 on 2 and 108 DF,  p-value: 0.08817
```

```r
linearMod38 <- lm(formula = performance_rating ~ gender_Female + gender_Male + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=merit_raises_combined_hourly_regression)
summary(linearMod38)
```

```
## 
## Call:
## lm(formula = performance_rating ~ gender_Female + gender_Male + 
##     age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + 
##     age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + 
##     age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + 
##     age_group_5_65_over, data = merit_raises_combined_hourly_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.77659 -0.26798 -0.07659  0.22153  0.74303 
## 
## Coefficients: (2 not defined because of singularities)
##                      Estimate Std. Error t value Pr(>|t|)    
## (Intercept)           3.52255    0.16374  21.512   <2e-16 ***
## gender_Female         0.04682    0.07343   0.638    0.525    
## gender_Male                NA         NA      NA       NA    
## age_group_5_under_25 -0.24596    0.28679  -0.858    0.393    
## age_group_5_25to29   -0.08878    0.16801  -0.528    0.598    
## age_group_5_30to34   -0.04220    0.17780  -0.237    0.813    
## age_group_5_35to39   -0.21655    0.18607  -1.164    0.247    
## age_group_5_40to44    0.25404    0.18325   1.386    0.169    
## age_group_5_45to49   -0.01376    0.17676  -0.078    0.938    
## age_group_5_50to54    0.01885    0.18008   0.105    0.917    
## age_group_5_55to59    0.03442    0.19241   0.179    0.858    
## age_group_5_60to64   -0.22042    0.24978  -0.882    0.380    
## age_group_5_65_over        NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3418 on 100 degrees of freedom
##   (8 observations deleted due to missingness)
## Multiple R-squared:  0.1312,	Adjusted R-squared:  0.04432 
## F-statistic:  1.51 on 10 and 100 DF,  p-value: 0.1468
```

```r
linearMod39 <- lm(formula = performance_rating ~ race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=merit_raises_combined_hourly_regression)
summary(linearMod39)
```

```
## 
## Call:
## lm(formula = performance_rating ~ race_grouping_white + race_grouping_person_of_color + 
##     age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + 
##     age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + 
##     age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + 
##     age_group_5_65_over, data = merit_raises_combined_hourly_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.68218 -0.24318 -0.04153  0.19243  0.69655 
## 
## Coefficients: (2 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    3.50345    0.15323  22.865   <2e-16 ***
## race_grouping_white            0.14139    0.07338   1.927   0.0569 .  
## race_grouping_person_of_color       NA         NA      NA       NA    
## age_group_5_under_25          -0.34483    0.28477  -1.211   0.2288    
## age_group_5_25to29            -0.10331    0.16547  -0.624   0.5339    
## age_group_5_30to34            -0.13408    0.17470  -0.767   0.4446    
## age_group_5_35to39            -0.24044    0.18142  -1.325   0.1881    
## age_group_5_40to44             0.17873    0.18180   0.983   0.3279    
## age_group_5_45to49            -0.05770    0.17475  -0.330   0.7419    
## age_group_5_50to54            -0.02759    0.17902  -0.154   0.8778    
## age_group_5_55to59            -0.01993    0.18858  -0.106   0.9161    
## age_group_5_60to64            -0.31150    0.24950  -1.249   0.2148    
## age_group_5_65_over                 NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3363 on 100 degrees of freedom
##   (8 observations deleted due to missingness)
## Multiple R-squared:  0.1589,	Adjusted R-squared:  0.07478 
## F-statistic: 1.889 on 10 and 100 DF,  p-value: 0.05533
```

```r
linearMod40 <- lm(formula = performance_rating ~ gender_Female + gender_Male + race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=merit_raises_combined_hourly_regression)
summary(linearMod40)
```

```
## 
## Call:
## lm(formula = performance_rating ~ gender_Female + gender_Male + 
##     race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + 
##     age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + 
##     age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + 
##     age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, 
##     data = merit_raises_combined_hourly_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.68129 -0.24287 -0.04171  0.19235  0.69532 
## 
## Coefficients: (3 not defined because of singularities)
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    3.500910   0.162366  21.562   <2e-16 ***
## gender_Female                  0.003766   0.076429   0.049   0.9608    
## gender_Male                          NA         NA      NA       NA    
## race_grouping_white            0.140193   0.077622   1.806   0.0739 .  
## race_grouping_person_of_color        NA         NA      NA       NA    
## age_group_5_under_25          -0.342986   0.288645  -1.188   0.2376    
## age_group_5_25to29            -0.103155   0.166335  -0.620   0.5366    
## age_group_5_30to34            -0.131600   0.182655  -0.720   0.4729    
## age_group_5_35to39            -0.239073   0.184425  -1.296   0.1979    
## age_group_5_40to44             0.180380   0.185744   0.971   0.3339    
## age_group_5_45to49            -0.056883   0.176417  -0.322   0.7478    
## age_group_5_50to54            -0.027612   0.179923  -0.153   0.8783    
## age_group_5_55to59            -0.018268   0.192492  -0.095   0.9246    
## age_group_5_60to64            -0.310280   0.251966  -1.231   0.2211    
## age_group_5_65_over                  NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.338 on 99 degrees of freedom
##   (8 observations deleted due to missingness)
## Multiple R-squared:  0.1589,	Adjusted R-squared:  0.06545 
## F-statistic:   1.7 on 11 and 99 DF,  p-value: 0.08412
```

## Commercial

### Gender


```r
current_commercial_gender_salaried <- commercial_salaried %>% group_by(gender)
current_commercial_gender_salaried <- current_commercial_gender_salaried %>% summarise(
  count = length(current_base_pay)
)
suppress(current_commercial_gender_salaried)
```

```
## # A tibble: 2 x 2
##   gender count
##   <chr>  <int>
## 1 Female    86
## 2 Male      47
```

```r
current_commercial_gender_hourly <- commercial_hourly %>% group_by(gender)
current_commercial_gender_hourly <- current_commercial_gender_hourly %>% summarise(
  count = length(current_base_pay)
)
suppress(current_commercial_gender_hourly)
```

```
## # A tibble: 2 x 2
##   gender count
##   <chr>  <int>
## 1 Female    74
## 2 Male      73
```

```r
current_commercial_gender_salaried_median <- commercial_salaried %>% group_by(gender)
current_commercial_gender_salaried_median <- current_commercial_gender_salaried_median %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_gender_salaried_median)
```

```
## # A tibble: 2 x 3
##   gender count median
##   <chr>  <int>  <dbl>
## 1 Female    86 85977.
## 2 Male      47 86880
```

```r
current_commercial_gender_hourly_median <- commercial_hourly %>% group_by(gender)
current_commercial_gender_hourly_median <- current_commercial_gender_hourly_median %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_gender_hourly_median)
```

```
## # A tibble: 2 x 3
##   gender count median
##   <chr>  <int>  <dbl>
## 1 Female    74   28.9
## 2 Male      73   23.4
```

```r
current_commercial_gender_age_salaried <- commercial_salaried %>% group_by(gender)
current_commercial_gender_age_salaried %>% summarise(
  median_age = median(age)
)
```

```
## # A tibble: 2 x 2
##   gender median_age
##   <chr>       <dbl>
## 1 Female         32
## 2 Male           39
```

```r
current_commercial_gender_age_hourly <- commercial_hourly %>% group_by(gender)
current_commercial_gender_age_hourly %>% summarise(
  median_age = median(age)
)
```

```
## # A tibble: 2 x 2
##   gender median_age
##   <chr>       <dbl>
## 1 Female       43.5
## 2 Male         47
```

```r
current_commercial_gender_age_5_salary <- commercial_salaried %>% group_by(age_group_5, gender)
current_commercial_gender_age_5_salary <- current_commercial_gender_age_5_salary %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_gender_age_5_salary)
```

```
## # A tibble: 14 x 4
## # Groups:   age_group_5 [9]
##    age_group_5 gender count  median
##    <fct>       <chr>  <int>   <dbl>
##  1 <25         Female     8  63500 
##  2 25-29       Female    29  75000 
##  3 25-29       Male       6  79140 
##  4 30-34       Female     9 100000 
##  5 30-34       Male       7  97696.
##  6 35-39       Female     9 149101 
##  7 35-39       Male       9  77627.
##  8 40-44       Female     8 124288.
##  9 45-49       Female     7  90585 
## 10 45-49       Male       6  85090.
## 11 50-54       Female     7  90669.
## 12 55-59       Female     5  96780 
## 13 55-59       Male       5  97135.
## 14 60-64       Male       6  95754.
```

```r
current_commercial_gender_age_5_hourly <- commercial_hourly %>% group_by(age_group_5, gender)
current_commercial_gender_age_5_hourly <- current_commercial_gender_age_5_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_gender_age_5_hourly)
```

```
## # A tibble: 18 x 4
## # Groups:   age_group_5 [10]
##    age_group_5 gender count median
##    <fct>       <chr>  <int>  <dbl>
##  1 <25         Male       7   23.1
##  2 25-29       Female    14   31.8
##  3 25-29       Male       8   26.2
##  4 30-34       Female     6   30.3
##  5 35-39       Female     5   30.8
##  6 35-39       Male       8   30.6
##  7 40-44       Female    12   29.5
##  8 40-44       Male       5   21.5
##  9 45-49       Female     7   31.3
## 10 45-49       Male      10   22.4
## 11 50-54       Female     6   23.3
## 12 50-54       Male      12   24.1
## 13 55-59       Female     9   26.4
## 14 55-59       Male       7   23.4
## 15 60-64       Female     6   24.5
## 16 60-64       Male       7   24.3
## 17 65+         Female     5   27.7
## 18 65+         Male       6   22.7
```

```r
current_commercial_gender_age_10_salary <- commercial_salaried %>% group_by(age_group_10, gender)
current_commercial_gender_age_10_salary <- current_commercial_gender_age_10_salary %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_gender_age_10_salary)
```

```
## # A tibble: 9 x 4
## # Groups:   age_group_10 [5]
##   age_group_10 gender count  median
##   <fct>        <chr>  <int>   <dbl>
## 1 <25          Female     8  63500 
## 2 25-34        Female    38  80212 
## 3 25-34        Male      13  86880 
## 4 35-44        Female    17 143576.
## 5 35-44        Male      10  84029.
## 6 45-54        Female    14  90627.
## 7 45-54        Male       9  85000 
## 8 55-64        Female     9  96780 
## 9 55-64        Male      11  97135.
```

```r
current_commercial_gender_age_10_hourly <- commercial_hourly %>% group_by(age_group_10, gender)
current_commercial_gender_age_10_hourly <- current_commercial_gender_age_10_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_gender_age_10_hourly)
```

```
## # A tibble: 11 x 4
## # Groups:   age_group_10 [6]
##    age_group_10 gender count median
##    <fct>        <chr>  <int>  <dbl>
##  1 <25          Male       7   23.1
##  2 25-34        Female    20   31.0
##  3 25-34        Male      11   26.0
##  4 35-44        Female    17   29.7
##  5 35-44        Male      13   27.2
##  6 45-54        Female    13   26.1
##  7 45-54        Male      22   23.5
##  8 55-64        Female    15   25.4
##  9 55-64        Male      14   23.9
## 10 65+          Female     5   27.7
## 11 65+          Male       6   22.7
```

```r
current_commercial_gender_salaried_under_40 <- filter(commercial_salaried, age < 40) %>% group_by(gender)
current_commercial_gender_salaried_under_40 <- current_commercial_gender_salaried_under_40 %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_gender_salaried_under_40)
```

```
## # A tibble: 2 x 3
##   gender count median
##   <chr>  <int>  <dbl>
## 1 Female    55  80424
## 2 Male      24  83140
```

```r
current_commercial_gender_salaried_over_40 <- filter(commercial_salaried, age > 39) %>% group_by(gender)
current_commercial_gender_salaried_over_40 <- current_commercial_gender_salaried_over_40 %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_gender_salaried_over_40)
```

```
## # A tibble: 2 x 3
##   gender count median
##   <chr>  <int>  <dbl>
## 1 Female    31  96780
## 2 Male      23  90000
```

```r
current_commercial_gender_hourly_under_40 <- filter(commercial_hourly, age < 40) %>% group_by(gender)
current_commercial_gender_hourly_under_40 <- current_commercial_gender_hourly_under_40 %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_gender_hourly_under_40)
```

```
## # A tibble: 2 x 3
##   gender count median
##   <chr>  <int>  <dbl>
## 1 Female    29   30.4
## 2 Male      26   26.5
```

```r
current_commercial_gender_hourly_over_40 <- filter(commercial_hourly, age > 39) %>% group_by(gender)
current_commercial_gender_hourly_over_40 <- current_commercial_gender_hourly_over_40 %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_gender_hourly_over_40)
```

```
## # A tibble: 2 x 3
##   gender count median
##   <chr>  <int>  <dbl>
## 1 Female    45   27.7
## 2 Male      47   23.2
```

### Race and ethnicity


```r
current_commercial_race_salaried <- commercial_salaried %>% group_by(race_ethnicity)
current_commercial_race_salaried <- current_commercial_race_salaried %>% summarise(
  count = length(current_base_pay)
)
suppress_count(current_commercial_race_salaried)
```

```
## # A tibble: 4 x 2
##   race_ethnicity                                       count
##   <chr>                                                <int>
## 1 White (United States of America)                        99
## 2 Black or African American (United States of America)    14
## 3 Asian (United States of America)                        13
## 4 Hispanic or Latino (United States of America)            5
```

```r
current_commercial_race_hourly <- commercial_hourly %>% group_by(race_ethnicity)
current_commercial_race_hourly <- current_commercial_race_hourly %>% summarise(
  count = length(current_base_pay)
)
suppress_count(current_commercial_race_hourly)
```

```
## # A tibble: 4 x 2
##   race_ethnicity                                       count
##   <chr>                                                <int>
## 1 Black or African American (United States of America)    82
## 2 White (United States of America)                        43
## 3 Hispanic or Latino (United States of America)            9
## 4 Asian (United States of America)                         7
```

```r
current_commercial_race_group_salaried <- commercial_salaried %>% group_by(race_grouping)
current_commercial_race_group_salaried <- current_commercial_race_group_salaried %>% summarise(
  count = length(current_base_pay)
)
suppress_count(current_commercial_race_group_salaried)
```

```
## # A tibble: 2 x 2
##   race_grouping   count
##   <chr>           <int>
## 1 white              99
## 2 person of color    32
```

```r
current_commercial_race_group_hourly <- commercial_hourly %>% group_by(race_grouping)
current_commercial_race_group_hourly <- current_commercial_race_group_hourly %>% summarise(
  count = length(current_base_pay)
)
suppress_count(current_commercial_race_group_hourly)
```

```
## # A tibble: 2 x 2
##   race_grouping   count
##   <chr>           <int>
## 1 person of color   101
## 2 white              43
```

```r
current_commercial_race_salaried_median <- commercial_salaried %>% group_by(race_ethnicity)
current_commercial_race_salaried_median <- current_commercial_race_salaried_median %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_race_salaried_median)
```

```
## # A tibble: 4 x 3
##   race_ethnicity                                       count median
##   <chr>                                                <int>  <dbl>
## 1 White (United States of America)                        99  88000
## 2 Black or African American (United States of America)    14  84640
## 3 Asian (United States of America)                        13  80000
## 4 Hispanic or Latino (United States of America)            5  80000
```

```r
current_commercial_race_hourly_median <- commercial_hourly %>% group_by(race_ethnicity)
current_commercial_race_hourly_median <- current_commercial_race_hourly_median %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_race_hourly_median)
```

```
## # A tibble: 4 x 3
##   race_ethnicity                                       count median
##   <chr>                                                <int>  <dbl>
## 1 White (United States of America)                        43   30.4
## 2 Asian (United States of America)                         7   26.0
## 3 Black or African American (United States of America)    82   24.9
## 4 Hispanic or Latino (United States of America)            9   23.1
```

```r
current_commercial_race_group_salaried_median <- commercial_salaried %>% group_by(race_grouping)
current_commercial_race_group_salaried_median <- current_commercial_race_group_salaried_median %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_race_group_salaried_median)
```

```
## # A tibble: 2 x 3
##   race_grouping   count median
##   <chr>           <int>  <dbl>
## 1 white              99 88000 
## 2 person of color    32 83445.
```

```r
current_commercial_race_group_hourly_median <- commercial_hourly %>% group_by(race_grouping)
current_commercial_race_group_hourly_median <- current_commercial_race_group_hourly_median %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_race_group_hourly_median)
```

```
## # A tibble: 2 x 3
##   race_grouping   count median
##   <chr>           <int>  <dbl>
## 1 white              43   30.4
## 2 person of color   101   25.2
```

```r
current_commercial_race_age_salaried <- commercial_salaried %>% group_by(race_ethnicity)
current_commercial_race_age_salaried %>% summarise(
  median_age = median(age)
)
```

```
## # A tibble: 5 x 2
##   race_ethnicity                                       median_age
##   <chr>                                                     <dbl>
## 1 Asian (United States of America)                           32  
## 2 Black or African American (United States of America)       48  
## 3 Hispanic or Latino (United States of America)              41  
## 4 Prefer Not to Disclose (United States of America)          35.5
## 5 White (United States of America)                           35
```

```r
current_commercial_race_age_hourly <- commercial_hourly %>% group_by(race_ethnicity)
current_commercial_race_age_hourly %>% summarise(
  median_age = median(age)
)
```

```
## # A tibble: 7 x 2
##   race_ethnicity                                              median_age
##   <chr>                                                            <dbl>
## 1 American Indian or Alaska Native (United States of America)       38  
## 2 Asian (United States of America)                                  28  
## 3 Black or African American (United States of America)              48.5
## 4 Hispanic or Latino (United States of America)                     30  
## 5 Prefer Not to Disclose (United States of America)                 35  
## 6 Two or More Races (United States of America)                      31  
## 7 White (United States of America)                                  39
```

```r
current_commercial_race_age_5_salary <- commercial_salaried %>% group_by(age_group_5, race_ethnicity)
current_commercial_race_age_5_salary <- current_commercial_race_age_5_salary %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_race_age_5_salary)
```

```
## # A tibble: 9 x 4
## # Groups:   age_group_5 [9]
##   age_group_5 race_ethnicity                   count  median
##   <fct>       <chr>                            <int>   <dbl>
## 1 <25         White (United States of America)     9  63000 
## 2 25-29       White (United States of America)    28  78692.
## 3 30-34       White (United States of America)    12  98848.
## 4 35-39       White (United States of America)    13 149101 
## 5 40-44       White (United States of America)     6 126865.
## 6 45-49       White (United States of America)     7  90000 
## 7 50-54       White (United States of America)     9  87392.
## 8 55-59       White (United States of America)     8  96957.
## 9 60-64       White (United States of America)     6  97651.
```

```r
current_commercial_race_age_5_hourly <- commercial_hourly %>% group_by(age_group_5, race_ethnicity)
current_commercial_race_age_5_hourly <- current_commercial_race_age_5_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_race_age_5_hourly)
```

```
## # A tibble: 11 x 4
## # Groups:   age_group_5 [9]
##    age_group_5 race_ethnicity                                  count median
##    <fct>       <chr>                                           <int>  <dbl>
##  1 <25         Black or African American (United States of Am…     5   22.4
##  2 25-29       White (United States of America)                   11   31.8
##  3 35-39       White (United States of America)                    6   30.8
##  4 40-44       Black or African American (United States of Am…    13   28.9
##  5 45-49       Black or African American (United States of Am…    14   23.1
##  6 50-54       Black or African American (United States of Am…    12   23.3
##  7 50-54       White (United States of America)                    5   24.4
##  8 55-59       Black or African American (United States of Am…    11   27.0
##  9 55-59       White (United States of America)                    5   25.4
## 10 60-64       Black or African American (United States of Am…    11   24.3
## 11 65+         Black or African American (United States of Am…     5   23.4
```

```r
current_commercial_race_age_10_salary <- commercial_salaried %>% group_by(age_group_10, race_ethnicity)
current_commercial_race_age_10_salary <- current_commercial_race_age_10_salary %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_race_age_10_salary)
```

```
## # A tibble: 6 x 4
## # Groups:   age_group_10 [5]
##   age_group_10 race_ethnicity                   count  median
##   <fct>        <chr>                            <int>   <dbl>
## 1 <25          White (United States of America)     9  63000 
## 2 25-34        Asian (United States of America)     6  82418.
## 3 25-34        White (United States of America)    40  82000 
## 4 35-44        White (United States of America)    19 148730.
## 5 45-54        White (United States of America)    16  88696.
## 6 55-64        White (United States of America)    14  97325.
```

```r
current_commercial_race_age_10_hourly <- commercial_hourly %>% group_by(age_group_10, race_ethnicity)
current_commercial_race_age_10_hourly <- current_commercial_race_age_10_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_race_age_10_hourly)
```

```
## # A tibble: 11 x 4
## # Groups:   age_group_10 [6]
##    age_group_10 race_ethnicity                                 count median
##    <fct>        <chr>                                          <int>  <dbl>
##  1 <25          Black or African American (United States of A…     5   22.4
##  2 25-34        Black or African American (United States of A…     7   26.7
##  3 25-34        Hispanic or Latino (United States of America)      6   25.0
##  4 25-34        White (United States of America)                  12   31.8
##  5 35-44        Black or African American (United States of A…    17   29.2
##  6 35-44        White (United States of America)                   8   30.6
##  7 45-54        Black or African American (United States of A…    26   23.3
##  8 45-54        White (United States of America)                   8   30.8
##  9 55-64        Black or African American (United States of A…    22   24.5
## 10 55-64        White (United States of America)                   7   26.4
## 11 65+          Black or African American (United States of A…     5   23.4
```

```r
current_commercial_race_group_age_5_salary <- commercial_salaried %>% group_by(age_group_5, race_grouping)
current_commercial_race_group_age_5_salary <- current_commercial_race_group_age_5_salary %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_race_group_age_5_salary)
```

```
## # A tibble: 12 x 4
## # Groups:   age_group_5 [9]
##    age_group_5 race_grouping   count  median
##    <fct>       <chr>           <int>   <dbl>
##  1 <25         white               9  63000 
##  2 25-29       person of color     7  72000 
##  3 25-29       white              28  78692.
##  4 30-34       white              12  98848.
##  5 35-39       person of color     5  73522.
##  6 35-39       white              13 149101 
##  7 40-44       white               6 126865.
##  8 45-49       person of color     6  85450.
##  9 45-49       white               7  90000 
## 10 50-54       white               9  87392.
## 11 55-59       white               8  96957.
## 12 60-64       white               6  97651.
```

```r
current_commercial_race_group_age_5_hourly <- commercial_hourly %>% group_by(age_group_5, race_grouping)
current_commercial_race_group_age_5_hourly <- current_commercial_race_group_age_5_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_race_group_age_5_hourly)
```

```
## # A tibble: 14 x 4
## # Groups:   age_group_5 [10]
##    age_group_5 race_grouping   count median
##    <fct>       <chr>           <int>  <dbl>
##  1 <25         person of color     7   25.6
##  2 25-29       person of color    10   26.3
##  3 25-29       white              11   31.8
##  4 30-34       person of color     8   28.8
##  5 35-39       person of color     6   30.8
##  6 35-39       white               6   30.8
##  7 40-44       person of color    14   28.5
##  8 45-49       person of color    14   23.1
##  9 50-54       person of color    13   23.2
## 10 50-54       white               5   24.4
## 11 55-59       person of color    11   27.0
## 12 55-59       white               5   25.4
## 13 60-64       person of color    11   24.3
## 14 65+         person of color     7   23.4
```

```r
current_commercial_race_group_age_10_salary <- commercial_salaried %>% group_by(age_group_10, race_grouping)
current_commercial_race_group_age_10_salary <- current_commercial_race_group_age_10_salary %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_race_group_age_10_salary)
```

```
## # A tibble: 9 x 4
## # Groups:   age_group_10 [5]
##   age_group_10 race_grouping   count  median
##   <fct>        <chr>           <int>   <dbl>
## 1 <25          white               9  63000 
## 2 25-34        person of color    10  74918.
## 3 25-34        white              40  82000 
## 4 35-44        person of color     7  90431.
## 5 35-44        white              19 148730.
## 6 45-54        person of color     7  85000 
## 7 45-54        white              16  88696.
## 8 55-64        person of color     6  82709.
## 9 55-64        white              14  97325.
```

```r
current_commercial_race_group_age_10_hourly <- commercial_hourly %>% group_by(age_group_10, race_grouping)
current_commercial_race_group_age_10_hourly <- current_commercial_race_group_age_10_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_race_group_age_10_hourly)
```

```
## # A tibble: 10 x 4
## # Groups:   age_group_10 [6]
##    age_group_10 race_grouping   count median
##    <fct>        <chr>           <int>  <dbl>
##  1 <25          person of color     7   25.6
##  2 25-34        person of color    18   26.5
##  3 25-34        white              12   31.8
##  4 35-44        person of color    20   29.1
##  5 35-44        white               8   30.6
##  6 45-54        person of color    27   23.2
##  7 45-54        white               8   30.8
##  8 55-64        person of color    22   24.5
##  9 55-64        white               7   26.4
## 10 65+          person of color     7   23.4
```

```r
current_commercial_race_salaried_under_40 <- filter(commercial_salaried, age < 40) %>% group_by(race_ethnicity)
current_commercial_race_salaried_under_40 <- current_commercial_race_salaried_under_40 %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_race_salaried_under_40)
```

```
## # A tibble: 2 x 3
##   race_ethnicity                   count median
##   <chr>                            <int>  <dbl>
## 1 White (United States of America)    62 82000 
## 2 Asian (United States of America)    10 77418.
```

```r
current_commercial_race_salaried_over_40 <- filter(commercial_salaried, age > 39) %>% group_by(race_ethnicity)
current_commercial_race_salaried_over_40 <- current_commercial_race_salaried_over_40 %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_race_salaried_over_40)
```

```
## # A tibble: 2 x 3
##   race_ethnicity                                       count median
##   <chr>                                                <int>  <dbl>
## 1 White (United States of America)                        37 97135.
## 2 Black or African American (United States of America)    10 84849.
```

```r
current_commercial_race_hourly_under_40 <- filter(commercial_hourly, age < 40) %>% group_by(race_ethnicity)
current_commercial_race_hourly_under_40 <- current_commercial_race_hourly_under_40 %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_race_hourly_under_40)
```

```
## # A tibble: 3 x 3
##   race_ethnicity                                       count median
##   <chr>                                                <int>  <dbl>
## 1 White (United States of America)                        22   31.5
## 2 Black or African American (United States of America)    16   26.5
## 3 Hispanic or Latino (United States of America)            8   25.6
```

```r
current_commercial_race_hourly_over_40 <- filter(commercial_hourly, age > 39) %>% group_by(race_ethnicity)
current_commercial_race_hourly_over_40 <- current_commercial_race_hourly_over_40 %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_race_hourly_over_40)
```

```
## # A tibble: 2 x 3
##   race_ethnicity                                       count median
##   <chr>                                                <int>  <dbl>
## 1 White (United States of America)                        21   29.2
## 2 Black or African American (United States of America)    66   24.4
```

### Gender x race/ethnicity


```r
current_commercial_race_gender_salaried <- commercial_salaried %>% group_by(race_ethnicity, gender)
current_commercial_race_gender_salaried <- current_commercial_race_gender_salaried %>% summarise(
  count = length(current_base_pay)
)
suppress(current_commercial_race_gender_salaried)
```

```
## # A tibble: 6 x 3
## # Groups:   race_ethnicity [3]
##   race_ethnicity                                       gender count
##   <chr>                                                <chr>  <int>
## 1 Asian (United States of America)                     Female     8
## 2 Asian (United States of America)                     Male       5
## 3 Black or African American (United States of America) Female     7
## 4 Black or African American (United States of America) Male       7
## 5 White (United States of America)                     Female    67
## 6 White (United States of America)                     Male      32
```

```r
current_commercial_race_gender_hourly <- commercial_hourly %>% group_by(race_ethnicity, gender)
current_commercial_race_gender_hourly <- current_commercial_race_gender_hourly %>% summarise(
  count = length(current_base_pay)
)
suppress(current_commercial_race_gender_hourly)
```

```
## # A tibble: 5 x 3
## # Groups:   race_ethnicity [3]
##   race_ethnicity                                       gender count
##   <chr>                                                <chr>  <int>
## 1 Black or African American (United States of America) Female    41
## 2 Black or African American (United States of America) Male      41
## 3 Hispanic or Latino (United States of America)        Female     6
## 4 White (United States of America)                     Female    22
## 5 White (United States of America)                     Male      21
```

```r
current_commercial_race_gender_median_salaried <- commercial_salaried %>% group_by(race_ethnicity, gender)
current_commercial_race_gender_median_salaried <- current_commercial_race_gender_median_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_race_gender_median_salaried)
```

```
## # A tibble: 6 x 4
## # Groups:   race_ethnicity [3]
##   race_ethnicity                                       gender count median
##   <chr>                                                <chr>  <int>  <dbl>
## 1 Asian (United States of America)                     Female     8 87500 
## 2 Asian (United States of America)                     Male       5 74837.
## 3 Black or African American (United States of America) Female     7 90585 
## 4 Black or African American (United States of America) Male       7 82609.
## 5 White (United States of America)                     Female    67 86105.
## 6 White (United States of America)                     Male      32 94497.
```

```r
current_commercial_race_gender_hourly_median <- commercial_hourly %>% group_by(race_ethnicity, gender)
current_commercial_race_gender_hourly_median <- current_commercial_race_gender_hourly_median %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_race_gender_hourly_median)
```

```
## # A tibble: 5 x 4
## # Groups:   race_ethnicity [3]
##   race_ethnicity                                       gender count median
##   <chr>                                                <chr>  <int>  <dbl>
## 1 Black or African American (United States of America) Female    41   26.3
## 2 Black or African American (United States of America) Male      41   23.3
## 3 Hispanic or Latino (United States of America)        Female     6   28.5
## 4 White (United States of America)                     Female    22   31.8
## 5 White (United States of America)                     Male      21   26.8
```

```r
current_commercial_race_gender_salaried_under_40 <- filter(commercial_salaried, age < 40) %>% group_by(race_ethnicity, gender)
current_commercial_race_gender_salaried_under_40 <- current_commercial_race_gender_salaried_under_40 %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_race_gender_salaried_under_40)
```

```
## # A tibble: 3 x 4
## # Groups:   race_ethnicity [2]
##   race_ethnicity                   gender count median
##   <chr>                            <chr>  <int>  <dbl>
## 1 Asian (United States of America) Female     6  85000
## 2 White (United States of America) Female    46  80212
## 3 White (United States of America) Male      16  90940
```

```r
current_commercial_race_gender_salaried_over_40 <- filter(commercial_salaried, age > 39) %>% group_by(race_ethnicity, gender)
current_commercial_race_gender_salaried_over_40 <- current_commercial_race_gender_salaried_over_40 %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_race_gender_salaried_over_40)
```

```
## # A tibble: 3 x 4
## # Groups:   race_ethnicity [2]
##   race_ethnicity                                       gender count median
##   <chr>                                                <chr>  <int>  <dbl>
## 1 Black or African American (United States of America) Female     6 94950.
## 2 White (United States of America)                     Female    21 97546 
## 3 White (United States of America)                     Male      16 95564.
```

```r
current_commercial_race_gender_hourly_under_40 <- filter(commercial_hourly, age < 40) %>% group_by(race_ethnicity, gender)
current_commercial_race_gender_hourly_under_40 <- current_commercial_race_gender_hourly_under_40 %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_race_gender_hourly_under_40)
```

```
## # A tibble: 5 x 4
## # Groups:   race_ethnicity [3]
##   race_ethnicity                                       gender count median
##   <chr>                                                <chr>  <int>  <dbl>
## 1 Black or African American (United States of America) Female     8   26.5
## 2 Black or African American (United States of America) Male       8   26.3
## 3 Hispanic or Latino (United States of America)        Female     6   28.5
## 4 White (United States of America)                     Female    12   33.3
## 5 White (United States of America)                     Male      10   30.6
```

```r
current_commercial_race_gender_hourly_over_40 <- filter(commercial_hourly, age > 39) %>% group_by(race_ethnicity, gender)
current_commercial_race_gender_hourly_over_40 <- current_commercial_race_gender_hourly_over_40 %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_race_gender_hourly_over_40)
```

```
## # A tibble: 4 x 4
## # Groups:   race_ethnicity [2]
##   race_ethnicity                                       gender count median
##   <chr>                                                <chr>  <int>  <dbl>
## 1 Black or African American (United States of America) Female    33   26.1
## 2 Black or African American (United States of America) Male      33   23.1
## 3 White (United States of America)                     Female    10   31.0
## 4 White (United States of America)                     Male      11   23.8
```

### Years of service


```r
current_commercial_yos_salaried <- commercial_salaried %>% group_by(years_of_service_grouped)
current_commercial_yos_salaried <- current_commercial_yos_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_yos_salaried)
```

```
## # A tibble: 8 x 3
##   years_of_service_grouped count median
##   <fct>                    <int>  <dbl>
## 1 0                           31 82000 
## 2 1-2                         36 80212 
## 3 3-5                         26 95770.
## 4 6-10                        15 99316 
## 5 11-15                        6 76331.
## 6 16-20                        6 81766.
## 7 21-25                        8 94007.
## 8 25+                          5 93491.
```

```r
current_commercial_yos_hourly <- commercial_hourly %>% group_by(years_of_service_grouped)
current_commercial_yos_hourly <- current_commercial_yos_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_yos_hourly)
```

```
## # A tibble: 8 x 3
##   years_of_service_grouped count median
##   <fct>                    <int>  <dbl>
## 1 0                           26   25.6
## 2 1-2                         33   27.0
## 3 3-5                         14   23.2
## 4 6-10                        19   24.0
## 5 11-15                       14   30.2
## 6 16-20                       17   24.3
## 7 21-25                        9   29.7
## 8 25+                         15   26.3
```

```r
current_commercial_yos_gender_salaried <- commercial_salaried %>% group_by(years_of_service_grouped, gender)
current_commercial_yos_gender_salaried <- current_commercial_yos_gender_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_yos_gender_salaried)
```

```
## # A tibble: 8 x 4
## # Groups:   years_of_service_grouped [5]
##   years_of_service_grouped gender count  median
##   <fct>                    <chr>  <int>   <dbl>
## 1 0                        Female    22  74640 
## 2 0                        Male       9  90000 
## 3 1-2                      Female    26  80212 
## 4 1-2                      Male      10  81640 
## 5 3-5                      Female    16  94108.
## 6 3-5                      Male      10 102497.
## 7 6-10                     Female    12  99500.
## 8 21-25                    Male       6  91466.
```

```r
current_commercial_yos_gender_hourly <- commercial_hourly %>% group_by(years_of_service_grouped, gender)
current_commercial_yos_gender_hourly <- current_commercial_yos_gender_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_yos_gender_hourly)
```

```
## # A tibble: 13 x 4
## # Groups:   years_of_service_grouped [8]
##    years_of_service_grouped gender count median
##    <fct>                    <chr>  <int>  <dbl>
##  1 0                        Female    10   29.5
##  2 0                        Male      16   22.0
##  3 1-2                      Female    18   30.3
##  4 1-2                      Male      15   24.4
##  5 3-5                      Female     5   30.8
##  6 3-5                      Male       9   22.1
##  7 6-10                     Female     5   26.3
##  8 6-10                     Male      14   23.6
##  9 11-15                    Male      10   29.0
## 10 16-20                    Female    10   24.2
## 11 16-20                    Male       7   27.3
## 12 21-25                    Female     8   27.9
## 13 25+                      Female    14   26.6
```

```r
current_commercial_yos_race_salaried <- commercial_salaried %>% group_by(years_of_service_grouped, race_ethnicity)
current_commercial_yos_race_salaried <- current_commercial_yos_race_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_yos_race_salaried)
```

```
## # A tibble: 6 x 4
## # Groups:   years_of_service_grouped [6]
##   years_of_service_grouped race_ethnicity                   count  median
##   <fct>                    <chr>                            <int>   <dbl>
## 1 0                        White (United States of America)    23  82000 
## 2 1-2                      White (United States of America)    30  80212 
## 3 3-5                      White (United States of America)    19 108780 
## 4 6-10                     White (United States of America)    11 102500 
## 5 16-20                    White (United States of America)     5  87392.
## 6 21-25                    White (United States of America)     6  97651.
```

```r
current_commercial_yos_race_hourly <- commercial_hourly %>% group_by(years_of_service_grouped, race_ethnicity)
current_commercial_yos_race_hourly <- current_commercial_yos_race_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_yos_race_hourly)
```

```
## # A tibble: 13 x 4
## # Groups:   years_of_service_grouped [8]
##    years_of_service_gro… race_ethnicity                        count median
##    <fct>                 <chr>                                 <int>  <dbl>
##  1 0                     Black or African American (United St…    11   25.6
##  2 0                     White (United States of America)          6   29.5
##  3 1-2                   Black or African American (United St…    14   23.6
##  4 1-2                   White (United States of America)         13   34.7
##  5 3-5                   Black or African American (United St…     6   21.8
##  6 3-5                   White (United States of America)          5   23.2
##  7 6-10                  Black or African American (United St…    12   23.6
##  8 6-10                  White (United States of America)          6   29.9
##  9 11-15                 Black or African American (United St…     7   30.4
## 10 11-15                 White (United States of America)          6   26.0
## 11 16-20                 Black or African American (United St…    12   24.1
## 12 21-25                 Black or African American (United St…     9   29.7
## 13 25+                   Black or African American (United St…    11   24.7
```

```r
current_commercial_yos_race_gender_salaried <- commercial_salaried %>% group_by(years_of_service_grouped, race_ethnicity, gender)
current_commercial_yos_race_gender_salaried <- current_commercial_yos_race_gender_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_yos_race_gender_salaried)
```

```
## # A tibble: 7 x 5
## # Groups:   years_of_service_grouped, race_ethnicity [4]
##   years_of_service_group… race_ethnicity                gender count median
##   <fct>                   <chr>                         <chr>  <int>  <dbl>
## 1 0                       White (United States of Amer… Female    15 7.43e4
## 2 0                       White (United States of Amer… Male       8 9.25e4
## 3 1-2                     White (United States of Amer… Female    21 7.74e4
## 4 1-2                     White (United States of Amer… Male       9 8.33e4
## 5 3-5                     White (United States of Amer… Female    14 9.41e4
## 6 3-5                     White (United States of Amer… Male       5 1.26e5
## 7 6-10                    White (United States of Amer… Female    10 1.01e5
```

```r
current_commercial_yos_race_gender_hourly <- commercial_hourly %>% group_by(years_of_service_grouped, race_ethnicity, gender)
current_commercial_yos_race_gender_hourly <- current_commercial_yos_race_gender_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_yos_race_gender_hourly)
```

```
## # A tibble: 11 x 5
## # Groups:   years_of_service_grouped, race_ethnicity [10]
##    years_of_service_gr… race_ethnicity                  gender count median
##    <fct>                <chr>                           <chr>  <int>  <dbl>
##  1 0                    Black or African American (Uni… Male       7   20.6
##  2 1-2                  Black or African American (Uni… Female     6   25.8
##  3 1-2                  Black or African American (Uni… Male       8   21.9
##  4 1-2                  White (United States of Americ… Female     9   35.0
##  5 3-5                  Black or African American (Uni… Male       5   21.5
##  6 6-10                 Black or African American (Uni… Male      10   23.4
##  7 11-15                Black or African American (Uni… Male       5   29.9
##  8 11-15                White (United States of Americ… Male       5   26.8
##  9 16-20                Black or African American (Uni… Female     8   23.7
## 10 21-25                Black or African American (Uni… Female     8   27.9
## 11 25+                  Black or African American (Uni… Female    10   25.5
```

### Age


```r
current_median_commercial_age_5_salaried <- commercial_salaried %>% group_by(age_group_5)
current_median_commercial_age_5_salaried <- current_median_commercial_age_5_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_commercial_age_5_salaried)
```

```
## # A tibble: 9 x 3
##   age_group_5 count  median
##   <fct>       <int>   <dbl>
## 1 <25            10  64000 
## 2 25-29          35  75000 
## 3 30-34          16  98848.
## 4 35-39          18 101092.
## 5 40-44           9 143576.
## 6 45-49          13  86105.
## 7 50-54          10  87002.
## 8 55-59          10  96957.
## 9 60-64          10  95754.
```

```r
current_median_commercial_age_5_hourly <- commercial_hourly %>% group_by(age_group_5)
current_median_commercial_age_5_hourly <- current_median_commercial_age_5_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_commercial_age_5_hourly)
```

```
## # A tibble: 10 x 3
##    age_group_5 count median
##    <fct>       <int>  <dbl>
##  1 <25            11   25.6
##  2 25-29          22   29.8
##  3 30-34           9   29.5
##  4 35-39          13   30.8
##  5 40-44          17   28.9
##  6 45-49          17   24.0
##  7 50-54          18   23.6
##  8 55-59          16   26.2
##  9 60-64          13   24.3
## 10 65+            11   23.4
```

```r
current_median_commercial_age_10_salaried <- commercial_salaried %>% group_by(age_group_10)
current_median_commercial_age_10_salaried <- current_median_commercial_age_10_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_commercial_age_10_salaried)
```

```
## # A tibble: 5 x 3
##   age_group_10 count  median
##   <fct>        <int>   <dbl>
## 1 <25             10  64000 
## 2 25-34           51  82000 
## 3 35-44           27 105000 
## 4 45-54           23  86613 
## 5 55-64           20  96957.
```

```r
current_median_commercial_age_10_hourly <- commercial_hourly %>% group_by(age_group_10)
current_median_commercial_age_10_hourly <- current_median_commercial_age_10_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_commercial_age_10_hourly)
```

```
## # A tibble: 6 x 3
##   age_group_10 count median
##   <fct>        <int>  <dbl>
## 1 <25             11   25.6
## 2 25-34           31   29.5
## 3 35-44           30   29.2
## 4 45-54           35   23.8
## 5 55-64           29   24.7
## 6 65+             11   23.4
```

```r
current_commercial_age_5_yos_salary <- commercial_salaried %>% group_by(age_group_5, years_of_service_grouped)
current_commercial_age_5_yos_salary <- current_commercial_age_5_yos_salary %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_age_5_yos_salary)
```

```
## # A tibble: 9 x 4
## # Groups:   age_group_5 [6]
##   age_group_5 years_of_service_grouped count  median
##   <fct>       <fct>                    <int>   <dbl>
## 1 <25         0                            6  62500 
## 2 25-29       0                           14  75000 
## 3 25-29       1-2                         17  76000 
## 4 30-34       0                            6 100000 
## 5 30-34       1-2                          7  96980 
## 6 35-39       3-5                          7 149101 
## 7 35-39       6-10                         6 101092.
## 8 40-44       3-5                          5 167000 
## 9 60-64       21-25                        5  97514.
```

```r
current_commercial_age_5_yos_hourly <- commercial_hourly %>% group_by(age_group_5, years_of_service_grouped)
current_commercial_age_5_yos_hourly <- current_commercial_age_5_yos_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_age_5_yos_hourly)
```

```
## # A tibble: 10 x 4
## # Groups:   age_group_5 [8]
##    age_group_5 years_of_service_grouped count median
##    <fct>       <fct>                    <int>  <dbl>
##  1 <25         0                            5   23.1
##  2 <25         1-2                          6   27.9
##  3 25-29       0                            6   33.3
##  4 25-29       1-2                         15   26.7
##  5 30-34       0                            5   22.0
##  6 35-39       11-15                        5   30.4
##  7 40-44       3-5                          5   29.2
##  8 55-59       25+                          6   27.9
##  9 60-64       16-20                        5   24.3
## 10 65+         25+                          5   26.8
```

```r
current_commercial_age_10_yos_salary <- commercial_salaried %>% group_by(age_group_10, years_of_service_grouped)
current_commercial_age_10_yos_salary <- current_commercial_age_10_yos_salary %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_age_10_yos_salary)
```

```
## # A tibble: 8 x 4
## # Groups:   age_group_10 [5]
##   age_group_10 years_of_service_grouped count  median
##   <fct>        <fct>                    <int>   <dbl>
## 1 <25          0                            6  62500 
## 2 25-34        0                           20  82000 
## 3 25-34        1-2                         24  80810.
## 4 25-34        3-5                          5  85850 
## 5 35-44        3-5                         12 158050.
## 6 35-44        6-10                         6 101092.
## 7 45-54        3-5                          5  86613 
## 8 55-64        21-25                        5  97514.
```

```r
current_commercial_age_10_yos_hourly <- commercial_hourly %>% group_by(age_group_10, years_of_service_grouped)
current_commercial_age_10_yos_hourly <- current_commercial_age_10_yos_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_commercial_age_10_yos_hourly)
```

```
## # A tibble: 15 x 4
## # Groups:   age_group_10 [6]
##    age_group_10 years_of_service_grouped count median
##    <fct>        <fct>                    <int>  <dbl>
##  1 <25          0                            5   23.1
##  2 <25          1-2                          6   27.9
##  3 25-34        0                           11   30.3
##  4 25-34        1-2                         15   26.7
##  5 35-44        0                            5   29.2
##  6 35-44        3-5                          6   26.2
##  7 35-44        11-15                        7   30.4
##  8 45-54        0                            5   20.5
##  9 45-54        1-2                          5   22.4
## 10 45-54        6-10                         7   23.8
## 11 45-54        16-20                        6   28.3
## 12 55-64        6-10                         6   23.4
## 13 55-64        16-20                        6   24.3
## 14 55-64        25+                          8   27.9
## 15 65+          25+                          5   26.8
```

```r
current_median_commercial_age_5_gender_salaried <- commercial_salaried %>% group_by(age_group_5, gender)
current_median_commercial_age_5_gender_salaried <- current_median_commercial_age_5_gender_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_commercial_age_5_gender_salaried)
```

```
## # A tibble: 14 x 4
## # Groups:   age_group_5 [9]
##    age_group_5 gender count  median
##    <fct>       <chr>  <int>   <dbl>
##  1 <25         Female     8  63500 
##  2 25-29       Female    29  75000 
##  3 25-29       Male       6  79140 
##  4 30-34       Female     9 100000 
##  5 30-34       Male       7  97696.
##  6 35-39       Female     9 149101 
##  7 35-39       Male       9  77627.
##  8 40-44       Female     8 124288.
##  9 45-49       Female     7  90585 
## 10 45-49       Male       6  85090.
## 11 50-54       Female     7  90669.
## 12 55-59       Female     5  96780 
## 13 55-59       Male       5  97135.
## 14 60-64       Male       6  95754.
```

```r
current_median_commercial_age_5_gender_hourly <- commercial_hourly %>% group_by(age_group_5, gender)
current_median_commercial_age_5_gender_hourly <- current_median_commercial_age_5_gender_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_commercial_age_5_gender_hourly)
```

```
## # A tibble: 18 x 4
## # Groups:   age_group_5 [10]
##    age_group_5 gender count median
##    <fct>       <chr>  <int>  <dbl>
##  1 <25         Male       7   23.1
##  2 25-29       Female    14   31.8
##  3 25-29       Male       8   26.2
##  4 30-34       Female     6   30.3
##  5 35-39       Female     5   30.8
##  6 35-39       Male       8   30.6
##  7 40-44       Female    12   29.5
##  8 40-44       Male       5   21.5
##  9 45-49       Female     7   31.3
## 10 45-49       Male      10   22.4
## 11 50-54       Female     6   23.3
## 12 50-54       Male      12   24.1
## 13 55-59       Female     9   26.4
## 14 55-59       Male       7   23.4
## 15 60-64       Female     6   24.5
## 16 60-64       Male       7   24.3
## 17 65+         Female     5   27.7
## 18 65+         Male       6   22.7
```

```r
current_median_commercial_age_10_gender_salaried <- commercial_salaried %>% group_by(age_group_10, gender)
current_median_commercial_age_10_gender_salaried <- current_median_commercial_age_10_gender_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_commercial_age_10_gender_salaried)
```

```
## # A tibble: 9 x 4
## # Groups:   age_group_10 [5]
##   age_group_10 gender count  median
##   <fct>        <chr>  <int>   <dbl>
## 1 <25          Female     8  63500 
## 2 25-34        Female    38  80212 
## 3 25-34        Male      13  86880 
## 4 35-44        Female    17 143576.
## 5 35-44        Male      10  84029.
## 6 45-54        Female    14  90627.
## 7 45-54        Male       9  85000 
## 8 55-64        Female     9  96780 
## 9 55-64        Male      11  97135.
```

```r
current_median_commercial_age_10_gender_hourly <- commercial_hourly %>% group_by(age_group_10, gender)
current_median_commercial_age_10_gender_hourly <- current_median_commercial_age_10_gender_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_commercial_age_10_gender_hourly)
```

```
## # A tibble: 11 x 4
## # Groups:   age_group_10 [6]
##    age_group_10 gender count median
##    <fct>        <chr>  <int>  <dbl>
##  1 <25          Male       7   23.1
##  2 25-34        Female    20   31.0
##  3 25-34        Male      11   26.0
##  4 35-44        Female    17   29.7
##  5 35-44        Male      13   27.2
##  6 45-54        Female    13   26.1
##  7 45-54        Male      22   23.5
##  8 55-64        Female    15   25.4
##  9 55-64        Male      14   23.9
## 10 65+          Female     5   27.7
## 11 65+          Male       6   22.7
```

```r
current_median_commercial_age_5_race_salaried <- commercial_salaried %>% group_by(age_group_5, race_ethnicity)
current_median_commercial_age_5_race_salaried <- current_median_commercial_age_5_race_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_commercial_age_5_race_salaried)
```

```
## # A tibble: 9 x 4
## # Groups:   age_group_5 [9]
##   age_group_5 race_ethnicity                   count  median
##   <fct>       <chr>                            <int>   <dbl>
## 1 <25         White (United States of America)     9  63000 
## 2 25-29       White (United States of America)    28  78692.
## 3 30-34       White (United States of America)    12  98848.
## 4 35-39       White (United States of America)    13 149101 
## 5 40-44       White (United States of America)     6 126865.
## 6 45-49       White (United States of America)     7  90000 
## 7 50-54       White (United States of America)     9  87392.
## 8 55-59       White (United States of America)     8  96957.
## 9 60-64       White (United States of America)     6  97651.
```

```r
current_median_commercial_age_5_race_hourly <- commercial_hourly %>% group_by(age_group_5, race_ethnicity)
current_median_commercial_age_5_race_hourly <- current_median_commercial_age_5_race_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_commercial_age_5_race_hourly)
```

```
## # A tibble: 11 x 4
## # Groups:   age_group_5 [9]
##    age_group_5 race_ethnicity                                  count median
##    <fct>       <chr>                                           <int>  <dbl>
##  1 <25         Black or African American (United States of Am…     5   22.4
##  2 25-29       White (United States of America)                   11   31.8
##  3 35-39       White (United States of America)                    6   30.8
##  4 40-44       Black or African American (United States of Am…    13   28.9
##  5 45-49       Black or African American (United States of Am…    14   23.1
##  6 50-54       Black or African American (United States of Am…    12   23.3
##  7 50-54       White (United States of America)                    5   24.4
##  8 55-59       Black or African American (United States of Am…    11   27.0
##  9 55-59       White (United States of America)                    5   25.4
## 10 60-64       Black or African American (United States of Am…    11   24.3
## 11 65+         Black or African American (United States of Am…     5   23.4
```

```r
current_median_commercial_age_10_race_salaried <- commercial_salaried %>% group_by(age_group_10, race_ethnicity)
current_median_commercial_age_10_race_salaried <- current_median_commercial_age_10_race_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_commercial_age_10_race_salaried)
```

```
## # A tibble: 6 x 4
## # Groups:   age_group_10 [5]
##   age_group_10 race_ethnicity                   count  median
##   <fct>        <chr>                            <int>   <dbl>
## 1 <25          White (United States of America)     9  63000 
## 2 25-34        Asian (United States of America)     6  82418.
## 3 25-34        White (United States of America)    40  82000 
## 4 35-44        White (United States of America)    19 148730.
## 5 45-54        White (United States of America)    16  88696.
## 6 55-64        White (United States of America)    14  97325.
```

```r
current_median_commercial_age_10_race_hourly <- commercial_hourly %>% group_by(age_group_10, race_ethnicity)
current_median_commercial_age_10_race_hourly <- current_median_commercial_age_10_race_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_commercial_age_10_race_hourly)
```

```
## # A tibble: 11 x 4
## # Groups:   age_group_10 [6]
##    age_group_10 race_ethnicity                                 count median
##    <fct>        <chr>                                          <int>  <dbl>
##  1 <25          Black or African American (United States of A…     5   22.4
##  2 25-34        Black or African American (United States of A…     7   26.7
##  3 25-34        Hispanic or Latino (United States of America)      6   25.0
##  4 25-34        White (United States of America)                  12   31.8
##  5 35-44        Black or African American (United States of A…    17   29.2
##  6 35-44        White (United States of America)                   8   30.6
##  7 45-54        Black or African American (United States of A…    26   23.3
##  8 45-54        White (United States of America)                   8   30.8
##  9 55-64        Black or African American (United States of A…    22   24.5
## 10 55-64        White (United States of America)                   7   26.4
## 11 65+          Black or African American (United States of A…     5   23.4
```

```r
current_median_commercial_age_5_race_group_salaried <- commercial_salaried %>% group_by(age_group_5, race_grouping)
current_median_commercial_age_5_race_group_salaried <- current_median_commercial_age_5_race_group_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_commercial_age_5_race_group_salaried)
```

```
## # A tibble: 12 x 4
## # Groups:   age_group_5 [9]
##    age_group_5 race_grouping   count  median
##    <fct>       <chr>           <int>   <dbl>
##  1 <25         white               9  63000 
##  2 25-29       person of color     7  72000 
##  3 25-29       white              28  78692.
##  4 30-34       white              12  98848.
##  5 35-39       person of color     5  73522.
##  6 35-39       white              13 149101 
##  7 40-44       white               6 126865.
##  8 45-49       person of color     6  85450.
##  9 45-49       white               7  90000 
## 10 50-54       white               9  87392.
## 11 55-59       white               8  96957.
## 12 60-64       white               6  97651.
```

```r
current_median_commercial_age_5_race_group_hourly <- commercial_hourly %>% group_by(age_group_5, race_grouping)
current_median_commercial_age_5_race_group_hourly <- current_median_commercial_age_5_race_group_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_commercial_age_5_race_group_hourly)
```

```
## # A tibble: 14 x 4
## # Groups:   age_group_5 [10]
##    age_group_5 race_grouping   count median
##    <fct>       <chr>           <int>  <dbl>
##  1 <25         person of color     7   25.6
##  2 25-29       person of color    10   26.3
##  3 25-29       white              11   31.8
##  4 30-34       person of color     8   28.8
##  5 35-39       person of color     6   30.8
##  6 35-39       white               6   30.8
##  7 40-44       person of color    14   28.5
##  8 45-49       person of color    14   23.1
##  9 50-54       person of color    13   23.2
## 10 50-54       white               5   24.4
## 11 55-59       person of color    11   27.0
## 12 55-59       white               5   25.4
## 13 60-64       person of color    11   24.3
## 14 65+         person of color     7   23.4
```

```r
current_median_commercial_age_10_race_group_salaried <- commercial_salaried %>% group_by(age_group_10, race_grouping)
current_median_commercial_age_10_race_group_salaried <- current_median_commercial_age_10_race_group_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_commercial_age_10_race_group_salaried)
```

```
## # A tibble: 9 x 4
## # Groups:   age_group_10 [5]
##   age_group_10 race_grouping   count  median
##   <fct>        <chr>           <int>   <dbl>
## 1 <25          white               9  63000 
## 2 25-34        person of color    10  74918.
## 3 25-34        white              40  82000 
## 4 35-44        person of color     7  90431.
## 5 35-44        white              19 148730.
## 6 45-54        person of color     7  85000 
## 7 45-54        white              16  88696.
## 8 55-64        person of color     6  82709.
## 9 55-64        white              14  97325.
```

```r
current_median_commercial_age_10_race_group_hourly <- commercial_hourly %>% group_by(age_group_10, race_grouping)
current_median_commercial_age_10_race_group_hourly <- current_median_commercial_age_10_race_group_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_commercial_age_10_race_group_hourly)
```

```
## # A tibble: 10 x 4
## # Groups:   age_group_10 [6]
##    age_group_10 race_grouping   count median
##    <fct>        <chr>           <int>  <dbl>
##  1 <25          person of color     7   25.6
##  2 25-34        person of color    18   26.5
##  3 25-34        white              12   31.8
##  4 35-44        person of color    20   29.1
##  5 35-44        white               8   30.6
##  6 45-54        person of color    27   23.2
##  7 45-54        white               8   30.8
##  8 55-64        person of color    22   24.5
##  9 55-64        white               7   26.4
## 10 65+          person of color     7   23.4
```

```r
current_median_commercial_age_5_race_gender_salaried <- commercial_salaried %>% group_by(age_group_5, race_ethnicity, gender)
current_median_commercial_age_5_race_gender_salaried <- current_median_commercial_age_5_race_gender_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_commercial_age_5_race_gender_salaried)
```

```
## # A tibble: 8 x 5
## # Groups:   age_group_5, race_ethnicity [7]
##   age_group_5 race_ethnicity                   gender count  median
##   <fct>       <chr>                            <chr>  <int>   <dbl>
## 1 <25         White (United States of America) Female     7  62000 
## 2 25-29       White (United States of America) Female    25  76000 
## 3 30-34       White (United States of America) Female     5 131097.
## 4 30-34       White (United States of America) Male       7  97696.
## 5 35-39       White (United States of America) Female     9 149101 
## 6 40-44       White (United States of America) Female     6 126865.
## 7 50-54       White (United States of America) Female     6  98281.
## 8 55-59       White (United States of America) Male       5  97135.
```

```r
current_median_commercial_age_5_race_gender_hourly <- commercial_hourly %>% group_by(age_group_5, race_ethnicity, gender)
current_median_commercial_age_5_race_gender_hourly <- current_median_commercial_age_5_race_gender_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_commercial_age_5_race_gender_hourly)
```

```
## # A tibble: 10 x 5
## # Groups:   age_group_5, race_ethnicity [8]
##    age_group_5 race_ethnicity                           gender count median
##    <fct>       <chr>                                    <chr>  <int>  <dbl>
##  1 <25         Black or African American (United State… Male       5   22.4
##  2 25-29       White (United States of America)         Female     7   35.0
##  3 40-44       Black or African American (United State… Female     9   29.7
##  4 45-49       Black or African American (United State… Male      10   22.4
##  5 50-54       Black or African American (United State… Female     6   23.3
##  6 50-54       Black or African American (United State… Male       6   23.0
##  7 50-54       White (United States of America)         Male       5   24.4
##  8 55-59       Black or African American (United State… Female     7   28.6
##  9 60-64       Black or African American (United State… Female     5   24.3
## 10 60-64       Black or African American (United State… Male       6   23.8
```

```r
current_median_commercial_age_10_race_gender_salaried <- commercial_salaried %>% group_by(age_group_10, race_ethnicity, gender)
current_median_commercial_age_10_race_gender_salaried <- current_median_commercial_age_10_race_gender_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_commercial_age_10_race_gender_salaried)
```

```
## # A tibble: 9 x 5
## # Groups:   age_group_10, race_ethnicity [6]
##   age_group_10 race_ethnicity                   gender count  median
##   <fct>        <chr>                            <chr>  <int>   <dbl>
## 1 <25          White (United States of America) Female     7  62000 
## 2 25-34        Asian (United States of America) Female     5  90000 
## 3 25-34        White (United States of America) Female    30  78692.
## 4 25-34        White (United States of America) Male      10  96348.
## 5 35-44        White (United States of America) Female    15 148730.
## 6 45-54        White (United States of America) Female    10  98281.
## 7 45-54        White (United States of America) Male       6  86196.
## 8 55-64        White (United States of America) Female     5  96780 
## 9 55-64        White (United States of America) Male       9  97514.
```

```r
current_median_commercial_age_10_race_gender_hourly <- commercial_hourly %>% group_by(age_group_10, race_ethnicity, gender)
current_median_commercial_age_10_race_gender_hourly <- current_median_commercial_age_10_race_gender_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_commercial_age_10_race_gender_hourly)
```

```
## # A tibble: 11 x 5
## # Groups:   age_group_10, race_ethnicity [8]
##    age_group_10 race_ethnicity                          gender count median
##    <fct>        <chr>                                   <chr>  <int>  <dbl>
##  1 <25          Black or African American (United Stat… Male       5   22.4
##  2 25-34        Black or African American (United Stat… Female     6   26.5
##  3 25-34        Hispanic or Latino (United States of A… Female     5   28.1
##  4 25-34        White (United States of America)        Female     8   33.4
##  5 35-44        Black or African American (United Stat… Female    11   29.7
##  6 35-44        Black or African American (United Stat… Male       6   24.8
##  7 45-54        Black or African American (United Stat… Female    10   23.7
##  8 45-54        Black or African American (United Stat… Male      16   22.4
##  9 45-54        White (United States of America)        Male       5   24.4
## 10 55-64        Black or African American (United Stat… Female    12   25.0
## 11 55-64        Black or African American (United Stat… Male      10   23.9
```

```r
current_median_commercial_age_5_race_group_gender_salaried <- commercial_salaried %>% group_by(age_group_5, race_grouping, gender)
current_median_commercial_age_5_race_group_gender_salaried <- current_median_commercial_age_5_race_group_gender_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_commercial_age_5_race_group_gender_salaried)
```

```
## # A tibble: 9 x 5
## # Groups:   age_group_5, race_grouping [8]
##   age_group_5 race_grouping   gender count  median
##   <fct>       <chr>           <chr>  <int>   <dbl>
## 1 <25         white           Female     7  62000 
## 2 25-29       white           Female    25  76000 
## 3 30-34       white           Female     5 131097.
## 4 30-34       white           Male       7  97696.
## 5 35-39       person of color Male       5  73522.
## 6 35-39       white           Female     9 149101 
## 7 40-44       white           Female     6 126865.
## 8 50-54       white           Female     6  98281.
## 9 55-59       white           Male       5  97135.
```

```r
current_median_commercial_age_5_race_group_gender_hourly <- commercial_hourly %>% group_by(age_group_5, race_grouping, gender)
current_median_commercial_age_5_race_group_gender_hourly <- current_median_commercial_age_5_race_group_gender_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_commercial_age_5_race_group_gender_hourly)
```

```
## # A tibble: 12 x 5
## # Groups:   age_group_5, race_grouping [10]
##    age_group_5 race_grouping   gender count median
##    <fct>       <chr>           <chr>  <int>  <dbl>
##  1 <25         person of color Male       5   22.4
##  2 25-29       person of color Female     7   26.3
##  3 25-29       white           Female     7   35.0
##  4 30-34       person of color Female     5   30.4
##  5 40-44       person of color Female    10   29.5
##  6 45-49       person of color Male      10   22.4
##  7 50-54       person of color Female     6   23.3
##  8 50-54       person of color Male       7   21.1
##  9 50-54       white           Male       5   24.4
## 10 55-59       person of color Female     7   28.6
## 11 60-64       person of color Female     5   24.3
## 12 60-64       person of color Male       6   23.8
```

```r
current_median_commercial_age_10_race_group_gender_salaried <- commercial_salaried %>% group_by(age_group_10, race_grouping, gender)
current_median_commercial_age_10_race_group_gender_salaried <- current_median_commercial_age_10_race_group_gender_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_commercial_age_10_race_group_gender_salaried)
```

```
## # A tibble: 10 x 5
## # Groups:   age_group_10, race_grouping [7]
##    age_group_10 race_grouping   gender count  median
##    <fct>        <chr>           <chr>  <int>   <dbl>
##  1 <25          white           Female     7  62000 
##  2 25-34        person of color Female     7  85000 
##  3 25-34        white           Female    30  78692.
##  4 25-34        white           Male      10  96348.
##  5 35-44        person of color Male       6  81977.
##  6 35-44        white           Female    15 148730.
##  7 45-54        white           Female    10  98281.
##  8 45-54        white           Male       6  86196.
##  9 55-64        white           Female     5  96780 
## 10 55-64        white           Male       9  97514.
```

```r
current_median_commercial_age_10_race_group_gender_hourly <- commercial_hourly %>% group_by(age_group_10, race_grouping, gender)
current_median_commercial_age_10_race_group_gender_hourly <- current_median_commercial_age_10_race_group_gender_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress(current_median_commercial_age_10_race_group_gender_hourly)
```

```
## # A tibble: 11 x 5
## # Groups:   age_group_10, race_grouping [7]
##    age_group_10 race_grouping   gender count median
##    <fct>        <chr>           <chr>  <int>  <dbl>
##  1 <25          person of color Male       5   22.4
##  2 25-34        person of color Female    12   27.4
##  3 25-34        person of color Male       6   26.2
##  4 25-34        white           Female     8   33.4
##  5 35-44        person of color Female    13   29.7
##  6 35-44        person of color Male       7   23.1
##  7 45-54        person of color Female    10   23.7
##  8 45-54        person of color Male      17   22.3
##  9 45-54        white           Male       5   24.4
## 10 55-64        person of color Female    12   25.0
## 11 55-64        person of color Male      10   23.9
```

### Departments


```r
current_commercial_median_department_salaried <- commercial_salaried %>% group_by(department)
current_commercial_median_department_salaried <- current_commercial_median_department_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_median_department_salaried)
```

```
## # A tibble: 5 x 3
##   department             count median
##   <chr>                  <int>  <dbl>
## 1 Finance                    8 90576.
## 2 WP News Media Services     9 86105.
## 3 Client Solutions         102 85634.
## 4 Marketing                  7 81196.
## 5 Production                 5 71665.
```

```r
current_commercial_median_department_hourly <- commercial_hourly %>% group_by(department)
current_commercial_median_department_hourly <- current_commercial_median_department_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_median_department_hourly)
```

```
## # A tibble: 4 x 3
##   department       count median
##   <chr>            <int>  <dbl>
## 1 Public Relations     5   35.0
## 2 Client Solutions    62   29.4
## 3 Finance             23   29.2
## 4 Circulation         49   22.4
```

```r
current_commercial_median_department_gender_salaried <- commercial_salaried %>% group_by(department, gender)
current_commercial_median_department_gender_salaried <- current_commercial_median_department_gender_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_median_department_gender_salaried)
```

```
## # A tibble: 4 x 4
## # Groups:   department [3]
##   department             gender count median
##   <chr>                  <chr>  <int>  <dbl>
## 1 Finance                Female     5 96780 
## 2 Client Solutions       Male      31 90000 
## 3 WP News Media Services Male       5 85900.
## 4 Client Solutions       Female    71 85000
```

```r
current_commercial_median_department_gender_hourly <- commercial_hourly %>% group_by(department, gender)
current_commercial_median_department_gender_hourly <- current_commercial_median_department_gender_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_median_department_gender_hourly)
```

```
## # A tibble: 7 x 4
## # Groups:   department [4]
##   department       gender count median
##   <chr>            <chr>  <int>  <dbl>
## 1 Public Relations Female     5   35.0
## 2 Client Solutions Male      24   30.1
## 3 Finance          Female    17   29.2
## 4 Finance          Male       6   28.8
## 5 Client Solutions Female    38   28.8
## 6 Circulation      Female     9   23.2
## 7 Circulation      Male      40   22.4
```

```r
current_commercial_median_department_race_salaried <- commercial_salaried %>% group_by(department, race_ethnicity)
current_commercial_median_department_race_salaried <- current_commercial_median_department_race_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_median_department_race_salaried)
```

```
## # A tibble: 5 x 4
## # Groups:   department [3]
##   department          race_ethnicity                           count median
##   <chr>               <chr>                                    <int>  <dbl>
## 1 Client Solutions    White (United States of America)            79 90000 
## 2 WP News Media Serv… White (United States of America)             8 88302.
## 3 Client Solutions    Black or African American (United State…    10 83805.
## 4 Marketing           White (United States of America)             5 83280 
## 5 Client Solutions    Asian (United States of America)             9 76139.
```

```r
current_commercial_median_department_race_hourly <- commercial_hourly %>% group_by(department, race_ethnicity)
current_commercial_median_department_race_hourly <- current_commercial_median_department_race_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_median_department_race_hourly)
```

```
## # A tibble: 8 x 4
## # Groups:   department [3]
##   department      race_ethnicity                               count median
##   <chr>           <chr>                                        <int>  <dbl>
## 1 Client Solutio… White (United States of America)                24   31.0
## 2 Finance         White (United States of America)                 5   29.5
## 3 Finance         Black or African American (United States of…    16   29.1
## 4 Client Solutio… Hispanic or Latino (United States of Americ…     6   28.5
## 5 Client Solutio… Black or African American (United States of…    25   27.0
## 6 Client Solutio… Asian (United States of America)                 5   26.3
## 7 Circulation     White (United States of America)                 8   22.8
## 8 Circulation     Black or African American (United States of…    35   22.4
```

```r
current_commercial_median_department_race_gender_salaried <- commercial_salaried %>% group_by(department, race_ethnicity, gender)
current_commercial_median_department_race_gender_salaried <- current_commercial_median_department_race_gender_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_median_department_race_gender_salaried)
```

```
## # A tibble: 4 x 5
## # Groups:   department, race_ethnicity [3]
##   department     race_ethnicity                         gender count median
##   <chr>          <chr>                                  <chr>  <int>  <dbl>
## 1 Client Soluti… White (United States of America)       Male      22 98894.
## 2 Client Soluti… Black or African American (United Sta… Female     6 92158 
## 3 Client Soluti… White (United States of America)       Female    57 86613 
## 4 Client Soluti… Asian (United States of America)       Female     5 80000
```

```r
current_commercial_median_department_race_gender_hourly <- commercial_hourly %>% group_by(department, race_ethnicity, gender)
current_commercial_median_department_race_gender_hourly <- current_commercial_median_department_race_gender_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_median_department_race_gender_hourly)
```

```
## # A tibble: 9 x 5
## # Groups:   department, race_ethnicity [6]
##   department     race_ethnicity                         gender count median
##   <chr>          <chr>                                  <chr>  <int>  <dbl>
## 1 Client Soluti… White (United States of America)       Female    13   31.7
## 2 Client Soluti… White (United States of America)       Male      11   30.8
## 3 Finance        Black or African American (United Sta… Female    12   29.1
## 4 Client Soluti… Hispanic or Latino (United States of … Female     6   28.5
## 5 Client Soluti… Black or African American (United Sta… Male       9   28.2
## 6 Client Soluti… Black or African American (United Sta… Female    16   26.0
## 7 Circulation    Black or African American (United Sta… Female     9   23.2
## 8 Circulation    White (United States of America)       Male       8   22.8
## 9 Circulation    Black or African American (United Sta… Male      26   22.4
```

```r
current_commercial_median_department_race_group_gender_salaried <- commercial_salaried %>% group_by(department, race_grouping, gender)
current_commercial_median_department_race_group_gender_salaried <- current_commercial_median_department_race_group_gender_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_median_department_race_group_gender_salaried)
```

```
## # A tibble: 4 x 5
## # Groups:   department, race_grouping [2]
##   department       race_grouping   gender count median
##   <chr>            <chr>           <chr>  <int>  <dbl>
## 1 Client Solutions white           Male      22 98894.
## 2 Client Solutions white           Female    57 86613 
## 3 Client Solutions person of color Female    13 80000 
## 4 Client Solutions person of color Male       9 76139.
```

```r
current_commercial_median_department_race_group_gender_hourly <- commercial_hourly %>% group_by(department, race_grouping, gender)
current_commercial_median_department_race_group_gender_hourly <- current_commercial_median_department_race_group_gender_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_median_department_race_group_gender_hourly)
```

```
## # A tibble: 8 x 5
## # Groups:   department, race_grouping [5]
##   department       race_grouping   gender count median
##   <chr>            <chr>           <chr>  <int>  <dbl>
## 1 Client Solutions white           Female    13   31.7
## 2 Client Solutions white           Male      11   30.8
## 3 Finance          person of color Female    13   28.9
## 4 Client Solutions person of color Male      13   27.0
## 5 Client Solutions person of color Female    25   26.3
## 6 Circulation      person of color Female     9   23.2
## 7 Circulation      white           Male       8   22.8
## 8 Circulation      person of color Male      30   22.4
```

```r
current_commercial_median_department_race_gender_age5_salaried <- commercial_salaried %>% group_by(department, race_ethnicity, gender, age_group_5)
current_commercial_median_department_race_gender_age5_salaried <- current_commercial_median_department_race_gender_age5_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_median_department_race_gender_age5_salaried)
```

```
## # A tibble: 6 x 6
## # Groups:   department, race_ethnicity, gender [2]
##   department     race_ethnicity             gender age_group_5 count median
##   <chr>          <chr>                      <chr>  <fct>       <int>  <dbl>
## 1 Client Soluti… White (United States of A… Female 35-39           9 1.49e5
## 2 Client Soluti… White (United States of A… Female 40-44           6 1.27e5
## 3 Client Soluti… White (United States of A… Female 50-54           5 1.06e5
## 4 Client Soluti… White (United States of A… Male   30-34           5 1.00e5
## 5 Client Soluti… White (United States of A… Female 25-29          23 7.50e4
## 6 Client Soluti… White (United States of A… Female <25             6 6.10e4
```

```r
current_commercial_median_department_race_gender_age5_hourly <- commercial_hourly %>% group_by(department, race_ethnicity, gender, age_group_5)
current_commercial_median_department_race_gender_age5_hourly <- current_commercial_median_department_race_gender_age5_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_median_department_race_gender_age5_hourly)
```

```
## # A tibble: 3 x 6
## # Groups:   department, race_ethnicity, gender [2]
##   department    race_ethnicity              gender age_group_5 count median
##   <chr>         <chr>                       <chr>  <fct>       <int>  <dbl>
## 1 Client Solut… White (United States of Am… Female 25-29           5   31.8
## 2 Circulation   Black or African American … Male   60-64           6   23.8
## 3 Circulation   Black or African American … Male   45-49           7   21.5
```

```r
current_commercial_median_department_race_group_gender_age5_salaried <- commercial_salaried %>% group_by(department, race_grouping, gender, age_group_5)
current_commercial_median_department_race_group_gender_age5_salaried <- current_commercial_median_department_race_group_gender_age5_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_median_department_race_group_gender_age5_salaried)
```

```
## # A tibble: 6 x 6
## # Groups:   department, race_grouping, gender [2]
##   department       race_grouping gender age_group_5 count  median
##   <chr>            <chr>         <chr>  <fct>       <int>   <dbl>
## 1 Client Solutions white         Female 35-39           9 149101 
## 2 Client Solutions white         Female 40-44           6 126865.
## 3 Client Solutions white         Female 50-54           5 105893 
## 4 Client Solutions white         Male   30-34           5 100000 
## 5 Client Solutions white         Female 25-29          23  75000 
## 6 Client Solutions white         Female <25             6  61000
```

```r
current_commercial_median_department_race_group_gender_age5_hourly <- commercial_hourly %>% group_by(department, race_grouping, gender, age_group_5)
current_commercial_median_department_race_group_gender_age5_hourly <- current_commercial_median_department_race_group_gender_age5_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_median_department_race_group_gender_age5_hourly)
```

```
## # A tibble: 5 x 6
## # Groups:   department, race_grouping, gender [3]
##   department       race_grouping   gender age_group_5 count median
##   <chr>            <chr>           <chr>  <fct>       <int>  <dbl>
## 1 Client Solutions white           Female 25-29           5   31.8
## 2 Client Solutions person of color Female 40-44           5   25.0
## 3 Circulation      person of color Male   60-64           6   23.8
## 4 Circulation      person of color Male   45-49           7   21.5
## 5 Circulation      person of color Male   50-54           5   20.8
```

### Job profiles


```r
current_commercial_median_job_salaried <- commercial_salaried %>% group_by(job_profile_current)
current_commercial_median_job_salaried <- current_commercial_median_job_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_median_job_salaried)
```

```
## # A tibble: 10 x 3
##    job_profile_current                  count  median
##    <chr>                                <int>   <dbl>
##  1 450220 - Sales Representative           25 153987.
##  2 350227 - Custom Content Writer           7 100000 
##  3 551104 - Senior Financial Accountant     5  90566 
##  4 450120 - Account Manager                26  88645.
##  5 390110 - Multiplatform Editor            9  86105.
##  6 280228 - Designer                        7  85000 
##  7 340227 - Artist                          5  75035.
##  8 481205 - Digital Analyst                 5  75000 
##  9 660127 - Make-Up Person                  5  71665.
## 10 231303 - Client Service Manager         15  67096.
```

```r
current_commercial_median_job_hourly <- commercial_hourly %>% group_by(job_profile_current)
current_commercial_median_job_hourly <- current_commercial_median_job_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_median_job_hourly)
```

```
## # A tibble: 5 x 3
##   job_profile_current                   count median
##   <chr>                                 <int>  <dbl>
## 1 341027 - Desktop Publisher                6   30.8
## 2 574504 - Senior Accounting Specialist    11   30.4
## 3 565005 - Accounting Specialist           12   26.6
## 4 470121 - Account Executive               16   25.2
## 5 600318 - Circulation Driver (Class A)    35   22.4
```

```r
current_commercial_median_job_gender_salaried <- commercial_salaried %>% group_by(job_profile_current, gender)
current_commercial_median_job_gender_salaried <- current_commercial_median_job_gender_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_median_job_gender_salaried)
```

```
## # A tibble: 6 x 4
## # Groups:   job_profile_current [4]
##   job_profile_current             gender count  median
##   <chr>                           <chr>  <int>   <dbl>
## 1 450220 - Sales Representative   Male       6 162339.
## 2 450220 - Sales Representative   Female    19 150780 
## 3 450120 - Account Manager        Female    17  90110 
## 4 390110 - Multiplatform Editor   Male       5  85900.
## 5 450120 - Account Manager        Male       9  85418.
## 6 231303 - Client Service Manager Female    13  68000
```

```r
current_commercial_median_job_gender_hourly <- commercial_hourly %>% group_by(job_profile_current, gender)
current_commercial_median_job_gender_hourly <- current_commercial_median_job_gender_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_median_job_gender_hourly)
```

```
## # A tibble: 5 x 4
## # Groups:   job_profile_current [4]
##   job_profile_current                   gender count median
##   <chr>                                 <chr>  <int>  <dbl>
## 1 574504 - Senior Accounting Specialist Female    10   30.1
## 2 565005 - Accounting Specialist        Male       5   27.2
## 3 565005 - Accounting Specialist        Female     7   26.0
## 4 470121 - Account Executive            Female    15   25.0
## 5 600318 - Circulation Driver (Class A) Male      34   22.5
```

```r
current_commercial_median_job_race_salaried <- commercial_salaried %>% group_by(job_profile_current, race_ethnicity)
current_commercial_median_job_race_salaried <- current_commercial_median_job_race_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_median_job_race_salaried)
```

```
## # A tibble: 6 x 4
## # Groups:   job_profile_current [5]
##   job_profile_current       race_ethnicity                     count median
##   <chr>                     <chr>                              <int>  <dbl>
## 1 450220 - Sales Represent… White (United States of America)      23 1.51e5
## 2 350227 - Custom Content … White (United States of America)       6 1.00e5
## 3 450120 - Account Manager  White (United States of America)      15 9.07e4
## 4 390110 - Multiplatform E… White (United States of America)       8 8.83e4
## 5 450120 - Account Manager  Black or African American (United…     7 8.54e4
## 6 231303 - Client Service … White (United States of America)      14 6.55e4
```

```r
current_commercial_median_job_race_hourly <- commercial_hourly %>% group_by(job_profile_current, race_ethnicity)
current_commercial_median_job_race_hourly <- current_commercial_median_job_race_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_median_job_race_hourly)
```

```
## # A tibble: 6 x 4
## # Groups:   job_profile_current [4]
##   job_profile_current         race_ethnicity                   count median
##   <chr>                       <chr>                            <int>  <dbl>
## 1 574504 - Senior Accounting… Black or African American (Unit…     8   30.1
## 2 565005 - Accounting Specia… Black or African American (Unit…     7   26.0
## 3 470121 - Account Executive  White (United States of America)     5   25.4
## 4 470121 - Account Executive  Black or African American (Unit…     9   24.7
## 5 600318 - Circulation Drive… White (United States of America)     7   23.0
## 6 600318 - Circulation Drive… Black or African American (Unit…    23   22.4
```

```r
current_commercial_median_job_race_gender_salaried <- commercial_salaried %>% group_by(job_profile_current, race_ethnicity, gender)
current_commercial_median_job_race_gender_salaried <- current_commercial_median_job_race_gender_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_median_job_race_gender_salaried)
```

```
## # A tibble: 4 x 5
## # Groups:   job_profile_current, race_ethnicity [3]
##   job_profile_current        race_ethnicity             gender count median
##   <chr>                      <chr>                      <chr>  <int>  <dbl>
## 1 450220 - Sales Representa… White (United States of A… Male       5 1.55e5
## 2 450220 - Sales Representa… White (United States of A… Female    18 1.50e5
## 3 450120 - Account Manager   White (United States of A… Female    11 9.01e4
## 4 231303 - Client Service M… White (United States of A… Female    12 6.60e4
```

```r
current_commercial_median_job_race_gender_hourly <- commercial_hourly %>% group_by(job_profile_current, race_ethnicity, gender)
current_commercial_median_job_race_gender_hourly <- current_commercial_median_job_race_gender_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_median_job_race_gender_hourly)
```

```
## # A tibble: 5 x 5
## # Groups:   job_profile_current, race_ethnicity [5]
##   job_profile_current      race_ethnicity               gender count median
##   <chr>                    <chr>                        <chr>  <int>  <dbl>
## 1 574504 - Senior Account… Black or African American (… Female     7   29.7
## 2 565005 - Accounting Spe… Black or African American (… Female     5   26.0
## 3 470121 - Account Execut… Black or African American (… Female     9   24.7
## 4 600318 - Circulation Dr… White (United States of Ame… Male       7   23.0
## 5 600318 - Circulation Dr… Black or African American (… Male      22   22.4
```

```r
current_commercial_median_job_race_group_gender_salaried <- commercial_salaried %>% group_by(job_profile_current, race_grouping, gender)
current_commercial_median_job_race_group_gender_salaried <- current_commercial_median_job_race_group_gender_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_median_job_race_group_gender_salaried)
```

```
## # A tibble: 6 x 5
## # Groups:   job_profile_current, race_grouping [4]
##   job_profile_current             race_grouping   gender count  median
##   <chr>                           <chr>           <chr>  <int>   <dbl>
## 1 450220 - Sales Representative   white           Male       5 155300 
## 2 450220 - Sales Representative   white           Female    18 149940.
## 3 450120 - Account Manager        person of color Female     5  99316 
## 4 450120 - Account Manager        white           Female    11  90110 
## 5 450120 - Account Manager        person of color Male       5  82609.
## 6 231303 - Client Service Manager white           Female    12  66001.
```

```r
current_commercial_median_job_race_group_gender_hourly <- commercial_hourly %>% group_by(job_profile_current, race_grouping, gender)
current_commercial_median_job_race_group_gender_hourly <- current_commercial_median_job_race_group_gender_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_median_job_race_group_gender_hourly)
```

```
## # A tibble: 5 x 5
## # Groups:   job_profile_current, race_grouping [5]
##   job_profile_current                   race_grouping   gender count median
##   <chr>                                 <chr>           <chr>  <int>  <dbl>
## 1 574504 - Senior Accounting Specialist person of color Female     7   29.7
## 2 565005 - Accounting Specialist        person of color Female     6   25.8
## 3 470121 - Account Executive            person of color Female    11   24.7
## 4 600318 - Circulation Driver (Class A) white           Male       7   23.0
## 5 600318 - Circulation Driver (Class A) person of color Male      26   22.4
```

```r
current_commercial_median_job_race_gender_age5_salaried <- commercial_salaried %>% group_by(job_profile_current, race_ethnicity, gender, age_group_5)
current_commercial_median_job_race_gender_age5_salaried <- current_commercial_median_job_race_gender_age5_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_median_job_race_gender_age5_salaried)
```

```
## # A tibble: 2 x 6
## # Groups:   job_profile_current, race_ethnicity, gender [2]
##   job_profile_current   race_ethnicity      gender age_group_5 count median
##   <chr>                 <chr>               <chr>  <fct>       <int>  <dbl>
## 1 450220 - Sales Repre… White (United Stat… Female 35-39           8 1.50e5
## 2 231303 - Client Serv… White (United Stat… Female 25-29           8 6.62e4
```

```r
current_commercial_median_job_race_gender_age5_hourly <- commercial_hourly %>% group_by(job_profile_current, race_ethnicity, gender, age_group_5)
current_commercial_median_job_race_gender_age5_hourly <- current_commercial_median_job_race_gender_age5_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_median_job_race_gender_age5_hourly)
```

```
## # A tibble: 2 x 6
## # Groups:   job_profile_current, race_ethnicity, gender [1]
##   job_profile_current  race_ethnicity       gender age_group_5 count median
##   <chr>                <chr>                <chr>  <fct>       <int>  <dbl>
## 1 600318 - Circulatio… Black or African Am… Male   60-64           6   23.8
## 2 600318 - Circulatio… Black or African Am… Male   45-49           7   21.5
```

```r
current_commercial_median_job_race_group_gender_age5_salaried <- commercial_salaried %>% group_by(job_profile_current, race_grouping, gender, age_group_5)
current_commercial_median_job_race_group_gender_age5_salaried <- current_commercial_median_job_race_group_gender_age5_salaried %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_median_job_race_group_gender_age5_salaried)
```

```
## # A tibble: 2 x 6
## # Groups:   job_profile_current, race_grouping, gender [2]
##   job_profile_current         race_grouping gender age_group_5 count median
##   <chr>                       <chr>         <chr>  <fct>       <int>  <dbl>
## 1 450220 - Sales Representat… white         Female 35-39           8 1.50e5
## 2 231303 - Client Service Ma… white         Female 25-29           8 6.62e4
```

```r
current_commercial_median_job_race_group_gender_age5_hourly <- commercial_hourly %>% group_by(job_profile_current, race_grouping, gender, age_group_5)
current_commercial_median_job_race_group_gender_age5_hourly <- current_commercial_median_job_race_group_gender_age5_hourly %>% summarise(
  count = length(current_base_pay),
  median = median(current_base_pay, na.rm = FALSE)
)
suppress_median(current_commercial_median_job_race_group_gender_age5_hourly)
```

```
## # A tibble: 2 x 6
## # Groups:   job_profile_current, race_grouping, gender [1]
##   job_profile_current         race_grouping gender age_group_5 count median
##   <chr>                       <chr>         <chr>  <fct>       <int>  <dbl>
## 1 600318 - Circulation Drive… person of co… Male   60-64           6   23.8
## 2 600318 - Circulation Drive… person of co… Male   45-49           7   21.5
```

### Performance evaluations


```r
commercial_ratings <- filter(ratings_combined, dept == 'Commercial')

commercial_ratings_gender <- commercial_ratings %>% group_by(gender)
commercial_ratings_gender <- commercial_ratings_gender %>% summarise(
  count = length(performance_rating),
  median = median(performance_rating, na.rm = TRUE)
)
suppress_median(commercial_ratings_gender)
```

```
## # A tibble: 2 x 3
##   gender count median
##   <chr>  <int>  <dbl>
## 1 Female  1308    3.3
## 2 Male     984    3.2
```

```r
commercial_ratings_race <- commercial_ratings %>% group_by(race_ethnicity)
commercial_ratings_race <- commercial_ratings_race %>% summarise(
  count = length(performance_rating),
  median = median(performance_rating, na.rm = TRUE)
)
suppress_median(commercial_ratings_race)
```

```
## # A tibble: 6 x 3
##   race_ethnicity                                       count median
##   <chr>                                                <int>  <dbl>
## 1 Asian (United States of America)                       168   3.3 
## 2 Two or More Races (United States of America)            36   3.3 
## 3 White (United States of America)                      1096   3.3 
## 4 Black or African American (United States of America)   860   3.2 
## 5 Hispanic or Latino (United States of America)           96   3.15
## 6 Prefer Not to Disclose (United States of America)       28   3
```

```r
commercial_ratings_race_gender <- commercial_ratings %>% group_by(race_ethnicity, gender)
commercial_ratings_race_gender <- commercial_ratings_race_gender %>% summarise(
  count = length(performance_rating),
  median = median(performance_rating, na.rm = TRUE)
)
suppress(commercial_ratings_race_gender)
```

```
## # A tibble: 12 x 4
## # Groups:   race_ethnicity [6]
##    race_ethnicity                                       gender count median
##    <chr>                                                <chr>  <int>  <dbl>
##  1 Asian (United States of America)                     Female   116   3.3 
##  2 Asian (United States of America)                     Male      52   3.1 
##  3 Black or African American (United States of America) Female   408   3.2 
##  4 Black or African American (United States of America) Male     452   3.05
##  5 Hispanic or Latino (United States of America)        Female    56   3.15
##  6 Hispanic or Latino (United States of America)        Male      40   3.1 
##  7 Prefer Not to Disclose (United States of America)    Female    16   3   
##  8 Prefer Not to Disclose (United States of America)    Male      12  NA   
##  9 Two or More Races (United States of America)         Female    20   3.3 
## 10 Two or More Races (United States of America)         Male      16   3.35
## 11 White (United States of America)                     Female   684   3.3 
## 12 White (United States of America)                     Male     412   3.3
```

```r
commercial_ratings_race_gender_under3 <- filter(commercial_ratings, performance_rating < 3.1) %>% group_by(race_grouping, gender)
commercial_ratings_race_gender_under3 <- commercial_ratings_race_gender_under3 %>% summarise(
  count = length(performance_rating),
  median = median(performance_rating, na.rm = TRUE)
)
suppress(commercial_ratings_race_gender_under3)
```

```
## # A tibble: 4 x 4
## # Groups:   race_grouping [2]
##   race_grouping   gender count median
##   <chr>           <chr>  <int>  <dbl>
## 1 person of color Female    81    3  
## 2 person of color Male     115    3  
## 3 white           Female    80    3  
## 4 white           Male      56    2.9
```

```r
commercial_ratings_race_gender_over4 <- filter(commercial_ratings, performance_rating > 3.9) %>% group_by(race_grouping, gender)
commercial_ratings_race_gender_over4 <- commercial_ratings_race_gender_over4 %>% summarise(
  count = length(performance_rating),
  median = median(performance_rating, na.rm = TRUE)
)
suppress(commercial_ratings_race_gender_over4)
```

```
## # A tibble: 3 x 4
## # Groups:   race_grouping [2]
##   race_grouping   gender count median
##   <chr>           <chr>  <int>  <dbl>
## 1 person of color Female     6   4.15
## 2 white           Female    17   4.2 
## 3 white           Male      12   4
```

### Pay changes


```r
commercial_change <- filter(reason_for_change_combined, dept == 'Commercial')

commercial_change_gender <- commercial_change %>% group_by(business_process_reason, gender)
commercial_change_gender %>% summarise(
  count = length(business_process_reason)
)
```

```
## # A tibble: 34 x 3
## # Groups:   business_process_reason [18]
##    business_process_reason                        gender count
##    <chr>                                          <chr>  <int>
##  1 Data Change > Data Change > Change Job Details Female    85
##  2 Data Change > Data Change > Change Job Details Male      61
##  3 Hire Employee > New Hire > Conversion          Female     6
##  4 Hire Employee > New Hire > Conversion          Male       3
##  5 Hire Employee > New Hire > Convert Contingent  Female     4
##  6 Hire Employee > New Hire > Convert Contingent  Male       3
##  7 Hire Employee > New Hire > Fill Vacancy        Female    70
##  8 Hire Employee > New Hire > Fill Vacancy        Male      58
##  9 Hire Employee > New Hire > New Position        Female    31
## 10 Hire Employee > New Hire > New Position        Male      22
## # … with 24 more rows
```

```r
commercial_change_race <- commercial_change %>% group_by(business_process_reason, race_ethnicity)
commercial_change_race <- commercial_change_race %>% summarise(
  count = length(business_process_reason)
)
suppress_count(commercial_change_race)
```

```
## # A tibble: 51 x 3
## # Groups:   business_process_reason [17]
##    business_process_reason              race_ethnicity                count
##    <chr>                                <chr>                         <int>
##  1 <NA>                                 White (United States of Amer…  2995
##  2 <NA>                                 Black or African American (U…  2340
##  3 <NA>                                 Asian (United States of Amer…   448
##  4 Request Compensation Change > Adjus… White (United States of Amer…   392
##  5 Request Compensation Change > Adjus… Black or African American (U…   339
##  6 <NA>                                 Hispanic or Latino (United S…   272
##  7 Merit > Performance > Annual Perfor… Black or African American (U…   239
##  8 Merit > Performance > Annual Perfor… White (United States of Amer…   220
##  9 Request Compensation Change > Adjus… White (United States of Amer…   179
## 10 Transfer > Transfer > Move to anoth… Black or African American (U…   116
## # … with 41 more rows
```

```r
commercial_change_race_gender <- commercial_change %>% group_by(business_process_reason, race_ethnicity, gender)
commercial_change_race_gender <- commercial_change_race_gender %>% summarise(
  count = length(business_process_reason)
)
suppress_count(commercial_change_race_gender)
```

```
## # A tibble: 77 x 4
## # Groups:   business_process_reason, race_ethnicity [45]
##    business_process_reason          race_ethnicity             gender count
##    <chr>                            <chr>                      <chr>  <int>
##  1 <NA>                             White (United States of A… Female  1839
##  2 <NA>                             Black or African American… Male    1272
##  3 <NA>                             White (United States of A… Male    1156
##  4 <NA>                             Black or African American… Female  1068
##  5 <NA>                             Asian (United States of A… Female   320
##  6 Request Compensation Change > A… White (United States of A… Female   236
##  7 Request Compensation Change > A… Black or African American… Female   179
##  8 <NA>                             Hispanic or Latino (Unite… Female   164
##  9 Request Compensation Change > A… Black or African American… Male     160
## 10 Request Compensation Change > A… White (United States of A… Male     156
## # … with 67 more rows
```

### Performance evaluations x merit raises


```r
reason_for_change_combined <- reason_for_change_combined %>% mutate(merit_raises = grepl('*Merit*', business_process_reason))
twenty14 = as.Date('2016-04-01')
twenty15 = as.Date('2017-04-01')
twenty16 = as.Date('2018-04-01')
twenty17 = as.Date('2019-04-01')
twenty18 = as.Date('2020-04-01')
  
reason_for_change_combined <- reason_for_change_combined %>%
    mutate(raise_after=case_when(
    effective_date < twenty14 ~ 'before 2015',
    effective_date < twenty15 ~ '2015',
    effective_date < twenty16 ~ '2016',
    effective_date < twenty17 ~ '2017',
    effective_date < twenty18 ~ '2018',
    TRUE ~ 'Other'))

merit_raises_commercial_gender_salaried <- filter(reason_for_change_combined, merit_raises == TRUE, dept == 'Commercial', pay_rate_type == 'Salaried') %>% group_by(gender)
merit_raises_commercial_gender_salaried <- merit_raises_commercial_gender_salaried %>% summarise(
  count = length(base_pay_change),
  median = median(base_pay_change, na.rm = TRUE)
)
suppress(merit_raises_commercial_gender_salaried)
```

```
## # A tibble: 2 x 3
##   gender count median
##   <chr>  <int>  <dbl>
## 1 Female    97  1317.
## 2 Male      74  1205.
```

```r
merit_raises_commercial_gender_hourly <- filter(reason_for_change_combined, merit_raises == TRUE, dept == 'Commercial', pay_rate_type == 'Hourly') %>% group_by(gender)
merit_raises_commercial_gender_hourly <- merit_raises_commercial_gender_hourly %>% summarise(
  count = length(base_pay_change),
  median = median(base_pay_change, na.rm = TRUE)
)
suppress(merit_raises_commercial_gender_hourly)
```

```
## # A tibble: 2 x 3
##   gender count median
##   <chr>  <int>  <dbl>
## 1 Female   170  0.425
## 2 Male     138  0.33
```

```r
merit_raises_commercial_race_salaried <- filter(reason_for_change_combined, merit_raises == TRUE, dept == 'Commercial', pay_rate_type == 'Salaried') %>% group_by(race_ethnicity)
merit_raises_commercial_race_salaried <- merit_raises_commercial_race_salaried %>% summarise(
  count = length(base_pay_change),
  median = median(base_pay_change, na.rm = TRUE)
)
suppress_median(merit_raises_commercial_race_salaried)
```

```
## # A tibble: 4 x 3
##   race_ethnicity                                       count median
##   <chr>                                                <int>  <dbl>
## 1 Asian (United States of America)                        23  1375 
## 2 Hispanic or Latino (United States of America)            6  1322.
## 3 White (United States of America)                       110  1287.
## 4 Black or African American (United States of America)    30  1117.
```

```r
merit_raises_commercial_race_hourly <- filter(reason_for_change_combined, merit_raises == TRUE, dept == 'Commercial', pay_rate_type == 'Hourly') %>% group_by(race_ethnicity)
merit_raises_commercial_race_hourly <- merit_raises_commercial_race_hourly %>% summarise(
  count = length(base_pay_change),
  median = median(base_pay_change, na.rm = TRUE)
)
suppress_median(merit_raises_commercial_race_hourly)
```

```
## # A tibble: 4 x 3
##   race_ethnicity                                       count median
##   <chr>                                                <int>  <dbl>
## 1 Asian (United States of America)                        11   0.45
## 2 White (United States of America)                        85   0.42
## 3 Hispanic or Latino (United States of America)           11   0.37
## 4 Black or African American (United States of America)   197   0.35
```

```r
merit_raises_commercial_race_group_salaried <- filter(reason_for_change_combined, merit_raises == TRUE, dept == 'Commercial', pay_rate_type == 'Salaried') %>% group_by(race_grouping)
merit_raises_commercial_race_group_salaried <- merit_raises_commercial_race_group_salaried %>% summarise(
  count = length(base_pay_change),
  median = median(base_pay_change, na.rm = TRUE)
)
suppress_median(merit_raises_commercial_race_group_salaried)
```

```
## # A tibble: 2 x 3
##   race_grouping   count median
##   <chr>           <int>  <dbl>
## 1 white             110  1287.
## 2 person of color    60  1225
```

```r
merit_raises_commercial_race_group_hourly <- filter(reason_for_change_combined, merit_raises == TRUE, dept == 'Commercial', pay_rate_type == 'Hourly') %>% group_by(race_grouping)
merit_raises_commercial_race_group_hourly <- merit_raises_commercial_race_group_hourly %>% summarise(
  count = length(base_pay_change),
  median = median(base_pay_change, na.rm = TRUE)
)
suppress_median(merit_raises_commercial_race_group_hourly)
```

```
## # A tibble: 2 x 3
##   race_grouping   count median
##   <chr>           <int>  <dbl>
## 1 white              85   0.42
## 2 person of color   223   0.35
```

```r
merit_raises_commercial_gender_race_group_salaried <- filter(reason_for_change_combined, merit_raises == TRUE, dept == 'Commercial', pay_rate_type == 'Salaried') %>% group_by(race_grouping, gender)
merit_raises_commercial_gender_race_group_salaried <- merit_raises_commercial_gender_race_group_salaried %>% summarise(
  count = length(base_pay_change),
  median = median(base_pay_change, na.rm = TRUE)
)
suppress_median(merit_raises_commercial_gender_race_group_salaried)
```

```
## # A tibble: 4 x 4
## # Groups:   race_grouping [2]
##   race_grouping   gender count median
##   <chr>           <chr>  <int>  <dbl>
## 1 white           Female    69  1317.
## 2 person of color Female    27  1305 
## 3 white           Male      41  1282.
## 4 person of color Male      33  1134.
```

```r
merit_raises_commercial_gender_race_group_hourly <- filter(reason_for_change_combined, merit_raises == TRUE, dept == 'Commercial', pay_rate_type == 'Hourly') %>% group_by(race_grouping, gender)
merit_raises_commercial_gender_race_group_hourly <- merit_raises_commercial_gender_race_group_hourly %>% summarise(
  count = length(base_pay_change),
  median = median(base_pay_change, na.rm = TRUE)
)
suppress_median(merit_raises_commercial_gender_race_group_hourly)
```

```
## # A tibble: 4 x 4
## # Groups:   race_grouping [2]
##   race_grouping   gender count median
##   <chr>           <chr>  <int>  <dbl>
## 1 white           Female    44  0.515
## 2 person of color Female   126  0.375
## 3 white           Male      41  0.35 
## 4 person of color Male      97  0.32
```

```r
fifteen_raises_amount <- filter(reason_for_change_combined, merit_raises == TRUE, dept == 'Commercial', pay_rate_type == 'Salaried', raise_after == '2015') %>% group_by(race_grouping, gender)
fifteen_raises_amount <- fifteen_raises_amount %>% summarise(
  count = length(base_pay_change),
  median_raise = median(base_pay_change, na.rm = TRUE)
)
suppress(fifteen_raises_amount)
```

```
## # A tibble: 2 x 4
## # Groups:   race_grouping [1]
##   race_grouping gender count median_raise
##   <chr>         <chr>  <int>        <dbl>
## 1 white         Female     7         937.
## 2 white         Male       5         851.
```

```r
fifteen_raises_score <- filter(reason_for_change_combined, merit_raises == TRUE, dept == 'Commercial', pay_rate_type == 'Salaried', raise_after == '2015') %>% group_by(race_grouping, gender)
fifteen_raises_score <- fifteen_raises_score %>% summarise(
  count = length('2015_annual_performance_rating'),
  median = median('2015_annual_performance_rating', na.rm = TRUE)
)
suppress(fifteen_raises_score)
```

```
## # A tibble: 0 x 4
## # Groups:   race_grouping [0]
## # … with 4 variables: race_grouping <chr>, gender <chr>, count <int>,
## #   median <chr>
```

```r
sixteen_raises_amount <- filter(reason_for_change_combined, merit_raises == TRUE, dept == 'Commercial', pay_rate_type == 'Salaried', raise_after == '2016') %>% group_by(race_grouping, gender)
sixteen_raises_amount <- sixteen_raises_amount %>% summarise(
  count = length(base_pay_change),
  median_raise = median(base_pay_change, na.rm = TRUE)
)
suppress(sixteen_raises_amount)
```

```
## # A tibble: 4 x 4
## # Groups:   race_grouping [2]
##   race_grouping   gender count median_raise
##   <chr>           <chr>  <int>        <dbl>
## 1 person of color Female     5        1729.
## 2 person of color Male       6        1507.
## 3 white           Female     9        1683 
## 4 white           Male       7        1291.
```

```r
sixteen_raises_score <- filter(reason_for_change_combined, merit_raises == TRUE, dept == 'Commercial', pay_rate_type == 'Salaried', raise_after == '2016') %>% group_by(race_grouping, gender)
sixteen_raises_score <- sixteen_raises_score %>% summarise(
  count = length('2016_annual_performance_rating'),
  median = median('2016_annual_performance_rating', na.rm = TRUE)
)
suppress(sixteen_raises_score)
```

```
## # A tibble: 0 x 4
## # Groups:   race_grouping [0]
## # … with 4 variables: race_grouping <chr>, gender <chr>, count <int>,
## #   median <chr>
```

```r
seventeen_raises_amount <- filter(reason_for_change_combined, merit_raises == TRUE, dept == 'Commercial', pay_rate_type == 'Salaried', raise_after == '2017') %>% group_by(race_grouping, gender)
seventeen_raises_amount <- seventeen_raises_amount %>% summarise(
  count = length(base_pay_change),
  median_raise = median(base_pay_change, na.rm = TRUE)
)
suppress(seventeen_raises_amount)
```

```
## # A tibble: 3 x 4
## # Groups:   race_grouping [2]
##   race_grouping   gender count median_raise
##   <chr>           <chr>  <int>        <dbl>
## 1 person of color Male       8        1000 
## 2 white           Female    13        1398.
## 3 white           Male       5        1415.
```

```r
seventeen_raises_score <- filter(reason_for_change_combined, merit_raises == TRUE, dept == 'Commercial', pay_rate_type == 'Salaried', raise_after == '2017') %>% group_by(race_grouping, gender)
seventeen_raises_score <- seventeen_raises_score %>% summarise(
  count = length('2017_annual_performance_rating'),
  median = median('2017_annual_performance_rating', na.rm = TRUE)
)
suppress(seventeen_raises_score)
```

```
## # A tibble: 0 x 4
## # Groups:   race_grouping [0]
## # … with 4 variables: race_grouping <chr>, gender <chr>, count <int>,
## #   median <chr>
```

```r
eighteen_raises_amount <- filter(reason_for_change_combined, merit_raises == TRUE, dept == 'Commercial', pay_rate_type == 'Salaried', raise_after == '2018') %>% group_by(race_grouping, gender)
eighteen_raises_amount <- eighteen_raises_amount %>% summarise(
  count = length(base_pay_change),
  median_raise = median(base_pay_change, na.rm = TRUE)
)
suppress(eighteen_raises_amount)
```

```
## # A tibble: 4 x 4
## # Groups:   race_grouping [2]
##   race_grouping   gender count median_raise
##   <chr>           <chr>  <int>        <dbl>
## 1 person of color Female     7        1416.
## 2 person of color Male       7        1050 
## 3 white           Female    21        1669.
## 4 white           Male       8        1417.
```

```r
eighteen_raises_score <- filter(reason_for_change_combined, merit_raises == TRUE, dept == 'Commercial', pay_rate_type == 'Salaried', raise_after == '2018') %>% group_by(race_grouping, gender)
eighteen_raises_score <- eighteen_raises_score %>% summarise(
  count = length('2018_annual_performance_rating'),
  median = median('2018_annual_performance_rating', na.rm = TRUE)
)
suppress(eighteen_raises_score)
```

```
## # A tibble: 0 x 4
## # Groups:   race_grouping [0]
## # … with 4 variables: race_grouping <chr>, gender <chr>, count <int>,
## #   median <chr>
```

```r
merit_raises_15 <- filter(reason_for_change_combined, raise_after == '2015', merit_raises == TRUE)
merit_raises_16 <- filter(reason_for_change_combined, raise_after == '2016', merit_raises == TRUE)
merit_raises_17 <- filter(reason_for_change_combined, raise_after == '2017', merit_raises == TRUE)
merit_raises_18 <- filter(reason_for_change_combined, raise_after == '2018', merit_raises == TRUE)

merit_raises_15 <- merit_raises_15[,c('base_pay_change','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2015_annual_performance_rating')]
merit_raises_16 <- merit_raises_16[,c('base_pay_change','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2016_annual_performance_rating')]
merit_raises_17 <- merit_raises_17[,c('base_pay_change','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2017_annual_performance_rating')]
merit_raises_18 <- merit_raises_18[,c('base_pay_change','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','2018_annual_performance_rating')]

names(merit_raises_15) <- c('base_pay_change','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','performance_rating')
names(merit_raises_16) <- c('base_pay_change','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','performance_rating')
names(merit_raises_17) <- c('base_pay_change','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','performance_rating')
names(merit_raises_18) <- c('base_pay_change','pay_rate_type','gender','race_ethnicity','race_grouping','age_group_5','dept','tier','performance_rating')

merit_raises_combined <- rbind(merit_raises_15, merit_raises_16, merit_raises_17, merit_raises_18)

commercial_salaried_raises <- filter(merit_raises_combined, pay_rate_type == 'Salaried', dept == 'Commercial') %>% group_by(race_grouping, gender)
commercial_salaried_raises <- commercial_salaried_raises %>% summarise(
  count = length(base_pay_change),
  median = median(base_pay_change, na.rm = TRUE)
)
suppress(commercial_salaried_raises)
```

```
## # A tibble: 4 x 4
## # Groups:   race_grouping [2]
##   race_grouping   gender count median
##   <chr>           <chr>  <int>  <dbl>
## 1 person of color Female    20  1360.
## 2 person of color Male      24  1096.
## 3 white           Female    50  1344.
## 4 white           Male      25  1291.
```

```r
commercial_salaried_raises_scores <- filter(merit_raises_combined, pay_rate_type == 'Salaried', dept == 'Commercial') %>% group_by(race_grouping, gender)
commercial_salaried_raises_scores <- commercial_salaried_raises_scores %>% summarise(
  count = length(performance_rating),
  median = median(performance_rating, na.rm = TRUE)
)
suppress(commercial_salaried_raises_scores)
```

```
## # A tibble: 4 x 4
## # Groups:   race_grouping [2]
##   race_grouping   gender count median
##   <chr>           <chr>  <int>  <dbl>
## 1 person of color Female    20    3.5
## 2 person of color Male      24    3.3
## 3 white           Female    50    3.4
## 4 white           Male      25    3.4
```

```r
commercial_hourly_raises <- filter(merit_raises_combined, pay_rate_type == 'Hourly', dept == 'Commercial') %>% group_by(race_grouping, gender)
commercial_hourly_raises <- commercial_hourly_raises %>% summarise(
  count = length(base_pay_change),
  median = median(base_pay_change, na.rm = TRUE)
)
suppress(commercial_hourly_raises)
```

```
## # A tibble: 4 x 4
## # Groups:   race_grouping [2]
##   race_grouping   gender count median
##   <chr>           <chr>  <int>  <dbl>
## 1 person of color Female   102  0.37 
## 2 person of color Male      89  0.28 
## 3 white           Female    34  0.515
## 4 white           Male      37  0.35
```

```r
commercial_hourly_raises_scores <- filter(merit_raises_combined, pay_rate_type == 'Hourly', dept == 'Commercial') %>% group_by(race_grouping, gender)
commercial_hourly_raises_scores <- commercial_hourly_raises_scores %>% summarise(
  count = length(performance_rating),
  median = median(performance_rating, na.rm = TRUE)
)
suppress(commercial_hourly_raises_scores)
```

```
## # A tibble: 4 x 4
## # Groups:   race_grouping [2]
##   race_grouping   gender count median
##   <chr>           <chr>  <int>  <dbl>
## 1 person of color Female   102    3.3
## 2 person of color Male      89    3.2
## 3 white           Female    34    3.4
## 4 white           Male      37    3.2
```

### Regression


```r
commercial_salaried_regression <- commercial_salaried[,c('department','gender','race_ethnicity','current_base_pay','job_profile_current','cost_center_current','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating','age','years_of_service','age_group_5','years_of_service_grouped','dept','desk','tier','race_grouping')]
commercial_salaried_regression <- fastDummies::dummy_cols(commercial_salaried_regression, select_columns = c('gender','race_ethnicity','age_group_5','years_of_service_grouped','dept','desk','tier','race_grouping'))
names(commercial_salaried_regression) <- gsub(' ', '_', names(commercial_salaried_regression))
names(commercial_salaried_regression) <- gsub('-', 'to', names(commercial_salaried_regression))
names(commercial_salaried_regression) <- gsub('\\+', '_over', names(commercial_salaried_regression))
names(commercial_salaried_regression) <- gsub('<', 'under_', names(commercial_salaried_regression))

linearMod41 <- lm(formula = current_base_pay ~ gender_Female + gender_Male, data=commercial_salaried_regression)
summary(linearMod41)
```

```
## 
## Call:
## lm(formula = current_base_pay ~ gender_Female + gender_Male, 
##     data = commercial_salaried_regression)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -42573 -22322  -9445   9259 115917 
## 
## Coefficients: (1 not defined because of singularities)
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)      94863       5051  18.780   <2e-16 ***
## gender_Female     1739       6282   0.277    0.782    
## gender_Male         NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 34630 on 131 degrees of freedom
## Multiple R-squared:  0.0005845,	Adjusted R-squared:  -0.007045 
## F-statistic: 0.07662 on 1 and 131 DF,  p-value: 0.7824
```

```r
linearMod42 <- lm(formula = current_base_pay ~ race_grouping_white + race_grouping_person_of_color, data=commercial_salaried_regression)
summary(linearMod42)
```

```
## 
## Call:
## lm(formula = current_base_pay ~ race_grouping_white + race_grouping_person_of_color, 
##     data = commercial_salaried_regression)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -44088 -23088  -8978   9692 111692 
## 
## Coefficients:
##                               Estimate Std. Error t value Pr(>|t|)   
## (Intercept)                      78404      24283   3.229  0.00157 **
## race_grouping_white              20684      24527   0.843  0.40059   
## race_grouping_person_of_color     9090      25030   0.363  0.71709   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 34340 on 130 degrees of freedom
## Multiple R-squared:  0.02468,	Adjusted R-squared:  0.009673 
## F-statistic: 1.645 on 2 and 130 DF,  p-value: 0.1971
```

```r
linearMod43 <- lm(formula = current_base_pay ~ gender_Female + gender_Male + race_grouping_white + race_grouping_person_of_color, data=commercial_salaried_regression)
summary(linearMod43)
```

```
## 
## Call:
## lm(formula = current_base_pay ~ gender_Female + gender_Male + 
##     race_grouping_white + race_grouping_person_of_color, data = commercial_salaried_regression)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -44357 -23357  -8858   9423 112255 
## 
## Coefficients: (1 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)   
## (Intercept)                    77571.2    25184.5   3.080  0.00253 **
## gender_Female                    832.3     6333.3   0.131  0.89565   
## gender_Male                         NA         NA      NA       NA   
## race_grouping_white            20953.5    24705.1   0.848  0.39793   
## race_grouping_person_of_color   9479.6    25300.1   0.375  0.70851   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 34470 on 129 degrees of freedom
## Multiple R-squared:  0.02481,	Adjusted R-squared:  0.00213 
## F-statistic: 1.094 on 3 and 129 DF,  p-value: 0.3542
```

```r
linearMod44 <- lm(formula = current_base_pay ~ gender_Female + gender_Male + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=commercial_salaried_regression)
summary(linearMod44)
```

```
## 
## Call:
## lm(formula = current_base_pay ~ gender_Female + gender_Male + 
##     age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + 
##     age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + 
##     age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + 
##     age_group_5_65_over, data = commercial_salaried_regression)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -69838 -19792  -4420  13357 101706 
## 
## Coefficients: (2 not defined because of singularities)
##                      Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          111172.9    21450.2   5.183 8.77e-07 ***
## gender_Female          9550.2     5934.5   1.609   0.1101    
## gender_Male                NA         NA      NA       NA    
## age_group_5_under_25 -54303.5    23972.3  -2.265   0.0253 *  
## age_group_5_25to29   -41249.7    22596.0  -1.826   0.0704 .  
## age_group_5_30to34    -2099.2    22994.9  -0.091   0.9274    
## age_group_5_35to39     -814.7    22804.3  -0.036   0.9716    
## age_group_5_40to44     5922.0    24293.7   0.244   0.8078    
## age_group_5_45to49   -13931.3    23261.7  -0.599   0.5504    
## age_group_5_50to54   -24879.8    23861.9  -1.043   0.2992    
## age_group_5_55to59   -21494.8    23684.1  -0.908   0.3659    
## age_group_5_60to64   -21443.9    23617.1  -0.908   0.3657    
## age_group_5_65_over        NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 30340 on 122 degrees of freedom
## Multiple R-squared:  0.2858,	Adjusted R-squared:  0.2272 
## F-statistic: 4.882 on 10 and 122 DF,  p-value: 6.47e-06
```

```r
linearMod45 <- lm(formula = current_base_pay ~ race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=commercial_salaried_regression)
summary(linearMod45)
```

```
## 
## Call:
## lm(formula = current_base_pay ~ race_grouping_white + race_grouping_person_of_color + 
##     age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + 
##     age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + 
##     age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + 
##     age_group_5_65_over, data = commercial_salaried_regression)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -62296 -18193  -3480  12866  90105 
## 
## Coefficients: (1 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)  
## (Intercept)                    70232.3    30123.3   2.331   0.0214 *
## race_grouping_white            49331.5    21787.6   2.264   0.0253 *
## race_grouping_person_of_color  32549.8    22264.8   1.462   0.1464  
## age_group_5_under_25          -53376.0    22895.2  -2.331   0.0214 *
## age_group_5_25to29            -38371.2    21443.9  -1.789   0.0761 .
## age_group_5_30to34              1111.7    22157.4   0.050   0.9601  
## age_group_5_35to39               231.2    21945.2   0.011   0.9916  
## age_group_5_40to44             15230.7    23150.7   0.658   0.5119  
## age_group_5_45to49             -9434.3    22320.8  -0.423   0.6733  
## age_group_5_50to54            -24907.3    22895.2  -1.088   0.2788  
## age_group_5_55to59            -21754.2    22836.8  -0.953   0.3427  
## age_group_5_60to64            -19302.0    22769.9  -0.848   0.3983  
## age_group_5_65_over                 NA         NA      NA       NA  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 29380 on 121 degrees of freedom
## Multiple R-squared:  0.3353,	Adjusted R-squared:  0.2749 
## F-statistic: 5.549 on 11 and 121 DF,  p-value: 3.833e-07
```

```r
linearMod46 <- lm(formula = current_base_pay ~ gender_Female + gender_Male + race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=commercial_salaried_regression)
summary(linearMod46)
```

```
## 
## Call:
## lm(formula = current_base_pay ~ gender_Female + gender_Male + 
##     race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + 
##     age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + 
##     age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + 
##     age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, 
##     data = commercial_salaried_regression)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -63447 -17726  -2978  12784  95358 
## 
## Coefficients: (2 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)  
## (Intercept)                      67196      29978   2.242   0.0268 *
## gender_Female                     9360       5747   1.629   0.1060  
## gender_Male                         NA         NA      NA       NA  
## race_grouping_white              51960      21700   2.394   0.0182 *
## race_grouping_person_of_color    35994      22215   1.620   0.1078  
## age_group_5_under_25            -60538      23162  -2.614   0.0101 *
## age_group_5_25to29              -45882      21792  -2.105   0.0373 *
## age_group_5_30to34               -3734      22208  -0.168   0.8667  
## age_group_5_35to39               -4268      21971  -0.194   0.8463  
## age_group_5_40to44                7429      23488   0.316   0.7523  
## age_group_5_45to49              -14443      22382  -0.645   0.5200  
## age_group_5_50to54              -31133      23059  -1.350   0.1795  
## age_group_5_55to59              -26190      22845  -1.146   0.2539  
## age_group_5_60to64              -22964      22728  -1.010   0.3143  
## age_group_5_65_over                 NA         NA      NA       NA  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 29190 on 120 degrees of freedom
## Multiple R-squared:  0.3497,	Adjusted R-squared:  0.2847 
## F-statistic: 5.377 on 12 and 120 DF,  p-value: 3.101e-07
```

```r
linearMod47 <- lm(formula = current_base_pay ~ gender_Female + gender_Male + race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over + years_of_service_grouped_0 + years_of_service_grouped_1to2 + years_of_service_grouped_3to5 + years_of_service_grouped_6to10 + years_of_service_grouped_11to15 + years_of_service_grouped_16to20 + years_of_service_grouped_21to25 + years_of_service_grouped_25_over, data=commercial_salaried_regression)
summary(linearMod47)
```

```
## 
## Call:
## lm(formula = current_base_pay ~ gender_Female + gender_Male + 
##     race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + 
##     age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + 
##     age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + 
##     age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over + 
##     years_of_service_grouped_0 + years_of_service_grouped_1to2 + 
##     years_of_service_grouped_3to5 + years_of_service_grouped_6to10 + 
##     years_of_service_grouped_11to15 + years_of_service_grouped_16to20 + 
##     years_of_service_grouped_21to25 + years_of_service_grouped_25_over, 
##     data = commercial_salaried_regression)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -60538 -17883  -3429  16197  91640 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)  
## (Intercept)                       66064.9    31507.6   2.097   0.0382 *
## gender_Female                      9634.6     6095.5   1.581   0.1168  
## gender_Male                            NA         NA      NA       NA  
## race_grouping_white               53503.0    21924.2   2.440   0.0162 *
## race_grouping_person_of_color     39591.6    22445.3   1.764   0.0804 .
## age_group_5_under_25             -68107.0    26381.2  -2.582   0.0111 *
## age_group_5_25to29               -54633.3    25065.3  -2.180   0.0314 *
## age_group_5_30to34                -9711.9    25048.3  -0.388   0.6989  
## age_group_5_35to39                -7693.3    24057.3  -0.320   0.7497  
## age_group_5_40to44                 -228.9    26494.5  -0.009   0.9931  
## age_group_5_45to49               -17846.6    25040.8  -0.713   0.4775  
## age_group_5_50to54               -29093.0    25702.4  -1.132   0.2601  
## age_group_5_55to59               -30069.1    23273.9  -1.292   0.1990  
## age_group_5_60to64               -25234.2    26257.9  -0.961   0.3386  
## age_group_5_65_over                    NA         NA      NA       NA  
## years_of_service_grouped_0         5031.0    19054.1   0.264   0.7922  
## years_of_service_grouped_1to2      9283.9    18949.1   0.490   0.6251  
## years_of_service_grouped_3to5     10317.9    18590.9   0.555   0.5800  
## years_of_service_grouped_6to10    -2878.6    18293.6  -0.157   0.8752  
## years_of_service_grouped_11to15  -20650.9    21676.3  -0.953   0.3428  
## years_of_service_grouped_16to20   -2368.4    22591.2  -0.105   0.9167  
## years_of_service_grouped_21to25   -3082.4    22725.9  -0.136   0.8924  
## years_of_service_grouped_25_over       NA         NA      NA       NA  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 29230 on 113 degrees of freedom
## Multiple R-squared:  0.3858,	Adjusted R-squared:  0.2825 
## F-statistic: 3.735 on 19 and 113 DF,  p-value: 5.587e-06
```

```r
merit_raises_combined_salaried_regression <- filter(merit_raises_combined, dept == 'Commercial', pay_rate_type == 'Salaried')

merit_raises_combined_salaried_regression <- fastDummies::dummy_cols(merit_raises_combined_salaried_regression, select_columns = c('gender','race_grouping','age_group_5'))
names(merit_raises_combined_salaried_regression) <- gsub(' ', '_', names(merit_raises_combined_salaried_regression))
names(merit_raises_combined_salaried_regression) <- gsub('-', 'to', names(merit_raises_combined_salaried_regression))
names(merit_raises_combined_salaried_regression) <- gsub('\\+', '_over', names(merit_raises_combined_salaried_regression))
names(merit_raises_combined_salaried_regression) <- gsub('<', 'under_', names(merit_raises_combined_salaried_regression))

linearMod48 <- lm(formula = base_pay_change ~ gender_Female + gender_Male, data=merit_raises_combined_salaried_regression)
summary(linearMod48)
```

```
## 
## Call:
## lm(formula = base_pay_change ~ gender_Female + gender_Male, data = merit_raises_combined_salaried_regression)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1442.2  -631.0  -253.3   258.0  4270.5 
## 
## Coefficients: (1 not defined because of singularities)
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     1349.6      144.8   9.319 8.22e-16 ***
## gender_Female    307.3      188.3   1.632    0.105    
## gender_Male         NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1014 on 118 degrees of freedom
## Multiple R-squared:  0.02208,	Adjusted R-squared:  0.01379 
## F-statistic: 2.664 on 1 and 118 DF,  p-value: 0.1053
```

```r
linearMod49 <- lm(formula = base_pay_change ~ race_grouping_white + race_grouping_person_of_color, data=merit_raises_combined_salaried_regression)
summary(linearMod49)
```

```
## 
## Call:
## lm(formula = base_pay_change ~ race_grouping_white + race_grouping_person_of_color, 
##     data = merit_raises_combined_salaried_regression)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1374.7  -642.8  -256.7   329.2  4338.1 
## 
## Coefficients:
##                               Estimate Std. Error t value Pr(>|t|)
## (Intercept)                    1400.00    1026.78   1.363    0.175
## race_grouping_white             189.38    1033.60   0.183    0.855
## race_grouping_person_of_color    35.73    1038.38   0.034    0.973
## 
## Residual standard error: 1027 on 117 degrees of freedom
## Multiple R-squared:  0.005419,	Adjusted R-squared:  -0.01158 
## F-statistic: 0.3188 on 2 and 117 DF,  p-value: 0.7277
```

```r
linearMod50 <- lm(formula = base_pay_change ~ gender_Female + gender_Male + race_grouping_white + race_grouping_person_of_color, data=merit_raises_combined_salaried_regression)
summary(linearMod50)
```

```
## 
## Call:
## lm(formula = base_pay_change ~ gender_Female + gender_Male + 
##     race_grouping_white + race_grouping_person_of_color, data = merit_raises_combined_salaried_regression)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1472.1  -575.6  -252.3   303.5  4240.7 
## 
## Coefficients: (1 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)
## (Intercept)                     1107.7     1039.7   1.065    0.289
## gender_Female                    292.3      194.5   1.503    0.136
## gender_Male                         NA         NA      NA       NA
## race_grouping_white              286.8     1030.1   0.278    0.781
## race_grouping_person_of_color    195.2     1038.3   0.188    0.851
## 
## Residual standard error: 1021 on 116 degrees of freedom
## Multiple R-squared:  0.02442,	Adjusted R-squared:  -0.0008146 
## F-statistic: 0.9677 on 3 and 116 DF,  p-value: 0.4105
```

```r
linearMod51 <- lm(formula = base_pay_change ~ gender_Female + gender_Male + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=merit_raises_combined_salaried_regression)
summary(linearMod51)
```

```
## 
## Call:
## lm(formula = base_pay_change ~ gender_Female + gender_Male + 
##     age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + 
##     age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + 
##     age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + 
##     age_group_5_65_over, data = merit_raises_combined_salaried_regression)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1656.3  -536.9  -176.1   316.8  3731.3 
## 
## Coefficients: (3 not defined because of singularities)
##                      Estimate Std. Error t value Pr(>|t|)    
## (Intercept)           1094.63     292.17   3.746 0.000287 ***
## gender_Female          317.63     228.83   1.388 0.167912    
## gender_Male                NA         NA      NA       NA    
## age_group_5_under_25   220.97    1045.13   0.211 0.832943    
## age_group_5_25to29     425.73     384.14   1.108 0.270164    
## age_group_5_30to34     -33.56     403.89  -0.083 0.933927    
## age_group_5_35to39     649.61     378.35   1.717 0.088800 .  
## age_group_5_40to44     883.45     496.12   1.781 0.077722 .  
## age_group_5_45to49      63.78     362.88   0.176 0.860804    
## age_group_5_50to54     -13.25     424.71  -0.031 0.975176    
## age_group_5_55to59     306.92     536.75   0.572 0.568612    
## age_group_5_60to64         NA         NA      NA       NA    
## age_group_5_65_over        NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1003 on 110 degrees of freedom
## Multiple R-squared:  0.1069,	Adjusted R-squared:  0.03384 
## F-statistic: 1.463 on 9 and 110 DF,  p-value: 0.1706
```

```r
linearMod52 <- lm(formula = base_pay_change ~ race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=merit_raises_combined_salaried_regression)
summary(linearMod52)
```

```
## 
## Call:
## lm(formula = base_pay_change ~ race_grouping_white + race_grouping_person_of_color + 
##     age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + 
##     age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + 
##     age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + 
##     age_group_5_65_over, data = merit_raises_combined_salaried_regression)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1745.2  -580.7  -163.9   286.8  3988.9 
## 
## Coefficients: (2 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)  
## (Intercept)                      755.3     1075.9   0.702   0.4841  
## race_grouping_white              412.0     1036.1   0.398   0.6917  
## race_grouping_person_of_color    174.9     1048.7   0.167   0.8678  
## age_group_5_under_25             148.3     1051.7   0.141   0.8881  
## age_group_5_25to29               644.7      369.7   1.744   0.0840 .
## age_group_5_30to34               237.5      380.1   0.625   0.5335  
## age_group_5_35to39               786.4      398.8   1.972   0.0511 .
## age_group_5_40to44              1217.3      465.2   2.617   0.0101 *
## age_group_5_45to49               288.2      372.2   0.774   0.4404  
## age_group_5_50to54               213.1      405.6   0.525   0.6004  
## age_group_5_55to59               630.9      507.9   1.242   0.2168  
## age_group_5_60to64                  NA         NA      NA       NA  
## age_group_5_65_over                 NA         NA      NA       NA  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1010 on 109 degrees of freedom
## Multiple R-squared:  0.1029,	Adjusted R-squared:  0.02056 
## F-statistic:  1.25 on 10 and 109 DF,  p-value: 0.2681
```

```r
linearMod53 <- lm(formula = base_pay_change ~ gender_Female + gender_Male + race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=merit_raises_combined_salaried_regression)
summary(linearMod53)
```

```
## 
## Call:
## lm(formula = base_pay_change ~ gender_Female + gender_Male + 
##     race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + 
##     age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + 
##     age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + 
##     age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, 
##     data = merit_raises_combined_salaried_regression)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1726.0  -516.9  -190.1   298.9  3825.7 
## 
## Coefficients: (3 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)  
## (Intercept)                     628.95    1078.72   0.583   0.5611  
## gender_Female                   283.57     235.15   1.206   0.2305  
## gender_Male                         NA         NA      NA       NA  
## race_grouping_white             486.84    1035.84   0.470   0.6393  
## race_grouping_person_of_color   300.96    1051.74   0.286   0.7753  
## age_group_5_under_25            199.80    1050.42   0.190   0.8495  
## age_group_5_25to29              487.48     391.31   1.246   0.2155  
## age_group_5_30to34               35.62     414.64   0.086   0.9317  
## age_group_5_35to39              754.73     398.79   1.893   0.0611 .
## age_group_5_40to44              966.05     508.81   1.899   0.0603 .
## age_group_5_45to49              168.08     384.54   0.437   0.6629  
## age_group_5_50to54               34.69     430.98   0.080   0.9360  
## age_group_5_55to59              381.78     547.29   0.698   0.4869  
## age_group_5_60to64                  NA         NA      NA       NA  
## age_group_5_65_over                 NA         NA      NA       NA  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1008 on 108 degrees of freedom
## Multiple R-squared:  0.1148,	Adjusted R-squared:  0.02463 
## F-statistic: 1.273 on 11 and 108 DF,  p-value: 0.2497
```

```r
linearMod54 <- lm(formula = performance_rating ~ gender_Female + gender_Male, data=merit_raises_combined_salaried_regression)
summary(linearMod54)
```

```
## 
## Call:
## lm(formula = performance_rating ~ gender_Female + gender_Male, 
##     data = merit_raises_combined_salaried_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.64714 -0.19583 -0.04714  0.20417  0.90417 
## 
## Coefficients: (1 not defined because of singularities)
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    3.39583    0.04602  73.786   <2e-16 ***
## gender_Female  0.05131    0.05975   0.859    0.392    
## gender_Male         NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3189 on 116 degrees of freedom
##   (2 observations deleted due to missingness)
## Multiple R-squared:  0.006316,	Adjusted R-squared:  -0.00225 
## F-statistic: 0.7373 on 1 and 116 DF,  p-value: 0.3923
```

```r
linearMod55 <- lm(formula = performance_rating ~ race_grouping_white + race_grouping_person_of_color, data=merit_raises_combined_salaried_regression)
summary(linearMod55)
```

```
## 
## Call:
## lm(formula = performance_rating ~ race_grouping_white + race_grouping_person_of_color, 
##     data = merit_raises_combined_salaried_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.63514 -0.23514 -0.03514  0.18837  0.88837 
## 
## Coefficients:
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    3.40000    0.32104  10.591   <2e-16 ***
## race_grouping_white            0.03514    0.32320   0.109    0.914    
## race_grouping_person_of_color  0.01163    0.32475   0.036    0.971    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.321 on 115 degrees of freedom
##   (2 observations deleted due to missingness)
## Multiple R-squared:  0.001325,	Adjusted R-squared:  -0.01604 
## F-statistic: 0.07628 on 2 and 115 DF,  p-value: 0.9266
```

```r
linearMod56 <- lm(formula = performance_rating ~ gender_Female + gender_Male + race_grouping_white + race_grouping_person_of_color, data=merit_raises_combined_salaried_regression)
summary(linearMod56)
```

```
## 
## Call:
## lm(formula = performance_rating ~ gender_Female + gender_Male + 
##     race_grouping_white + race_grouping_person_of_color, data = merit_raises_combined_salaried_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.65112 -0.20182 -0.04513  0.20716  0.91016 
## 
## Coefficients: (1 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    3.35070    0.32750  10.231   <2e-16 ***
## gender_Female                  0.04930    0.06209   0.794    0.429    
## gender_Male                         NA         NA      NA       NA    
## race_grouping_white            0.05112    0.32435   0.158    0.875    
## race_grouping_person_of_color  0.03915    0.32712   0.120    0.905    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3216 on 114 degrees of freedom
##   (2 observations deleted due to missingness)
## Multiple R-squared:  0.006818,	Adjusted R-squared:  -0.01932 
## F-statistic: 0.2609 on 3 and 114 DF,  p-value: 0.8535
```

```r
linearMod57 <- lm(formula = performance_rating ~ gender_Female + gender_Male + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=merit_raises_combined_salaried_regression)
summary(linearMod57)
```

```
## 
## Call:
## lm(formula = performance_rating ~ gender_Female + gender_Male + 
##     age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + 
##     age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + 
##     age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + 
##     age_group_5_65_over, data = merit_raises_combined_salaried_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.64015 -0.17720 -0.04015  0.15956  0.87133 
## 
## Coefficients: (3 not defined because of singularities)
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)           3.340445   0.090604  36.869   <2e-16 ***
## gender_Female         0.007333   0.072497   0.101   0.9196    
## gender_Male                 NA         NA      NA       NA    
## age_group_5_under_25 -0.040445   0.323985  -0.125   0.9009    
## age_group_5_25to29    0.029423   0.119983   0.245   0.8067    
## age_group_5_30to34    0.035438   0.128245   0.276   0.7828    
## age_group_5_35to39    0.128850   0.117283   1.099   0.2744    
## age_group_5_40to44    0.127223   0.154297   0.825   0.4115    
## age_group_5_45to49   -0.019111   0.112599  -0.170   0.8655    
## age_group_5_50to54    0.292376   0.131963   2.216   0.0288 *  
## age_group_5_55to59    0.318889   0.166851   1.911   0.0586 .  
## age_group_5_60to64          NA         NA      NA       NA    
## age_group_5_65_over         NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3111 on 108 degrees of freedom
##   (2 observations deleted due to missingness)
## Multiple R-squared:  0.1195,	Adjusted R-squared:  0.04617 
## F-statistic: 1.629 on 9 and 108 DF,  p-value: 0.1158
```

```r
linearMod58 <- lm(formula = performance_rating ~ race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=merit_raises_combined_salaried_regression)
summary(linearMod58)
```

```
## 
## Call:
## lm(formula = performance_rating ~ race_grouping_white + race_grouping_person_of_color + 
##     age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + 
##     age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + 
##     age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + 
##     age_group_5_65_over, data = merit_raises_combined_salaried_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.62288 -0.18117 -0.03998  0.15664  0.86318 
## 
## Coefficients: (2 not defined because of singularities)
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    3.366472   0.332970  10.110   <2e-16 ***
## race_grouping_white           -0.023117   0.320658  -0.072   0.9427    
## race_grouping_person_of_color -0.043378   0.325240  -0.133   0.8942    
## age_group_5_under_25          -0.043355   0.325176  -0.133   0.8942    
## age_group_5_25to29             0.033528   0.115288   0.291   0.7718    
## age_group_5_30to34             0.046149   0.119104   0.387   0.6992    
## age_group_5_35to39             0.140343   0.123475   1.137   0.2582    
## age_group_5_40to44             0.139243   0.143864   0.968   0.3353    
## age_group_5_45to49            -0.006536   0.115233  -0.057   0.9549    
## age_group_5_50to54             0.299782   0.125425   2.390   0.0186 *  
## age_group_5_55to59             0.330065   0.157051   2.102   0.0379 *  
## age_group_5_60to64                   NA         NA      NA       NA    
## age_group_5_65_over                  NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3124 on 107 degrees of freedom
##   (2 observations deleted due to missingness)
## Multiple R-squared:  0.1203,	Adjusted R-squared:  0.03808 
## F-statistic: 1.463 on 10 and 107 DF,  p-value: 0.1633
```

```r
linearMod59 <- lm(formula = performance_rating ~ gender_Female + gender_Male + race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=merit_raises_combined_salaried_regression)
summary(linearMod59)
```

```
## 
## Call:
## lm(formula = performance_rating ~ gender_Female + gender_Male + 
##     race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + 
##     age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + 
##     age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + 
##     age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, 
##     data = merit_raises_combined_salaried_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.62357 -0.18119 -0.04026  0.15697  0.86251 
## 
## Coefficients: (3 not defined because of singularities)
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    3.365646   0.336384  10.005   <2e-16 ***
## gender_Female                  0.001768   0.075435   0.023   0.9813    
## gender_Male                          NA         NA      NA       NA    
## race_grouping_white           -0.022617   0.322870  -0.070   0.9443    
## race_grouping_person_of_color -0.042505   0.328885  -0.129   0.8974    
## age_group_5_under_25          -0.043029   0.327001  -0.132   0.8956    
## age_group_5_25to29             0.032585   0.122617   0.266   0.7909    
## age_group_5_30to34             0.044783   0.133103   0.336   0.7372    
## age_group_5_35to39             0.140115   0.124436   1.126   0.2627    
## age_group_5_40to44             0.137660   0.159528   0.863   0.3901    
## age_group_5_45to49            -0.007312   0.120416  -0.061   0.9517    
## age_group_5_50to54             0.298661   0.134777   2.216   0.0288 *  
## age_group_5_55to59             0.328498   0.171364   1.917   0.0579 .  
## age_group_5_60to64                   NA         NA      NA       NA    
## age_group_5_65_over                  NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3138 on 106 degrees of freedom
##   (2 observations deleted due to missingness)
## Multiple R-squared:  0.1203,	Adjusted R-squared:  0.02901 
## F-statistic: 1.318 on 11 and 106 DF,  p-value: 0.2247
```

```r
commercial_hourly_regression <- commercial_hourly[,c('department','gender','race_ethnicity','current_base_pay','job_profile_current','cost_center_current','2015_annual_performance_rating','2016_annual_performance_rating','2017_annual_performance_rating','2018_annual_performance_rating','age','years_of_service','age_group_5','years_of_service_grouped','dept','desk','tier','race_grouping')]
commercial_hourly_regression <- fastDummies::dummy_cols(commercial_hourly_regression, select_columns = c('gender','race_ethnicity','age_group_5','years_of_service_grouped','dept','desk','tier','race_grouping'))
names(commercial_hourly_regression) <- gsub(' ', '_', names(commercial_hourly_regression))
names(commercial_hourly_regression) <- gsub('-', 'to', names(commercial_hourly_regression))
names(commercial_hourly_regression) <- gsub('\\+', '_over', names(commercial_hourly_regression))
names(commercial_hourly_regression) <- gsub('<', 'under_', names(commercial_hourly_regression))

linearMod60 <- lm(formula = current_base_pay ~ gender_Female + gender_Male, data=commercial_hourly_regression)
summary(linearMod60)
```

```
## 
## Call:
## lm(formula = current_base_pay ~ gender_Female + gender_Male, 
##     data = commercial_hourly_regression)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -10.731  -4.314  -1.518   3.761  29.419 
## 
## Coefficients: (1 not defined because of singularities)
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    25.7881     0.7581  34.019  < 2e-16 ***
## gender_Female   3.9126     1.0684   3.662  0.00035 ***
## gender_Male         NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.477 on 145 degrees of freedom
## Multiple R-squared:  0.08466,	Adjusted R-squared:  0.07834 
## F-statistic: 13.41 on 1 and 145 DF,  p-value: 0.0003499
```

```r
linearMod61 <- lm(formula = current_base_pay ~ race_grouping_white + race_grouping_person_of_color, data=commercial_hourly_regression)
summary(linearMod61)
```

```
## 
## Call:
## lm(formula = current_base_pay ~ race_grouping_white + race_grouping_person_of_color, 
##     data = commercial_hourly_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -11.2002  -4.4456  -0.9006   3.5548  28.1098 
## 
## Coefficients:
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     22.113      3.710   5.961 1.85e-08 ***
## race_grouping_white              8.897      3.837   2.319   0.0218 *  
## race_grouping_person_of_color    4.427      3.764   1.176   0.2415    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.425 on 144 degrees of freedom
## Multiple R-squared:  0.1054,	Adjusted R-squared:  0.09293 
## F-statistic: 8.479 on 2 and 144 DF,  p-value: 0.0003303
```

```r
linearMod62 <- lm(formula = current_base_pay ~ gender_Female + gender_Male + race_grouping_white + race_grouping_person_of_color, data=commercial_hourly_regression)
summary(linearMod62)
```

```
## 
## Call:
## lm(formula = current_base_pay ~ gender_Female + gender_Male + 
##     race_grouping_white + race_grouping_person_of_color, data = commercial_hourly_regression)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -12.330  -3.851  -1.531   2.554  26.270 
## 
## Coefficients: (1 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                     22.113      3.559   6.213 5.36e-09 ***
## gender_Female                    3.767      1.028   3.665 0.000348 ***
## gender_Male                         NA         NA      NA       NA    
## race_grouping_white              6.969      3.719   1.874 0.062943 .  
## race_grouping_person_of_color    2.488      3.650   0.682 0.496647    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.165 on 143 degrees of freedom
## Multiple R-squared:  0.1822,	Adjusted R-squared:  0.165 
## F-statistic: 10.62 on 3 and 143 DF,  p-value: 2.4e-06
```

```r
linearMod63 <- lm(formula = current_base_pay ~ gender_Female + gender_Male + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=commercial_hourly_regression)
summary(linearMod63)
```

```
## 
## Call:
## lm(formula = current_base_pay ~ gender_Female + gender_Male + 
##     age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + 
##     age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + 
##     age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + 
##     age_group_5_65_over, data = commercial_hourly_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -11.3940  -3.8376  -0.9446   3.1079  28.4860 
## 
## Coefficients: (2 not defined because of singularities)
##                      Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          24.30914    1.97881  12.285  < 2e-16 ***
## gender_Female         3.75388    1.08589   3.457 0.000729 ***
## gender_Male                NA         NA      NA       NA    
## age_group_5_under_25 -0.03328    2.71181  -0.012 0.990225    
## age_group_5_25to29    3.14429    2.35523   1.335 0.184098    
## age_group_5_30to34    3.14160    2.86587   1.096 0.274924    
## age_group_5_35to39    6.17705    2.60480   2.371 0.019123 *  
## age_group_5_40to44    0.95164    2.47439   0.385 0.701137    
## age_group_5_45to49    2.57102    2.45973   1.045 0.297765    
## age_group_5_50to54   -0.40099    2.43587  -0.165 0.869487    
## age_group_5_55to59    0.60055    2.49206   0.241 0.809931    
## age_group_5_60to64   -0.80863    2.60371  -0.311 0.756604    
## age_group_5_65_over        NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.356 on 136 degrees of freedom
## Multiple R-squared:  0.1733,	Adjusted R-squared:  0.1125 
## F-statistic: 2.851 on 10 and 136 DF,  p-value: 0.00298
```

```r
linearMod64 <- lm(formula = current_base_pay ~ race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=commercial_hourly_regression)
summary(linearMod64)
```

```
## 
## Call:
## lm(formula = current_base_pay ~ race_grouping_white + race_grouping_person_of_color + 
##     age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + 
##     age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + 
##     age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + 
##     age_group_5_65_over, data = commercial_hourly_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -10.4844  -3.8899  -0.9866   3.0028  27.2032 
## 
## Coefficients: (1 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    17.8158     4.1801   4.262 3.78e-05 ***
## race_grouping_white            10.8972     3.8021   2.866  0.00482 ** 
## race_grouping_person_of_color   6.6583     3.7576   1.772  0.07866 .  
## age_group_5_under_25           -0.3745     2.6699  -0.140  0.88864    
## age_group_5_25to29              3.5514     2.3257   1.527  0.12909    
## age_group_5_30to34              5.0083     2.8306   1.769  0.07909 .  
## age_group_5_35to39              6.0117     2.5857   2.325  0.02156 *  
## age_group_5_40to44              3.3295     2.4475   1.360  0.17598    
## age_group_5_45to49              3.2038     2.4333   1.317  0.19018    
## age_group_5_50to54             -0.4921     2.3985  -0.205  0.83776    
## age_group_5_55to59              1.2226     2.4532   0.498  0.61905    
## age_group_5_60to64              0.1069     2.5775   0.041  0.96698    
## age_group_5_65_over                 NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.261 on 135 degrees of freedom
## Multiple R-squared:  0.2035,	Adjusted R-squared:  0.1386 
## F-statistic: 3.136 on 11 and 135 DF,  p-value: 0.0008472
```

```r
linearMod65 <- lm(formula = current_base_pay ~ gender_Female + gender_Male + race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=commercial_hourly_regression)
summary(linearMod65)
```

```
## 
## Call:
## lm(formula = current_base_pay ~ gender_Female + gender_Male + 
##     race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + 
##     age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + 
##     age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + 
##     age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, 
##     data = commercial_hourly_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -11.4252  -3.9045  -0.7517   2.7593  25.1857 
## 
## Coefficients: (2 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   18.35904    4.04063   4.544 1.22e-05 ***
## gender_Female                  3.43733    1.04958   3.275  0.00135 ** 
## gender_Male                         NA         NA      NA       NA    
## race_grouping_white            8.78809    3.72820   2.357  0.01986 *  
## race_grouping_person_of_color  4.55451    3.68554   1.236  0.21870    
## age_group_5_under_25          -0.06206    2.58039  -0.024  0.98085    
## age_group_5_25to29             2.83157    2.25695   1.255  0.21181    
## age_group_5_30to34             4.27783    2.74288   1.560  0.12121    
## age_group_5_35to39             6.09079    2.49738   2.439  0.01604 *  
## age_group_5_40to44             2.34053    2.38306   0.982  0.32779    
## age_group_5_45to49             3.34986    2.35050   1.425  0.15643    
## age_group_5_50to54            -0.07588    2.32001  -0.033  0.97396    
## age_group_5_55to59             0.85121    2.37207   0.359  0.72027    
## age_group_5_60to64             0.08175    2.48936   0.033  0.97385    
## age_group_5_65_over                 NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.047 on 134 degrees of freedom
## Multiple R-squared:  0.2625,	Adjusted R-squared:  0.1965 
## F-statistic: 3.975 on 12 and 134 DF,  p-value: 2.988e-05
```

```r
linearMod66 <- lm(formula = current_base_pay ~ gender_Female + gender_Male + race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over + years_of_service_grouped_0 + years_of_service_grouped_1to2 + years_of_service_grouped_3to5 + years_of_service_grouped_6to10 + years_of_service_grouped_11to15 + years_of_service_grouped_16to20 + years_of_service_grouped_21to25 + years_of_service_grouped_25_over, data=commercial_hourly_regression)
summary(linearMod66)
```

```
## 
## Call:
## lm(formula = current_base_pay ~ gender_Female + gender_Male + 
##     race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + 
##     age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + 
##     age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + 
##     age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over + 
##     years_of_service_grouped_0 + years_of_service_grouped_1to2 + 
##     years_of_service_grouped_3to5 + years_of_service_grouped_6to10 + 
##     years_of_service_grouped_11to15 + years_of_service_grouped_16to20 + 
##     years_of_service_grouped_21to25 + years_of_service_grouped_25_over, 
##     data = commercial_hourly_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -11.3880  -3.2873  -0.7906   2.6392  24.8288 
## 
## Coefficients: (3 not defined because of singularities)
##                                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                      18.71412    4.45884   4.197 5.04e-05 ***
## gender_Female                     3.04122    1.25333   2.427   0.0166 *  
## gender_Male                            NA         NA      NA       NA    
## race_grouping_white               8.52247    4.05027   2.104   0.0373 *  
## race_grouping_person_of_color     4.04668    3.96852   1.020   0.3098    
## age_group_5_under_25             -0.43984    3.05384  -0.144   0.8857    
## age_group_5_25to29                2.31526    2.80073   0.827   0.4100    
## age_group_5_30to34                4.90639    3.20985   1.529   0.1289    
## age_group_5_35to39                5.89797    2.79979   2.107   0.0371 *  
## age_group_5_40to44                2.40918    2.73489   0.881   0.3800    
## age_group_5_45to49                2.82012    2.60479   1.083   0.2810    
## age_group_5_50to54               -0.30475    2.48080  -0.123   0.9024    
## age_group_5_55to59                0.92733    2.44641   0.379   0.7053    
## age_group_5_60to64               -0.40829    2.66712  -0.153   0.8786    
## age_group_5_65_over                    NA         NA      NA       NA    
## years_of_service_grouped_0       -0.14159    2.73822  -0.052   0.9588    
## years_of_service_grouped_1to2     1.19324    2.56055   0.466   0.6420    
## years_of_service_grouped_3to5    -0.97287    2.76173  -0.352   0.7252    
## years_of_service_grouped_6to10    0.04366    2.54249   0.017   0.9863    
## years_of_service_grouped_11to15   0.46224    2.81566   0.164   0.8699    
## years_of_service_grouped_16to20   0.98286    2.41854   0.406   0.6851    
## years_of_service_grouped_21to25   2.57982    2.83996   0.908   0.3654    
## years_of_service_grouped_25_over       NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 6.156 on 127 degrees of freedom
## Multiple R-squared:  0.2757,	Adjusted R-squared:  0.1673 
## F-statistic: 2.544 on 19 and 127 DF,  p-value: 0.001075
```

```r
merit_raises_combined_hourly_regression <- filter(merit_raises_combined, dept == 'Commercial', pay_rate_type == 'Hourly')

merit_raises_combined_hourly_regression <- fastDummies::dummy_cols(merit_raises_combined_hourly_regression, select_columns = c('gender','race_grouping','age_group_5'))
names(merit_raises_combined_hourly_regression) <- gsub(' ', '_', names(merit_raises_combined_hourly_regression))
names(merit_raises_combined_hourly_regression) <- gsub('-', 'to', names(merit_raises_combined_hourly_regression))
names(merit_raises_combined_hourly_regression) <- gsub('\\+', '_over', names(merit_raises_combined_hourly_regression))
names(merit_raises_combined_hourly_regression) <- gsub('<', 'under_', names(merit_raises_combined_hourly_regression))

linearMod67 <- lm(formula = base_pay_change ~ gender_Female + gender_Male, data=merit_raises_combined_hourly_regression)
summary(linearMod67)
```

```
## 
## Call:
## lm(formula = base_pay_change ~ gender_Female + gender_Male, data = merit_raises_combined_hourly_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.35809 -0.12809 -0.03789  0.07230  1.08191 
## 
## Coefficients: (1 not defined because of singularities)
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    0.34770    0.01886  18.434  < 2e-16 ***
## gender_Female  0.11039    0.02618   4.217 3.43e-05 ***
## gender_Male         NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.2117 on 260 degrees of freedom
## Multiple R-squared:  0.06401,	Adjusted R-squared:  0.06041 
## F-statistic: 17.78 on 1 and 260 DF,  p-value: 3.427e-05
```

```r
linearMod68 <- lm(formula = base_pay_change ~ race_grouping_white + race_grouping_person_of_color, data=merit_raises_combined_hourly_regression)
summary(linearMod68)
```

```
## 
## Call:
## lm(formula = base_pay_change ~ race_grouping_white + race_grouping_person_of_color, 
##     data = merit_raises_combined_hourly_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.36944 -0.13105 -0.04944  0.07895  1.07056 
## 
## Coefficients: (1 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    0.38105    0.01558  24.464  < 2e-16 ***
## race_grouping_white            0.08839    0.02992   2.954  0.00342 ** 
## race_grouping_person_of_color       NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.2153 on 260 degrees of freedom
## Multiple R-squared:  0.03247,	Adjusted R-squared:  0.02875 
## F-statistic: 8.727 on 1 and 260 DF,  p-value: 0.003423
```

```r
linearMod69 <- lm(formula = base_pay_change ~ gender_Female + gender_Male + race_grouping_white + race_grouping_person_of_color, data=merit_raises_combined_hourly_regression)
summary(linearMod69)
```

```
## 
## Call:
## lm(formula = base_pay_change ~ gender_Female + gender_Male + 
##     race_grouping_white + race_grouping_person_of_color, data = merit_raises_combined_hourly_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.42912 -0.11989 -0.05686  0.08011  1.01088 
## 
## Coefficients: (2 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    0.31989    0.02037  15.700  < 2e-16 ***
## gender_Female                  0.11452    0.02573   4.450 1.28e-05 ***
## gender_Male                         NA         NA      NA       NA    
## race_grouping_white            0.09471    0.02893   3.274  0.00121 ** 
## race_grouping_person_of_color       NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.2079 on 259 degrees of freedom
## Multiple R-squared:  0.1012,	Adjusted R-squared:  0.09426 
## F-statistic: 14.58 on 2 and 259 DF,  p-value: 9.987e-07
```

```r
linearMod70 <- lm(formula = base_pay_change ~ gender_Female + gender_Male + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=merit_raises_combined_hourly_regression)
summary(linearMod70)
```

```
## 
## Call:
## lm(formula = base_pay_change ~ gender_Female + gender_Male + 
##     age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + 
##     age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + 
##     age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + 
##     age_group_5_65_over, data = merit_raises_combined_hourly_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.36671 -0.12659 -0.03807  0.09317  1.10473 
## 
## Coefficients: (2 not defined because of singularities)
##                      Estimate Std. Error t value Pr(>|t|)    
## (Intercept)           0.28807    0.04187   6.880 4.76e-11 ***
## gender_Female         0.10705    0.02859   3.744 0.000224 ***
## gender_Male                NA         NA      NA       NA    
## age_group_5_under_25  0.23052    0.10196   2.261 0.024625 *  
## age_group_5_25to29    0.04015    0.06040   0.665 0.506887    
## age_group_5_30to34    0.09536    0.06767   1.409 0.160013    
## age_group_5_35to39    0.18773    0.06092   3.082 0.002289 ** 
## age_group_5_40to44    0.08816    0.05992   1.471 0.142450    
## age_group_5_45to49    0.08158    0.05712   1.428 0.154453    
## age_group_5_50to54    0.01802    0.05554   0.324 0.745869    
## age_group_5_55to59    0.04244    0.05497   0.772 0.440778    
## age_group_5_60to64    0.02171    0.05503   0.394 0.693592    
## age_group_5_65_over        NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.2081 on 251 degrees of freedom
## Multiple R-squared:  0.127,	Adjusted R-squared:  0.09219 
## F-statistic: 3.651 on 10 and 251 DF,  p-value: 0.000145
```

```r
linearMod71 <- lm(formula = base_pay_change ~ race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=merit_raises_combined_hourly_regression)
summary(linearMod71)
```

```
## 
## Call:
## lm(formula = base_pay_change ~ race_grouping_white + race_grouping_person_of_color + 
##     age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + 
##     age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + 
##     age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + 
##     age_group_5_65_over, data = merit_raises_combined_hourly_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.34399 -0.12826 -0.03793  0.08002  1.09654 
## 
## Coefficients: (2 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    0.28793    0.04257   6.763 9.42e-11 ***
## race_grouping_white            0.08634    0.03092   2.792  0.00564 ** 
## race_grouping_person_of_color       NA         NA      NA       NA    
## age_group_5_under_25           0.23480    0.10317   2.276  0.02370 *  
## age_group_5_25to29             0.06919    0.05994   1.154  0.24953    
## age_group_5_30to34             0.16213    0.06621   2.449  0.01502 *  
## age_group_5_35to39             0.17782    0.06207   2.865  0.00452 ** 
## age_group_5_40to44             0.15605    0.05749   2.714  0.00710 ** 
## age_group_5_45to49             0.13514    0.05623   2.403  0.01697 *  
## age_group_5_50to54             0.04568    0.05552   0.823  0.41139    
## age_group_5_55to59             0.08178    0.05453   1.500  0.13492    
## age_group_5_60to64             0.04124    0.05549   0.743  0.45802    
## age_group_5_65_over                 NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.2106 on 251 degrees of freedom
## Multiple R-squared:  0.106,	Adjusted R-squared:  0.07036 
## F-statistic: 2.975 on 10 and 251 DF,  p-value: 0.001468
```

```r
linearMod72 <- lm(formula = base_pay_change ~ gender_Female + gender_Male + race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=merit_raises_combined_hourly_regression)
summary(linearMod72)
```

```
## 
## Call:
## lm(formula = base_pay_change ~ gender_Female + gender_Male + 
##     race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + 
##     age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + 
##     age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + 
##     age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, 
##     data = merit_raises_combined_hourly_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.37347 -0.12809 -0.03730  0.07365  1.06653 
## 
## Coefficients: (3 not defined because of singularities)
##                                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    0.267098   0.041599   6.421 6.78e-10 ***
## gender_Female                  0.115674   0.028184   4.104 5.50e-05 ***
## gender_Male                          NA         NA      NA       NA    
## race_grouping_white            0.097972   0.030125   3.252  0.00130 ** 
## race_grouping_person_of_color        NA         NA      NA       NA    
## age_group_5_under_25           0.230173   0.100069   2.300  0.02226 *  
## age_group_5_25to29            -0.007271   0.061049  -0.119  0.90529    
## age_group_5_30to34             0.092451   0.066419   1.392  0.16518    
## age_group_5_35to39             0.161822   0.060321   2.683  0.00779 ** 
## age_group_5_40to44             0.078351   0.058884   1.331  0.18453    
## age_group_5_45to49             0.081860   0.056060   1.460  0.14548    
## age_group_5_50to54             0.009004   0.054584   0.165  0.86911    
## age_group_5_55to59             0.037571   0.053968   0.696  0.48697    
## age_group_5_60to64             0.022601   0.054009   0.418  0.67596    
## age_group_5_65_over                  NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.2043 on 250 degrees of freedom
## Multiple R-squared:  0.1624,	Adjusted R-squared:  0.1256 
## F-statistic: 4.407 on 11 and 250 DF,  p-value: 4.681e-06
```

```r
linearMod73 <- lm(formula = performance_rating ~ gender_Female + gender_Male, data=merit_raises_combined_hourly_regression)
summary(linearMod73)
```

```
## 
## Call:
## lm(formula = performance_rating ~ gender_Female + gender_Male, 
##     data = merit_raises_combined_hourly_regression)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -0.6541 -0.2254 -0.0254  0.1459  0.8459 
## 
## Coefficients: (1 not defined because of singularities)
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    3.22540    0.02192 147.173  < 2e-16 ***
## gender_Female  0.12868    0.03047   4.223 3.34e-05 ***
## gender_Male         NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.246 on 259 degrees of freedom
##   (1 observation deleted due to missingness)
## Multiple R-squared:  0.06441,	Adjusted R-squared:  0.0608 
## F-statistic: 17.83 on 1 and 259 DF,  p-value: 3.344e-05
```

```r
linearMod74 <- lm(formula = performance_rating ~ race_grouping_white + race_grouping_person_of_color, data=merit_raises_combined_hourly_regression)
summary(linearMod74)
```

```
## 
## Call:
## lm(formula = performance_rating ~ race_grouping_white + race_grouping_person_of_color, 
##     data = merit_raises_combined_hourly_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.57737 -0.23099 -0.03099  0.16901  0.92263 
## 
## Coefficients: (1 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    3.27737    0.01837 178.418   <2e-16 ***
## race_grouping_white            0.05362    0.03522   1.522    0.129    
## race_grouping_person_of_color       NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.2532 on 259 degrees of freedom
##   (1 observation deleted due to missingness)
## Multiple R-squared:  0.008869,	Adjusted R-squared:  0.005043 
## F-statistic: 2.318 on 1 and 259 DF,  p-value: 0.1291
```

```r
linearMod75 <- lm(formula = performance_rating ~ gender_Female + gender_Male + race_grouping_white + race_grouping_person_of_color, data=merit_raises_combined_hourly_regression)
summary(linearMod75)
```

```
## 
## Call:
## lm(formula = performance_rating ~ gender_Female + gender_Male + 
##     race_grouping_white + race_grouping_person_of_color, data = merit_raises_combined_hourly_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.63883 -0.20762 -0.03883  0.16117  0.86117 
## 
## Coefficients: (2 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    3.20762    0.02401 133.570  < 2e-16 ***
## gender_Female                  0.13121    0.03038   4.319 2.24e-05 ***
## gender_Male                         NA         NA      NA       NA    
## race_grouping_white            0.06053    0.03411   1.774   0.0772 .  
## race_grouping_person_of_color       NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.245 on 258 degrees of freedom
##   (1 observation deleted due to missingness)
## Multiple R-squared:  0.07569,	Adjusted R-squared:  0.06853 
## F-statistic: 10.56 on 2 and 258 DF,  p-value: 3.893e-05
```

```r
linearMod76 <- lm(formula = performance_rating ~ gender_Female + gender_Male + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=merit_raises_combined_hourly_regression)
summary(linearMod76)
```

```
## 
## Call:
## lm(formula = performance_rating ~ gender_Female + gender_Male + 
##     age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + 
##     age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + 
##     age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + 
##     age_group_5_65_over, data = merit_raises_combined_hourly_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.59991 -0.19415 -0.01871  0.15558  0.85594 
## 
## Coefficients: (2 not defined because of singularities)
##                      Estimate Std. Error t value Pr(>|t|)    
## (Intercept)           3.10402    0.04884  63.560  < 2e-16 ***
## gender_Female         0.09989    0.03341   2.990  0.00307 ** 
## gender_Male                NA         NA      NA       NA    
## age_group_5_under_25  0.09600    0.11891   0.807  0.42023    
## age_group_5_25to29    0.14051    0.07046   1.994  0.04720 *  
## age_group_5_30to34    0.11372    0.07893   1.441  0.15091    
## age_group_5_35to39    0.20510    0.07105   2.887  0.00423 ** 
## age_group_5_40to44    0.22366    0.06989   3.200  0.00155 ** 
## age_group_5_45to49    0.11480    0.06662   1.723  0.08610 .  
## age_group_5_50to54    0.14015    0.06478   2.163  0.03145 *  
## age_group_5_55to59    0.19049    0.06437   2.959  0.00338 ** 
## age_group_5_60to64    0.09014    0.06418   1.404  0.16142    
## age_group_5_65_over        NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.2427 on 250 degrees of freedom
##   (1 observation deleted due to missingness)
## Multiple R-squared:  0.1209,	Adjusted R-squared:  0.08575 
## F-statistic: 3.439 on 10 and 250 DF,  p-value: 0.0003027
```

```r
linearMod77 <- lm(formula = performance_rating ~ race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=merit_raises_combined_hourly_regression)
summary(linearMod77)
```

```
## 
## Call:
## lm(formula = performance_rating ~ race_grouping_white + race_grouping_person_of_color + 
##     age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + 
##     age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + 
##     age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + 
##     age_group_5_65_over, data = merit_raises_combined_hourly_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.53801 -0.17477 -0.01916  0.16199  0.91980 
## 
## Coefficients: (2 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    3.11171    0.04980  62.481  < 2e-16 ***
## race_grouping_white            0.04145    0.03619   1.145 0.253240    
## race_grouping_person_of_color       NA         NA      NA       NA    
## age_group_5_under_25           0.10000    0.12070   0.829 0.408159    
## age_group_5_25to29             0.18441    0.07013   2.630 0.009075 ** 
## age_group_5_30to34             0.17509    0.07745   2.261 0.024643 *  
## age_group_5_35to39             0.20581    0.07261   2.835 0.004963 ** 
## age_group_5_40to44             0.28863    0.06725   4.292 2.54e-05 ***
## age_group_5_45to49             0.16306    0.06578   2.479 0.013840 *  
## age_group_5_50to54             0.16849    0.06495   2.594 0.010041 *  
## age_group_5_55to59             0.22630    0.06414   3.528 0.000498 ***
## age_group_5_60to64             0.10745    0.06491   1.655 0.099138 .  
## age_group_5_65_over                 NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.2464 on 250 degrees of freedom
##   (1 observation deleted due to missingness)
## Multiple R-squared:  0.09424,	Adjusted R-squared:  0.05801 
## F-statistic: 2.601 on 10 and 250 DF,  p-value: 0.005093
```

```r
linearMod78 <- lm(formula = performance_rating ~ gender_Female + gender_Male + race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, data=merit_raises_combined_hourly_regression)
summary(linearMod78)
```

```
## 
## Call:
## lm(formula = performance_rating ~ gender_Female + gender_Male + 
##     race_grouping_white + race_grouping_person_of_color + age_group_5_under_25 + 
##     age_group_5_25to29 + age_group_5_30to34 + age_group_5_35to39 + 
##     age_group_5_40to44 + age_group_5_45to49 + age_group_5_50to54 + 
##     age_group_5_55to59 + age_group_5_60to64 + age_group_5_65_over, 
##     data = merit_raises_combined_hourly_regression)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.59314 -0.18358 -0.01585  0.13979  0.86727 
## 
## Coefficients: (3 not defined because of singularities)
##                               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                    3.09296    0.04932  62.706  < 2e-16 ***
## gender_Female                  0.10436    0.03348   3.117  0.00204 ** 
## gender_Male                         NA         NA      NA       NA    
## race_grouping_white            0.05172    0.03573   1.447  0.14903    
## race_grouping_person_of_color       NA         NA      NA       NA    
## age_group_5_under_25           0.09583    0.11865   0.808  0.42008    
## age_group_5_25to29             0.11553    0.07239   1.596  0.11177    
## age_group_5_30to34             0.11223    0.07876   1.425  0.15544    
## age_group_5_35to39             0.19143    0.07152   2.677  0.00793 ** 
## age_group_5_40to44             0.21854    0.06983   3.129  0.00196 ** 
## age_group_5_45to49             0.11499    0.06648   1.730  0.08492 .  
## age_group_5_50to54             0.13542    0.06472   2.092  0.03743 *  
## age_group_5_55to59             0.18769    0.06426   2.921  0.00381 ** 
## age_group_5_60to64             0.09062    0.06404   1.415  0.15829    
## age_group_5_65_over                 NA         NA      NA       NA    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.2422 on 249 degrees of freedom
##   (1 observation deleted due to missingness)
## Multiple R-squared:  0.1283,	Adjusted R-squared:  0.08974 
## F-statistic:  3.33 on 11 and 249 DF,  p-value: 0.0002689
```
