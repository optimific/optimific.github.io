---
title: "Discussion of Margin of Error"
author: "Simone Roy"
date: "October 11, 2017"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Discussion of Margin of Error 

Due to smaller samples, margin of error is larger for American Community Survey 1-Year estimates than for 5-Year estimates. In some cases, the margin of error is so large that it is difficult to determine whether the estimates have changed over time. In these cases, it is difficult to determine whether the data is capturing an emerging trend or if it is merely the result of sampling error. 

This often becomes a problem when a jurisdiction meets the minimum population threshold (65,000), but some subpopulations are much smaller. An example of this is shown below: the margin of error is large for the mean commute length of public transportation users in Loudoun County and Prince William County. In 2016, there were 7,051 public transportation users in Loudoun County and 9,476 users in Prince William County, which is much smaller compared to the number of public transportation users in the D.C. metro area (436,979) or in the District (130,451).  


In jurisdictions where the subpopulation size was relatively large (for example, the D.C. metro area and the District), it was easier to be confident that the observed changes occurred. In other cases, such as Loudoun County and Prince William County, margin of error is very large, reducing confidence in the estimates. Looking at the estimates alone, it appears that the average commute time for Loudoun County public transportation users has greatly increased. Looking at the margin of error for each estimate shows that this might not be the case.  

Public transportation commute lengths are different from commute times for those who drove alone. In this case, estimates in Loudoun County and Prince William County have similar error terms to the District of Columbia. 


```{r commute_time_by_means, echo=TRUE}
library(tidyverse)

commute_time_by_means <- read.csv("commute_time_by_means.csv")

commute_time_by_means %>% filter(Method == "Public Transportation") %>%  #filter for public transportation
  ggplot(aes(x=value, y=Year)) +
  geom_errorbarh(aes(xmin = value - moe, xmax = value + moe)) + #error bar
  geom_point(color = "red", size = 2) + #estimate
  xlim(30, 80) + #set limits so that both charts are scaled similarly
  facet_wrap(~Geography) + #small multiples by location
  labs(caption = "Source:  ACS 1-Year Estimates, 2012-2016",
       title = "Mean Commute Length - Public Transportation, 2012-2016",
       x = "Commute Length (Minutes)",
       y = "") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust=0))

commute_time_by_means %>% filter(Method == "Drove Alone") %>% #filter for drove alone
  ggplot(aes(x=value, y=Year)) +
  geom_errorbarh(aes(xmin = value - moe, xmax = value + moe)) + #error bar
  geom_point(color = "red", size = 2) + #estimate
  xlim(0, 50) + #set limits so that both charts are scaled similarly
  facet_wrap(~Geography) + #small multiples by location
  labs(caption = "Source:  ACS 1-Year Estimates, 2012-2016",
       title = "Mean Commute Length - Drove Alone, 2012-2016",
       x = "Commute Length (Minutes)",
       y = "") +
  theme_minimal() +
  theme(plot.caption = element_text(hjust=0))


```


## Discussion of Data

For tables B08007 (Sex of Workers by Place of Work--State and County Level), B08130 (Means of Transportation to Work by Place of Work--State and County Level), B08301 (Means of Transportation to Work), B08302 (Time Leaving Home to Go to Work), and B08134 (Means of Transportation to Work by Travel Time to Work), the tidycensus R package was used to download the acs data. These required little cleaning because they were already in a tidy format.

Data was cleaned and reshaped using tidyverse packages (dplyr, purrr, forcats). Data was explored and plotted using ggplot2 and plotly. Charts made using ggplot2 were edited in Adobe Illustrator.
For subject tables (S0801 and S0802), acquiring and cleaning the data was more complicated. Subject tables were not supported by the Census ACS API until 2015, so tables were downloaded using the American Fact Finder website and cleaned in Excel. S0801 was used for mean travel time and S0802 for mean travel time by means of transportation. Given the relatively small number of variables, this was easiest to do manually outside of R. 

```{r time_leaving_for_work, echo=TRUE}
#B08302 - Time Leaving for Work
library(tidyverse) 
library(tidycensus) #for acs data
library(forcats) #for recoding

census_api_key('xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx')

#FIPS codes from selected states and counties
dmv_state_fips <- c("11","24", "51")
dmv_counties_fips <- c("11001", "24031", "24033", "51013", "51059", "51107", "51153", "51510")

acs_years <- c("2012", "2013", "2014", "2015", "2016") #select years

b08302_vars <-sprintf("B08302_%03d", seq(2,15)) #select values for variables

#get time leaving for work data for both DC MSA and selected counties within the DC MSA
#DC MSA
time_leaving_for_work_msa <- map_df(acs_years,  function(x) {
  get_acs(geography = "metropolitan statistical area/micropolitan statistical area", 
          variables = b08302_vars, summary_var = "B08302_001", 
          survey="acs1", endyear = x) %>%  filter(GEOID == "47900") %>% 
    mutate(year = x,
           pct = round((estimate/summary_est) * 100, 2))
})


#DC Counties
time_leaving_for_work_dmv <- map_df(acs_years, function(x) {
  get_acs(geography = "county", variables = b08302_vars, summary_var = "B08302_001",
          state = dmv_state_fips, survey="acs1", endyear = x) %>% 
    filter(GEOID %in% dmv_counties_fips) %>% 
    mutate(year = x,
           pct = round((estimate/summary_est) * 100, 2))
})


#combine data frames for msa and counties
time_leaving_for_work_dmv <- rbind(time_leaving_for_work_msa, time_leaving_for_work_dmv)

B08302_varnames <- read.csv("B08302_varnames.csv") #read in variable labels

#join variable names to the time leaving for work data frame
time_leaving_for_work_df <- right_join(time_leaving_for_work_dmv, B08302_varnames)


#reorder factors
time_leaving_for_work_df$time <- ordered(time_leaving_for_work_df$time, 
                                         levels = c("12:00 a.m. to 4:59 a.m.",
                                                    "5:00 a.m. to 5:29 a.m.",
                                                    "5:30 a.m. to 5:59 a.m.",
                                                    "6:00 a.m. to 6:29 a.m.",
                                                    "6:30 a.m. to 6:59 a.m.",
                                                    "7:00 a.m. to 7:29 a.m.",
                                                    "7:30 a.m. to 7:59 a.m.",
                                                    "8:00 a.m. to 8:29 a.m.",
                                                    "8:30 a.m. to 8:59 a.m.",
                                                    "9:00 a.m. to 9:59 a.m.",
                                                    "10:00 a.m. to 10:59 a.m.",
                                                    "11:00 a.m. to 11:59 a.m.",
                                                    "12:00 p.m. to 3:59 p.m.",
                                                    "4:00 p.m. to 11:59 p.m."
                                         ))


#reverse factor order so that times will be in order when plotted
time_leaving_for_work_df$time <- factor(time_leaving_for_work_df$time, 
                                        levels=rev(levels(time_leaving_for_work_df$time)))

#create alternative labels for plotting
time_leaving_for_work_df <- time_leaving_for_work_df %>%
  mutate(Location = fct_recode(GEOID,
                               "District of Columbia" = "11001",
                               "Montgomery County, Maryland" = "24031",
                               "Prince George's County, Maryland" = "24033",
                               "Arlington County, Virginia" = "51013",
                               "Fairfax County, Virginia" ="51059",
                               "Loudoun County, Virginia" ="51107",
                               "Prince William County, Virginia" = "51153",
                               "Alexandria" = "51510",
                               "D.C. Metro Area" = "47900")) 

time_leaving_for_work_df <- time_leaving_for_work_df %>%
  mutate(Short_Name = fct_recode(GEOID,
                                 "D.C." = "11001",
                                 "Montgomery County" = "24031",
                                 "Prince George's County" = "24033",
                                 "Arlington County" = "51013",
                                 "Fairfax County" ="51059",
                                 "Loudoun County" ="51107",
                                 "Prince William County" = "51153",
                                 "Alexandria" = "51510",
                                 "D.C. Metro" = "47900")) 


```



