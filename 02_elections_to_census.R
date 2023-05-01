setwd(dirname(rstudioapi::getSourceEditorContext()$path))
set.seed(1966)
library(dplyr)
library(ggplot2)
library(data.table)
library(fuzzyjoin)
library(tidyverse)

# Data Read --------------------------------------------------------------------
data.jop <- fread("../data/clean/election_results.csv")
data.census <- fread("../data/raw/gov_finances_relevant.txt")

# Some Data Not Super Relevant -------------------------------------------------
data.census <- 
  dplyr::filter(
    data.census,
    `Type Code` %in% c("2", "3")
  )

# String Patches ---------------------------------------------------------------
string.cleaning <- fread("../data/string_patches.csv",
                         header = FALSE)

data.jop <-
  dplyr::mutate(
    data.jop,
    city = toupper(city)
  ) 

for(i in 1:nrow(string.cleaning)){
  
  data.jop <- 
    dplyr::mutate(
      data.jop,
      city = str_replace(
        city,
        as.character(string.cleaning[i,1]),
        as.character(string.cleaning[i,2])
      )
    )
}

# Merge with Census ------------------------------------------------------------
data.jop$pop <- NA
data.jop$spend <- NA
data.jop$pop.y2 <- NA
data.jop$spend.y2 <- NA

# Notice we want second year after election... 

for(i in 1:nrow(data.jop)){
  
  city = data.jop$city[i]
  year2 = data.jop$Year4[i] + 2
  year = data.jop$Year4[i]
  state = data.jop$State.Code[i]
  
  datafilter = data.census[str_detect(data.census$Name, city) & data.census$Year4 == year & data.census$`State Code` == state,]
  if(nrow(datafilter==1)){
    data.jop$pop[i] = datafilter$Population[1]
    data.jop$spend[i] = datafilter$`Total Expenditure`[1]
  }
  
  datafilter = data.census[str_detect(data.census$Name, city) & data.census$Year4 == year2 & data.census$`State Code` == state,]
  if(nrow(datafilter==1)){
    data.jop$pop.y2[i] = datafilter$Population[1]
    data.jop$spend.y2[i] = datafilter$`Total Expenditure`[1]
  }
  
  print(i)
}

# Output  ----------------------------------------------------------------------

# Approx 20 problem cases beyond my lack of data before '67
data.inspect <- 
  data.jop[is.na(data.jop$pop) & data.jop$Year4 > 1967]

# And some that we can't be sure didn't involve independents


# Leaves us with a sample of 745 observations, about 75% of the reported sample
data.jop <- data.jop[!is.na(data.jop$pop)]
data.jop <- data.jop[!is.na(data.jop$lost_party)]
data.jop <- data.jop[!is.na(data.jop$won_party)]

write.csv(
  data.jop,
  "../data/clean/analysis_data.csv"
)
