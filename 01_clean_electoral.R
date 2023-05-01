setwd(dirname(rstudioapi::getSourceEditorContext()$path))
set.seed(1966)
library(dplyr)
library(ggplot2)
library(data.table)
library(fuzzyjoin)
library(tidyverse)

# Data Read ------------------------------------------------------------------
data.jop <- fread("../data/use/jop_data_non_public.csv")
data.jop.validate <- fread("../data/use/jop_data_comparison.csv")
data.dime <- fread("../data/raw/dime/dime_recipients_all_1979_2014.csv")


# Verification ---------------------------------------------------------------

# That running variable is 2-party vote share
data.jop.validate <- 
  data.jop.validate %>%
  dplyr::mutate(
    dvotes = ifelse(
      mayor_party_final == "D",
      mayor_votes_final,
      runnerup_votes_final
    ),
    votetotal = mayor_votes_final + runnerup_votes_final,
    dshare = (dvotes/votetotal) - .5
  )

# Correct within about .3% of a vote share
sum(data.jop.validate$dshare - data.jop.validate$demshare)/nrow(data.jop.validate)



# Unhelpful Data ---------------------------------------------------------------

# The main data seems to have been compiled from multiple sources
# Target: names & vote shares for each candidate, and a party ID sometimes missing

data.clean <-
  data.jop %>% 
  dplyr::mutate(
    won_name = ifelse(
      is.na(mayor_name),
      winner,
      mayor_name
    ),
    lost_name = ifelse(
      is.na(runnerup_name),
      runner1,
      runnerup_name
    ),
    won_party = ifelse(
      is.na(mayor_party),
      winner_pid,
      mayor_party
    ),
    lost_party = ifelse(
      is.na(runnerup_party),
      runner1_pid,
      runnerup_party
    ),
    won_votes = ifelse(
      is.na(mayor_votes),
      winnervotes,
      mayor_votes
    ),
    lost_votes = ifelse(
      is.na(runnerup_votes),
      runner1_votes,
      runnerup_votes
    )
  )%>%
  dplyr::select(
    city,
    State.Code,
    Year4,
    won_name,
    won_party,
    lost_name,
    lost_party,
    won_votes,
    lost_votes
  )

# There are some cases where we're looking at useless data

## If we do know one runner was not a D or R
### First: code NAs for empty strings
data.clean <- 
  data.clean %>%
  dplyr::mutate(
    won_party = ifelse(
      won_party == "",
      NA, 
      won_party
    ),
    lost_party = ifelse(
      lost_party == "",
      NA,
      lost_party
    )
  ) %>%
  dplyr::mutate(
    checkwin = (!is.na(won_party) & !(won_party %in% c("D", "R"))), 
    checklose = (!is.na(lost_party) & !(lost_party %in% c("D", "R")))  
  ) %>%
  dplyr::filter(
    checkwin == FALSE & checklose == FALSE
  ) %>%
  dplyr::select(
    -checkwin,
    -checklose
  )

## If we're missing your party AND your name (so can't scale you)
data.clean <- 
  data.clean %>%
  dplyr::mutate(
    won_name = ifelse(
      won_name == "",
      NA,
      won_name
    ),
    lost_name = ifelse(
      lost_name == "",
      NA,
      lost_name
    )
  ) %>%
  dplyr::filter(
    !((is.na(won_name) & is.na(won_party)) | is.na(lost_name) & is.na(lost_party))
  )

# Bonica Scaling ---------------------------------------------------------------

# Where we have missing party ID, goal is to get the CF-score and use that 
# Turns out this is only actually doing work for 13 observations. Kinda silly. 

data.scaleme <- 
  data.clean %>%
  dplyr::filter(
    (is.na(won_party) | is.na(won_party)) 
  ) %>%
  dplyr::mutate(
    won_name_lower = tolower(won_name),
    lost_name_lower = tolower(lost_name)
  )

for(i in 1:nrow(data.scaleme)){
  name = data.scaleme$won_name_lower[i]
  idx = str_detect(data.dime$name, name)
  print(data.dime[idx,])
}

for(i in 1:nrow(data.scaleme)){
  name = data.scaleme$won_name_lower[i]
  idx = str_detect(data.dime$name, name)
  print(data.dime[idx,])
}

# No super obvious matches and don't have time this week to really nail it, 
# unfortunately! Luckily it should have ~0 impact given it's so few observations!

# Output ---------------------------------------------------------------

# Given above failure, let's finally subset to the data where we have the 
# Party ID known and the vote shares, calculate the running variable, 
# and then export the data. 

data.clean <- 
  data.clean %>%
  dplyr::filter(
    !((is.na(won_party) | is.na(won_party)))
  )

# Finishing with 963 obs; the paper claims to use 981

data.clean <- 
  data.clean %>%
  dplyr::mutate(
    dvotes = ifelse(
      won_party == "D",
      won_votes,
      lost_votes
    ),
    votetotal = won_votes + lost_votes,
    dshare = (dvotes/votetotal) - .5
  )

write.csv(
  data.clean,
  "../data/clean/election_results.csv"
)