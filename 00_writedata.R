setwd(dirname(rstudioapi::getSourceEditorContext()$path))
set.seed(1966)
library(dplyr)
library(ggplot2)
library(data.table)


# Reading in data, including silly Rdata format
data.combined <- fread("../data/raw/gov_finances_relevant.txt", 
                       header = TRUE)

load("../data/raw/harvard_dataverse/mayors_rdd_analysis_final.Rdata")
data.jop <- data2
rm(data2)

# Here's the final version of running variables etc. from the JOP data
data.jop.validate <- 
  dplyr::select(
    data.jop,
    mayor_party_final:demshare
  )

# Now we're gonna subset the JOP data to the stuff that isn't publicly available
data.jop <- 
  data.jop %>%
  dplyr::select(
    mayor_name:partisan,
    winner:runner1_pid,
    Year4,
    type,
    city,
    State.Code
  )

# Write some files for later
write.csv(
  data.jop,
  "../data/use/jop_data_non_public.csv"
)

write.csv(
  data.jop.validate,
  "../data/use/jop_data_comparison.csv"
)