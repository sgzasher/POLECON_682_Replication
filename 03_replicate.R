setwd(dirname(rstudioapi::getSourceEditorContext()$path))
set.seed(1966)
library(dplyr)
library(ggplot2)
library(data.table)
library(fuzzyjoin)
library(tidyverse)
library(rdrobust)

# Data Read  -------------------------------------------------------------------
data.replicate <- read.csv("../data/clean/analysis_data.csv")
cpi <- read.csv("../data/raw/cpi.csv") 


# Variable Construction  -------------------------------------------------------

# Need to adjust for 2012 dollars
cpi.2012 = cpi$CPI[cpi$Year == 2012]
cpi <- 
  cpi %>%
  dplyr::mutate(
    CPI = (CPI / cpi.2012) * 100
  )

# Node that spending is in thousands of dollars
data.replicate <- 
  left_join(
    data.replicate,
    cpi,
    by = c("Year4" = "Year")
  ) %>%
  dplyr::mutate(
    spend.pc = ((spend * 1000 * CPI)/100)/pop,
    spend.y2.pc = ((spend.y2 * 1000* CPI)/100)/pop,
    spend.pc.diff = spend.y2.pc - spend.pc
  )

  
# Initial Version -------------------------------------------------------
outcome = data.replicate$spend.pc.diff
forcing = data.replicate$dshare

# Running with a standard triangular and uniform kernel
summary(rdrobust(outcome,
          forcing))

summary(rdrobust(outcome,
                 forcing,
                 kernel = "uniform"))

# This is the local linear weighting function, given a uniform kernel
tri <- function (x, h, c=0) pmax(0, 1 - abs((x - c) / h))
h = rdrobust(outcome,
             forcing,
             kernel = "uniform")$bws[1,1]

# Local linear plot replicating figure
ggplot(data.replicate) + 
geom_point(aes(x = dshare, y = data.replicate$spend.pc.diff), alpha=.1) + 
geom_smooth(data=subset(data.replicate, dshare > 0),
            aes(x = dshare, y = spend.pc.diff,
                weight=tri(dshare, h)),
            method = 'lm', formula = y ~ poly(x, 1), size=1.5) + 
  geom_smooth(data=subset(data.replicate, dshare < 0),
              aes(x = dshare, y = spend.pc.diff,
                  weight=tri(dshare, h)),
              method = 'lm', formula = y ~ poly(x, 1), size=1.5) + 
coord_cartesian(ylim = c(-1000, 1000)) + 
geom_vline(xintercept=0) + 
geom_hline(yintercept=0, lty='dotted') + 
theme_bw() + 
labs(x = "Democrats' Vote Share",
     y = "Change in Per Capita Expenditures") +
ggtitle("Second Year after Mayoral Election")

# Handle Outliers Version -------------------------------------------------------
var(data.replicate$spend.pc.diff, na.rm = TRUE)
data.replicate$spend.pc.diff[data.replicate$spend.pc.diff > 22000]

# Going to get rid of approx. 3 s.d. outliers
data.replicate <- 
  data.replicate %>%
  dplyr::filter(
    spend.pc.diff < 30000 & spend.pc.diff > -30000
  )

# And repeat
outcome = data.replicate$spend.pc.diff
forcing = data.replicate$dshare

summary(rdrobust(outcome,
                 forcing))

summary(rdrobust(outcome,
                 forcing,
                 kernel = "uniform"))

tri <- function (x, h, c=0) pmax(0, 1 - abs((x - c) / h))
h = rdrobust(outcome,
             forcing,
             kernel = "uniform")$bws[1,1]

## Headline Replication

pdf("../output.pdf", width=8, height=6)
ggplot(data.replicate) + 
  geom_point(aes(x = dshare, y = data.replicate$spend.pc.diff), alpha=.1) + 
  geom_smooth(data=subset(data.replicate, dshare > 0),
              aes(x = dshare, y = spend.pc.diff,
                  weight=tri(dshare, h)),
              method = 'lm', formula = y ~ poly(x, 1), size=1.5) + 
  geom_smooth(data=subset(data.replicate, dshare < 0),
              aes(x = dshare, y = spend.pc.diff,
                  weight=tri(dshare, h)),
              method = 'lm', formula = y ~ poly(x, 1), size=1.5) + 
  coord_cartesian(ylim = c(-1000, 1000)) + 
  geom_vline(xintercept=0) + 
  geom_hline(yintercept=0, lty='dotted') + 
  theme_bw() + 
  labs(x = "Democrats' Vote Share",
       y = "Change in Per Capita Expenditures") +
  ggtitle("Second Year after Mayoral Election")
dev.off()
