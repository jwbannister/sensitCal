source("~/code/sensitCal/scripts/load.R")
library(tidyverse)
library(lubridate)

# remove outlier data - unrealistic mass/pc ratios (both high and low)
col_record$ratio <- col_record$dwp_mass/col_record$sumpc_total
quantile_filter <- 0.9
filter_data <- col_record %>% 
    filter((ratio < quantile(col_record$ratio, probs=quantile_filter, na.rm=T) &
           1/ratio < quantile(1/col_record$ratio, probs=quantile_filter, na.rm=T)) |
           is.na(ratio))

# filter to masses in range of desired thresholds
filter_data2 <- filter_data %>%
    filter((group=='TwB2' & dwp_mass<2) | (group!='TwB2' & dwp_mass<10)) %>%
# remove sumpc_total > 700, screws up Poisson regression model
    filter(sumpc_total<700)

filter_data3 <- filter_data2
filter_data3[!(filter_data2$group %in% c('SF Studies', 'TwB2')), ]$group <- 
    'Other Areas'
filter_data3$group <- factor(filter_data3$group, ordered=T)
# convert decimal masses into counts of 0.1g for use in Poisson regression
filter_data3$dwp_mass_count <- filter_data3$dwp_mass * 10
# express mass threshold in terms of counts of 0.1g
count_threshold <- c('SF Studies'=50, 'TwB2'=10, 'Other Areas'=50)
# add T/F exceedence flag for logistic regression
mass_threshold <- c('SF Studies'=5, 'TwB2'=1, 'Other Areas'=5)
filter_data3$flag <- rep(NA, nrow(filter_data3))
filter_data3$model_flag <- rep(NA, nrow(filter_data3))
for (i in 1:nrow(filter_data3)){
    thresh <- mass_threshold[[as.character(filter_data3$group[i])]]
    filter_data3$flag[i] <- 
        ifelse(filter_data3$dwp_mass[i]>thresh, T, F)
}

yesterday_hourly$date <- 
    as.Date(substring(yesterday_hourly$datetime %m-% seconds(1), 1, 10))
sensits_yesterday <- yesterday_hourly %>% 
    group_by(sensit, date, group, dca, met) %>%
    summarize(sumpc_total=sum(sumpc)) %>% ungroup() 
sensits_yesterday[!(sensits_yesterday$group %in% c('SF Studies', 'TwB2')), ]$group <- 
    'Other Areas'
sensits_yesterday$group <- factor(sensits_yesterday$group, ordered=T)

met_summary <- met_df %>% group_by(deployment) %>%
    summarize(max_ws=max(ws_10m, na.rm=T), 
              wd_max=wd_10m[which(ws_10m==max(ws_10m))]) %>%
    left_join(met_loc, by="deployment")
