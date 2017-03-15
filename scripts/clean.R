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

# remove sumpc_total > 700, screws up Poisson regression model
# remvoe SF Study area Sensits
filter_data2 <- filter_data %>% filter(sumpc_total<700) %>%
    filter(group!='SF Studies')
filter_data2$group2 <- filter_data2$group

filter_data3 <- filter_data2
filter_data3[!(filter_data3$group %in% c('SF Studies', 'TwB2')), ]$group <- 
    'Other Areas'
filter_data3$group <- factor(filter_data3$group, ordered=T)
# convert decimal masses into counts of 0.1g for use in Poisson regression
filter_data3$dwp_mass_count <- filter_data3$dwp_mass * 10
# add T/F exceedence flag for logistic regression
mass_threshold <- c('TwB2'=0.5, 'Other Areas'=5)
filter_data3$flag <- rep(NA, nrow(filter_data3))
for (i in 1:nrow(filter_data3)){
    thresh <- mass_threshold[[as.character(filter_data3$group[i])]]
    filter_data3$flag[i] <- 
        if_else(filter_data3$dwp_mass[i]>thresh, T, F)
}

yesterday_hourly$date <- 
    as.Date(substring(yesterday_hourly$datetime %m-% seconds(1), 1, 10))
sensits_yesterday <- yesterday_hourly %>% 
    group_by(sensit, date, group, dca, met) %>%
    summarize(sumpc_total=sum(sumpc)) %>% ungroup() %>%
# remove SF Study area Sensits
    filter(group!='SF Studies')
sensits_yesterday$group2 <- sensits_yesterday$group
sensits_yesterday[!(sensits_yesterday$group %in% c('TwB2')), ]$group <- 
    'Other Areas'
sensits_yesterday$group <- factor(sensits_yesterday$group, ordered=T)

met_summary <- met_df %>% group_by(deployment) %>%
    summarize(max_ws=max(ws_10m, na.rm=T), 
              wd_max=wd_10m[which(ws_10m==max(ws_10m))]) %>%
    left_join(met_loc, by="deployment")
