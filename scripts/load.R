load_all()
library(tidyverse)
library(lubridate)

report_date <- Sys.Date() - 1

col_record <- pull_collections_active()
col_dates <- col_record %>% group_by(group) %>%
    summarize(date=as.Date(max(collection_datetime)))

yesterday_hourly <- pull_sensit_day(day=report_date)
yesterday_hourly <- filter(yesterday_hourly, !invalid)

met_df <- pull_met_day(day=report_date)
met_loc <- pull_deployment_locations(deployments=met_df$deployment)

site_locs <- pull_deployment_locations(deployments=yesterday_hourly$sensit)
colnames(site_locs)[1] <- "sensit"


