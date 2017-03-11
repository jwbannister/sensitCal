load_all()
load_all("~/code/owensData")
library(tidyverse)
library(lubridate)

query1 <- "SELECT * FROM sandcatch.sensit_pc_records"
col_record <- query_owens(query1)
col_dates <- col_record %>% group_by(group) %>%
    summarize(date=as.Date(max(collection_datetime)))

yesterday_hourly <- pull_sensit_day()
yesterday_hourly <- filter(yesterday_hourly, !invalid)

query_m <- paste0("SELECT i.deployment, m.datetime, m.ws_10m, m.wd_10m ",
                  "FROM met.met_1hour m JOIN instruments.deployments i ",
                  "ON m.deployment_id=i.deployment_id ",
                  "WHERE (m.datetime - '1 second'::interval)::date='",
                  Sys.Date() - 1, "'::date;")
met_df <- query_owens(query_m)
query_m2 <- paste0("SELECT deployment, ",
                   "st_x(st_transform(geom, 26911)) AS x, ",
                   "st_y(st_transform(geom, 26911)) AS y ",
                   "FROM instruments.deployments ",
                   "WHERE deployment IN ('",
                   paste(unique(met_df$deployment), collapse="', '"), "');")
met_loc <- query_owens(query_m2)

query <- paste0("SELECT deployment AS sensit, ",
                "st_x(st_transform(geom, 26911)) AS x, ", 
                "st_y(st_transform(geom, 26911)) AS y ", 
                "FROM instruments.deployments ",
                "WHERE deployment IN ('", 
                paste(yesterday_df$sensit, collapse="', '"), "');")
site_locs <- query_owens(query)

