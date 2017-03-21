load_all()
library(tidyverse)
library(lubridate)
library(gridExtra)
library(grid)

cl_args <- commandArgs(trailingOnly=T)
report_date <- if_else(is.na(cl_args[1]), Sys.Date() - 1, as.Date(cl_args[1]))

yesterday_hourly <- pull_sensit_day(day=report_date)
yesterday_hourly <- filter(yesterday_hourly, !invalid)

met_df <- pull_met_day(day=report_date)
met_loc <- pull_deployment_locations(deployments=met_df$deployment)

site_locs <- pull_deployment_locations(deployments=yesterday_hourly$sensit)
colnames(site_locs)[1] <- "sensit"

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

yesterday_df <- c()
for (i in names(predict_list)){
    # predicted flux on yesterday's data
    new_data <- sensits_yesterday %>% filter(sensit==i)
    grp <- unique(new_data$group) 
    predict_df <- data.frame('sensit'=new_data$sensit, 'date'=new_data$date, 
                      'predict_flux'=predict(predict_list[[i]], new_data, 
                                              type="response"),
                      'exceed_prob'=predict(logistic_list[[grp]], new_data, 
                                              type="response"))
    tmp2 <- new_data %>%
        left_join(predict_df, by=c("sensit", "date"))
    yesterday_df <- rbind(yesterday_df, tmp2)
}

yesterday_df <- yesterday_df %>% left_join(site_locs, by="sensit") %>%
    left_join(select(met_summary, deployment, max_ws, wd_max), 
              by=c("met"="deployment")) 

# identify potential bad sensits
yesterday_df$bad <- rep(F, nrow(yesterday_df))
for (i in 1:nrow(yesterday_df)){
    if (yesterday_df$predict_flux[i]>10 & yesterday_df$max_ws[i]<5){
        yesterday_df$bad[i] <- T
        yesterday_df$exceed_prob[i] <- NA
    } 
}

# parse and format data for display
yesterday_df$table_mass <- as.character(round(yesterday_df$predict_flux, 1))
for (i in 1:nrow(yesterday_df)){
    if (yesterday_df$bad[i]){
        yesterday_df$table_mass[i] <- 'BAD'
    } else if (yesterday_df$predict_flux[i] > 200){
        yesterday_df$table_mass[i] <- ">200"
    }
}

thresh <- data.frame(group=ordered(c('Other Areas', 'TwB2')), 
                     threshold=c(5, 0.5))
yesterday_df <- left_join(yesterday_df, thresh, by='group')
yesterday_df$flag <- rep(NA, nrow(yesterday_df))
yesterday_df$over <- rep(NA, nrow(yesterday_df))
for (i in 1:nrow(yesterday_df)){
yesterday_df$flag[i] <- if_else(yesterday_df$predict_flux[i] >
                               yesterday_df$threshold[i], T, F)
yesterday_df$over[i] <- yesterday_df$predict_flux[i] / yesterday_df$threshold[i]
}

# build table of exceedance sites for display in LADWP report
table_sites <- yesterday_df %>% filter(flag & !bad) %>%
    select(sensit, dca, table_mass, group2)
table_sites$group2 <- factor(table_sites$group2, ordered=T, 
                             levels=c('TwB2', 'Brine', 'DWM', 'T1A-1', 
                                      'Channel Area'))
if (nrow(table_sites)>0){
    plot_grob <- build_exceedance_table(table_sites)
} 

# plot exceedance report for LADWP (if required)
if (nrow(table_sites)>0){
    p_dwp <- plot_ladwp_report()
    pdf(file=paste0("~/dropbox/owens/sensit_notify/ladwp/", 
                    report_date, "_sensit_alerts.pdf"), 
        height=11, width=8.5, paper='letter')
    print(p_dwp)
    dev.off()
}

# build table of exceedance sites for display in AirSci report
airsci_sites <- yesterday_df %>% filter(flag) %>%
    select(sensit, dca, table_mass, group2)
airsci_sites$group2 <- factor(airsci_sites$group2, ordered=T, 
                             levels=c('TwB2', 'Brine', 'DWM', 'T1A-1', 
                                      'Channel Area'))
if (nrow(airsci_sites)>0){
    airsci_grob <- build_exceedance_table(airsci_sites)
} else{
    airsci_grob <- grid::grid.rect(gp=grid::gpar(col='white'))
    dev.off()
}

met_labels <- build_met_labels()

# plot Air Sciences internal report
p_as <- plot_airsci_report()
pdf(file=paste0("~/dropbox/owens/sensit_notify/airsci/daily_reports/", 
                report_date, "_airsci.pdf"), 
    height=11, width=8.5, paper='letter')
print(p_as)
dev.off()

# save results
report_csv <- select(yesterday_df, sensit, date, group2, dca, sumpc_total, 
                     predict_flux, exceed_prob, max_ws, bad)
write.csv(report_csv, row.names=F, 
          file=paste0("~/dropbox/owens/sensit_notify/airsci/all_sensits/", 
                      report_date, ".csv"))
write.table(filter(report_csv, sensit %in% airsci_sites$sensit), 
          file=paste0("~/dropbox/owens/sensit_notify/flag_sites.csv"), 
          row.names=F, col.names=F, append=T, sep=",")
