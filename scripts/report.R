source("~/code/sensitCal/scripts/model.R")
library(tidyverse)
library(ggplot2)
library(gridExtra)

yesterday_df <- yesterday_df %>% left_join(site_locs, by="sensit") %>%
    left_join(select(met_summary, deployment, max_ws, wd_max), 
              by=c("met"="deployment")) 
yesterday_df$pred_mass <- round(yesterday_df$predict_count / 10, 1)

# identify potential bad sensits
yesterday_df$bad <- rep(F, nrow(yesterday_df))
for (i in 1:nrow(yesterday_df)){
    if (yesterday_df$pred_mass[i]>10 & yesterday_df$max_ws[i]<5){
        yesterday_df$bad[i] <- T
        yesterday_df$predict_prob[i] <- NA
    } 
}

# parse and format data for display
yesterday_df$table_mass <- as.character(yesterday_df$pred_mass)
for (i in 1:nrow(yesterday_df)){
yesterday_df$table_mass[i] <- ifelse(yesterday_df$bad[i], 
                                     "BAD", yesterday_df$table_mass[i])
yesterday_df$table_mass[i] <- ifelse(yesterday_df$pred_mass[i] > 200, 
                                     ">200", yesterday_df$table_mass[i])
}
thresh <- data.frame(group=c('TwB2', 'Other Areas'), 
                     threshold=c(0.5, 5))
yesterday_df <- left_join(yesterday_df, thresh, by='group')
yesterday_df$flag <- rep(NA, nrow(yesterday_df))
yesterday_df$over <- rep(NA, nrow(yesterday_df))
for (i in 1:nrow(yesterday_df)){
yesterday_df$flag[i] <- ifelse(yesterday_df$pred_mass[i] >
                               yesterday_df$threshold[i], T, F)
yesterday_df$over[i] <- yesterday_df$pred_mass[i] / yesterday_df$threshold[i]
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

if (nrow(table_sites)>0){
    p_dwp <- plot_ladwp_report()
    pdf(file=paste0("~/dropbox/owens/ladwp/", report_date, "_ladwp.pdf"), 
        height=11, width=8.5, paper='letter')
    print(p_dwp)
    dev.off()
}

p_as <- plot_airsci_report()
pdf(file=paste0("~/dropbox/owens/sensit_notify/airsci/daily_reports/", 
                report_date, "_airsci.pdf"), 
    height=11, width=8.5, paper='letter')
print(p_as)
dev.off()

report_csv <- select(yesterday_df, sensit, date, group2, dca, sumpc_total, 
                     predict_prob, pred_mass, max_ws, bad)
write.csv(report_csv, row.names=F, 
          file=paste0("~/dropbox/owens/sensit_notify/airsci/all_sensits/", 
                      report_date, ".csv"))
write.table(filter(report_csv, sensit %in% airsci_sites$sensit), 
          file=paste0("~/dropbox/owens/sensit_notify/airsci/flag_sites.csv"), 
          row.names=F, col.names=F, append=T, sep=",")

