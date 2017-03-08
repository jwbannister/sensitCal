load_all()
load_all("~/code/owensData")
library(tidyverse)
library(ggplot2)
library(lubridate)

query1 <- "SELECT * FROM sandcatch.sensit_pc_records"
col_record <- query_owens(query1)
col_record$ratio <- col_record$dwp_mass/col_record$sumpc_total
col_dates <- col_record %>% group_by(group) %>%
    summarize(date=as.Date(max(collection_datetime)))

# remove outlier data - unrealistic mass/pc ratios (both high and low)
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

yesterday_hourly <- pull_sensit_day()
yesterday_hourly <- filter(yesterday_hourly, !invalid)
yesterday_hourly$date <- 
    as.Date(substring(yesterday_hourly$datetime %m-% seconds(1), 1, 10))
sensits_yesterday <- yesterday_hourly %>% group_by(sensit, date, group, dca) %>%
    summarize(sumpc_total=sum(sumpc)) %>% ungroup() 
sensits_yesterday[!(sensits_yesterday$group %in% c('SF Studies', 'TwB2')), ]$group <- 
    'Other Areas'
sensits_yesterday$group <- factor(sensits_yesterday$group, ordered=T)


poisson_list <- vector(mode="list", length=3)
names(poisson_list) <- c('SF Studies', 'TwB2', 'Other Areas')
lr_list <- poisson_list
history_df <- c()
yesterday_df <- c()
for (i in names(poisson_list)){
    # build regression models
    col_data <- filter_data3 %>% filter(group==i)
    poisson_list[[i]] <- glm(dwp_mass_count ~ sumpc_total, data=col_data, 
                         family=quasipoisson)
    lr_list[[i]] <- glm(flag ~ sumpc_total, data=col_data, 
                         family=binomial)
    poisson_df <- cbind(select(col_data, sensit, coll_id), 
                    "poisson_fitted_count"=poisson_list[[i]]$fitted)
    lr_df <- cbind(select(col_data, sensit, coll_id), 
                    "lr_fitted"=lr_list[[i]]$fitted)
    tmp1 <- col_data %>%
        left_join(poisson_df, by=c("sensit", "coll_id")) %>%
        left_join(lr_df, by=c("sensit", "coll_id"))
    history_df <- rbind(history_df, tmp1)
    # predict on yesterday's data
    new_data <- sensits_yesterday %>% filter(group==i)
    predict_df <- data.frame('sensit'=new_data$sensit, 'date'=new_data$date, 
                      'predict_count'=predict(poisson_list[[i]], new_data, 
                                              type="response"),
                      'predict_prob'=predict(lr_list[[i]], new_data, 
                                              type="response"))
    tmp2 <- new_data %>%
        left_join(predict_df, by=c("sensit", "date"))
    yesterday_df <- rbind(yesterday_df, tmp2)
}

plots <- vector(mode="list", length=3)
names(plots) <- c('SF Studies', 'TwB2', 'Other Areas')
clrs <- c('SF Studies'='#e41a1c', 'TwB2'='#377eb8', 'Other Areas'='#4daf4a')
for (i in names(plots)){
    plots[[i]]$dist <- history_df %>% filter(group==i) %>%
        ggplot(aes(x=dwp_mass_count)) +
        geom_density()
    plots[[i]]$xy <-  history_df %>% filter(group==i) %>%
        ggplot(aes(x=sumpc_total, y=dwp_mass_count)) +
        geom_point(aes(color=group)) +
        geom_point(mapping=aes(y=poisson_fitted_count)) +
        scale_color_manual(values=clrs) +
        theme(legend.position='none') +
        ggtitle(names(plots[i]))
}
fl <- tempfile()
png(file=fl, width=8, height=8, units="in", res=300) 
gridExtra::grid.arrange(plots[[1]]$xy, plots[[2]]$xy, plots[[3]]$xy, 
                        nrow=2)
dev.off()



    
