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
    filter(ratio < quantile(col_record$ratio, probs=quantile_filter) &
           1/ratio < quantile(1/col_record$ratio, probs=quantile_filter))

# filter to masses in range of desired thresholds
filter_data2 <- filter_data %>%
    filter((group=='TwB2' & dwp_mass<2) | (group!='TwB2' & dwp_mass<10))

filter_data3 <- filter_data2
filter_data3[!(filter_data2$group %in% c('SF Studies', 'TwB2')), ]$group <- 
    'Other Areas'
filter_data3$group <- factor(filter_data3$group, ordered=T)
filter_data3$dwp_mass_count <- filter_data3$dwp_mass * 10

lm_list <- vector(mode="list", length=3)
names(lm_list) <- c('SF Studies', 'TwB2', 'Other Areas')
plots <- lm_list
lm_summary <- data.frame(group=c(), coeff=c(), p=c())
clrs <- c('SF Studies'='#e41a1c', 'TwB2'='#377eb8', 'Other Areas'='#4daf4a')

for (i in names(plots)){
    tmp_data <- filter_data3 %>% filter(group==i)
    lm_list[[i]] <- glm(dwp_mass_count ~ 0 + sumpc_total, data=tmp_data, 
                        family=poisson)
    tmp <- data.frame(group=i, 
                      coeff=summary(lm_list[[i]])$coefficients[ , 1],  
                      p=summary(lm_list[[i]])$coefficients[ , 4]) 
    lm_summary <- rbind(lm_summary, tmp)
    plots[[i]] <-  tmp_data %>%
        ggplot(aes(x=sumpc_total, y=dwp_mass)) +
        geom_point(aes(color=group)) +
        scale_color_manual(values=clrs) +
        geom_smooth(method="lm", formula = y ~ 0 + x, se=F) +
        theme(legend.position='none') +
        ggtitle(names(plots[i]))
}
fl <- tempfile()
png(file=fl, width=8, height=8, units="in", res=300) 
gridExtra::grid.arrange(plots[[1]], plots[[2]], plots[[3]], 
                        nrow=2)
dev.off()

yesterday_hourly <- summarize_sensit_day()
yesterday_hourly <- filter(yesterday_hourly, !invalid)
yesterday_hourly$date <- 
    as.Date(substring(yesterday_hourly$datetime %m-% seconds(1), 1, 10))
sensits_yesterday <- yesterday_hourly %>% group_by(sensit, date, group) %>%
    summarize(sumpc_total=sum(sumpc)) %>% ungroup()
sensits_yesterday[!(sensits_yesterday$group %in% c('SF Studies', 'TwB2')), ]$group <- 
    'Other Areas'
sensits_yesterday$group <- factor(sensits_yesterday$group, ordered=T)

mass_threshold <- c('SF Studies'=5, 'TwB2'=1, 'Other Areas'=5)
predict_df <- c()
for (k in levels(sensits_yesterday$group)){
    new_data <- sensits_yesterday %>% filter(group==k)
    tmp <- as.data.frame(predict(lm_list[[k]], new_data, type="terms", 
                                 se.fit=T)) 
    tmp$flag <- ifelse(tmp$lwr>mass_threshold[[k]], TRUE, FALSE)
    tmp2 <- cbind(new_data, tmp)
    predict_df <- rbind(predict_df, tmp2)
}


    
