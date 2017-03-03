load_all("~/code/owensData")
library(tidyverse)
library(ggplot2)
library(lubridate)

query1 <- "SELECT * FROM sandcatch.sensit_pc_records"
col_record <- query_owens(query1)
col_record$name <- factor(col_record$name, ordered=T)
col_record$ratio <- col_record$dwp_mass/col_record$sumpc_total
col_dates <- col_record %>% group_by(name) %>%
    summarize(date=as.Date(max(collection_datetime)))

# remove outlier data - unrealistic mass/pc ratios
quantile_filter <- 0.95
filter_data <- col_record %>% 
    filter(ratio < quantile(col_record$ratio, probs=quantile_filter))

lm_list <- vector(mode="list", length=length(plots))
lm_summary <- data.frame(name=c(), coeff=c(), adj.r.2=c(), p=c())
names(lm_list) <- names(plots)
for (j in names(lm_list)){
    tmp <- filter(filter_data, name==j)
    lm_list[[j]] <- lm(dwp_mass ~ 0 + sumpc_total, tmp)
    tmp2 <- data.frame(name=j, coeff=summary(lm_list[[j]])$coefficients[ , 1],  
                       adj.2=summary(lm_list[[j]])$adj.r.squared, 
                       p=summary(lm_list[[j]])$coefficients[ , 4]) 
    lm_summary <- rbind(lm_summary, tmp2)
}

new_data <- data.frame(sumpc_total=seq(1, 500, 1))
predict_list <- vector(mode="list", length=length(lm_list))
predict_summary <- data.frame(name=c(), sumpc_total=c(), lwr=c())
names(predict_list) <- names(lm_list)
thresholds <- c('Brine'=5, 'Channel Area'=5, 'SF Studies'=5, 'T1A-1'=5, 
                'TwB2'=1)
for (k in names(predict_list)){
    thresh <- thresholds[[k]]
    tmp <- as.data.frame(predict(lm_list[[k]], new_data, interval="prediction", level=0.9))
    predict_list[[k]] <- cbind(new_data, tmp)
    tmp2 <- data.frame(name=k, 
                       threshold=thresh,
                       sumpc_total=min(filter(predict_list[[k]], lwr>=thresh)$sumpc_total), 
                       lwr=min(filter(predict_list [[k]], lwr>=thresh)$lwr))
    predict_summary <- rbind(predict_summary, tmp2)

}

plots <- vector(mode="list", length=length(unique(col_record$name)))
names(plots) <- levels(col_record$name)
clrs <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00')
full_q_cutoff <- 0.5
name_q_cutoff <- 0.75
full_plot <- filter_data %>%
    filter(sumpc_total<quantile(.$sumpc_total, probs=full_q_cutoff)) %>%
    ggplot(aes(x=sumpc_total, y=dwp_mass)) +
    geom_point(aes(color=name)) +
    theme(legend.position="none", 
          legend.title=element_blank()) +
    scale_color_manual(values=clrs) +
    ggtitle(paste0("All Areas (bottom ", full_q_cutoff*100, "% sumpc_total)"))
for (i in 1:length(plots)){
    r2 <- round(lm_summary[lm_summary$name==names(plots)[i], ]$adj.2, 2)
    th <- predict_summary[predict_summary$name==names(plots)[i], ]$threshold
    pc <- predict_summary[predict_summary$name==names(plots)[i], ]$sumpc_total
    cd <- col_dates[col_dates$name==names(plots)[i], ]$date
    plot_data <- filter_data %>%
        filter(sumpc_total<quantile(.$sumpc_total, probs=name_q_cutoff)) %>%
        filter(name==names(plots)[i])
    x_ann <- max(plot_data$sumpc_total)
    y_ann <- max(plot_data$dwp_mass)
    plots[[i]] <-  plot_data %>%
    ggplot(aes(x=sumpc_total, y=dwp_mass)) +
    geom_point(color=clrs[i]) +
    geom_smooth(method="lm", formula = y ~ 0 + x, se=F) +
    annotate('text', x=x_ann*.5, y=y_ann*.99, size=3, 
             label=paste0("Most Recent Collection ", cd)) +
    annotate('text', x=x_ann*.5, y=y_ann*.93, size=3, 
             label=paste0("R^{2}==", r2), parse=T) +
    annotate('text', x=x_ann*.5, y=y_ann*.86, size=3, 
             label=paste0("Mass Threshold = ", th, " g/day")) +
    annotate('text', x=x_ann*.5, y=y_ann*.79, size=3, 
             label=paste0("Predicted PC = ", pc)) +
    ggtitle(paste0(names(plots[i]), " (bottom ", name_q_cutoff*100, 
                   "% sumpc_total)")) 
}

fl <- tempfile()
pdf(file=fl, width=8, height=10.5) 
gridExtra::grid.arrange(full_plot, plots[[1]], plots[[2]], 
                        plots[[3]], plots[[4]], plots[[5]], 
                        nrow=3)
dev.off()

doi <- Sys.Date() - 1
site_areas <- distinct(select(col_record, sensit, name))
query2 <- paste0("SELECT i.deployment AS sensit, s.datetime, s.sumpc, ", 
                 "flags.is_invalid(s.deployment_id, ",
                 "s.datetime - '01:00:00'::interval, ",
                 "s.datetime + '01:00:00'::interval) AS invalid ",
                 "FROM sensit.sensit_1hour s ",
                 "JOIN instruments.deployments i ", 
                 "ON s.deployment_id=i.deployment_id ", 
                 "WHERE (s.datetime -'1 second'::interval)::date='", 
                 doi, "';") 
sensits_yesterday <- query_owens(query2) %>% 
    left_join(site_areas, by="sensit")
sensits_yesterday <- filter(sensits_yesterday, !invalid)
sensits_yesterday$date <- 
    as.Date(substring(sensits_yesterday$datetime %m-% seconds(1), 1, 10))
# ADD IN FUNCTION TO MAKE SURE ALL SENSITS ARE ASSINGED TO AREA (NAME)
day_pc <- sensits_yesterday %>% group_by(sensit, date, name) %>%
    summarize(sumpc_total=sum(sumpc)) %>%
    # TAKE THIS OUT ONCE AREAS ARE PROPERLY ASSIGNED
    filter(!is.na(name))
day_pc$flag <- rep(NA, nrow(day_pc))
for (i in 1:nrow(day_pc)){
    day_pc$flag[i] <- 
        ifelse(day_pc[i, ]$sumpc_total >=
             predict_summary[predict_summary$name==day_pc[i, ]$name, ]$sumpc_total,
             T, F)
}


