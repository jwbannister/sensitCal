load_all()
library(tidyverse)
library(lubridate)
library(ggplot2)

col_record <- pull_collections_active()
col_dates <- col_record %>% group_by(group) %>%
    summarize(date=as.Date(max(collection_datetime)))

# remove outlier data - unrealistic mass/pc ratios (both high and low)
col_record$ratio <- col_record$dwp_mass/col_record$sumpc_total
quantile_filter <- 0.9
filter_data <- col_record %>% 
    filter((ratio < quantile(col_record$ratio, probs=quantile_filter, na.rm=T) &
           1/ratio < quantile(1/col_record$ratio, probs=quantile_filter, na.rm=T)) |
           is.na(ratio))

# remove sumpc_total > 700, screws up Poisson regression model
# remove dwp_mass > 20, focus on exceedance order of magnitude
# remvoe SF Study area Sensits
filter_data2 <- filter_data %>% filter(sumpc_total<700 & dwp_mass<20) %>%
    filter(group!='SF Studies')
filter_data2$group2 <- filter_data2$group

filter_data3 <- filter_data2
filter_data3[filter_data3$group!='TwB2', ]$group <- 'Other Areas'
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

predict_list <- vector(mode="list", length=length(unique(filter_data3$sensit)))
names(predict_list) <- unique(filter_data3$sensit)
lm_summary <- data.frame(sensit=c(), n=c(), coeff=c(), st.err=c(), 
                         r.sq=c(), p=c(), group2=c())
for (i in names(predict_list)){
    model_data <- filter_data3 %>% filter(sensit==i)
    # build linear regression models
    predict_list[[i]] <- lm(dwp_mass ~ sumpc_total + 0, data=model_data) 
    predict_list[[i]]$data <- cbind(model_data, 
                        "fitted"=predict_list[[i]]$fitted)
    lm_summary <- 
        rbind(lm_summary, 
              data.frame(sensit=i, n=nrow(model_data), 
                         coeff=summary(predict_list[[i]])$coefficients[1], 
                         st.err=summary(predict_list[[i]])$coefficients[2], 
                         r.sq=summary(predict_list[[i]])$r.squared,
                         p=summary(predict_list[[i]])$coefficients[4],
                         group2=unique(model_data$group2)))
}

logistic_list <- vector(mode="list", length=2)
names(logistic_list) <- unique(filter_data3$group)
for (i in names(logistic_list)){
    model_data <- filter_data3 %>% filter(group==i)
    # build logistic regression models
    logistic_list[[i]] <- glm(flag ~ sumpc_total, data=model_data, 
                              family=binomial)
    logistic_list[[i]]$data <- cbind(model_data, 
                   "logistic.prob"=logistic_list[[i]]$fitted)
}

plot_data <- lm_summary[complete.cases(lm_summary), ]
p1 <- ggplot(plot_data, aes(x=n, y=r.sq)) +
    geom_point(aes(color=group2)) +
    ggrepel::geom_label_repel(data=filter(plot_data, r.sq<0.6), 
                              aes(label=sensit)) +
    ylab("Model Rsquared") + xlab("# of collections in model") +
    theme(legend.position=c(1, 0), 
          legend.justification=c(1, 0))
png(file="./data/rsquared_plot.png", height=6, width=6, units="in", res=300)
print(p1)
dev.off()

save(predict_list, logistic_list, file="./data/model.RData")

