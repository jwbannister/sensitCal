source("~/code/sensitCal/scripts/clean.R")
library(tidyverse)
library(lubridate)
library(ggplot2)

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

