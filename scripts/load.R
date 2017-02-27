load_all("~/code/owensData")
library(tidyverse)

query4 <- "SELECT * FROM sandcatch.sensit_pc_records"
col_record <- query_owens(query4)
col_record$name <- factor(col_record$name, ordered=T)
col_record$ratio <- col_record$dwp_mass/col_record$sumpc_total

quantile_filter <- 0.95
filter_data <- col_record %>% 
    filter(ratio < quantile(col_record$ratio, probs=quantile_filter))

ratio_dist <- filter_data %>%
    ggplot(aes(x=ratio)) +
    geom_density(aes(color=name))
#    stat_ecdf()

plots <- vector(mode="list", length=length(unique(col_record$name)))
names(plots) <- levels(col_record$name)
clrs <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00')
full_plot <- filter_data %>%
    ggplot(aes(x=sumpc_total, y=dwp_mass)) +
    geom_point(aes(color=name)) +
    theme(legend.position=c(.2, .8), 
          legend.title=element_blank()) +
    scale_color_manual(values=clrs) 
for (i in 1:length(plots)){
    plots[[i]] <- filter_data %>%
        filter(name==names(plots)[i]) %>%
    ggplot(aes(x=sumpc_total, y=dwp_mass)) +
    geom_point(color=clrs[i]) +
    geom_smooth(method="lm", formula = y ~ 0 + x, se=F) +
    xlim(c(0, 20)) + ylim(c(0, 10)) +
    ggtitle(names(plots[i])) 
}
gridExtra::grid.arrange(full_plot, plots[[1]], plots[[2]], 
                        plots[[3]], plots[[4]], plots[[5]], 
                        nrow=3)

lm_list <- vector(mode="list", length=length(unique(col_record$name)))
names(lm_list) <- levels(col_record$name) 
for (i in 1:length(lm_list)){
    lm_list[[i]] <- filter_data %>% 
        filter(name==names(plots)[i]) %>%
        lm(dwp_mass ~ 0 + sumpc_total, .)
    lm_list[[i]]$res_plot <- ggplot(data.frame(x=lm_list[[i]]$fitted.values, 
                                               y=lm_list[[i]]$residuals), 
                                    aes(x=x, y=y)) +
geom_point() +
    ggtitle(names(plots[i])) +
    ylab("residual") + xlab("fitted.values") + 
    ylim(c(-25, 25)) + xlim(c(0, 20))
}
gridExtra::grid.arrange(lm_list[[1]]$res_plot, lm_list[[2]]$res_plot, 
                        lm_list[[3]]$res_plot, lm_list[[4]]$res_plot, 
                        lm_list[[5]]$res_plot, 
                        nrow=3)


new.data <- data.frame(sumpc_total=seq(.1, 3, .5))
logmass.pred <- cbind(new.data, 
                      predict(sensit.lm, new.data, interval="prediction", 
                              level=0.6))

query5 <- paste0("SELECT i.deployment, ",
                 "(s.datetime - '1 second'::interval)::date AS date, ",
                 "SUM(sumpc) AS sumpc_total ",
                 "FROM sensit.sensit_1hour s ",
                 "JOIN instruments.deployments i ",
                 "ON i.deployment_id=s.deployment_id ",
                 "WHERE (s.datetime - '1 second'::interval)::date ", 
                 "= '", Sys.Date() - 1, "'::date ", 
                 "GROUP BY i.deployment, ", 
                 "(s.datetime - '1 second'::interval)::date;")
pc_yesterday <- query_owens(query5)

a <- cbind(pc_yesterday, predict(pc.lm, pc_yesterday, interval="predict", 
                                 level=0.6))





query5 <- "SELECT * FROM sandcatch.sensit_calibrate"
col_summary <- query_owens(query5)

a <- filter(col_summary, n >= 3)
b <- filter(col_record, sensit %in% a$sensit)
d <- filter(col_record, dwp_mass > 1 & sensit %in% a$sensit)

library(ggplot2)
for (i in unique(d$sensit)){
    p1 <- d %>% 
#        filter(sensit==i) %>% 
        ggplot(aes(x=log(dwp_mass), y=log(sumpc_total))) +
        geom_point() + 
        geom_smooth(method="lm")
    png(filename=paste0("~/Desktop/temp/", i, ".png"), width=6, height=6, 
        units="in", res=300)
    print(p1)
    dev.off()
}

save_plot <- function(plt, nm){
    png(filename=paste0("~/Desktop/", nm, ".png"), height=6, width=6, 
        units="in", res=300)
    print(plt)
    dev.off()
}

query2 <- paste0("CREATE TEMP TABLE swp AS ", 
                     "SELECT deployment_id, ", 
                     "MAX(end_datetime) AS swap_datetime ",
                     "FROM field_data.site_visits ", 
                     "WHERE instrument_swap ", 
                     "AND comments ILIKE '%sensit%' ",
                     "GROUP BY deployment_id; ",
                 "CREATE TEMP TABLE col AS ",
                     "SELECT i.deployment AS csc, ii.deployment AS sensit, ",
                     "s.start_datetime, s.collection_datetime, s.dwp_mass, ",
                     "ss.coll_id, s.sumpc_total, ii.deployment_id ",
                     "FROM sandcatch.csc_summary s ", 
                     "JOIN instruments.deployments i ",
                     "ON s.csc_deployment_id=i.deployment_id ",
                     "JOIN instruments.deployments ii ",
                     "ON s.sensit_deployment_id=ii.deployment_id ",
                     "JOIN sandcatch.csc_data ss ",
                     "ON s.csc_data_id=ss.csc_data_id ",
                     "WHERE i.deployment=ii.deployment ",
                     "AND ii.active; ",
                 "SELECT col.deployment_id, col.csc, col.sensit, ",
                 "col.start_datetime, col.collection_datetime, ",
                 "col.coll_id, col.dwp_mass, ",
                 "col.sumpc_total ",
                 "FROM col ", 
                 "LEFT OUTER JOIN swp ",
                 "ON col.deployment_id=swp.deployment_id ",
                 "WHERE (swp.swap_datetime IS NULL ", 
                 "OR col.start_datetime > swp.swap_datetime);") 

query3 <- paste0("CREATE TEMP TABLE swp AS ", 
                     "SELECT deployment_id, ", 
                     "MAX(end_datetime) AS swap_datetime ",
                     "FROM field_data.site_visits ", 
                     "WHERE instrument_swap ", 
                     "AND comments ILIKE '%sensit%' ",
                     "GROUP BY deployment_id; ",
                 "CREATE TEMP TABLE col AS ",
                     "SELECT i.deployment AS csc, ii.deployment AS sensit, ",
                     "s.start_datetime, s.collection_datetime, s.dwp_mass, ",
                     "ss.coll_id, s.sumpc_total, ii.deployment_id ",
                     "FROM sandcatch.csc_summary s ", 
                     "JOIN instruments.deployments i ",
                     "ON s.csc_deployment_id=i.deployment_id ",
                     "JOIN instruments.deployments ii ",
                     "ON s.sensit_deployment_id=ii.deployment_id ",
                     "JOIN sandcatch.csc_data ss ",
                     "ON s.csc_data_id=ss.csc_data_id ",
                     "WHERE i.deployment=ii.deployment ",
                     "AND ii.active; ",
                 "SELECT col.sensit, COUNT(col.dwp_mass) AS n, ", 
                 "REGR_SLOPE(LN(col.sumpc_total), LN(col.dwp_mass)) AS slope, ",
                 "REGR_INTERCEPT(LN(col.sumpc_total), LN(col.dwp_mass)) AS intercept, ",
                 "REGR_R2(LN(col.sumpc_total), LN(col.dwp_mass)) AS r_sq ",
                 "FROM col ", 
                 "LEFT OUTER JOIN swp ",
                 "ON col.deployment_id=swp.deployment_id ",
                 "WHERE (swap_datetime IS NULL ", 
                 "OR start_datetime > swap_datetime) ", 
                 "AND col.dwp_mass IS NOT NULL ", 
                 "AND col.dwp_mass > 1 ",
                 "AND col.sumpc_total IS NOT NULL ", 
                 "AND col.sumpc_total > 0 ",
                 "GROUP BY col.sensit;") 
