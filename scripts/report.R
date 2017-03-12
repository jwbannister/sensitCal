source("~/code/sensitCal/scripts/model.R")
load_all("~/code/owensMaps")
library(tidyverse)
library(ggplot2)
library(gridExtra)

# ADD IN FAKE EXCEEDANCES FOR EXAMPLE REPORT
# REMOVE IN PRODUCTION
yesterday_df[yesterday_df$sensit=='1602', ]$predict_count <- 9
yesterday_df[yesterday_df$sensit=='1605', ]$predict_count <- 20
yesterday_df[yesterday_df$sensit=='1606', ]$predict_count <- 25

yesterday_df <- yesterday_df %>% left_join(site_locs, by="sensit") %>%
    left_join(select(met_summary, deployment, max_ws, wd_max), 
              by=c("met"="deployment")) %>%
# do not report SF Studies sites
    filter(group!='SF Studies')
yesterday_df$pred_mass <- round(yesterday_df$predict_count / 10, 1)

# parse and format data for display
day <- format(unique(yesterday_df$date), '%m-%d-%Y')
yesterday_df$bad <- rep(F, nrow(yesterday_df))
for (i in 1:nrow(yesterday_df)){
    if (yesterday_df$group[i]!='TwB2'){
        if (yesterday_df$pred_mass[i]>5 & yesterday_df$max_ws[i]<5){
            yesterday_df$bad[i] <- T
            yesterday_df$predict_prob[i] <- NA
        } 
    } else{
        if (yesterday_df$pred_mass[i]>1 & yesterday_df$max_ws[i]<3){
            yesterday_df$bad[i] <- T
            yesterday_df$predict_prob[i] <- NA
        }
    }
}
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

table_sites <- yesterday_df %>% filter(flag) %>%
    select(sensit, dca, table_mass)
colnames(table_sites) <- c("Sensit", "DCA", "Estimated Flux (g/day)") 

grob_theme <- ttheme_default(base_size=4, parse=T, 
                             colhead=list(fg_params=list(parse=T)))
t1 <- tableGrob(table_sites, theme=grob_theme, rows=NULL)
title <- grid::textGrob("Sites Exceeding Sand Flux Limit",
                        gp=grid::gpar(fontsize=6))
padding <- unit(5,"mm")
table_grob <- gtable::gtable_add_rows(t1, 
                                      heights = grid::grobHeight(title) + padding, 
                                      pos = 0)
table_grob <- gtable::gtable_add_grob(table_grob, title, 1, 1, 1, 
                                      ncol(table_grob))

met_labels <- data.frame(label=rep(NA, nrow(met_summary)), 
                         nudge_x=rep(0, nrow(met_summary)), 
                         nudge_y=rep(0, nrow(met_summary)), 
                         x=rep(NA, nrow(met_summary)), 
                         y=rep(NA, nrow(met_summary)))
for (i in 1:nrow(met_summary)){
    met_labels$label[i] <- paste0("Met Station ", met_summary$deployment[i], 
                                  "\n", "Max WS (m/s) = ", 
                                  met_summary$max_ws[i]) 
    met_labels$x[i] <- met_summary$x[i]
    met_labels$y[i] <- met_summary$y[i]
}

plot_sites <- filter(yesterday_df, sensit %in% table_sites$Sensit)
p1 <- ggplot(shoreline$polygons, aes(x=x, y=y)) +
    geom_path(mapping=aes(group=objectid)) +
    geom_path(data=owens$polygons, mapping=aes(group=objectid), color="grey") +
    coord_equal() +
    geom_point(data=plot_sites, aes(color=over)) +
#    geom_point(data=yesterday_df, shape=21, aes(fill=active)) +
#    geom_point(data=filter(yesterday_df, predict_prob>0.5), 
#               aes(color=predict_prob)) +
    ggrepel::geom_label_repel(data=plot_sites, aes(label=sensit), size=2) +
    scale_color_gradient(name="Percentage of Exceedance Limit", 
                         breaks=c(1, 3, 5),
                         limits=c(1, 5),
                         labels=c('100%', '300%', '500%'),
                         low='yellow', high='red', na.value="red") + 
#    scale_fill_manual(name=NULL, values=c('grey', 'black')) +
    guides(color=guide_colorbar(title.position='top', title.hjust=0.5, 
                                direction="horizontal", order=2)) +
    annotation_custom(table_grob, xmin=400000, xmax=405000, 
                      ymin=4035000, ymax=4045000) +
    xlim(400000, 425000) +
    ggtitle(paste0("Owens Lake Sensit Notification System Report for ", day),
            subtitle="Flux values are estimates only. Official sand flux results will be determined after monthly CSC mass collections") + 
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          plot.title=element_text(hjust=0.5), 
          plot.subtitle=element_text(hjust=0.5), 
          panel.background=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          legend.title=element_text(size=8), 
          legend.position=c(.05, .9), 
          legend.justification=c(0, 1), 
          plot.background = element_rect(color='black', fill=NA, size=0.5))

pdf(file="~/code/sensitCal/output/report.pdf", height=11, width=8.5, 
    paper='letter')
print(p1)
dev.off()

