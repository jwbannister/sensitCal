source("~/code/sensitCal/scripts/model.R")
load_all("~/code/owensMaps")
library(tidyverse)
library(ggplot2)
library(gridExtra)

yesterday_df <- yesterday_df %>% left_join(site_locs, by="sensit") %>%
    left_join(select(met_summary, deployment, max_ws, wd_max), 
              by=c("met"="deployment")) %>%
# do not report SF Studies sites
    filter(group!='SF Studies')
yesterday_df$pred_mass <- round(yesterday_df$predict_count / 10, 1)

prob_thresh <- 0.5

# parse and format data for display
day <- format(unique(yesterday_df$date), '%m-%d-%Y')
yesterday_df$table_prob <- paste0(round(yesterday_df$predict_prob*100, 0), "%")
yesterday_df$bad <- rep(F, nrow(yesterday_df))
for (i in 1:nrow(yesterday_df)){
    if (yesterday_df$group[i]!='TwB2'){
        if (yesterday_df$pred_mass[i]>5 & yesterday_df$max_ws[i]<5){
            yesterday_df$bad[i] <- T
            yesterday_df$predict_prob[i] <- NA
            yesterday_df$table_prob[i] <- '-'
        } 
    } else{
        if (yesterday_df$pred_mass[i]>1 & yesterday_df$max_ws[i]<3){
            yesterday_df$bad[i] <- T
            yesterday_df$predict_prob[i] <- NA
            yesterday_df$table_prob[i] <- '-'
        }
    }
}
yesterday_df$active <- 
    sapply(yesterday_df$bad, 
           function(x) ifelse(x, "Potential Bad Sensit", "Active Site"))
yesterday_df$table_mass <- as.character(yesterday_df$pred_mass)
for (i in 1:nrow(yesterday_df)){
yesterday_df$table_mass[i] <- ifelse(yesterday_df$bad[i], 
                                     "BAD", yesterday_df$table_mass[i])
yesterday_df$table_mass[i] <- ifelse(yesterday_df$table_mass[i] > 200, 
                                     ">200", yesterday_df$table_mass[i])
}

table_sites <- yesterday_df %>% filter(sumpc_total>0) %>%
    select(sensit, dca, table_mass, table_prob)
colnames(table_sites) <- c("Sensit", "DCA", "Predicted\nFlux\\ (g/day)", 
                           "Probability\nOf Exceedance")

grob_theme <- ttheme_default(base_size=4, parse=T, 
                             colhead=list(fg_params=list(parse=T)))
t1 <- tableGrob(table_sites, theme=grob_theme, rows=NULL)
title <- grid::textGrob("Sites with Sensit Activity",gp=grid::gpar(fontsize=6))
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
met_labels$nudge_x[1] <- 3000
met_labels$nudge_y[1] <- -3000
met_labels$nudge_x[2] <- -2000
met_labels$nudge_y[2] <- 3000
met_labels$nudge_x[3] <- 1000
met_labels$nudge_y[3] <- -3000
met_labels$nudge_x[4] <- 3000
met_labels$nudge_x[5] <- 3000
met_labels$nudge_x[6] <- -2000

p1 <- ggplot(shoreline$polygons, aes(x=x, y=y)) +
    geom_path(mapping=aes(group=objectid)) +
    coord_equal() +
    geom_point(data=yesterday_df, shape=21, aes(fill=active)) +
    geom_point(data=filter(yesterday_df, predict_prob>0.5), 
               aes(color=predict_prob)) +
    ggrepel::geom_label_repel(data=filter(yesterday_df, 
                                          sensit %in% table_sites$Sensit), 
                              aes(label=sensit), size=2) +
    scale_color_gradient(name="Probability of Exceedance", 
                         breaks=c(prob_thresh, 1), 
                         limits=c(prob_thresh, 1), 
                         labels=c(paste0(prob_thresh*100, '%'), '100%'), 
                         low='yellow', high='red', na.value="black") + 
    scale_fill_manual(name=NULL, values=c('grey', 'black')) +
    guides(fill=guide_legend(direction="vertical", order=1), 
           color=guide_colorbar(title.position='top', title.hjust=0.5, 
                                direction="horizontal", order=2)) +
    annotation_custom(table_grob, xmin=400000, xmax=405000, 
                      ymin=4025000, ymax=4035000) +
    ggrepel::geom_label_repel(data=met_labels[1, ], size=2, color='blue', 
               mapping=aes(label=label), nudge_x=met_labels[1, ]$nudge_x, 
               nudge_y=met_labels[1, ]$nudge_y) +
    ggrepel::geom_label_repel(data=met_labels[2, ], size=2, color='blue', 
               mapping=aes(label=label), nudge_x=met_labels[2, ]$nudge_x, 
               nudge_y=met_labels[2, ]$nudge_y) +
    ggrepel::geom_label_repel(data=met_labels[3, ], size=2, color='blue', 
               mapping=aes(label=label), nudge_x=met_labels[3, ]$nudge_x, 
               nudge_y=met_labels[3, ]$nudge_y) +
    ggrepel::geom_label_repel(data=met_labels[4, ], size=2, color='blue', 
               mapping=aes(label=label), nudge_x=met_labels[4, ]$nudge_x, 
               nudge_y=met_labels[4, ]$nudge_y) +
    ggrepel::geom_label_repel(data=met_labels[5, ], size=2, color='blue', 
               mapping=aes(label=label), nudge_x=met_labels[5, ]$nudge_x, 
               nudge_y=met_labels[5, ]$nudge_y) +
    ggrepel::geom_label_repel(data=met_labels[6, ], size=2, color='blue', 
               mapping=aes(label=label), nudge_x=met_labels[6, ]$nudge_x, 
               nudge_y=met_labels[6, ]$nudge_y) +
    geom_spoke(data=met_summary, arrow=arrow(length=unit(0.5, "cm")),  
               mapping=aes(x=x, y=y, angle=wd_max, radius=max_ws*500), 
               color="blue", size=2, alpha=0.5) +
    xlim(400000, 425000) +
    ggtitle(paste0("Sand Motion Monitoring Sites Report for ", day)) + 
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          plot.title=element_text(hjust=0.5), 
          panel.background=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          legend.title=element_text(size=8), 
          legend.position=c(0, 1), 
          legend.justification=c(0, 1), 
          plot.background = element_rect(color='black', fill=NA, size=0.5))

pdf(file="~/code/sensitCal/output/report.pdf", height=11, width=8.5, 
    paper='letter')
print(p1)
dev.off()

