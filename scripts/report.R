source("~/code/sensitCal/scripts/load.R")
load_all()
load_all("~/code/owensData")
load_all("~/code/owensMaps")
library(tidyverse)
library(ggplot2)
library(gridExtra)

query <- paste0("SELECT deployment AS sensit, ",
                "st_x(st_transform(geom, 26911)) AS x, ", 
                "st_y(st_transform(geom, 26911)) AS y ", 
                "FROM instruments.deployments ",
                "WHERE deployment IN ('", 
                paste(yesterday_df$sensit, collapse="', '"), "');")
site_locs <- query_owens(query)
yesterday_df <- yesterday_df %>% left_join(site_locs, by="sensit")

prob_thresh <- 0.5
flag_sites <- yesterday_df %>% filter(predict_prob>prob_thresh)

# parse and format data for display
day <- format(unique(yesterday_df$date), '%m-%d-%Y')
flag_sites$table_prob <- flag_sites$predict_prob
flag_sites$bad <- rep(F, nrow(flag_sites))
for (i in 1:nrow(flag_sites)){
    if (flag_sites$group[i]!='TwB2'){
        if (met_summary[met_summary$deployment==flag_sites$met[i], ]$max_ws<5){
            flag_sites$bad[i] <- T
            flag_sites$predict_prob[i] <- NA
            flag_sites$table_prob[i] <- '-'
        } 
    } else{
        if (met_summary[met_summary$deployment==flag_sites$met[i], ]$ws_max<1){
            flag_sites$bad[i] <- T
            flag_sites$predict_prob[i] <- NA
            flag_sites$table_prob[i] <- '-'
        }
    }
}
yesterday_df$active <- 
    sapply(yesterday_df$sensit, 
           function(x) 
               ifelse(x %in% filter(flag_sites, bad)$sensit, 
                      "Potential Bad Sensit", "Active Site"))

table_sites <- flag_sites %>% filter(!bad) %>%
    select(sensit, dca, table_prob) 
colnames(table_sites) <- c("Sensit", "DCA", "Probability\nOf Exceedance")
grob_theme <- ttheme_default(base_size=6, parse=T, 
                             colhead=list(fg_params=list(parse=T)))
if (nrow(table_sites)>0){
table_grob <- tableGrob(table_sites, theme=grob_theme, rows=NULL)
} else{
    table_grob <- grid::textGrob("No sites with\nprobable exceedance.", 
                                 hjust=0)
}

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
met_labels$nudge_x[1] <- -3000
met_labels$nudge_x[2] <- -3000
met_labels$nudge_x[3] <- -2000
met_labels$nudge_x[4] <- 3000
met_labels$nudge_x[5] <- 3000
met_labels$nudge_x[6] <- -2000

p1 <- ggplot(shoreline$polygons, aes(x=x, y=y)) +
    geom_path(mapping=aes(group=objectid)) +
    coord_equal() +
    geom_point(data=yesterday_df, shape=21, aes(fill=active)) +
    geom_point(data=flag_sites, mapping=aes(color=predict_prob)) +
    ggrepel::geom_label_repel(data=flag_sites, aes(label=sensit), size=2) +
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
