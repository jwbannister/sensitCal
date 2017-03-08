source("~/code/sensitCal/scripts/load.R")
load_all()
load_all("~/code/owensData")
load_all("~/code/owensMaps")
library(tidyverse)
library(ggplot2)

prob_thresh <- 0.75
plot_data <- yesterday_df %>% filter(predict_prob>prob_thresh)
query <- paste0("SELECT deployment AS sensit, ",
                "st_x(st_transform(geom, 26911)) AS x, ", 
                "st_y(st_transform(geom, 26911)) AS y ", 
                "FROM instruments.deployments ",
                "WHERE deployment IN ('", 
                paste(plot_data$sensit, collapse="', '"), "');")
site_locs <- query_owens(query)
plot_data <- plot_data %>% left_join(site_locs, by="sensit")

# parse and format data for display
day <- format(unique(plot_data$date), '%m-%d-%Y')

site_grob <- plot_data %>% 
    mutate(predicted_flux=predict_count/10) %>% 
    select(sensit, dca, predicted_flux, exceedance_prob=predict_prob) %>%
    gridExtra::tableGrob(.)

p1 <- ggplot(shoreline$polygons, aes(x=x, y=y)) +
    geom_path(mapping=aes(group=objectid)) +
    coord_equal() +
    geom_point(data=plot_data, mapping=aes(color=predict_prob)) +
    scale_color_gradient(name="Probability of Exceedance", 
                         breaks=c(.5, 1), 
                         limits=c(0.5, 1), labels=c('50%', '100%'), 
                         low='yellow', high='red') + 
#    guides(color=guide_legend(title.position='top')) +
    annotation_custom(site_grob, xmin=425000, xmax=430000, 
                      ymin=4035000, ymax=4040000) +
    xlim(405000, 435000) +
    ggtitle(paste0("Possible Sand Flux Exceedances for ", day)) + 
    theme(axis.line=element_blank(),
#          axis.text=element_blank(),
#          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(), 
          legend.position=c(1, 1), 
          legend.justification=c(1, 1), 
          legend.direction='horizontal')
