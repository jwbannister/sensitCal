
#' Plot report of daily Sensit exceedances for transmittal to LADWP
#' 
#' @return ggplot object.
plot_ladwp_report <- function(){
    plot_sites <- filter(yesterday_df, sensit %in% table_sites$sensit)
    img <- tiff::readTIFF("~/code/sensitCal/data/logo.tiff")
    logo <- grid::rasterGrob(img, interpolate=T)
    ttle <- paste0("Owens Lake Sensit Notification System Report for ",
                   month(report_date, label=T, abbr=F), " ", mday(report_date), 
                   ", ", year(report_date))
    subttle <- "Flux values are estimates only. Official sand flux results will be determined after monthly CSC mass collections.\nAir Sciences staff will conduct investigations into potential exceedances and include explanatory comments in\nmonthly area reports."
    p1 <- ggplot(shoreline$polygons, aes(x=x, y=y)) +
        geom_path(mapping=aes(group=objectid)) +
        geom_path(data=owens$polygons, mapping=aes(group=objectid), color="grey") +
        coord_equal() +
        xlim(400000, 425000) + 
        geom_point(data=plot_sites, aes(color=over)) +
        ggrepel::geom_label_repel(data=plot_sites, aes(label=sensit), size=2) +
        scale_color_gradient(name="Percentage of Exceedance Limit", 
                             breaks=c(1, 3, 5),
                             limits=c(1, 5),
                             labels=c('100%', '300%', '500%'),
                             low='yellow', high='red', na.value="red") + 
        guides(color=guide_colorbar(title.position='top', title.hjust=0.5, 
                                    direction="horizontal", order=2)) +
        annotation_custom(plot_grob, xmin=400000, xmax=405000, 
                          ymin=4020000, ymax=4050000) +
        annotation_custom(logo, xmin=420000, xmax=425000, ymin=4018000, 
                          ymax=4022000) +
        ggtitle(ttle, subtitle=subttle) + 
        theme(axis.line=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              axis.title=element_blank(),
              plot.title=element_text(hjust=0.5), 
              plot.subtitle=element_text(hjust=0), 
              panel.background=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              legend.key.width=unit(.5, "inches"), 
              legend.title=element_text(size=8), 
              legend.position=c(.6, .95), 
              legend.justification=c(0, 1), 
              plot.background = element_rect(color='black', fill=NA, size=0.5))
    p1
}

#' Build table grob of exceedance sites
#' 
#' @return table grob
build_exceedance_table <- function(sites){
    grob_theme <- ttheme_default(base_size=6, parse=T, 
                                 colhead=list(fg_params=list(parse=T)))
    padding <- unit(5,"mm")
    for (i in 1:(length(unique(sites$group2)))){
        grp <- rev(unique(sites$group2))[i]
        thresh <- if_else(grp=='TwB2', "0.5", "5")
        tmp <- filter(sites, group2==grp) %>% select(-group2)
        colnames(tmp) <- c("Sensit", "DCA", "Estimated Flux (g/day)") 
        table_grob <- tableGrob(tmp, theme=grob_theme, rows=NULL)
        header_grob <- grid::textGrob(paste0(grp, " (threshold = ", thresh, " g/day)"), 
                                      gp=grid::gpar(fontsize=8), vjust=1)
        table_grob <- gtable::gtable_add_rows(table_grob, 
                                              heights = grid::grobHeight(header_grob) + padding, 
                                              pos = 0)
        table_grob <- gtable::gtable_add_grob(table_grob, header_grob, 1, 1, 1, 
                                              ncol(table_grob))
        if (i==1){
            plot_grob <- table_grob
        } else{
            plot_grob <- gtable::gtable_add_rows(plot_grob, 
                                                 heights = grid::grobHeight(table_grob) + padding, 
                                                 pos = 0)
            plot_grob <- gtable::gtable_add_grob(plot_grob, table_grob, 1, 1, 1, 
                                                 ncol(table_grob))
        }
    }
    title_grob <- grid::textGrob("Sites Exceeding Sand Flux Limit", 
                                 gp=grid::gpar(fontsize=8, fontfacce='bold'))
    plot_grob <- gtable::gtable_add_rows(plot_grob, 
                                         heights = grid::grobHeight(title_grob) + padding, 
                                         pos = 0)
    plot_grob <- gtable::gtable_add_grob(plot_grob, title_grob, 1, 1, 1, 
                                         ncol(table_grob))
    plot_grob
}

#' Build Met station labels
#' 
#' @return dataframe
build_met_labels <- function(){
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
    met_labels[1, ]$nudge_x <- -4500
    met_labels[1, ]$nudge_y <- 0
    met_labels[2, ]$nudge_x <- -6000
    met_labels[2, ]$nudge_y <- 1500
    met_labels[3, ]$nudge_x <- 4500
    met_labels[3, ]$nudge_y <- -2000
    met_labels[4, ]$nudge_x <- 4000
    met_labels[4, ]$nudge_y <- -1000
    met_labels[5, ]$nudge_x <- 3000
    met_labels[5, ]$nudge_y <- 1500
    met_labels[6, ]$nudge_x <- 5500
    met_labels[6, ]$nudge_y <- 2500
    met_labels$x <- met_labels$x + met_labels$nudge_x
    met_labels$y <- met_labels$y + met_labels$nudge_y
    met_labels
}

#' Plot report of daily Sensit results for internal AirSci use
#' 
#' @return ggplot object.
plot_airsci_report <- function(){
    plot_sites <- filter(yesterday_df, sensit %in% airsci_sites$sensit)
    ttle <- paste0("Owens Lake Sensit System Report for ",
                   month(report_date, label=T, abbr=F), " ", mday(report_date), 
                   ", ", year(report_date))
    subttle <- "DO NOT TRANSMIT OUTSIDE OF AIR SCIENCES. Internal use only. Flux values are estimates only."
    p1 <- ggplot(shoreline$polygons, aes(x=x, y=y)) +
        geom_path(mapping=aes(group=objectid)) +
        geom_path(data=owens$polygons, mapping=aes(group=objectid), color="grey") +
        coord_equal() +
        xlim(400000, 425000) + 
        geom_spoke(data=met_summary, 
                   mapping=aes(angle=wd_max, radius=max_ws * 250, x=x, y=y), 
                   color="lightblue", size=3, 
                   arrow=arrow(length=unit(3, "mm"))) +
        geom_label(data=met_labels, aes(label=label), size=3) +
        geom_point(data=yesterday_df, shape=21, aes(fill=bad)) +
        scale_fill_manual(name=NULL, values=c('grey', 'black'), 
                          breaks=c(FALSE, TRUE), 
                          labels=c("Active Site", "Potential Bad Site")) +
        geom_point(data=filter(plot_sites, !bad), aes(color=over)) +
        ggrepel::geom_label_repel(data=plot_sites, aes(label=sensit), size=2) +
        scale_color_gradient(name="Percentage of Exceedance Limit", 
                             breaks=c(1, 3, 5),
                             limits=c(1, 5),
                             labels=c('100%', '300%', '500%'),
                             low='yellow', high='red', na.value="red") + 
        guides(color=guide_colorbar(title.position='top', title.hjust=0.5, 
                                    direction="horizontal", order=2)) +
        annotation_custom(airsci_grob, xmin=400000, xmax=405000, 
                          ymin=4020000, ymax=4050000) +
        ggtitle(ttle, subtitle=subttle) + 
        theme(axis.line=element_blank(),
              axis.text=element_blank(),
              axis.ticks=element_blank(),
              axis.title=element_blank(),
              plot.title=element_text(hjust=0.5), 
              plot.subtitle=element_text(hjust=0.5), 
              panel.background=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              legend.key.width=unit(.4, "inches"), 
              legend.margin=margin(t=0, unit="inches"), 
              legend.title=element_text(size=8), 
              legend.position=c(0.6, 0.95), 
              legend.justification=c(0, 1), 
              plot.background = element_rect(color='black', fill=NA, size=0.5))
    p1
}
