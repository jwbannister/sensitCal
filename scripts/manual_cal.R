library(ggplot2)
library(tidyverse)

cal_path <- "~/dropbox/owens/Owens Field Operations/Sensit calibration/"
cal_files <- list.files(path=cal_path, pattern=".dat")
manual_cal <- data.frame()
for (i in cal_files){
    tmp <- read.csv(file=paste0(cal_path, i), header=F, skip=4) 
    manual_cal <- rbind(manual_cal, tmp)
}
header <- readLines(paste0(cal_path, cal_files[1]))[2] %>%
    gsub('\"', "", .) %>%
    strsplit(., ",")
names(manual_cal) <- tolower(header[[1]])
manual_cal <- filter(manual_cal, as.Date(timestamp) > '2017-02-07')
manual_cal$snstsnmbr <- factor(manual_cal$snstsnmbr)
# calibration mass = 10g
manual_cal$ratio <- 10/manual_cal$sensitpc_tot
# correct sensits where gain is suspected to have been off?
manual_cal[manual_cal$snstsnmbr=='1882', ]$ratio <-
    manual_cal[manual_cal$snstsnmbr=='1882', ]$ratio / 10
manual_cal[manual_cal$snstsnmbr=='1888', ]$ratio <-
    manual_cal[manual_cal$snstsnmbr=='1888', ]$ratio / 10

manual_plot <- manual_cal %>% 
    ggplot(aes(x=snstsnmbr, y=ratio)) +
    geom_point() +
    theme(axis.text.x=element_text(angle=90))

