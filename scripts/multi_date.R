library(lubridate)
stretch <- as.character(seq(ymd('2016-11-01'), Sys.Date() - 1, by = '1 day'))
for (day in stretch){
    system(paste0("Rscript ~/code/sensitCal/scripts/sensit_check.R ", day))
}
