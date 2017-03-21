library(lubridate)
stretch <- as.character(seq(ymd('2017-03-17'), Sys.Date() - 1, by = '1 day'))
for (day in stretch){
    system(paste0("Rscript ~/code/sensitCal/scripts/sensit_check.R ", day))
}
