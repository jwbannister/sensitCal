load_all("~/code/owensData")
library(tidyverse)

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
                 "swp.swap_datetime, col.coll_id, col.dwp_mass, ",
                 "col.sumpc_total ",
                 "FROM col ", 
                 "LEFT OUTER JOIN swp ",
                 "ON col.deployment_id=swp.deployment_id ",
                 "WHERE (swp.swap_datetime IS NULL ", 
                 "OR col.start_datetime > swp.swap_datetime) ", 
                 "AND col.dwp_mass IS NOT NULL ", 
                 "AND col.dwp_mass > 0 ",
                 "AND col.sumpc_total IS NOT NULL ", 
                 "AND col.sumpc_total > 0;")
col_record <- query_owens(query2)

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
col_summary <- query_owens(query3)

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
