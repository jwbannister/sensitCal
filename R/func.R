
#' Pull 1 hour sensit particle counddata for all active sensits 
#' 
#' @param day Date. Single day for which sensit data should be pulled. 
#' @return Data frame.
pull_sensit_day <- function(day=Sys.Date()-1){
    query <- paste0("SELECT i.deployment AS sensit, s.datetime, s.sumpc, ", 
                    "g.name AS group, a.area AS dca, ",
                    "flags.is_invalid(s.deployment_id, ",
                    "s.datetime - '01:00:00'::interval, ",
                    "s.datetime + '01:00:00'::interval) AS invalid ",
                    "FROM sensit.sensit_1hour s ",
                    "LEFT JOIN instruments.deployments i ",
                    "ON s.deployment_id=i.deployment_id ", 
                    "LEFT JOIN info.deployment_groups ig ",
                    "ON s.deployment_id = ig.deployment_id ",
                    "LEFT JOIN info.groups g ON ig.group_id = g.group_id ",
                    "LEFT JOIN instruments.areas a ON i.area_id = a.area_id ",
                    "WHERE (s.datetime -'1 second'::interval)::date='", 
                    day, "';") 
    df1 <- query_owens(query) 
    df1
}
