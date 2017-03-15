
#' Pull all available collection data for all active sensits
#' 
#' @return Data frame.
pull_collections_active <- function(){
    query1 <- "SELECT * FROM sandcatch.sensit_pc_records"
    col_record <- query_owens(query1)
    col_record
}

#' Pull 1 hour sensit particle count data for all active sensits 
#' 
#' @param day Date. Single day for which sensit data should be pulled. 
#' @return Data frame.
pull_sensit_day <- function(day=Sys.Date()-1){
    query <- paste0("SELECT i.deployment AS sensit, s.datetime, s.sumpc, ", 
                    "g.name AS group, a.area AS dca, i2.deployment AS met, ",
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
                    "LEFT JOIN instruments.sensit_metstations sm ",
                    "ON s.deployment_id=sm.sensit_deployment_id ",
                    "LEFT JOIN instruments.deployments i2 ",
                    "ON sm.met_deployment_id=i2.deployment_id ",
                    "WHERE (s.datetime -'1 second'::interval)::date='", 
                    day, "'", 
                    "AND g.name!='LADWP Sand Flux Sites';") 
    df1 <- query_owens(query) 
    df1
}

#' Pull hourly data from all met stations for a single day
#' 
#' @param day Date. Single day for which met data should be pulled. 
#' @return Data frame.
pull_met_day <- function(day){
    query_m <- paste0("SELECT i.deployment, m.datetime, m.ws_10m, m.wd_10m ",
                      "FROM met.met_1hour m JOIN instruments.deployments i ",
                      "ON m.deployment_id=i.deployment_id ",
                      "WHERE (m.datetime - '1 second'::interval)::date='",
                      day, "'::date;")
    met_df <- query_owens(query_m)
    met_df
}
    
#' Get locations of list of deployments
#' 
#' @param deployments Vector. String or integer deployment ids. 
#' @return Data frame.
pull_deployment_locations <- function(deployments){
    query1 <- paste0("SELECT deployment, ",
                     "st_x(st_transform(geom, 26911)) AS x, ",
                     "st_y(st_transform(geom, 26911)) AS y ",
                     "FROM instruments.deployments ",
                     "WHERE deployment IN ('",
                     paste(deployments, collapse="', '"), "');")
    df1 <- query_owens(query1)
    df1
}
