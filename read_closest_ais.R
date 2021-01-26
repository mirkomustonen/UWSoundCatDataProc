#' Read selected time period closest ship data
#'
#' The read_closest_ais function reads interpolated AIS data time series data
#' and creates a R dataframe from the data.
#' 
#' @param loc_nam Location name c('B20', 'B21', 'B22', 'B23') to be read.
#' @param beg_per Beginning time in character 2014-01-13 of the data to be read.
#' @param end_per Ending time in character 2014-02-13 of the data to be read.
#' 
#' @return Dataframe with timestamps
#' 
read_closest_ais <- function(loc_nam, beg_per, end_per) {
        library(sqldf)
        # For subsetting convert the input times to posixct time format
        beg_time <- as.POSIXct(paste(beg_per, '00:00'), 
                               format =  '%Y-%m-%d %H:%M', tz = 'UTC')
        end_time <- as.POSIXct(paste(end_per, '00:00'), 
                               format =  '%Y-%m-%d %H:%M', tz = 'UTC')
        # Construct the file path for reading
        file_path <- file.path('AIS_data_processing', 'AIS_data_interpolated')
        file_name <- paste0(loc_nam, '_AIS_closest_ts.csv')
        # For subsetting create sql query string
        query_string <- paste0("select * from file where Time>=", 
                               as.character(as.integer(beg_time)),
                               ' and Time<', as.character(as.integer(end_time)))
        # Read the AIS data from the file
        ais_tab <- read.csv.sql(file = file.path(file_path, file_name), sql = query_string)
        # Convert timestamps to POSIXct timestamps
        ais_tab$Time <- as.POSIXct(ais_tab$Time, origin = "1970-01-01", tz = 'UTC')
        # Order the dataframe according to Time
        ais_tab <- ais_tab[order(ais_tab$Time),]
        colnames(ais_tab)[colnames(ais_tab) == "Time"] <- "DateTime"
        base::closeAllConnections()  # Close temporary file created during reading
        return(ais_tab)
}
# loc_name <- 'B20'
# beg_per <- '2014-01-01' 
# end_per <- '2014-02-01'
# 
# ais_tab <- read_closest_ais(loc_name, beg_per, end_per)