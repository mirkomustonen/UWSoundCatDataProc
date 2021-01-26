#' Read selected time period wind speed data
#'
#' The read_wind_data function reads wind speed data from .txt files 
#' corresponding to selected time period and converts the wind speed from knots
#' to m/s.
#' 
#' @param loc_nam Location name c('20', '21', '22', '23') to be read.
#' @param beg_per Beginning time in character 2014-01-13 of the data to be read.
#' @param end_per Ending time in character 2014-02-13 of the data to be read.
#' 
#' @return Dataframe wind speed in m/s and timestamps
#' 
read_wind_data <- function(loc_nam, beg_period, end_period) {
        # Convert the input beginning time period to POSIXct time format
        beg_period <- as.POSIXct(beg_period, format =  '%Y-%m-%d', tz = 'UTC')
        # Convert the input end time period to POSIXct time format
        end_period <- as.POSIXct(end_period, format =  '%Y-%m-%d', tz = 'UTC')
        
        loc_nam <- paste0('0', substr(loc_nam, 2, 3))  # For reading wind data change the loc_name
        files_path <- file.path('Meteo_sea', 'Wind', 'QUONOPS_only_ESTONIA')  # Construct file path for wind speed data
        loc_files <- list.files(path = files_path, pattern = paste0('+', loc_nam, '+'))  # List the wind data files to be read in
        
        k <- 0  # Flag value for marking the first iteration
        for (i_file in loc_files) {
                wind_data <- read.table(file = file.path(files_path, i_file), 
                                        header = TRUE, skip = 2, sep = '\t', 
                                        col.names = c('DateTime', 'Wind_speed_knots'))
                
                wind_data$Wind_speed_ms <- 0.51444444444444*wind_data$Wind_speed_knots  # Convert knots to m/s
                wind_data <- wind_data[ ,c(1,3)]  # Exclude wind speed in knots column
                wind_data$DateTime <- as.POSIXct(wind_data$DateTime, tz = 'UTC')  # Convert timestamps to POSIXct format
                if (k == 0) {
                        wind_data_l <- wind_data  # For first iteration create new table the data will be added to
                        k <- 1  # Set the first iteration flag to 1
                } else {
                        wind_data_l <- rbind(wind_data_l, wind_data)  # For iterations after first aggregate dataframes into larger dataframe
                }
        }
        wind_data_l <- subset(wind_data_l, DateTime >= beg_period)  # Subset data to selected time period
        wind_data_l <- subset(wind_data_l, DateTime <= end_period)  # Subset data to selected time period
        return(wind_data_l)
}
# w_data <- read_wind_data('B23', '2014-01-01', '2014-02-01')