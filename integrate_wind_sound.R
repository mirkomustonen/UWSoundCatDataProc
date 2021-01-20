#' Read and integrate selected time period SPL and wind speed data
#'
#' The integrate_wind_sound function reads the HDF5 SPL data, wind speed data 
#' interpolates the wind speed data and creates a composite R dataframe
#' 
#' @param loc_num Location number 1 (B20), 2 (B21), 3 (B22), 4 (B23) of read data.
#' @param beg_per Beginning time in character 2014-01-13 of the data to be read.
#' @param end_per Ending time in character 2014-02-13 of the data to be read.
#' 
#' @return Fataframe with timestamped 1/3 oct. band SPLs and wind speeds
#' 
integrate_wind_sound <- function(loc_num, beg_t_per, end_t_per) {
        source('read_sound.R')  # Read the read_sound function from read_sound.R file
        source('read_wind_data.R')  # Read the read_wind_data function from read_wind_data.R file
        s_locations <- c('B20', 'B21', 'B22', 'B23')  # List of sound data location names
        w_locations <- c('020', '021', '022', '023')  # List of wind data location names
        
        s_out_list <- read_sound(s_locations[loc_num], beg_t_per, end_t_per)  # Read sound data
        TOB_data_l <- s_out_list[[1]]  # Extract SPL dataframe from sound data only
        remove(s_out_list)  # Remove unnecessary variables
        wind_data_l <- read_wind_data(w_locations[loc_num], beg_t_per, end_t_per)  # Read wind data
        
        # Interpolate wind data for same time stamps as the SPL data
        wind_int <- approx(wind_data_l$DateTime, wind_data_l$Wind_speed_ms, xout = TOB_data_l$DateTime)
        wind_data_int <- data.frame('DateTime' = wind_int$x, 'WS_ms' = wind_int$y)  
        
        TOB_data_l$WS_ms <- wind_data_int$WS_ms # Add wind data to the TOB data frame
        return(TOB_data_l)
}