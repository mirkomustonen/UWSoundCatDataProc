#' Read and integrate selected time period SPL and wind speed data
#'
#' The integrate_wind_sound function reads the HDF5 SPL data, wind speed data 
#' interpolates the wind speed data and creates a composite R dataframe
#' 
#' @param loc_num Location number B20, B21, B22, B23 of read data.
#' @param beg_per Beginning time in character 2014-01-13 of the data to be read.
#' @param end_per Ending time in character 2014-02-13 of the data to be read.
#' 
#' @return Fataframe with timestamped 1/3 oct. band SPLs and wind speeds
#' 
integrate_wind_sound <- function(loc_name, beg_t_per, end_t_per) {
        source('read_sound.R')  # Read the read_sound function from read_sound.R file
        source('read_wind_data.R')  # Read the read_wind_data function from read_wind_data.R file
        
        s_out_list <- read_sound(loc_name, beg_t_per, end_t_per)  # Read sound data
        tob_data_l <- s_out_list[[1]]  # Extract SPL dataframe from sound data only
        remove(s_out_list)  # Remove unnecessary variables
        wind_data_l <- read_wind_data(loc_name, beg_t_per, end_t_per)  # Read wind data
        
        # Interpolate wind data for same time stamps as the SPL data
        wind_int <- approx(wind_data_l$DateTime, wind_data_l$Wind_speed_ms, xout = tob_data_l$DateTime)
        wind_data_int <- data.frame('DateTime' = wind_int$x, 'WS_ms' = wind_int$y)  
        
        tob_data_l$WS_ms <- wind_data_int$WS_ms # Add wind data to the tob data frame
        return(tob_data_l)
}

# int <- integrate_wind_sound('B20', '2014-01-01', '2014-02-01')