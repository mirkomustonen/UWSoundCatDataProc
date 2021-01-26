#' Read and integrate selected time period SPL and wind speed data
#'
#' The integrate_wind_sound function reads the HDF5 SPL data, wind speed data 
#' interpolates the wind speed data and creates a composite R dataframe
#' 
#' @param loc_name Location name c('B20', 'B21', 'B22', 'B23') of read data.
#' @param beg_per Beginning time in character 2014-01-13 of the data to be read.
#' @param end_per Ending time in character 2014-02-13 of the data to be read.
#' 
#' @return Fataframe with timestamped 1/3 oct. band SPLs and wind speeds
#' 
integrate_ais_sound <- function(loc_name, beg_t_per, end_t_per) {
        source('read_sound.R')  # Read the read_sound function from read_sound.R file
        source('read_closest_ais.R')  # Read the read_wind_data function from read_wind_data.R file
        
        s_out_list <- read_sound(loc_name, beg_t_per, end_t_per)  # Read sound data
        tob_data <- s_out_list[[1]]  # Extract SPL dataframe from sound data only
        remove(s_out_list)  # Remove unnecessary variables
        ais_data <- read_closest_ais(loc_name, beg_t_per, end_t_per)  # Read AIS data
        ais_data <- subset(ais_data, select = c('DateTime', 'Distance')) # Keep only distances
        # Merge the sound and AIS data
        sound_ais = merge(x = tob_data,y = ais_data,by = "DateTime")
        return(sound_ais)
}