#' Read and integrate selected time period SPL, wind speed and AIS data
#'
#' The integrate_wind_sound_ais function reads the HDF5 SPL data, wind speed data 
#' interpolates the wind speed data, read AIS data, and creates a composite 
#' R dataframe
#' 
#' @param loc_name Location name B20, B21, B22, B23 of read data.
#' @param beg_per Beginning time in character 2014-01-13 of the data to be read.
#' @param end_per Ending time in character 2014-02-13 of the data to be read.
#' 
#' @return Fataframe with timestamped 1/3 oct. band SPLs, wind speeds, and AIS
#' closest ship distance
#' 
integrate_sound_wind_ais <- function(loc_name, beg_t_per, end_t_per) {
        source('integrate_wind_sound.R')  # For integrating wind and sound data read the integrate_wind_sound
        source('read_closest_ais.R')  # For reading AIS data get the read_closest_ais function
        
        wind_sound_data <- integrate_wind_sound(loc_name, beg_t_per, end_t_per)  # Read sound data
        ais_data <- read_closest_ais(loc_name, beg_t_per, end_t_per)  # Read AIS data
        ais_data <- subset(ais_data, select = c('DateTime', 'Distance')) # Keep only distances
        
        # Merge the sound, wind and AIS data, when AIS data missing assign NA
        sound_wind_ais <- merge(x = wind_sound_data, y = ais_data, all.x = TRUE, by = "DateTime")
        return(sound_wind_ais)
}