#' Read selected time period SPL data from HDF5 files
#'
#' The read_sound function reads the HDF5 SPL files and creates a R dataframe
#' from the data.
#' 
#' @param loc Location name c('B20', 'B21', 'B22', 'B23') to be read.
#' @param beg_per Beginning time in character 2014-01-13 of the data to be read.
#' @param end_per Ending time in character 2014-02-13 of the data to be read.
#' 
#' @return Dataframe with timestamps, 1/3 oct SPLs
#' 
read_sound <- function(loc, beg_per, end_per) {
  options(warn = -1)  # Turn warnings off
  library(rhdf5)  # Library for handling HDF5 files
  library(lubridate)  # Library for better datetime handling
  
  # Convert the input beginning time period to R workable POSIXct time format
  beg_per <- as.POSIXct(beg_per, format = '%Y-%m-%d', tz = 'UTC')
  # Convert the input end time period to R workable POSIXct time format
  end_per <- as.POSIXct(end_per, format = '%Y-%m-%d', tz = 'UTC')
  # Create a time period from the selected beginning and end times
  sel_per <- interval(beg_per, end_per, tzone = "UTC")
  # List the HDF5 files to be read in
  loc_files <- list.files(path = "PAM_Output", pattern = paste0('+', loc, '+'))
  
  # Read third-octave nominal and numerical values from file
  tobs <- read.table(file = 'Third_octs.txt', sep = '\t', header = TRUE)
  
  # Function for reading a single HDF5 file from the loc_files list
  read_sound_file <- function(i_file) {
    f_beg_d <- as.POSIXct(substr(i_file, 26, 33), format =  '%Y%m%d', tz = 'UTC')  # Beginning time of the file to be read in
    f_end_d <- as.POSIXct(substr(i_file, 35, 42), format =  '%Y%m%d', tz = 'UTC')  # End time of the file to be read in
    file_per <- interval(f_beg_d, f_end_d, tzone = "UTC") # Create a time period with file beginning and end times
    
    file_per_test <- !is.na(intersect(sel_per, file_per))  # Check whether there is overlap between the selected and file time periods
    if (file_per_test) {
      file_path <- file.path("PAM_Output", i_file)  # Construct the HDF5 file path to be read in
      
      tob_data <- h5read(file_path, "/Data/LeqMeasurementsOfChannel1")  # Read Third Octave Band data from the file in matrix form
      tob_data <- as.data.frame(t(tob_data))  # Transform the tob data matrix and convert to dataframe
      
      # Read the center frequencies of the third octave bands
      freqs_data <- h5read(file_path, "Metadata/FrequencyIndex")
      # For first tob center freq find index of closest value from the nominal values list
      min_tob <- which.min(abs(tobs$Base_10_Calculated_Frequency - freqs_data[1]))
      # Extract the nominal tob values from the tobs dataframe
      freqs_nom <- tobs$Nominal_Frequency[min_tob:(min_tob - 1 + length(freqs_data))]
      names(tob_data) <- as.character(freqs_nom)  # Assign the nominal tob values as dataframe column names
      
      # Read the tob data time stamps from the HDF5 file
      DateTime_data <- h5read(file_path, "/Data/DateTime")
      # Convert the time stamps to POSIXct format and add as dataframe column vector
      tob_data$DateTime <- as.POSIXct(DateTime_data, tz = 'UTC')
      tob_data
    }
  }
  
  # Vectorized reading of the HDF5 SPL data files and read the data into a dataframe
  tob_data_l <- do.call(rbind, lapply(loc_files, read_sound_file))

  tob_data_l <- subset(tob_data_l, DateTime >= beg_per)  # Subset data to selected time period
  tob_data_l <- subset(tob_data_l, DateTime <= end_per)  # Subset data to selected time period
  # Move DateTime to first position
  name_tob <- names(tob_data_l)
  tob_data_l <- tob_data_l[c(name_tob[length(name_tob)], name_tob[-length(name_tob)])]
  # Change the variable names to be to more technically correct
  names(tob_data_l) <- c(names(tob_data_l)[1], paste0('tob_', names(tob_data_l)[-1]))
  detach("package:lubridate", unload = TRUE)  # Detach the lubridate package
  detach("package:rhdf5", unload = TRUE)  # Detach the rhdf5 package
  options(warn = 0)  # Turn warnings on
  tob_data_l
}


#' Convert the 1/3 oct band SPLs to spectral density levels
#'
#' The tob_to_sp_den function reads a dataframe of 1/3 oct. band SPL values and 
#' converts the to spectral density levels
#' 
#' @param tob_df Dataframe where 1 column lists timestamps and next 28 columns 
#' the one third octave band values from 25 Hz to 12500 Hz.
#' 
#' @return Dataframe with spectral density levels instead 1/3 oct SPLs
#' 
tob_to_sp_den <- function(tob_df) {
  # If input dataframes column names not 'tob_25' and 'tob_12500' for selected 
  # columns don't run the conversion
  if (names(tob_df)[2] == 'tob_25' & names(tob_df)[29] == 'tob_12500') {
    f_centre <- 10^(0.1*14:41)  # Calculate band's center frequencies
    f_upper <- f_centre*10^0.05  # Calculate band's upper frequencies
    f_lower <- f_centre/10^0.05  # Calculate band's lower frequencies
    psd_cor <- 10*log10(f_upper - f_lower)  # Conversion correction
    # Loop through the columns and calculate spectral density from 1/3 oct SPLs
    for (i in 1:length(psd_cor)) {
      tob_df[ ,i + 1] <- tob_df[ ,i + 1] - psd_cor[i]
    }
    # Change the variable names to reflect the change in the values
    names(tob_df) <- gsub('tob_', 'sdl_', names(tob_df))
    tob_df
  } else {
    stop("The 1/3 oct. band conversion works only case where lowest nominal band is 25 Hz")
  }
}

# tob_data <- read_sound('B20', '2014-01-01', '2014-01-16')
# spd_data <- tob_to_sp_den(tob_data)
