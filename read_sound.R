read_sound <- function(loc, beg_period, end_period) {
  
  library(rhdf5)  # Library for handling HDF5 files
  library(lubridate)  # Library for better datetime handling
  
  beg_period <- as.POSIXct(beg_period, format =  '%Y-%m-%d', tz = 'UTC')  # Convert the input beginning time period to POSIXct time format
  end_period <- as.POSIXct(end_period, format =  '%Y-%m-%d', tz = 'UTC')  # Convert the input end time period to POSIXct time format
  sel_period <- interval(beg_period, end_period, tzone = "UTC")  # Create a time period from the selected beginning and end times
  
  loc_files <- list.files(path = "PAM_Output", pattern = paste0('+', loc, '+'))  # List the HDF5 files to be read in
  
  TOBs <- read.table(file = 'Third_octs.txt', sep = '\t', header = TRUE)  # Read third-octave nominal and numerical values from file 
  
  # Loop through the HDF5 SPL data files and read the data into a dataframe
  k <- 0  # Flag value for marking the first iteration
  for (i_file in loc_files) {
          f_beg_d <- as.POSIXct(substr(i_file, 26, 33), format =  '%Y%m%d', tz = 'UTC')  # Beginning time of the file to be read in
          f_end_d <- as.POSIXct(substr(i_file, 35, 42), format =  '%Y%m%d', tz = 'UTC')  # End time of the file to be read in
          file_period <- interval(f_beg_d, f_end_d, tzone = "UTC") # Create a time period with file beginning and end times
          
          file_per_test <- !is.na(intersect(sel_period, file_period))  # Check whether there is overlap between the selected and file time periods
          if (file_per_test) {
            file_path <- file.path("PAM_Output", i_file)  # Construct the HDF5 file path to be read in
            
            TOB_data <- h5read(file_path, "/Data/LeqMeasurementsOfChannel1")  # Read Third Octave Band data from the file in matrix form
            TOB_data <- as.data.frame(t(TOB_data))  # Transform the TOB data matrix and convert to dataframe
            
            Freqs_data <- h5read(file_path, "Metadata/FrequencyIndex")  # Read the center frequencies of the third octave bands
            min_tob <- which.min(abs(TOBs$Base_10_Calculated_Frequency - Freqs_data[1]))  # For first TOB center freq find index of closest value from the nominal values list
            Freqs_nom <- TOBs$Nominal_Frequency[min_tob:(min_tob - 1 + length(Freqs_data))]  # Extract the nominal TOB values from the TOBs dataframe
            names(TOB_data) <- as.character(Freqs_nom)  # Assign the nominal TOB values as dataframe column names
            
            DateTime_data <- h5read(file_path, "/Data/DateTime")  # Read the TOB data time stamps from the HDF5 file
            TOB_data$DateTime <- as.POSIXct(DateTime_data, tz = 'UTC')  # Convert the time stamps to POSIXct format and add as dataframe column vector
            
            if (k == 0) {
                    TOB_data_l <- TOB_data  # For first iteration create new table the data will be added to
                    k <- 1  # Set the first iteration flag to 1
            }
            else{
                    TOB_data_l <- rbind(TOB_data_l, TOB_data)  # For iterations after first aggregate dataframes into larger dataframe
            }
          }
  }
  TOB_data_l <- subset(TOB_data_l, DateTime > beg_period)  # Subset data to selected time period
  TOB_data_l <- subset(TOB_data_l, DateTime < end_period)  # Subset data to selected time period
  return(list(TOB_data_l, Freqs_data))
}