#' Make a spectral density plot of the input 1/3 oct SPL dataframe
#'
#' The spec_den_plot function makes a spectral density plot from the inputted
#' dataframe and saves it into specified folder with specified file name.
#' 
#' @param tob_data Dataframe listing the 1/3 oct band SPLs to be plotted
#' @param freqs_data Vector listing the center frequencies of 1/3 oct bands
#' @param out_folder Name of the folder where the plot will be saved
#' @param out_file_name File name of the saved file
#' 
spec_den_plot <- function(tob_data, freqs_data, out_folder, out_file_name) {
   library(RColorBrewer)
   library(data.table)
   
   # DateTime is not needed in the plot
   tob_data <- subset(tob_data, select = -DateTime)
   
   # Get the nominal 1/3 oct band frequencies for the dataframe column names
   freqs_nom <- names(tob_data)
   # Change the column names from nominal to actual center frequencies
   names(tob_data) <- as.character(freqs_data)
   
   #  For plotting 1/3 oct values on x axis and SPL in y axis put all 'SPL' in one
   # column and list the 1/3 oct names in second column
   tob_melt <- melt(tob_data, value.name = 'SPL')
   # For plotting the 1/3 oct names are converte to numeric
   tob_melt$variable <- as.numeric(as.character(tob_melt$variable))
   
   # Create 1/3 oct band wide x-axis bins
   tob_freq_upper <- freqs_data*10^0.05  # Lower edges of bins
   tob_freq_lower <- freqs_data/10^0.05  # Higher edges of bins
   x_bin <- c(tob_freq_lower, tob_freq_upper[length(tob_freq_upper)])
   
   # Create 1 dB wide y-axis bins
   SPL_min <- 55  # Lowest SPL bin value
   SPL_max <- 155  # Highest SPL bin value
   SPL_bin <- (SPL_max - SPL_min) # No of SPL bins
   y_bin <- seq(SPL_min, SPL_max, length = SPL_bin)
   
   # Count the number of points within each bin
   no_of_points <-  as.data.frame(table(findInterval(tob_melt$variable, x_bin), findInterval(tob_melt$SPL, y_bin)))
   # Create matrix listing counts of points in each bin
   no_of_points2D <- diag(ncol = SPL_bin, nrow =  length(x_bin))*0  # Create empty matrix
   no_of_points2D[cbind(no_of_points[,1], no_of_points[,2])] <- no_of_points[,3]
   no_of_points2D[no_of_points2D == 0] <- NA  # For plotting it is better to replace 0-s with NA
   
   # =============Plotting================
   # Create color palette
   col_pal <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
   col_list <- col_pal(42)  # For plotting select 42 colors from the color palette
   # Create title for plot
   pl_title <- substr(gsub("_", " ", out_file_name), 1, 38)
   # Save plot as .png 
   png(file = file.path(out_folder, out_file_name), width = 1000, height = 600, res = 100)
   image(x_bin, y_bin, no_of_points2D, col = col_list, 
         xlab = "Frequency [Hz]", 
         ylab = "SPL [dB re 1 uPa]", 
         main = pl_title,
         xlim = c(min(tob_freq_lower), max(tob_freq_lower)),
         xaxt = "n",
         log = "x")
   axis(1, at = tob_freq_lower, labels = freqs_nom)
   dev.off()
   image(x_bin, y_bin, no_of_points2D, col = col_list, xlab = "Frequency [Hz]", 
         ylab = "SPL [dB re 1 uPa]", main = pl_title, 
         xlim = c(min(tob_freq_lower), max(tob_freq_lower)), xaxt = "n", log = "x")
   axis(1, at = tob_freq_lower, labels = freqs_nom)
}

# locations <- c('B20', 'B21', 'B22', 'B23')
# source('read_sound.R')
# 
# for (loc in locations) {
#    beg_per <- '2014-07-01'
#    end_per <- '2014-08-02'
#    out_list <- read_sound(loc, beg_per, end_per)
#    tob_data_i <- out_list[[1]]
#    freqs_data_i <- out_list[[2]]
#    remove(out_list)
#    out_folder_i <- file.path('Figures', 'Spectra_2D_density_Period')
#    out_file_nam_i <- paste(loc, '_spec_den_', beg_per, '_',end_per, '.png')
#    spec_den_plot(tob_data_i, freqs_data_i, out_folder_i, out_file_nam_i)
# }