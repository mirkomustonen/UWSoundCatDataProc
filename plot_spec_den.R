#' Make a spectral density plot of the input 1/3 oct SPL dataframe
#'
#' The spec_den_plot function makes a spectral density plot from the inputted
#' dataframe and saves it into specified folder with specified file name.
#' 
#' @param tob_data Dataframe listing the 1/3 oct band SPLs to be plotted
#' @param freqs_data Vector listing the center frequencies of 1/3 oct bands
#' @param pl_title Title of the plot
#' 
plot_spec_den <- function(tob_data, freqs_data, pl_title) {
   library(RColorBrewer)
   library(data.table)
   
   # DateTime is not needed in the plot
   tob_data <- subset(tob_data, select = -DateTime)
   
   # Get the nominal 1/3 oct band frequencies for the dataframe column names
   freqs_nom <- names(tob_data)
   # Change the column names from nominal to actual center frequencies
   names(tob_data) <- as.character(freqs_data)
   
   tob_freq_upper <- freqs_data*10^0.05  # Lower edges 1/3 oct band
   tob_freq_lower <- freqs_data/10^0.05  # Higher edges 1/3 oct band
   
   # For converting 1/3 oct levels to spectrum density levels of the center frequencies
   # the correction in dBs that equals 10log10(width of freq band)
   PSD_cor <- 10*log10(tob_freq_upper - tob_freq_lower)
   spec_lev_dat <- tob_data
   for (i in 1:ncol(tob_data)) {
      spec_lev_dat[ ,i] <- tob_data[ ,i] - PSD_cor[i]
   }
   
   # spec_lev_dat <- tob_data[ ,] - PSD_cor[1]
   
   #  For plotting 1/3 oct values on x axis and SPL in y axis put all 'SPL' in one
   # column and list the 1/3 oct names in second column
   tob_melt <- melt(spec_lev_dat, value.name = 'SPL')
   # For plotting the 1/3 oct names are converte to numeric
   tob_melt$variable <- as.numeric(as.character(tob_melt$variable))
   
   # Create 1/3 oct band wide x-axis bins
   x_bin <- c(tob_freq_lower, tob_freq_upper[length(tob_freq_upper)])
   
   # Create 1 dB wide y-axis bins
   SPL_min <- floor(min(tob_melt$SPL))  # Lowest SPL bin value
   SPL_max <- ceiling(max(tob_melt$SPL))  # Highest SPL bin value
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
   
   # Plot the figure
   op <- par(mar = c(5, 6, 4, 2) + 0.1)
   image(x_bin, y_bin, no_of_points2D, col = col_list, xlab = "Frequency [Hz]", 
         ylab = expression(paste("SDL"["1/3 "], " L"["p,f"]^"1/3 ", " [dB ref 1 ", mu, 'Pa'^2, 'Hz'^{-1},"]")),
         main = pl_title,
         xlim = c(min(tob_freq_lower), max(tob_freq_lower)),
         ylim = c(40, 140),
         xaxt = "n", log = "x")
   axis(1, at = tob_freq_lower, labels = freqs_nom)
   par(op)
}


source('read_sound.R')

loc <- 'B23'
beg_per <- '2014-07-01'
end_per <- '2014-08-01'
out_list <- read_sound(loc, beg_per, end_per)
tob_data_i <- out_list[[1]]
freqs_data_i <- out_list[[2]]
remove(out_list)

pl_t <- paste(loc, 'Sec_den', beg_per, end_per)
plot_spec_den(tob_data_i, freqs_data_i, pl_t)