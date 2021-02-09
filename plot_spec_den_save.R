#' Make a spectral density plot of the input 1/3 oct SPL dataframe
#'
#' The spec_den_plot function makes a spectral density plot from the inputted
#' dataframe and saves it into specified folder with specified file name.
#' 
#' @param tob_data Dataframe listing the 1/3 oct band SPLs to be plotted
#' @param freqs_data Vector listing the center frequencies of 1/3 oct bands
#' @param pl_title Title of the plot
#' @param out_folder Name of the folder where the plot will be saved
#' @param out_file_name File name of the saved file
#' 
plot_spec_den_save <- function(tob_data, freqs_data, pl_title, out_folder, out_file_name) {
        source('plot_spec_den.R')
        
        # Save plot as .png
        png(file = file.path(out_folder, out_file_name), width = 1000, height = 600, res = 100)
        plot_spec_den(tob_data, freqs_data, pl_title)
        dev.off()
}

# locations <- c('B20', 'B21', 'B22', 'B23')
# source('read_sound.R')
# loc <- locations[1]
# 
# beg_per <- '2014-07-01'
# end_per <- '2014-08-01'
# out_list <- read_sound(loc, beg_per, end_per)
# tob_data_i <- out_list[[1]]
# freqs_data_i <- out_list[[2]]
# remove(out_list)
# 
# pl_title <- paste(loc, '2D Density of SDLs',
#                   gsub('-', '/', beg_per), '-',
#                   gsub('-', '/', substr(end_per, 6, 10)))
# 
# out_folder_i <- file.path('Figures', 'Spectra_2D_density_Period')
# out_file_nam_i <- paste(loc, '2D_density_SDL_', beg_per, '_',end_per, '.png')
# plot_spec_den_save(tob_data_i, freqs_data_i, pl_title, out_folder_i, out_file_nam_i)