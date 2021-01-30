#' Make a correlation plot from the input dataframe
#'
#' The corr_plot function makes a correlation plot from the inputted
#' dataframe and saves it into specified folder with specified file name.
#' 
#' @param tob_data Dataframe listing data for which correlation is calculated
#' @param pl_title Plot title
#' @param out_folder Name of the folder where the plot will be saved
#' @param out_file_name File name of the saved file
#' 
plot_corr_save <- function(tob_data, pl_title, out_folder, out_file_name) {
        source('plot_corr.R')
        
        # Make and save the corralation plot
        png(file = file.path(out_folder, out_file_name), width = 600, height = 600, res = 100)
        plot_corr(tob_data, pl_title)
        dev.off()
}

# source('integrate_sound_wind_ais.R')  # Read the read_sound function from read_sound.R file
# 
# loc <- 'B20'
# beg_per <- '2014-07-01'
# end_per <- '2014-08-01'
# sound_wind_ais <- integrate_sound_wind_ais(loc, beg_per, end_per)
# pl_title_i <- paste(loc,
#                     'Correlation plot',
#                     gsub('-', '/', beg_per), '-',
#                     gsub('-', '/', substr(end_per, 6, 10)))
# out_folder_i <- file.path('Figures', 'Sound_wind_AIS_corr')
# out_file_nam_i <- paste(loc, '_Corr_plot_', beg_per, '_',end_per, '.png')
# plot_corr_save(sound_wind_ais, pl_title_i, out_folder_i, out_file_nam_i)
