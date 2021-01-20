#' Make a correlation plot from the input dataframe
#'
#' The corr_plot function makes a correlation plot from the inputted
#' dataframe and saves it into specified folder with specified file name.
#' 
#' @param tob_data Dataframe listing data for which correlation is calculated
#' @param out_folder Name of the folder where the plot will be saved
#' @param out_file_name File name of the saved file
#' 
corr_plot <- function(tob_data, out_folder, out_file_name) {
        library(corrplot)  # Library for making correlation plots
        
        # Calculate correlation coefficients between variables
        data_cor = cor(subset(tob_data, select = -DateTime), method = c("spearman"))
        
        # Make and save the corralation plot
        pl_title <- substr(gsub("_", " ", out_file_name), 1, 40)
        # Save plot as .png 
        png(file = file.path(out_folder, out_file_name), width = 600, height = 600, res = 100)
        corrplot(data_cor, is.corr = FALSE, cl.lim = c(0,1))
        title(pl_title)
        dev.off()
        corrplot(data_cor, is.corr = FALSE, cl.lim = c(0,1))
        title(pl_title)
}

# source('integrate_wind_sound.R')  # Read the read_sound function from read_sound.R file
# locations <- c(1, 2, 3, 4)  # List of location names to loop through
# s_locations <- c('B20', 'B21', 'B22', 'B23')  # List of sound data location names
# beg_per <- '2014-01-01'
# end_per <- '2014-02-02'
# 
# for (loc in locations) {
#         # Read the sound data of a specific time period
#         tob_data_i <- integrate_wind_sound(loc, '2014-07-01', '2014-08-02')
#         out_folder_i <- file.path('Figures', 'TOB_wind_correlation')
#         out_file_nam_i <- paste(s_locations[loc], '_Corr_plot_', beg_per, '_',end_per, '.png')
#         corr_plot(tob_data_i, out_folder_i, out_file_nam_i)
# }