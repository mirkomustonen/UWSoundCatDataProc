#' Make a correlation plot from the input dataframe
#'
#' The corr_plot function makes a correlation plot from the inputted
#' dataframe and saves it into specified folder with specified file name.
#' 
#' @param tob_data Dataframe listing data for which correlation is calculated
#' @param pl_title Title of the plot
#' 
plot_corr <- function(tob_data, pl_title) {
        library(corrplot)  # Library for making correlation plots
        
        # For taking less space on plot shorten the names of longest variable names
        names(tob_data)[names(tob_data) == 'Distance'] <- 'Dist km'
        names(tob_data)[names(tob_data) == 'WS_ms'] <- 'Ws m/s'
        
        # Calculate correlation coefficients between variables
        data_cor = cor(subset(tob_data, select = -DateTime), 
                       method = c("spearman"), use = 'pairwise.complete.obs')
        
        if (min(data_cor, na.rm = TRUE) < 0) {
                min_col <- -1
        } else {
                min_col <- 0
        }
        # Make and save the corralation plot
        corrplot(data_cor,  
                 is.corr = FALSE, 
                 cl.lim = c(min_col,1),
                 mar = c(0,0,1,0),
                 title = pl_title)
        # abline(h = 2.5, v = 2.5, col = "black", lwd = 2)
        segments(-3.0, 2.5, x1 = 30.5, y1 = 2.5, col = "black", lwd = 2)
        segments(28.5, 0.5, x1 = 28.5, y1 = 33.5, col = "black", lwd = 2)
}


source('integrate_sound_wind_ais.R')  # Read the read_sound function from read_sound.R file

# loc <- 'B20'
# beg_per <- '2014-01-01'
# end_per <- '2014-01-05'
# sound_wind_ais <- integrate_sound_wind_ais(loc, beg_per, end_per)
# pl_title <- paste(loc_name,
#                   'Correlation plot',
#                   gsub('-', '/', beg_per), '-',
#                   gsub('-', '/', substr(end_per, 6, 10)))
# 
# plot_corr(sound_wind_ais, pl_title)