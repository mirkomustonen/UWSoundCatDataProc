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
        
        # DateTime not needed for correlation
        tob_data <- subset(tob_data, select = -DateTime)
        
        # For taking less space on plot shorten the names of longest variable names
        names(tob_data)[names(tob_data) == 'Distance'] <- 'Dist km'
        names(tob_data)[names(tob_data) == 'WS_ms'] <- 'Ws m/s'
        
        # For faster correlation calculation subset complete cases only
        tob_data_comp <- tob_data[!is.na(tob_data$`Dist km`), ]
        
        # Calculate correlation coefficients between complete cases subset
        data_cor = cor(tob_data_comp, method = c("spearman"))
        
        # Calculate correlation without the `Dist km` where all missing values are
        data_cor_a = cor(subset(tob_data, select = -`Dist km`), 
                       method = c("spearman"))
        
        # Replace the correlations from complete cases correlations
        data_cor[1:29, 1:29] <- data_cor_a
        
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
        # For separatind the sound variables from wind and ships add line segments 
        segments(-3.0, 2.5, x1 = 30.5, y1 = 2.5, col = "black", lwd = 2)
        segments(28.5, 0.5, x1 = 28.5, y1 = 33.5, col = "black", lwd = 2)
}


# source('integrate_sound_wind_ais.R')  # Read the read_sound function from read_sound.R file
# 
# loc <- 'B20'
# beg_per <- '2014-01-01'
# end_per <- '2014-01-05'
# sound_wind_ais <- integrate_sound_wind_ais(loc, beg_per, end_per)
# pl_title <- paste(loc,
#                   'Correlation plot',
#                   gsub('-', '/', beg_per), '-',
#                   gsub('-', '/', substr(end_per, 6, 10)))
# 
# plot_corr(sound_wind_ais, pl_title)