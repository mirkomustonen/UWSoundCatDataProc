# This script tries to estimate the wind dependence curves for that it: 
# 1. divides data by wind speeds, 
# 2. calculates lower quantile values of the 1/3 oct. SPLs or SDLs.
# 3. plots the found curves on top of the 2D density plot of the data

# TODO When plot_spec_den is changed from base plot to ggplot change to ggplot here also
# TODO Currently the 0.05 quantile is calculated, try calculating more quantiles 
# for estimating the standard deviation and possible mean value for the natural
# sound levels for given wind speed and 1/3 oct band. For normal distribution 
# 1/3.1514872 corrsponds to 1σ, 1/20 to 1.959964σ, 1/21.977895 2σ and so on.

loc_n <- 'B21'
beg_time = '2014-01-01'
end_time = '2014-01-20'

source('integrate_wind_sound.R')
tob_data_l <- integrate_wind_sound(loc_n, beg_time, end_time)

source('read_sound.R')
sdl_data_l <- tob_to_sp_den(tob_data_l)

source('plot_spec_den.R')
plot_spec_den(subset(sdl_data_l, select = -WS_ms), paste(loc_n, beg_time, '-', end_time))

wind_speeds <- seq(floor(min(sdl_data_l$WS_ms)),(ceiling(max(sdl_data_l$WS_ms)) - 1), by = 2)

# out_folder <- file.path('Figures', 'Wenz_try_01')
# out_file_name <- paste0(loc_n, '_', beg_time, '_', end_time, '.png')

# png(file = file.path(out_folder, out_file_name), width = 1000, height = 600, res = 100)

# plot(1, type = "n",
#      log = 'x',
#      main = paste(loc_n, beg_time, '-', end_time),
#      ylim = c(40, 85),
#      xlim = c(20, 15*10^3),
#      ylab = expression(paste("SDL"["1/3 "], " L"["p,f"]^"1/3 ", " [dB ref 1 ", mu, 'Pa'^2, 'Hz'^{-1},"]")),
#      xlab = 'Frequency [Hz]')


plot_wenz_l <- function(tob_data, i_ws) {
        tob_data_s <- subset(tob_data, WS_ms >= i_ws & WS_ms < i_ws + 2)
        if (nrow(tob_data_s) > 200) {
                tob_data_s <- subset(tob_data_s, select = -WS_ms)
                tob_data_s <- subset(tob_data_s, select = -DateTime)
                
                m_freqs <- 10^(0.1*14:41)
                mode_vec <- sapply(tob_data_s, quantile, probs = 0.05)
                
                lines(m_freqs/10^0.05, mode_vec, log = 'x', type = 'l')
                text(x = 2000, y = (mode_vec[[18]]), 
                     labels = paste(i_ws, '-', (i_ws + 2)),
                     col = 'red')
        }
}

sapply(wind_speeds, plot_wenz_l, tob_data = sdl_data_l)

# dev.off()