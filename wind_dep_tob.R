# This script tries to estimate the wind dependence curves for that it: 
# 1. finds 0.05 quantile values for given 1/3 oct band at wind speed bins
# 2. fits a logarithmic wind dependenc curve thtou the 0.05 quantile values
# 3. plots the found wind dep curves on top of the 2D density plot of the data

# TODO Write documentation to this scrip
# TODO Currently the 0.05 quantile is calculated, try calculating more quantiles 
# for estimating the standard deviation and possible mean value for the natural
# sound levels for given wind speed and 1/3 oct band. For normal distribution 
# 1/3.1514872 corrsponds to 1σ, 1/20 to 1.959964σ, 1/21.977895 2σ and so on.

loc_n <- 'B23'

beg_ts <- seq(as.Date('2014-01-01'), as.Date('2014-12-20'), by = '10 day')
end_ts <- seq(as.Date('2014-01-20'), as.Date('2014-12-29'), by = '10 day')

beg_time = beg_ts[1]
end_time = end_ts[1]

# Read/integrate the wind and sound data
source('integrate_wind_sound.R')
tob_data_l <- integrate_wind_sound(loc_n, beg_time, end_time)
# Convert the third octave bands SPL to spectral density levels
source('read_sound.R')
sdl_data_l <- tob_to_sp_den(tob_data_l)

##===== function that divides data according to wind speeds and finds given quantile values ====
calc_q_wind_spl <- function(band_num, tob_wind_df, quantile_val = 0.05) {
        wind_bin_num <- round((max(tob_wind_df$WS_ms) - min(tob_wind_df$WS_ms))*0.7)
        wind_bins <- seq(min(tob_wind_df$WS_ms), max(tob_wind_df$WS_ms), length.out = wind_bin_num)

        calc_stat <- function(w_ind, calc_df) {
                w_bin_log <- (tob_wind_df$WS_ms >= wind_bins[w_ind]) & (tob_wind_df$WS_ms < wind_bins[w_ind + 1])
                unname(quantile(calc_df[w_bin_log], quantile_val))
        }

        wind_quants <- sapply(1:(length(wind_bins) - 1), FUN = calc_stat, calc_df = tob_wind_df$WS_ms)
        spl_quantls <- sapply(1:(length(wind_bins) - 1), FUN = calc_stat, calc_df = tob_wind_df[ ,band_num])

        data.frame('wind_quants' = wind_quants, 'spl_quantls' = spl_quantls)
}

band_nums <- 1:(ncol(sdl_data_l) - 2)

q_list <- lapply(band_nums, FUN = calc_q_wind_spl, tob_wind_df = sdl_data_l[ , -1])

w_den <- density(sdl_data_l$WS_ms)

##==== Function for fitting ====
fit_lower_q <- function(inp_ind) {
        ws_spl_qdf <- q_list[[inp_ind]]
        band_num <- inp_ind + 1
        winds_q <- ws_spl_qdf[['wind_quants']]

        spl0_trl <- unname(quantile(sdl_data_l[ ,band_num], 0.005))
        tri_par <- list('spl_0' = spl0_trl, 'u_crit' = 5.1, 'k' = 4.4)
        low_par <- list('spl0_low' = spl0_trl - 4, 'uc_low' = 1.0, 'k_low' = 1.2)
        hig_par <- list('spl0_hgh' = spl0_trl + 1, 'uc_hgh' = 10.0, 'k_hgh' = 8.0)

        fit_trial <- nls(ws_spl_qdf[['spl_quantls']] ~ spl_0 + 10*log10(1+(winds_q/u_crit)^k),
                         start = tri_par, lower = low_par, upper = hig_par,
                         algorithm = 'port',
                         control = list(maxiter = 500000, minFactor = 1/20000, warnOnly = T))
        # if (any(summary(fit_trial)$coefficients[, 4] >= 0.01)) {
        #         fit_trial <- lm(ws_spl_qdf[['spl_quantls']] ~ winds_q, weights = wind_weights)
        #         # if (any(summary(fit_trial)$coefficients[ ,4] >= 0.01)) {
        #         #         fit_trial <- lm(ws_spl_qdf[['spl_quantls']]  ~ 1)
        #         # }
        # }
        fit_trial
}

trial_list <- lapply(1:28, FUN = fit_lower_q)

##==========Wenz plot====================

# winds <- seq(floor(min(sdl_data_l$WS_ms)), ceiling(max(sdl_data_l$WS_ms)), by = 1)
winds <- seq(0, 14, by = 2)

pred_i_wind <- function(tri_ind, i_wind) {
        predict(trial_list[[tri_ind]], newdata = data.frame('winds_q' = i_wind))
}

spl_wind <- sapply(1:28, FUN = pred_i_wind, i_wind = winds[1])


png(file.path('Figures', 'Wenz_try_02', paste0(loc_n, beg_time, '_', end_time, '.png')))

# plot(10^(0.1*14:41), spl_wind,
#      main = paste(loc_n, beg_time, '-', end_time),
#      type = 'n',
#      ylim = c(40, 85),
#      log = 'x', xlab = 'Frequency [Hz]',
#      ylab = expression(paste("SDL"["1/3 "], " L"["p,f"]^"1/3 ", " [dB ref 1 ", mu, 'Pa'^2, 'Hz'^{-1},"]")))

source('plot_spec_den.R')
plot_spec_den(subset(sdl_data_l, select = -WS_ms), paste(loc_n, beg_time, '-', end_time))


plot_w_line <- function(w_ind) {
        lines(10^(0.1*14:41)/10^0.05, sapply(1:28, FUN = pred_i_wind, i_wind = w_ind))
}

sapply(winds, plot_w_line)
text(x = 50, y = 80, labels = paste0(min(winds), '-', max(winds), ' m/s'))

dev.off()

#=========This code plot the wind dependenc and predicted time series===========

# save_dep <- function(band_n) {
#         trial_sum <- summary(trial_list[[band_n - 1]])
#         trial_pred <- predict(trial_list[[band_n - 1]], newdata = data.frame('winds_q' = sdl_data_l$WS_ms))
# 
#         # Plotting
#         library(ggplot2)
#         g1 <- ggplot() + geom_point(aes(x = sdl_data_l$WS_ms, y = sdl_data_l[ , band_n]), alpha = 0.2) +
#                 ylab(paste('SPL TOB', names(sdl_data_l)[band_n], '[dB ref 1 uPa]')) +
#                 xlab('Wind speed [m/s]') +
#                 geom_line(aes(x = sdl_data_l$WS_ms, y = trial_pred), color = 'blue') +
#                 ggtitle(paste(round(trial_sum$coefficients[ ,1],1)[1],
#                               round(trial_sum$coefficients[ ,1],1)[2],
#                               round(trial_sum$coefficients[ ,1],1)[3]))
# 
# 
#         g2 <- ggplot() + geom_line(aes(x = sdl_data_l$DateTime, y = sdl_data_l[ , band_n])) +
#                 geom_line(aes(x = sdl_data_l$DateTime, y = trial_pred), color = 'blue') +
#                 ylab(paste('SPL TOB', names(sdl_data_l)[band_n], '[dB ref 1 uPa]')) +
#                 xlab('Time [month day]')
# 
#         library(gridExtra)
# 
#         g3 <- grid.arrange(g1,g2)
# 
#         f_name <- paste0(loc_n, 'TOB_', names(sdl_data_l)[band_n], '.png')
#         ggsave(file.path('Figures', 'Wenz_try_02', f_name), plot = g3)
# }
# 
# # lapply(2:29, FUN = save_dep)
