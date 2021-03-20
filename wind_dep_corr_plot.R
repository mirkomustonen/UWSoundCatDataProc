# A script that makes spectral density plots when subsetting for different wind speeds

source('integrate_wind_sound.R')
source('plot_spec_den.R')

locations <- c('B20', 'B21', 'B22', 'B23')  # List of sound data location names

beg_time = '2014-01-01'
end_time = '2014-02-01'

for (loc_n in locations) {
        tob_data_l <- integrate_wind_sound(loc_n, beg_time, end_time)
        wind_speeds <- seq(floor(min(tob_data_l$WS_ms)),floor(max(tob_data_l$WS_ms)), by = 2)

        for (i_ws in wind_speeds) {
                tob_data_l_s <- subset(tob_data_l, WS_ms >= i_ws & WS_ms < i_ws + 1)
                tob_data_l_s <- subset(tob_data_l_s, select = -WS_ms)
                p_title_i <- paste(loc_n, '2D Density of SDLs',
                                   ' Ws ', i_ws, '-', i_ws + 1, 'm/s',
                                   gsub('-', '/', beg_time), '-',
                                   gsub('-', '/', substr(end_time, 6, 10)))
                out_folder_i <- file.path('Figures', 'Spectra_2D_density_WS_sorted')
                out_file_i <- paste0(loc_n, '_2D_density_SDL_',
                                     '_Ws_', i_ws, '_', i_ws + 1, '_ms_',
                                     beg_time, '_',end_time, '.png')
                plot_spec_den_save(tob_data_l_s, p_title_i, out_folder_i, out_file_i)
        }
}
