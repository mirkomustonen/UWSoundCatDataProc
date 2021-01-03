library(rhdf5)

loc <- 'B23'


path <- "PAM_Output"
loc_files <- list.files(path = path, pattern = paste0('+', loc, '+'))


TOBs <- read.table(file = 'Third_octs.txt', sep = '\t', header = TRUE)

k <- 0
for (i_file in loc_files) {
        file_path <- file.path(path, i_file)
        TOB_data_path <- "/Data/LeqMeasurementsOfChannel1"
        DateTime_data_path <- "/Data/DateTime"
        Freqs_data_path <- "Metadata/FrequencyIndex"
        
        TOB_data <- h5read(file_path, TOB_data_path)
        DateTime_data <- h5read(file_path, DateTime_data_path)
        Freqs_data <- h5read(file_path, Freqs_data_path)
        
        DateTime <- as.POSIXct(DateTime_data, tz = 'UTC')
        
        min_tob <- which.min(abs(TOBs$Base_10_Calculated_Frequency - Freqs_data[1]))
        
        Freqs_nom <- TOBs$Nominal_Frequency[min_tob:(min_tob - 1 + length(Freqs_data))]
        
        TOB_data <- as.data.frame(t(TOB_data))
        names(TOB_data) <- as.character(Freqs_data)
        TOB_data$DateTime <- DateTime
        if (k == 0) {
                TOB_data_l <- TOB_data
                k <- 1
        }
        else{
                TOB_data_l <- rbind(TOB_data_l, TOB_data)
        }    

}

## Correlation plot

remove(Freqs_data, TOB_data, DateTime, DateTime_data, DateTime_data_path, 
       file_path, Freqs_data_path, i_file, k, loc_files, path, TOB_data_path,
       TOBs)

TOB_data_l <- subset(TOB_data_l, DateTime < as.POSIXct('2015-01-01', format =  '%Y-%m-%d', tz = 'UTC'))

TOB_data_l <- subset(TOB_data_l, DateTime > as.POSIXct('2014-07-01', format =  '%Y-%m-%d', tz = 'UTC'))

TOB_data_l <- subset(TOB_data_l, DateTime < as.POSIXct('2014-08-02', format =  '%Y-%m-%d', tz = 'UTC'))

# sound.cor = cor(TOB_data_l[  , 1:(ncol(TOB_data_l) - 1)], method = c("spearman"))
# 
# library(corrplot)
# png(file = paste0(loc, "corr_plot2.png"))
# corrplot(sound.cor, is.corr = FALSE, cl.lim = c(0,1))
# title(loc)
# dev.off()


## Sprectra density plot

library(data.table)

TOB_melt <- melt(TOB_data_l[,1:(ncol(TOB_data_l) - 1)], value.name = 'SPL')

# for (third_oct in levels(TOB_melt$variable)) {
#         TOB_melt_TOB <- subset(TOB_melt, variable == third_oct)
# 
#         TOB_hist <- hist(TOB_melt_TOB$SPL, breaks = 100)
#         max_dif <- sort(TOB_hist$counts, decreasing = TRUE)[1] - sort(TOB_hist$counts, decreasing = TRUE)[2]
#         if (max_dif > 500) {
#                 print(third_oct)
#                 self_n_l <- TOB_hist$breaks[which.max(a$counts)]
#                 TOB_melt <- subset(TOB_melt, !(variable == third_oct & SPL <= (self_n_l + 1)))
#         }
# 
# }

TOB_melt$variable <- as.numeric(as.character(TOB_melt$variable))





library(RColorBrewer)
rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
r <- rf(42)

f_c <- as.numeric(names(TOB_data_l)[1:(length(names(TOB_data_l)) - 1)])
f_u <- f_c*10^0.05
f_l <- f_c/10^0.05

x.bin <- c(f_l, f_u[length(f_u)])

SPL_min <- 55
SPL_max <- 155
SPL_bin <- (SPL_max - SPL_min)
y.bin <- seq(SPL_min, SPL_max, length = SPL_bin)

freq <-  as.data.frame(table(findInterval(TOB_melt$variable, x.bin),findInterval(TOB_melt$SPL, y.bin)))
freq[,1] <- as.numeric(freq[,1])
freq[,2] <- as.numeric(freq[,2])
freq2D <- diag(ncol = SPL_bin, nrow =  length(x.bin))*0
freq2D[cbind(freq[,1], freq[,2])] <- freq[,3]

freq2D[freq2D == 0] <- NA 

p_title <- paste(loc, substr(as.character(min(TOB_data_l$DateTime)), 1, 10), ' ', substr(as.character(max(TOB_data_l$DateTime)), 1, 10))
png(file = paste0(p_title, ".png"), width = 1000, height = 600, res = 100)
image(x.bin, y.bin, freq2D, col = r, 
      xlab = "Frequency [Hz]", 
      ylab = "SPL [dB re 1 uPa]", main = p_title,
      xlim = c(min(f_l), max(f_l)),
      xaxt = "n",
      log = "x")
axis(1, at = f_l, labels = as.character(Freqs_nom))

dev.off()
