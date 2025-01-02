library(tuneR)

fn <- 'a440hz_2.wav'
fn <- '/home/lukas/Documents/sync/audioproc/a440hz_2.wav'

data <- tuneR::readWave(fn)

# mono-average + normalize
ts <- tuneR::mono(data)@left / (2^data@bit/2)

# make dividable by 4
ts <- ts[1:(floor(length(ts) / 4) * 4)]


# reduce sampling rate by one
dim(ts) <- c(4, length(ts)/4)

# reduce resolution
ts_piano_sample <- colMeans(ts)

usethis::use_data(ts_piano_sample, overwrite = TRUE)
