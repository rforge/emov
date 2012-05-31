# sample analysis of eye movement data using emov in R
# by Simon Schwab

setwd("~/dev/emov/pkg/R")
source("emov.R")

data = emov.read_iviewsamples(
  "/home/simon/Data/nscenes/natural_scenes_samples.txt", 46)

# select the data
# raw data
data = data.frame(x = -data$L.Raw.X..px, y = -data$L.Raw.Y..px)

# trial segmentation
n = 12 # number of trials
idx = c() # index
start = 1
for (i in 1:n) {
  idx = c(idx, start, start - 1 + 2000)
  start = start + 2000
}
idx <- matrix(idx, nrow=n, ncol=2, byrow=TRUE)
idx <- data.frame(start=idx[,1], end=idx[,2]) # easy to access

# fixation detecton for each trial
fix = list()
for (i in 1:n) {
  fix[[i]] = emov.idt(data$x[idx$start[i]:idx$end[i]],
                      data$y[idx$start[i]:idx$end[i]],
                      16, 80/1000*200)
}

# Plot all trials, raw data and fixations
par(mfcol=c(4,3))
for (i in 1:n) {  
  plot(data$x[idx$start[i]:idx$end[i]],
       data$y[idx$start[i]:idx$end[i]],
       type="l", xlim=c(-220,-125),  ylim=c(-200,-130),
       xlab="Horizontal (px)", ylab="Vertical (px)")
  par(new=TRUE)
  plot(fix[[i]]$x, fix[[i]]$y, xlim=c(-220,-125),  ylim=c(-200,-130), 
       xlab=NA, ylab=NA)
}
# Plot single trial
par(mfcol=c(1,1))
nr = 1
plot(data$x[idx$start[nr]:idx$end[nr]],
     data$y[idx$start[nr]:idx$end[nr]],
     type="l", xlim=c(-220,-125),  ylim=c(-200,-130),
     xlab="Horizontal (px)", ylab="Vertical (px)")
par(new=TRUE)
plot(fix[[nr]]$x, fix[[nr]]$y, xlim=c(-220,-125),  ylim=c(-200,-130), 
     xlab=NA, ylab=NA)

# Plot stimuli
# install.packages("jpeg")
library(jpeg)

img = list()
img[[1]] <- readJPEG("/home/simon/Data/nscenes/stimuli/000.jpg")
img[[2]] <- readJPEG("/home/simon/Data/nscenes/stimuli/001.jpg")
img[[3]] <- readJPEG("/home/simon/Data/nscenes/stimuli/002.jpg")
img[[4]] <- readJPEG("/home/simon/Data/nscenes/stimuli/003.jpg")
img[[5]] <- readJPEG("/home/simon/Data/nscenes/stimuli/004.jpg")
img[[6]] <- readJPEG("/home/simon/Data/nscenes/stimuli/005.jpg")
img[[7]] <- readJPEG("/home/simon/Data/nscenes/stimuli/006.jpg")
img[[8]] <- readJPEG("/home/simon/Data/nscenes/stimuli/007.jpg")
img[[9]] <- readJPEG("/home/simon/Data/nscenes/stimuli/008.jpg")
img[[10]] <- readJPEG("/home/simon/Data/nscenes/stimuli/009.jpg")
img[[11]] <- readJPEG("/home/simon/Data/nscenes/stimuli/010.jpg")
img[[12]] <- readJPEG("/home/simon/Data/nscenes/stimuli/011.jpg")

par(mfcol=c(4,3))

for (i in 1:n) {
  plot(c(0,1), c(0,1))
  rasterImage(img[[i]], 0, 0, 1, 1)
}
