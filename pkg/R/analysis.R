# sample analysis of eye movement data using emov in R
# by Simon Schwab

setwd("~/dev/emov/pkg/R")
source("emov.R")

# read raw data file
data = emov.read_iviewsamples(
  "/home/simon/Data/nscenes/natural_scenes_samples.txt", 46)

# handle missing data: Iview has 0 for missing data
data$L.Raw.X..px.[data$L.Raw.X..px. == 0] = NA
data$L.Raw.Y..px.[data$L.Raw.Y..px. == 0] = NA
data$L.POR.X..mm.[data$L.POR.X..mm. == 0] = NA
data$L.POR.Y..mm.[data$L.POR.Y..mm. == 0] = NA
data$L.GVEC.X[data$L.GVEC.X == 0] = NA
data$L.GVEC.Y[data$L.GVEC.Y == 0] = NA
data$L.GVEC.Z[data$L.GVEC.Z == 0] = NA

# select channels to use
data = data.frame(x = data$L.POR.X..mm, y = -data$L.POR.Y..mm)

# filter data
data = emov.filter(data$x, data$y, 10500/200)

# cart2sphere
#data = emov.cart2sphere(data$L.GVEC.X, data$L.GVEC.Y, data$L.GVEC.Z)
# rad2deg and 
#library("circular")
#data = data.frame(x=deg(data$az), y=-deg(data$elev))

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
disp  = 2.8*10 # in cm, 2.8 cm (2 deg)
fix = list()
for (i in 1:n) {
  fix[[i]] = emov.idt(data$x[idx$start[i]:idx$end[i]],
                      data$y[idx$start[i]:idx$end[i]],
                      disp, 80/1000*200)
}

# Plot all trials, raw data and fixations
#my_xlim = c(-35, 25)
#my_ylim = c(-20, 15)
my_xlim = c(0, 770)
my_ylim = c(-640, -250)

par(mfcol=c(4,3))
for (i in 1:n) {  
  plot(data$x[idx$start[i]:idx$end[i]],
       data$y[idx$start[i]:idx$end[i]],
       type="l", xlim=my_xlim,  ylim=my_ylim,
       xlab="Horizontal (px)", ylab="Vertical (px)")
  par(new=TRUE)
  plot(fix[[i]]$x, fix[[i]]$y, xlim=my_xlim, ylim=my_ylim, xlab=NA, ylab=NA)
}

# Plot single trial
par(mfcol=c(1,1))
nr = 1
plot(data$x[idx$start[nr]:idx$end[nr]],
     data$y[idx$start[nr]:idx$end[nr]],
     type="l", xlim=my_xlim,  ylim=my_ylim,
     xlab="Horizontal (px)", ylab="Vertical (px)")
par(new=TRUE)
plot(fix[[nr]]$x, fix[[nr]]$y, xlim=my_xlim,  ylim=my_ylim, 
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
