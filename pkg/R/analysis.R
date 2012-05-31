# sample analysis of eye movement data using emov in R
# by Simon Schwab

setwd("~/workspace/emov/pkg/R")
source("emov.R")

data = emov.read_iviewsamples(
  "/home/simon/Data/nscenes/natural_scenes_samples.txt", 46)

par(mfcol=c(4,3))

n = 12 # number of trials
start = 1
for (i in 1:n) {
  
  plot(-data$L.Raw.X..px.[start:(start - 1 + 2000)],
       -data$L.Raw.Y..px.[start:(start - 1 + 2000)],
       type="l", xlim=c(-220,-125),  ylim=c(-200,-130),
       xlab="Horizontal (px)", ylab="Vertical (px)")
  start = start + 2000
}

install.packages("jpeg")
library(jpeg)

# img <- readJPEG(system.file("img", "Rlogo.jpg", package="jpeg"))

img <- readJPEG("")
