setwd('~/Work/code/emov/pkg')
raw <- read.csv(file="data/sample.txt", head=TRUE, sep=",")

raw$x = filter(raw$x, rep(1/3, 3))
raw$y = filter(raw$y, rep(1/3, 3))

# data is in seconds, 200Hz
# dispersion, e.g. 2 cm at a viewing distance of 80 cm is ~1.4 deg
# minimal fixation duration 0.1 s -> 40 samples
fixations = emov.idt(raw$time, raw$x, raw$y, 2, 20)

plot(raw$x, raw$y, xlim=c(-20,30), ylim=c(-12,-6), type="l", 
     xlab="Horizontal position (deg) ", 
     ylab="Vertical position (deg)")
par(new=T)
lines(fixations$x, fixations$y, type="b", col="red")