emov_demo <- function() {

    raw <- read.csv(file="data/sample.txt", head=TRUE, sep=",")
    
    raw$x = filter(raw$x, rep(1/3, 3))
    raw$y = filter(raw$y, rep(1/3, 3))


    fixations = idt(raw$x, raw$y, 1, 20)

    # plot
    # for sublots:
   #par(mfrow = c(2, 2))
    #postscript(file="testplot.eps",
    #    paper="special",
    #    width=3.5,
    #    height=2.8,
    #    horizontal=FALSE)

    #par(mar=c(4, 4, 0.5, 0.5)) # margin: north, west, south, east
    plot(raw$x, raw$y, xlim=c(-20,30), ylim=c(-12,-6), type="l", 
        	xlab="Horizontal position (deg) ", 
                ylab="Vertical position (deg)")
    par(new=T)
    lines(fixations$x, fixations$y, type="b", col="red")

    #dev.off()
    
    return(fixations)

}

