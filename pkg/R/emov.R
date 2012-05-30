# emov
# Copyright (C) 2010-2012 Simon Schwab,
# Department of Psychitric Neurophysiology, University of Bern.
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# I-DT algorithm, adapted from: Salvucci, D. D., & Goldberg, J. H. (2000).
# Identifying fixations and saccades in eye-tracking protocols. In Proceedings
# of the 2000 symposium on eye tracking research & applications (pp. 71-78).
# New York: ACM.
emov.idt <- function(x, y, dispersion, duration) {

    # init variables
    fix_start <- c()
    fix_end <- c()
    fix_x <- c()
    fix_y <- c()

    start <- 1 # window start position

    while (start <= length(x) - duration) {

        end <- start + duration - 1 # window end position
        # create window        
	x_win <- x[start:end]
        y_win <- y[start:end]
        # dispersion

        D <- (max(x_win, na.rm=T) - min(x_win, na.rm=T)) + (max(y_win, na.rm=T) - min(y_win, na.rm=T))

        j <- 1  # window expander

        while (D <= dispersion & end + j <= length(x)) {
            # expand window by 1 using j
            x_win <- x[start:(end + j)]
            y_win <- y[start:(end + j)]
            D <- (max(x_win, na.rm=T) - min(x_win ,na.rm=T)) + (max(y_win ,na.rm=T) - min(y_win, na.rm=T))

            if (D > dispersion) {
                # select window as fixation
                fix_start <- c(fix_start, start)
                fix_end <- c(fix_end, end + j - 1)
                fix_x <- c(fix_x, mean(x_win,na.rm=T))
                fix_y <- c(fix_y, mean(y_win,na.rm=T))
                start <- end + j - 1;  # skip window points
                break
            } else if (end + j == length(x)) {
                # handle last window if data ends during a fixation
                fix_start <- c(fix_start, start)
                fix_end <- c(fix_end, end)
                fix_x <- c(fix_x, mean(x_win,na.rm=T))
                fix_y <- c(fix_y, mean(y_win,na.rm=T))
                break
            }            
            j <- j + 1
        }

    start <- start + 1;

    }

    result <- data.frame(start=fix_start, end=fix_end, x=fix_x, y=fix_y)
    return(result)

}

emov.read_iviewsamples <- function(file, nr_of_headerlines) {
  data = read.table(file, header=TRUE, skip=nr_of_headerlines, sep="\t")
  data$L.Raw.X..px.[data$L.Raw.X..px. == 0] = NA
  data$L.Raw.Y..px.[data$L.Raw.Y..px. == 0] = NA
  data$L.POR.X..mm.[data$L.POR.X..mm. == 0] = NA
  data$L.POR.Y..mm.[data$L.POR.Y..mm. == 0] = NA  
  
  plot(data$L.Raw.X..px., data$L.Raw.Y..px., type="l", xlim=c(120,220),  ylim=c(130,200))
  plot(data$L.POR.X..mm., data$L.POR.Y..mm., type="l", xlim=c(0,800),  ylim=c(200,700))
  
  plot(data$L.Raw.X..px.[1:2000], -data$L.Raw.Y..px.[1:2000], type="l", xlim=c(120,220),  ylim=c(-130,-200))
  plot(data$L.Raw.X..px.[4000:6000], data$L.Raw.Y..px.[4000:6000], type="l", xlim=c(120,220),  ylim=c(130,200))
  plot(data$L.Raw.X..px.[22000:24000], data$L.Raw.Y..px.[22000:24000], type="l", xlim=c(120,220),  ylim=c(130,200))
  
  par(new=T)
  plot(67,99, col="red", xlim=c(0,800),  ylim=c(200,700))
  xlim=c(120,220)
  ylim=c(130,200)
  return(data)
}
