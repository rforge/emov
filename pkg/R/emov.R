# emov
# Copyright (C) 2010-2012 Simon Schwab,
# Department of Psychitric Neurophysiology, University of Bern.
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#' I-DT algorithm.
#' 
#' @param t Vector of timepoints.
#' @param x horizontal eye position.
#' @param y vertical eye position.
#' @param dispersion Maximal dispersion allowed.
#' @param duration Minimal fixation duration allowed.
#' @return Fixations: position, start, end.
#' @export
#' @references
#' Salvucci, D. D., & Goldberg, J. H. (2000).
#' Identifying fixations and saccades in eye-tracking protocols. In Proceedings
#' of the 2000 symposium on eye tracking research & applications (pp. 71-78).
#' New York: ACM.
#' @examples
#' max_disp  = 19.0 # in cm, 28.8 cm (2 deg)
#' min_dur =  80/1000*200 # 80ms / 200Hz
#' fix = emov.idt(data$t, data$x, data$y, max_disp, min_dur)
emov.idt <- function(t, x, y, dispersion, duration) {
  
  # init variables
  fix_start <- c()
  fix_end <- c()
  fix_x <- c()
  fix_y <- c()
  
  start <- 1 # window start position
  
  while (start <= length(x) - duration + 1) { # added + 1
    # while: we move window by 1, if D > threshold
    
    end <- start + duration - 1 # window end position
    # create window        
    x_win <- x[start:end]
    y_win <- y[start:end]
    # dispersion    
    D <- (max(x_win, na.rm=T) - min(x_win, na.rm=T)) + 
      (max(y_win, na.rm=T) - min(y_win, na.rm=T))
    
    j <- 1  # window expander
    
    while (D <= dispersion & end + j <= length(x)) {
      # while: we expand window by 1 using j
      x_win <- x[start:(end + j)]
      y_win <- y[start:(end + j)]
      D <- (max(x_win, na.rm=T) - min(x_win ,na.rm=T))
      + (max(y_win ,na.rm=T) - min(y_win, na.rm=T))
      
      if (D > dispersion) {
        # select window (j - 1) as fixation
        fix_start <- c(fix_start, t[start])
        fix_end <- c(fix_end, t[end + j - 1]) # j - 1 is previous window
        fix_x <- c(fix_x, mean(x_win, na.rm=T))
        fix_y <- c(fix_y, mean(y_win, na.rm=T))
        start <- end + j - 1 # skip window points
        break # something wroing with -1
      } else if (end + j == length(x)) {
        # handle last window if data ends during a fixation
        fix_start <- c(fix_start, t[start])
        fix_end <- c(fix_end, t[end])
        fix_x <- c(fix_x, mean(x_win,na.rm=T))
        fix_y <- c(fix_y, mean(y_win,na.rm=T))
        start <- end + j - 1
        break
      }            
      j <- j + 1
    }
    
    start <- start + 1;
    
  }
  
  result <- data.frame(start=fix_start,
                       end=fix_end,
                       dur=fix_end-fix_start,
                       x=fix_x,
                       y=fix_y)
  return(result)
  
}

#' Read SMI iview sample file.
#' 
#' @param file Filename.
#' @param nr_of_headerlines No. of header lines in datafile.
#' @return data file.
#' @export
#' @examples
#' data = emov.read_iviewsamples("~/Data/nscenes/natural_scenes_samples.txt", 46)
emov.read_iviewsamples <- function(file, nr_of_headerlines) {
  
  return(read.table(file, header=TRUE, skip=nr_of_headerlines, sep="\t"))
  
}

#' Convert Cartesian to Spherical coordinates.
#' 
#' @param x x.
#' @param y y.
#' @param z z.
#' @return Two angles and radius-
#' @export
#' @examples
#' data = emov.cart2sphere(data$L.GVEC.X, data$L.GVEC.Y, data$L.GVEC.Z)
emov.cart2sphere <- function(x, y, z) {
  
  srootssxy = sqrt(abs(x)^2 + abs(y)^2)
  r = sqrt(abs(srootssxy)^2 + abs(z)^2)
  elev = atan2(z, srootssxy);
  az = atan2(y, x);
  
  return(data.frame(az, elev, r))
  
}

#' Velocity threshold filter.
#' 
#' @param x Eye position.
#' @param y Eye position.
#' @param threshold Velocity threshold.
#' @return Filtered data.
#' @export
#' @examples
#' flt = emov.filter(data$x, data$y, 10500/200)
emov.filter <- function(x, y, threshold) {
  
  idx = diff(abs(x)) > threshold | diff(abs(y)) > threshold  
  x[idx] = NA
  y[idx] = NA
  x[idx + 1] = NA
  y[idx + 1] = NA
  print(sprintf("Filtered %.0f percent of data",
                sum(idx, na.rm=TRUE)/length(x)*100))
  return(data.frame(x=x, y=y))
  
}

#' Angular size of stimulus.
#' 
#' @param stimsize Size of the stimulus.
#' @param distance Viewing distance from stimulus.
#' @return Angular size in degrees.
#' @export
emov.angdia <- function(stimsize, distance) {
  
  return(deg(2*atan((0.5*stimsize)/distance)))
  
}