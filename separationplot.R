separation.plot <- function(predicted.values, actual.values, line=FALSE) {
  oldpar <- par(no.readonly=TRUE)
  
  # Build separation plot data frame
  separation.frame <- data.frame(predicted=predicted.values, actual=actual.values)
  separation.frame <- separation.frame[order(separation.frame$predicted),]
  separation.frame$position <- 1:nrow(separation.frame)
  
  # Add colors to rows
  separation.frame$color <- NA
  separation.frame$color[separation.frame$actual == 1] <- "#7F3212"
  separation.frame$color[separation.frame$actual == 0] <- "#FFCFB7"
  
  events <- separation.frame[separation.frame$actual == 1, ]
  nonevents <- separation.frame[separation.frame$actual == 0, ]
  
  # Start the plot
  par(mgp = c(0, 0, 0), lend = 2, mar = c(0, 0, 0, 0))
  plot(1:nrow(separation.frame), 1:nrow(separation.frame), xlim=c(0.5, nrow(separation.frame) + 0.5), ylim=c(0, 1), type="n", bty="n", yaxt="n", xaxt="n", xlab="", ylab="")
  
  # Rectangles
  rect(xleft = separation.frame$position - 0.5, ybottom = 0, 
       xright = separation.frame$position + 0.5, ytop = 1, 
       col = separation.frame$color, border=NA)
  
  # Predicted line
  if(line) {
    lines(1:nrow(separation.frame), separation.frame$predicted, lwd=.5)
  }
  
  # Restore par settings
  par(oldpar)
}