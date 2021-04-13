heatmapmaker <- function(matrix, names1, names2) {
  library(RColorBrewer)
  dimnames(matrix) <- list(as.character(names1), as.character(names2))
  newdf <- as_tibble(matrix, rownames = NA)
  newdf <- cbind(firstletter = as.character(names1), newdf)
  newdf <- pivot_longer(newdf, !firstletter)
  newdf$firstletter <- as.character(newdf$firstletter)
  names(newdf)[1] <- "n"
  names(newdf)[2] <- "t"
  names(newdf)[3] <- "variable"
  column2 <- rep(names2, length(names1))
  newdf$t <- as.character(column2)
  newdf$t <- as.numeric(newdf$t)
  newdf$n <- as.numeric(newdf$n)
  uniquepi <- sort(unique(newdf$variable))
  newdf <- data.frame(cbind(newdf, rep(0, nrow(newdf))))
  colors <- c(rainbow(length(uniquepi)), "black")
  names(newdf)[4] <- "color"
  for (i in 1:nrow(newdf)){
    newdf[i, 4] <- match(newdf[i,3], uniquepi)
  }
  plot(newdf$t, newdf$n, col = colors[length(colors) + 1 - newdf$color], 
       pch = 15, cex = 1.6, ylab = "n", xlab = "t", xaxs = "i", yaxs = "i")
}
