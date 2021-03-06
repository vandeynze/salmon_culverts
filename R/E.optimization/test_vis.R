#######################################
# Test optimal barrier removal problem:
# Graphing the test problem
# Author: Sunny Jardine
# Date: February, 2021
#######################################

library(sf)
library(tidyverse)

#stream lines
str <- list(rbind(c(3, 0), c(3, 1), c(1, 3), c(3, 1), c(4, 2)),
             rbind(c(1, 3), c(0, 4)), 
             rbind(c(4, 2), c(5, 3), c(5, 4), c(5, 3), c(6, 3)),
             rbind(c(5, 4), c(5, 8)),
             rbind(c(6, 3), c(7, 3)), 
             rbind(c(10, 0), c(10, 1), c(9, 2), c(10, 1), c(11, 2)),
             rbind(c(9, 2), c(8, 3)),
             rbind(c(11, 2), c(14, 5), c(13, 4), c(12, 5)),
             rbind(c(15, 0), c(15, 1)),
             rbind(c(15, 1), c(15, 5))) %>% 
  st_multilinestring()
 
#barriers
bar <- rbind(c(3, 0), c(1, 3), c(4, 2), c(5, 4), c(6, 3),
                    c(10, 0), c(9, 2), c(11, 2),
                    c(15, 0), c(15, 1)) %>% 
  st_multipoint()

#expensive barrier
eb <- st_point(c(15, 1))

#counters
cnt = rbind(c(3, 1), c(2, 2), c(0, 4), c(5, 3), c(5, 5), c(7, 3), c(5, 6), c(5, 7), c(5, 8),
                c(10, 1), c(8, 3), c(12, 3), c(13, 4), c(12, 5), c(14, 5),
                c(15, 2), c(15, 3), c(15, 4), c(15, 5)) %>% 
  st_multipoint()

#plot
vis <- function(soln, panel) {
plot(str, lwd = 1.5)
plot(bar, col = "red", pch = 19, cex=1.5, add = T)
if(sum(soln) > 0) {
plot(st_multilinestring(str[as.logical(soln)]), col = "green",  lwd = 1.5, add = T)
plot(st_multipoint(bar[as.logical(soln), ]), col = "green", pch = 19, cex = 1.5, add = T)
}
plot(cnt, col = "black", pch = 19, cex = 1.5, add = T)
plot(eb,  col = "blue", cex = 2, lwd = 2, add = T)
text(x = c(3, 1, 4, 6, 5, 10, 9, 11, 15, 15) - 0.5, 
     y = c(0, 3, 2, 3, 4, 0, 2, 2, 0, 1), 
     labels = c("1", "2", "3", "5", "4", "6", "7", "8", "9", "10")) 
text(x = c(3, 10, 14), y = c(8, 8, 8), 
     labels = c("S1, U1", "S2, U1", "S3, U2"))
text(x = 0.1, y = 8, 
     labels = panel)
}