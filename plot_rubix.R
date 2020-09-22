library(ggplot2)

plot_rubix <- function(cube = rubix()){
  data <- rbind(cube$top, cube$base, cube$front, cube$hind, cube$left, cube$right)
  
  p <- ggplot(data) +
    geom_rect(aes(xmin = 3.9, xmax = 7.1, ymin = 0.9, ymax = 13.1), fill = "black") +
    geom_rect(aes(xmin = 0.9, xmax = 10.1, ymin = 3.9, ymax = 7.1), fill = "black") +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = col)) +
    scale_fill_manual(values = c(B = "#0046AD", G = "#009B48", O = "#FF5800", 
                                 R = "#B71234", W = "#FFFFFF", Y = "#FFD500")) +
    theme(legend.position = "none", panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), axis.line = element_blank(), 
          axis.ticks = element_blank(), axis.text = element_blank(),
          axis.title = element_blank()) + 
    coord_fixed(ratio = 1)
  
  return(p)
}