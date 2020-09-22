rc <- function(cube = rubix()){
  
  hold1 <- cube$top[c(3,6,9),3]
  
  cube$top[c(3,6,9), 3] <- cube$front[c(3,6,9), 3]
  cube$front[c(3,6,9), 3] <- cube$base[c(3,6,9), 3]
  cube$base[c(3,6,9), 3] <- cube$hind[c(3,6,9), 3]
  cube$hind[c(3,6,9), 3] <- hold1
  
  hold2 <- cube$right[4,3]
  hold3 <- cube$right[1,3]
  
  cube$right[4,3] <- cube$right[8,3]
  cube$right[8,3] <- cube$right[6,3]
  cube$right[6,3] <- cube$right[2,3]
  cube$right[2,3] <- hold2
  
  cube$right[1,3] <- cube$right[7,3]
  cube$right[7,3] <- cube$right[9,3]
  cube$right[9,3] <- cube$right[3,3]
  cube$right[3,3] <- hold3
  
  p <- plot_rubix(cube)
  print(p)
  return(cube)
  
}

ra <- function(cube = rubix()){
  
  hold1 <- cube$top[c(3,6,9),3]
  
  cube$top[c(3,6,9), 3] <- cube$hind[c(3,6,9), 3]
  cube$hind[c(3,6,9), 3] <- cube$base[c(3,6,9), 3]
  cube$base[c(3,6,9), 3] <- cube$front[c(3,6,9), 3]
  cube$front[c(3,6,9), 3] <- hold1
  
  hold2 <- cube$right[4,3]
  hold3 <- cube$right[1,3]
  
  cube$right[4,3] <- cube$right[2,3]
  cube$right[2,3] <- cube$right[6,3]
  cube$right[6,3] <- cube$right[8,3]
  cube$right[8,3] <- hold2
  
  cube$right[1,3] <- cube$right[3,3]
  cube$right[3,3] <- cube$right[9,3]
  cube$right[9,3] <- cube$right[7,3]
  cube$right[7,3] <- hold3
  
  p <- plot_rubix(cube)
  print(p)
  return(cube)
  
}

lc <- function(cube = rubix()){
  
  hold1 <- cube$top[c(1,4,7),3]
  
  cube$top[c(1,4,7), 3] <- cube$hind[c(1,4,7), 3]
  cube$hind[c(1,4,7), 3] <- cube$base[c(1,4,7), 3]
  cube$base[c(1,4,7), 3] <- cube$front[c(1,4,7), 3]
  cube$front[c(1,4,7), 3] <- hold1
  
  hold2 <- cube$left[4,3]
  hold3 <- cube$left[1,3]
  
  cube$left[4,3] <- cube$left[8,3]
  cube$left[8,3] <- cube$left[6,3]
  cube$left[6,3] <- cube$left[2,3]
  cube$left[2,3] <- hold2
  
  cube$left[1,3] <- cube$left[7,3]
  cube$left[7,3] <- cube$left[9,3]
  cube$left[9,3] <- cube$left[3,3]
  cube$left[3,3] <- hold3
  
  p <- plot_rubix(cube)
  print(p)
  return(cube)
  
}

la <- function(cube = rubix()){
  
  hold1 <- cube$top[c(1,4,7),3]
  
  cube$top[c(1,4,7), 3] <- cube$front[c(1,4,7), 3]
  cube$front[c(1,4,7), 3] <- cube$base[c(1,4,7), 3]
  cube$base[c(1,4,7), 3] <- cube$hind[c(1,4,7), 3]
  cube$hind[c(1,4,7), 3] <- hold1
  
  hold2 <- cube$left[4,3]
  hold3 <- cube$left[1,3]
  
  cube$left[4,3] <- cube$left[2,3]
  cube$left[2,3] <- cube$left[6,3]
  cube$left[6,3] <- cube$left[8,3]
  cube$left[8,3] <- hold2
  
  cube$left[1,3] <- cube$left[3,3]
  cube$left[3,3] <- cube$left[9,3]
  cube$left[9,3] <- cube$left[7,3]
  cube$left[7,3] <- hold3
  
  p <- plot_rubix(cube)
  print(p)
  return(cube)
  
}

fc <- function(cube = rubix()){
  
  hold1 <- cube$top[c(7,8,9),3]
  
  cube$top[c(7,8,9), 3] <- cube$left[c(7,8,9), 3]
  cube$left[c(7,8,9), 3] <- cube$base[c(3,2,1), 3]
  cube$base[c(3,2,1), 3] <- cube$right[c(7,8,9), 3]
  cube$right[c(7,8,9), 3] <- hold1
  
  hold2 <- cube$front[4,3]
  hold3 <- cube$front[1,3]
  
  cube$front[4,3] <- cube$front[8,3]
  cube$front[8,3] <- cube$front[6,3]
  cube$front[6,3] <- cube$front[2,3]
  cube$front[2,3] <- hold2
  
  cube$front[1,3] <- cube$front[7,3]
  cube$front[7,3] <- cube$front[9,3]
  cube$front[9,3] <- cube$front[3,3]
  cube$front[3,3] <- hold3
  
  p <- plot_rubix(cube)
  print(p)
  return(cube)
  
}

fa <- function(cube = rubix()){
  
  hold1 <- cube$top[c(7,8,9),3]
  
  cube$top[c(7,8,9), 3] <- cube$right[c(7,8,9), 3]
  cube$right[c(7,8,9), 3] <- cube$base[c(3,2,1), 3]
  cube$base[c(3,2,1), 3] <- cube$left[c(7,8,9), 3]
  cube$left[c(7,8,9), 3] <- hold1
  
  hold2 <- cube$front[4,3]
  hold3 <- cube$front[1,3]
  
  cube$front[4,3] <- cube$front[2,3]
  cube$front[2,3] <- cube$front[6,3]
  cube$front[6,3] <- cube$front[8,3]
  cube$front[8,3] <- hold2
  
  cube$front[1,3] <- cube$front[3,3]
  cube$front[3,3] <- cube$front[9,3]
  cube$front[9,3] <- cube$front[7,3]
  cube$front[7,3] <- hold3
  
  p <- plot_rubix(cube)
  print(p)
  return(cube)
  
}

hc <- function(cube = rubix()){
  
  hold1 <- cube$top[c(1,2,3),3]
  
  cube$top[c(1,2,3), 3] <- cube$right[c(1,2,3), 3]
  cube$right[c(1,2,3), 3] <- cube$base[c(9,8,7), 3]
  cube$base[c(9,8,7), 3] <- cube$left[c(1,2,3), 3]
  cube$left[c(1,2,3), 3] <- hold1
  
  hold2 <- cube$hind[4,3]
  hold3 <- cube$hind[1,3]
  
  cube$hind[4,3] <- cube$hind[8,3]
  cube$hind[8,3] <- cube$hind[6,3]
  cube$hind[6,3] <- cube$hind[2,3]
  cube$hind[2,3] <- hold2
  
  cube$hind[1,3] <- cube$hind[7,3]
  cube$hind[7,3] <- cube$hind[9,3]
  cube$hind[9,3] <- cube$hind[3,3]
  cube$hind[3,3] <- hold3
  
  p <- plot_rubix(cube)
  print(p)
  return(cube)
  
}

ha <- function(cube = rubix()){
  
  hold1 <- cube$top[c(1,2,3),3]
  
  cube$top[c(1,2,3), 3] <- cube$left[c(1,2,3), 3]
  cube$left[c(1,2,3), 3] <- cube$base[c(9,8,7), 3]
  cube$base[c(9,8,7), 3] <- cube$right[c(1,2,3), 3]
  cube$right[c(1,2,3), 3] <- hold1
  
  hold2 <- cube$hind[4,3]
  hold3 <- cube$hind[1,3]
  
  cube$hind[4,3] <- cube$hind[2,3]
  cube$hind[2,3] <- cube$hind[6,3]
  cube$hind[6,3] <- cube$hind[8,3]
  cube$hind[8,3] <- hold2
  
  cube$hind[1,3] <- cube$hind[3,3]
  cube$hind[3,3] <- cube$hind[9,3]
  cube$hind[9,3] <- cube$hind[7,3]
  cube$hind[7,3] <- hold3
  
  p <- plot_rubix(cube)
  print(p)
  return(cube)
  
}

tc <- function(cube = rubix()){
  
  hold1 <- cube$hind[c(7,8,9),3]
  
  cube$hind[c(7,8,9), 3] <- cube$left[c(9,6,3), 3]
  cube$left[c(9,6,3), 3] <- cube$front[c(3,2,1), 3]
  cube$front[c(3,2,1), 3] <- cube$right[c(1,4,7), 3]
  cube$right[c(1,4,7), 3] <- hold1
  
  hold2 <- cube$top[4,3]
  hold3 <- cube$top[1,3]
  
  cube$top[4,3] <- cube$top[8,3]
  cube$top[8,3] <- cube$top[6,3]
  cube$top[6,3] <- cube$top[2,3]
  cube$top[2,3] <- hold2
  
  cube$top[1,3] <- cube$top[7,3]
  cube$top[7,3] <- cube$top[9,3]
  cube$top[9,3] <- cube$top[3,3]
  cube$top[3,3] <- hold3
  
  p <- plot_rubix(cube)
  print(p)
  return(cube)
  
}

ta <- function(cube = rubix()){
  
  hold1 <- cube$hind[c(7,8,9),3]
  
  cube$hind[c(7,8,9), 3] <- cube$right[c(1,4,7), 3]
  cube$right[c(1,4,7), 3] <- cube$front[c(3,2,1), 3]
  cube$front[c(3,2,1), 3] <- cube$left[c(9,6,3), 3]
  cube$left[c(9,6,3), 3] <- hold1
  
  hold2 <- cube$top[4,3]
  hold3 <- cube$top[1,3]
  
  cube$top[4,3] <- cube$top[2,3]
  cube$top[2,3] <- cube$top[6,3]
  cube$top[6,3] <- cube$top[8,3]
  cube$top[8,3] <- hold2
  
  cube$top[1,3] <- cube$top[3,3]
  cube$top[3,3] <- cube$top[9,3]
  cube$top[9,3] <- cube$top[7,3]
  cube$top[7,3] <- hold3
  
  p <- plot_rubix(cube)
  print(p)
  return(cube)
  
}

bc <- function(cube = rubix()){
  
  hold1 <- cube$hind[c(1,2,3),3]
  
  cube$hind[c(1,2,3), 3] <- cube$right[c(3,6,9), 3]
  cube$right[c(3,6,9), 3] <- cube$front[c(9,8,7), 3]
  cube$front[c(9,8,7), 3] <- cube$left[c(7,4,1), 3]
  cube$left[c(7,4,1), 3] <- hold1
  
  hold2 <- cube$base[4,3]
  hold3 <- cube$base[1,3]
  
  cube$base[4,3] <- cube$base[8,3]
  cube$base[8,3] <- cube$base[6,3]
  cube$base[6,3] <- cube$base[2,3]
  cube$base[2,3] <- hold2
  
  cube$base[1,3] <- cube$base[7,3]
  cube$base[7,3] <- cube$base[9,3]
  cube$base[9,3] <- cube$base[3,3]
  cube$base[3,3] <- hold3
  
  p <- plot_rubix(cube)
  print(p)
  return(cube)
  
}

ba <- function(cube = rubix()){
  
  hold1 <- cube$hind[c(1,2,3),3]
  
  cube$hind[c(1,2,3), 3] <- cube$left[c(7,4,1), 3]
  cube$left[c(7,4,1), 3] <- cube$front[c(9,8,7), 3]
  cube$front[c(9,8,7), 3] <- cube$right[c(3,6,9), 3]
  cube$right[c(3,6,9), 3] <- hold1
  
  hold2 <- cube$base[4,3]
  hold3 <- cube$base[1,3]
  
  cube$base[4,3] <- cube$base[2,3]
  cube$base[2,3] <- cube$base[6,3]
  cube$base[6,3] <- cube$base[8,3]
  cube$base[8,3] <- hold2
  
  cube$base[1,3] <- cube$base[3,3]
  cube$base[3,3] <- cube$base[9,3]
  cube$base[9,3] <- cube$base[7,3]
  cube$base[7,3] <- hold3
  
  p <- plot_rubix(cube)
  print(p)
  return(cube)
  
}
