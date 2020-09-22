source("rubix.R")
source("plot_rubix.R")
source("rubix_moves.R")

cube <- rubix()
plot_rubix(cube)

move <- function(x = cube, f = rc){
  cube <- f(x)
  plot_rubix(cube)
  return(cube)
}

shuffle <- function(x = rubix(), it = 5){
  
  funcs <- list("rc" = rc, "ra" = ra, "lc" = lc, "la" = la, 
                "fc" = fc, "fa" = fa, "hc" = hc, "ha" = ha, 
                "tc" = tc, "ta" = ta, "bc" = bc, "ba" = ba)
  
  
  numbers <- sample(1:length(funcs), it, replace = T)
  moves <- numeric(it)
  
  for(i in 1:it){
    x <- funcs[[numbers[i]]](cube = x)
    plot_rubix(x)
    moves[i] <- names(funcs)[[numbers[i]]]
    print(moves[i])
  }
  
}
