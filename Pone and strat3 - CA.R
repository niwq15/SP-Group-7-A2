n <- 50
prisoner_numbers <- c(1:(2*n))
boxes <- sample(prisoner_numbers)

strat3 <- function(x){
  select_box = sample(prisoner_numbers,n)
  #cat("Prisoner ", x, " selected boxes ", select_box, " which contained cards ", boxes[select_box])
  if (x %in% boxes[select_box]){
    return(1)
    #cat("The prisoner found their card in box ", match(x,boxes[select_box]))
  } else {
    return(0)
    #print("The prisoner did not find their card.")
  }
}

Pone <- function(n,k,strategy,nreps){
  survived = c(rep(0,nreps))
  for (i in 1:nreps) {
    survived[i] <- strategy(k)
  }
  return(length(which(survived == 1))/nreps)
}