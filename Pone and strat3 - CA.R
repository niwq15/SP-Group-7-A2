n <- 50
prisoner_numbers <- c(1:(2*n))
boxes <- sample(prisoner_numbers)

strat3 <- function(x){
  select_box = sample(prisoner_numbers,n)
  #cat("Prisoner ", x, " selected boxes ", select_box, " which contained cards ", boxes[select_box])
  if (x %in% boxes[select_box]){
    return(TRUE)
    #cat("The prisoner found their card in box ", match(x,boxes[select_box]))
  } else {
    return(FALSE)
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

Pall <- function(n,strategy,nreps){
  allsurvived = c(rep(0,nreps))
  for (i in 1:nreps){
  survived = c(rep(0,n))
  for (i in 1:n) {
    survived[i] <- strategy(prisoner_numbers[i])
  }
  if (FALSE %in% survived) {
    allsurvived[i] <- FALSE
  } else {
    allsurvived[i] <- TRUE
  }
  }
  return(which(allsurvived == 1)/nreps)
}