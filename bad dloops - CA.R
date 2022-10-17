dloops <- function(n,nreps){
  loop_length_prob <- c(rep(0,2*n))
  for (k in 1:nreps){
    boxes <- sample(1:(2*n))
    cards_found <- array(rep(0,2*2*n*n), dim = c(2*n,2*n))
    cards_found[,1] <- boxes
    for (i in 1:(2*n)){
      for (j in 1:(2*n-1)){
      if (i == cards_found[i,j]){
        break
      } else {
        cards_found[i,j+1] <- boxes[cards_found[i,j]]
      }
      }
    }
    for (i in 1:(2*n)){
      loop_length_prob[length(which(cards_found[i,] != 0))] <- loop_length_prob[length(which(cards_found[i,] != 0))] + 1
    }
  }
  return(loop_length_prob/(2*n*nreps))
  }

barplot(dloops(50,10000))


