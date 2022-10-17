st <- Sys.time() # start timer

Strategy1 <- function(n,nreps){
  prisoner_numbers <- c(1:(2*n)) # created in function so we can make n a variable
  s1boxes <- array(rep(0,nreps*2*n*n), dim = c(2*n,n,nreps)) # create template for which cards were found
  for (k in 1:nreps){ # for each simulation
  box_contains <- sample(prisoner_numbers) # randomise cards in boxes
  for (i in prisoner_numbers){ # for each prisoner
    s1boxes[i,1,k] <- box_contains[i] # intialise by choosing the box with their number on it
    for (j in 1:(n-1)){  # for every other choice
      s1boxes[i,j+1,k] <- box_contains[s1boxes[i,j,k]] # select the box number of the card they just found
    }
  }
  }
  return(s1boxes)
}

Strategy2 <- function(n,nreps){
  prisoner_numbers <- c(1:(2*n)) # created in function so we can make n a variable
  s2boxes <- array(rep(0,nreps*2*n*n), dim = c(2*n,n,nreps)) # create template for which cards were found
  for (k in 1:nreps){ # for each simulation
  box_contains <- sample(prisoner_numbers) # randomise cards in boxes
  for (i in prisoner_numbers){ # for each prisoner
    s2boxes[i,1,k] <- box_contains[sample(prisoner_numbers,1)] # intialise by choosing a random box
    for (j in 1:(n-1)){  # for every other choice
      s2boxes[i,j+1,k] <- box_contains[s2boxes[i,j,k]] # select the box number of the card they just found
    }
  }
  } 
  return(s2boxes)
}

Strategy3 <- function(n,nreps){
  prisoner_numbers <- c(1:(2*n)) # created in function so we can make n a variable
  s3boxes <- array(rep(0,nreps*2*n*n), dim = c(2*n,n,nreps)) # create template for which cards were found
  for (k in 1:nreps){ # for each simulation
  box_contains <- sample(prisoner_numbers) # randomise cards in boxes
  for (i in prisoner_numbers){ # for each prisoner
    s3boxes[i,,k] <- box_contains[sample(prisoner_numbers,n)] # pick n random boxes
  }
  }
  return(s3boxes)
}
  
Pone <- function(n,i,strategy,nreps){
  successes <- array(rep(0,2*n*nreps), dim = c(2*n,nreps)) # create template for successful prisoners in each simulation
  cards_found <- strategy(n,nreps)
  for (k in 1:nreps){ # for each simulation
    if (i %in% cards_found[i,,k]){ # check if prisoner card number is in their row
      successes[i,k] <- 1 # if they found it, we say they were successful in that simulation
    } else { # otherwise they were not successful
      successes[i,k] <- 0 
    }
  }
  length(which(successes == 1))/(nreps) # output probability
}

Pall <- function(n,strategy,nreps){ # identical to Pone but does not outputs the appropriate probability
  successes <- array(rep(0,2*n*nreps), dim = c(2*n,nreps)) # create template for successful prisoners in each simulation
  cards_found <- strategy(n,nreps)
  for (k in 1:nreps){ # for each simulation
    for(i in 1:(2*n)){ # for each prisoner
    if (i %in% cards_found[i,,k]){ # check if their card number is in their row
      successes[i,k] <- 1 # if they found it, we say they were successful in that simulation
    } else { # otherwise they were not successful
      successes[i,k] <- 0
    }
    }
  }
  success_array <- array(successes,dim = c(2*n,nreps)) # create an array from successes
  allescape <- apply(success_array, MARGIN = 2, prod) # so we can use apply over each simulation, if prod = 1 everyone escapes
  length(which(allescape == 1))/nreps # output probability
}

Pone(5,1,Strategy1,10000)
Pone(5,1,Strategy2,10000)
Pone(5,1,Strategy3,10000)
Pall(5,Strategy1,10000)
Pall(5,Strategy2,10000)
Pall(5,Strategy3,10000)

Pone(50,1,Strategy1,10000)
Pone(50,1,Strategy2,10000)
Pone(50,1,Strategy3,10000)
Pall(50,Strategy1,10000)
Pall(50,Strategy2,10000)
Pall(50,Strategy3,10000)

et <- Sys.time() # end timer
et - st # time taken
# 1.030558 mins on 5800H, 3.2GHz :(