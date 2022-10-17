st <- Sys.time() # Start Timer

## initialisms
n <- 50 # standard n
nreps <- 10000 # standard nreps
prisoner_numbers <- c(1:100) # create vector of prisoner numbers

## simulaton initialisms
s2boxes <- array(rep(0,nreps*2*n*n), dim = c(2*n,n,nreps)) # create an array of the cards pulled from the boxes. Each matrix is a simulation
successes <- array(rep(0,2*n*nreps), dim = c(2*n,nreps)) # create array tracking tracking success of a prisoner escaping in a given simulation
# note the indices are:
# the rows, i, for each prisoner
# the columns, j, for each choice of box
# the matrices, k, for each simulation


## simulation
for (k in 1:nreps){ # for each simulation
  box_contains <- sample(prisoner_numbers) # randomise cards in boxes
  #strat2
  for (i in prisoner_numbers){ # for each prisoner
    s2boxes[i,1,k] <- box_contains[sample(prisoner_numbers,1)] # intialise by choosing the box with their number on it
    for (j in 1:(n-1)){  # for every other choice
      s2boxes[i,j+1,k] <- box_contains[s2boxes[i,j,k]] # select the box number of the card they just found
    }
  } # prisoner has selected all n boxes now, need to check if they found their card
  for (i in prisoner_numbers){ # for each prisoner
    if (i %in% s2boxes[i,,k]){ # check if their card number is in their row
      successes[i,k] <- 1 # if they found it, we say they were successful in that simulation
    } else { # otherwise they were not successful
      successes[i,k] <- 0 
    }
  }
} # end of simulation

## checking simulation was a success

# create an array of the successes
success_array <- array(successes,dim = c(2*n,nreps))

# likelihood of each prisoner escaping individually (Pone)
length(which(successes == 1))/(2*n*nreps) 

# create an array of the product of each simulation,
# if any prisoner did not escape then not everyone escaped.
allescape <- apply(success_array, MARGIN = 2, prod) 

# check the probability everyone escapes is about 0
length(which(allescape == 1))/nreps 

et <- Sys.time() # end timer
et - st # check time difference