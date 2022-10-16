#### Team Members

#### address of github repo


#### Contributions

#### Overview / summary


## There are 2n cards each printed with a unique number from 1 to 2n.
## The 2n cards are randomly placed one in each of 2n boxes
## Use sample function to give a random order of card numbers
cards_num <- sample(1:(2*n), 2*n, replace=FALSE)


## Prisoner k uses Strategy 1 to find their number
## Each prisoner with their number 'p' starts at the p-th box whose number on the card is k
## If k is not equal to p, then the prisoner goes to box number k and open it
## The process will be repeated until they have either found the card with number p or opened n boxes without finding it

##problem: whether necessarily needs to put n /cards_num as inputs?

Strategy1 <- function(n,k,cards_num) {
  #create an empty vector to store card numbers that have been read
  cards_picked <- rep(0, n) # the prisoner can read at most n cards
  cards_picked[1] <- cards_num[k] # start at the box with his number on it
  #for each of the remaining steps, firstly check whether he has found its number
  #if so, he will stop; if not, he will continue to read cards
  for (b in 2:n) {
    if (cards_picked[b-1] == k) {#if the prisoner k has found his number
      return(TRUE)
      break
    } else {#otherwise prisoner will open the box with number k
      cards_picked[b] <- cards_num[cards_picked[b-1]]
    }
  }
  #check whether he finds his number in the final step
  if (cards_picked[n] != k) { #if the final number is not his number
    return(FALSE)
  } else {#if the final number is equal to his number
    return(TRUE)
  }
}

## Strategy 2

Strategy2 <- function(n,k,cards_num) {
  #create an empty vector to store card numbers that have been read
  cards_picked <- rep(0, n) # the prisoner can read at most n cards
  cards_picked[1] <- sample(cards_num,1)# start at a randomly selected box 
  #for each of the remaining steps, firstly check whether he has found its number
  #if so, he will stop; if not, he will continue to read cards
  for (b in 2:n) {
    if (cards_picked[b-1] == k) {#if the prisoner k has found his number
      return(TRUE)
      break
    } else {#otherwise prisoner will open the box with number k
      cards_picked[b] <- cards_num[cards_picked[b-1]]
    }
  }
  #check whether he finds his number in the final step
  if (cards_picked[n] != k) { #if the final number is not his number
    return(FALSE)
  } else {#if the final number is equal to his number
    return(TRUE)
  }
}


## Strategy 3, selecting n boxes randomly to open. We use sample to get these numbers.

Strategy3 <- function(n,k,cards_num){ # Begin function, using n,k,cards_num parameters to work with Pone and Pall
  cards_picked <- sample(cards_num,n) # Randomly select n boxes from cards_num to open
  if (k %in% cards_num[cards_picked]){ # Check if prisoner number is in any of the boxes selected
    return(TRUE) 
    #cat("The prisoner found their card in box ", match(x,boxes[select_box])) # Uncomment this if running Strategy3 standalone for a success message
  } else { # If prisoner number not in any of the n boxes
    return(FALSE)
    #print("The prisoner did not find their card.") # Uncomment this for a failure message.
  }
} # End function

## Write a function 'Pone' to estimate the probability of a prisoner successfully finding their number

Pone <- function(n,k,strategy,nreps) {
  # create an empty vector to store whether the prisoner k has found his number in each simulation
  success <- rep(0, nreps)
  for (i in 1:nreps) {
    #simulate the order of card numbers
    cards_num <- sample(1:(2*n), 2*n, replace=FALSE)
    success[i] <- strategy(n,k,cards_num)
  }
  #return(success)
  #use 'sum' to count the number of successes or TRUE values
  #show the success probability
  return(sum(success)/nreps)
}

Pone(10,1,Strategy1,1000)


pall <- function (n,k,strategy,nreps) {
  boxes <- sample(1 : (2*n), 2*n , replace=FALSE)
    #num_successes stores the success/failure of all the prisoners in each simulation
  num_successes <- rep(0,nreps)
  #In each simulation, loop through each prisoner, applying the strategy each time 
  for (k in 1:(2*n)) {
    num_successes(i) <- num_successes(i)*strategy(n,k,cards_num)
  }
  #output the total frequentist probability 
  return(sum(num_successes)/nreps)
}

## Create a function Pall - version with 'nreps'

Pall <- function(n,strategy,nreps) {
  #create a vector to record the result of each simulation with default values of 1s
  num_success <- rep(1,nreps)
  for (i in 1:nreps) {#for each simulation
    #generate a random order of card numbers
    cards_num <- sample(1:(2*n), 2*n, replace=FALSE)
    #use the strategies to calculate whether each prisoner can find their correct number within n times
    for (k in 1:(2*n)) {
      num_success[i] <- num_success[i] * strategy(n,k,cards_num)
    }
  }
  return(sum(num_success)/nreps)
}


