#### Team Members

#### address of github repo


#### Contributions

#### Overview / summary
# Consider a case of 2n prisoners, each with a unique prisoner number ranging from 1 to 2n. 
# There is a room with 2n boxes, each with a unique number between 1 and 2n painted on its lid. 
# In each box, there is one card with a unique number between 1 and 2n. 
# The prisoners have n chances to open boxes to find their number. 
# Communication is allowable among those prisoners and the room will be returned 
# to its original state after each prisoner’s go. 

# Firstly, we will show 3 strategies for the prisoners, create a Pone function to estimate 
# the probability of a single prisoner successfully finding their number, use a Pall function to 
# calculate the probability of all the prisoners finding their numbers. 
# Based on the results of the individual and joint success probabilities using different strategies, 
# we will give some comments about which strategy has the highest success probabilities for 
# both individuals and all. 

# The following code uses stochastic simulation to explore the prisoner problem. 


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
      break #break the for loop
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


## Strategy 3 

Strategy3 <- function(n,k,cards_num) {
  #create an empty vector to store card numbers that have been read#  
  cards_picked <- rep(0, n) # the prisoner can read at most n cards
  for (b in 1:n) {
    cards_picked[b] <- sample(cards_num,1)#open each box at random
    if (cards_picked[b] == k) { #if the prisoner k has found his number
      return(TRUE)
      break
    } 
  }
  #if the prisoner has not found his number at the n-th door
  return(FALSE)
}

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

## Write a function Pall to estimate the probability of all prisoners finding their numbers
# which is the product of the probability of each prisoner finding their numbers

Pall <- function(n,strategy,nreps) {
  num_success <- rep(1,nreps)
  for (i in 1:nreps) {
    cards_num <- sample(1:(2*n), 2*n, replace=FALSE)
    #success = 1 # default value
    for (k in 1:(2*n)) {
      num_success[i] <- num_success[i] * strategy(n,k,cards_num)
    }
  }
  return(sum(num_success)/nreps)
}


## When n = 5, the estimated individual success probabilities for strategies
Pone(5,1,Strategy1,10000) # for Strategy 1
Pone(5,1,Strategy2,10000) # for Strategy 2
Pone(5,1,Strategy3,10000) # for Strategy 3
## The joint success probabilities are 
Pall(5,Strategy1,10000) # for Strategy 1
Pall(5,Strategy2,10000) # for Strategy 2
Pall(5,Strategy3,10000) # for Strategy 3
## When n = 50, the estimated individual success probabilities
Pone(50,1,Strategy1,10000) # for Strategy 1
Pone(50,1,Strategy2,10000) # for Strategy 2
Pone(50,1,Strategy3,10000) # for Strategy 3
## The joint success probabilities are 
Pall(50,Strategy1,10000) # for Strategy 1
Pall(50,Strategy2,10000) # for Strategy 2
Pall(50,Strategy3,10000) # for Strategy 3

## Comments
# Strategy 1 has a quite high success probability that all prisoners find their numbers, which is 0.324
# Strategy 2 and 3 have



## Write a function dloop

dloop <- function(n,nreps) {
  # create an empty array to store the number of occurrences of loops with lengths from 1 to 2n for each simulation
  # default value is 1 as ...
  loop_len <- array(rep(1,nreps*(2*n)), dim=c(nreps,2*n)) #each row referring to one simulation
  prob <- rep(0,2*n) #create an empty vector to store the targeted probabilities
  for (i in 1:nreps) {#for each simulation
    cards_num <- sample(1:(2*n), 2*n, replace=FALSE)
    cards_picked <- rep(0,2*n)
    #calculate loop lengths for each prisoner's number
    for (k in 1:(2*n)) {
      cards_picked[1] <- cards_num[k]
      for (b in 2:(2*n)) {
        if (cards_picked[b-1] ==k) {
          loop_len[i,k] <- loop_len[i,k]
          break #stop the loop if the loop lenth has been found
        } else {
          cards_picked[b] <- cards_num[cards_picked[b-1]]
          loop_len[i,k] <- loop_len[i,k] + 1
        }
      }
    }
    #now we obtain a row which specifies the loop length for each k in 1:2n
    #use tabulate function to count the occurrences of each loop length which is between 1 and 2n
    len <- append(loop_len[i,], 2*n) #add a '2*n' to keep the length of the vector when using tabulate
    len <- tabulate(len)
    loop_len[i,] <- replace(len, length(len), len[length(len)]-1) #eliminate the impact of the added '2*n'
    #convert the values to be binary
    loop_len[i,] <- as.numeric(loop_len[i,] >= 1) # give 1s to those loop lengths which occur at least once
  }#end for each simulation
  #notice that each column of 'loop_len' shows whether the loop length appearing in each simulation
  #calculate the probability of each loop length
  return(colSums(loop_len)/nreps)
}

## Show an example to use 'dloop' to estimate the probabilities for n = 50

loop50 <- dloop(50,10000)
# The probability of no loop length more than 50
1 - sum(loop50[51:100])

# Visualize the probabilities
hist(loop50)
