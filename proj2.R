st <- Sys.time()
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
#cards_num <- sample(1:(2*n), 2*n, replace=FALSE)


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

## Create a function Pall - version with 'nreps'

Pall <- function(n,strategy,nreps) {
  #create a vector to record the result of each simulation with default values of 1s
  num_success <- rep(1,nreps)
  st <- Sys.time()
  for (i in 1:nreps) {#for each simulation
    #generate a random order of card numbers
    cards_num <- sample(1:(2*n), 2*n, replace=FALSE)
    #use the strategies to calculate whether each prisoner can find their correct number within n times
    for (k in 1:(2*n)) {
      result <- strategy(n,k,cards_num) # save result
      num_success[i] <- num_success[i] * result # if result is 1 keep 1 in the vector. If 0 change to 0.
      if (result == 0){ # stop if any prisoner does not escape
        break
      }
    }
  }
  return(sum(num_success)/nreps)
}

###pone takes args: n,k,strategy,nreps
###individual probs

Pone(5,5,Strategy1,10000)
Pone(50,50,Strategy1,10000)
#one prisoner & strategy 1: prob success ~ 50%

Pone(5,5,Strategy2,10000)
Pone(500,500,Strategy2,10000)
#one prisoner & strategy 2: prob success ~40% (n=5) ~37% (n>50)

Pone(5,5,Strategy3,10000)
Pone(50,50,Strategy3,10000)
#one prisoner & strategy 3: prob success ~50% 


#pall takes args: n,strategy,nreps
#joint probs 

Pall(5,Strategy1,10000)
Pall(50,Strategy1,10000)
#10 prisoners & strategy 1: prob success ~35%
#100 prisoners & strategy 1: prob success ~30%

Pall(5,Strategy2,10000)
Pall(50,Strategy2,10000)
#10 prisoners & strategy 2: prob success ~0.01%
#100 prisoners & strategy 2: prob success ~0% (smaller than precision)

Pall(5,Strategy3,10000)
Pall(50,Strategy3,10000)
#10 prisoners & strategy 3: prob success ~1%
#100 prisoners & strategy 3: prob success ~0% (smaller than precision)


#
#
# Add some commentary about the funky results ;)
#
#


#calculate the probability of loop lengths 
dloop <- function(n, nreps) {
  each_loop_lengths <- c() #list to store the loop lengths generated by all simulations
  max_loop_lengths <- c()
  
  for (i in 1:nreps){ 
    #simulate the boxes nreps times 
    boxes <- sample(1 : (2*n), 2*n , replace=FALSE) 
    unopened_boxes <- 1:(2*n)
    
    #store the unique loop lengths generated from each simulation 
    new_loop_lengths <- c()
    
    #scan for loops, (while more than 2 boxes are still closed, indexing breaks down for smaller loops)
    while (length(unopened_boxes) > 2 ){
      #pick the smallest number unopened box and store the value of the card inside 
      cards_picked <- boxes[unopened_boxes[1]]
      
      #pick your next box according to your last card and scan each closed loop
      for (b in 2:length(unopened_boxes)){ #max loop length is the number of closed boxes
        cards_picked <- c(cards_picked, boxes[cards_picked[b-1]])
      }#end for 
      
      #add your new loop length to your list
      new_loop_lengths <- c(new_loop_lengths, length(unique(cards_picked))) 
      
      #remove the boxes opened 
      unopened_boxes <- setdiff(unopened_boxes,unique(cards_picked))
    } #end while 
    
    
    each_loop_lengths <- c(each_loop_lengths, unique(new_loop_lengths))
    max_loop_lengths <- c(max_loop_lengths, max(new_loop_lengths))
    
    
  } #end for 
  
  #probability vector for the occurrence of each maximum loop length 
  prob_vec_ymax <- tabulate(max_loop_lengths)/nreps
  x_axis <- 1:length(prob_vec_ymax)
  #prints a plot for probailities of maximum loop lengths
  plot(x_axis, prob_vec_ymax, xlab = "max loop length", ylab = "probability")
  
  #find the probability of max loop length is less than n
  prob_less_n_loop = sum(prob_vec_ymax[1:n])
  cat("\n","prob your max loop has a length less than n",   prob_less_n_loop, "\n")
  
  
  #probability vector for the occurrence of each type of loop length 
  prob_vec_y <- tabulate(each_loop_lengths)/nreps
  cat("your probability vector for producing at least one loop of length i: \n", prob_vec_y)
  return(prob_vec_y)
}#end function
VAR_1 <- dloop(50, 10000)
et <- Sys.time()
et - st