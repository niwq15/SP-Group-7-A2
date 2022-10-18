#### Team Members

#### address of github repo


#### Contributions

#### Overview / summary

#don't define any parameters outside of the functions

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


pone <- function(n,k,strategy,nreps) {
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

pone(10,1,Strategy1,1000)


#corrected pall function 
pall <- function (n,strategy,nreps) {
  #store outcomes of each simulation (true = success, false = failure)
  num_successes <- rep(1,nreps)
  for (i in 1:nreps){
    #simulates a new room of boxes 
    cards_num <- sample(1 : (2*n), 2*n , replace=FALSE)
    for (k in 1:(2*n)) {
      num_successes[i] <- num_successes[i]*strategy(n,k,cards_num)
    }#end for 
  }#end for 
  #returns the frequentist probability of success for all the prisoners 
  return(sum(num_successes)/nreps)
}




###pone takes args: n,k,strategy,nreps
###individual probs
pone(5,5,Strategy1,10000)
pone(50,50,Strategy1,10000)
#one prisoner & strategy 1: prob success ~ 50%

pone(5,5,Strategy2,10000)
pone(500,500,Strategy2,10000)
#one prisoner & strategy 2: prob success ~40% (n=5) ~37% (n>50)

pone(5,5,Strategy3,10000)
pone(50,50,Strategy3,10000)
#one prisoner & strategy 3: prob success ~50% 


#pall takes args: n,strategy,nreps
#joint probs 
pall(5,Strategy1,10000)
pall(50,Strategy1,10000)
#10 prisoners & strategy 1: prob success ~35%
#100 prisoners & strategy 1: prob success ~30%

pall(5,Strategy2,10000)
pall(50,Strategy2,10000)
#10 prisoners & strategy 2: prob success ~0.01%
#100 prisoners & strategy 2: prob success ~0% (smaller than precision)

pall(5,Strategy3,10000)
pall(50,Strategy3,10000)
#10 prisoners & strategy 3: prob success ~1%
#100 prisoners & strategy 3: prob success ~0% (smaller than precision)


#
#
# Add some commentary about the funky results ;)
#
#


#calculate the probability of loop lengths 

dloop <- function(n, nreps) {
  loop_lengths <- c() #list to store the loop lengths as they are calculated - for the histogram
  for (i in 1:nreps){ 
    #simulate the boxes nreps times 
    boxes <- sample(1 : (2*n), 2*n , replace=FALSE) 
    unopened_boxes <- 1:(2*n)
    
    while (length(unopened_boxes) > 2 ){ #while more than 2 boxes are still closed, indexing breaks down for smaller loops
      #pick the smallest number unopened box and store the value of the card inside 
      cards_picked <- boxes[unopened_boxes[1]]
      #pick your next box according to your last card and scan each closed loop
      for (b in 2:length(unopened_boxes)){ #max loop length is the number of closed boxes
        cards_picked <- c(cards_picked, boxes[cards_picked[b-1]])
      }#end for 
      #add your loop length to your list
      loop_lengths <- c(loop_lengths, length(unique(cards_picked))) 
      #remove the boxes opened 
      unopened_boxes <- setdiff(unopened_boxes,unique(cards_picked))
    } #end while 
    
    # if (length(unopened_boxes) == 2 && boxes[unopened_boxes[1]] == unopened_boxes[2] ) {
    #  #we have found a 2 item long loop 
    #  loop_lengths <- c(loop_lengths, 2) 
    # }  else-if (length(unopened_boxes) <= 2 ) {
    #  #we have found either 1 or 2 single item loops
    #  loop_lengths <- c(loop_lengths, rep(1,length(unopened_boxes) )) 
    # }
  } #end for 
  #hist(loop_lengths)
  prob_vec_y <- tabulate(loop_lengths)/nreps
  x_axis <- 1:length(prob_vec_y)
  plot(x_axis, prob_vec_y, xlab = "loop length", ylab = "probability")
  return(prob_vec_y)
}#end function


dloop(10, 50)









