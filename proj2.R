#### Team Members:
# Fatima Kasenally (S2443602); Wenqi Ni (s1792412); Cameron Allan (S1748084)

#### address of github repo:
# https://github.com/niwq15/SP-Group-7-A2


#### Contributions

# Functions we each made versions of:
# Cameron - Strategy1, Strategy2, Strategy3, Pone, Pall, dloop
# Wenqi - Strategy1, Strategy2, Strategy3, Pone, Pall, dloop 
# Fatima - Strategy2, Pone, Pall, dloop 

# Further Details: 

# Cameron wrote the used Strategy 3 function and added an if statement to speed up the Pall function. 
# Some attempts were made to use arrays to write more efficient strategy functions but these were slower. 
# Other versions of Pone and Pall were also made but the final versions used were faster. 
# The dloop function written was not used as other members' dloops were better.


# Wenqi created the final versions of the functions Strategy 1, Strategy 2, Pone, Pall and the probability of no loop longer than 50 
# in a random reshuffling of cards. She wrote comments for these functions and those results. 
# She also provided a version of Strategy3, dloop function and the visualisation of the probabilities sensibly. 

# Fatima created an alternative version of Strategy 2 and pone. She created the final version of pall and dloop. 
# She also provided the sample solutions for various n and strategy combinations, and wrote comments to explain the results,
# generated the probability ditribution plot and returned the total probability of success for Strategy 1


#### Overview 
# This file holds the self-contained code to use stochastic simulation to study a generalised version of the famous '100 Prisoners Problem' 
# The problem is defined as follows: 
# Consider a prison containing 2n prisoners, each with a unique identification number ranging from 1 to 2n. 
# The prison has a room with 2n boxes, each uniquely numbered from 1 to 2n and each containing a single card. 
# These cards are each labelled with a unique number between 1 and 2n. 
# Each prisoner is allowed to open n boxes to find the card with their number.
# If all 2n prisoners successfully find their cards then they all go free. 
# After their turn,the prisoners are not allowed to communicate any new information from the boxes 
# to prisoners who have yet to take their turn.
# The room is returned to its original state after each prisoner’s go. 

# Firstly, we will show 3 strategies for the prisoners, then create a 'Pone' function to estimate 
# the probability of a single prisoner successfully finding their number, then use a 'Pall' function to 
# calculate the probability of all the prisoners finding their numbers. 
# Based on the results of the individual and joint success probabilities using different strategies, 
# we will give some comments about which strategy has the highest success probabilities for 
# both individuals and the collective. 

## Each function expressing the different strategies for opening boxes to find cards will require the same 
## input parameters : n,k,cards_num. Where n parameterises the 2n number of boxes, prisoner and cards in our problem.
## k is the prisoner's number and cards_num represents the random orders of cards placed in boxes.
## Uniform input parameters allows the implemented strategy to be a variable in each simulation as each strategy 
## function is interchangeable. This is required by functions 'Pone' and 'Pall'.


## Strategy 1:

## The Strategy1 function has inputs, n, k, and 'cards_num' (the card numbers indexed by the box numbers). 
## Each prisoner with their number 'k' starts at the k-th box which contains card b. If k is not equal to b, then the prisoner 
## goes to box b and opens it. The process will be repeated until they have either found the card k or opened n boxes without finding it
## The Strategy1 function will return 'TRUE' if the prisoner k will find their number within the n tries, otherwise it will return 'FALSE'.
Strategy1 <- function(n,k,cards_num) {
  #create an empty vector to store card numbers as they are read
  cards_picked <- rep(0, n) # the prisoner can read at most n cards
  cards_picked[1] <- cards_num[k] # start at the box with his number (k) on it 
  #for each of the remaining steps, first check if he has found his card number
  #if so, he will stop; if not, he will continue to read cards
  for (b in 2:n) {
    if (cards_picked[b-1] == k) {#if the prisoner k has found his number
      return(TRUE) #yes the prisoner has successfully found his card
      break # stop here and the prisoner will not open other boxes
    } else {#otherwise prisoner will open another box with the number of the last card
      cards_picked[b] <- cards_num[cards_picked[b-1]]
    }
  }
  #check whether he finds his number in the final step
  if (cards_picked[n] != k) { #if the final number is not his number
    return(FALSE) #prisoner has failed to find his card
  } else {#if the final number is equal to his number
    return(TRUE) #prisoner has successfully found his card
  }
}

## Strategy 2:

## The Strategy2 function has inputs, n, k, and cards_num.
## This strategy follows an identical procedure to strategy 1, except that the starting box is randomly selected
## The prisoner uses the card number in each box to pick his next box to open.
## The prisoner can open at most n boxes in order to find the card with their number on it.
## The Strategy2 function will return 'TRUE' if the prisoner k will find their number within the n tries, otherwise 'FALSE'.
Strategy2 <- function(n,k,cards_num) {
  #create an empty vector to store card numbers that have been read
  cards_picked <- rep(0, n) # the prisoner can read at most n cards
  cards_picked[1] <- sample(cards_num,1)# start at a randomly selected box 
  #for each of the remaining steps, firstly check whether he has found him number card
  #if so, he will stop; if not, he will continue to read cards
  for (b in 2:n) {
    if (cards_picked[b-1] == k) {#if the prisoner k has found his number
      return(TRUE)
      break #stop here and the prisoner will not open other boxes
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
} #The function returns TRUE/FALSE expressing that the prisoner has FOUND/FAILED TO FIND his card 

## Strategy 3:
## The Strategy3 function has inputs, n, k, and cards_num.
## The prisoner opens n boxes at random, without replacement and the prisoner will open n different boxes. 
## The Strategy3 function will return 'TRUE' if the prisoner k will find their number within the n tries, otherwise 'FALSE'.
Strategy3 <- function(n,k,cards_num){ 
  cards_picked <- sample(cards_num,n,replace=FALSE) # Randomly select n boxes from cards_num to open
  any(k == cards_picked) # check whether the prisoner's number appears in the selected card_num
} # End function


## The 'Pone' function takes the inputs 'n', 'k', 'strategy' and 'nreps' (the number of replicate simulations).
## 'Pone' is a function which calculates the probability of an individual prisoner 'k' finding his card number amoungst 2n boxes. 
## The prisoner can open n boxes and the trial is simulated nreps times. In each simulation a set-up is generated and one strategy 
## is used for the prisoner's search. The outcome of each simulation is recorded (success/failure) and 
## (total number of successes/nreps) calculates the probability that the prisoner finds his card. This probability is the function output. 
Pone <- function(n,k,strategy,nreps) {
  # create an empty vector to store the outcome of each simulation as 1/0 = Success/ Failure to find his card
  success <- rep(0, nreps)
  for (i in 1:nreps) {
    # simulate the random order of card numbers
    cards_num <- sample(1:(2*n), 2*n, replace=FALSE)
    success[i] <- strategy(n,k,cards_num)
  }
  # return the frequentist probability of success as: 
  # total number of simulated successes / total number of simulations
  return(sum(success)/nreps)
}


## The 'Pall' function takes the inputs 'n', 'strategy' and 'nreps'.
## 'Pall' is a function to find the probability of all the 2n prisoners finding their cards in a single simulation
## and all being able to escape. All prisoners must use the same strategy. The probability is calculated by finding
## the total number of all prisoner successes and dividing by the number of simulations run (nreps).
## The Function then returns this probability as the output.
Pall <- function(n,strategy,nreps) {
  #create a vector to record the result of each simulation with default values of 1s
  num_success <- rep(1,nreps)
  for (i in 1:nreps) {#for each simulation
    #generate a random order of card numbers
    cards_num <- sample(1:(2*n), 2*n, replace=FALSE)
    #use the strategies to calculate whether each prisoner can find their correct number within n times
    for (k in 1:(2*n)) {
      result <- strategy(n,k,cards_num) # save the result of strategy for the k-th prisoner
      num_success[i] <- num_success[i] * result # if result is 1 keep 1 in the vector. If 0 change to 0.
      if (result == 0){ # stop if any prisoner does not escape, this cuts down simulation run-time
        break
      }
    }
  }
  # return the frequentist probability as: total number of simulated successes / total number of simulated trials
  return(sum(num_success)/nreps)
}

### Sample Results ###

### individual prisoner probabilities: 
### Pone takes args: n,k,strategy,nreps

Pone(5,5,Strategy1,10000)
Pone(50,50,Strategy1,10000)
# one prisoner & strategy 1: prob success ~ 50%  (for all n)

Pone(5,5,Strategy2,10000)
Pone(50,50,Strategy2,10000)
# one prisoner & strategy 2: prob success ~40%  (n=5) ~37%  (n>50)

Pone(5,5,Strategy3,10000)
Pone(50,50,Strategy3,10000)
# one prisoner & strategy 3: prob success ~50%  (for all n)

### joint probabilities for all prisoners as a collective:  
### Pall takes args: n,strategy,nreps


Pall(5,Strategy1,10000)
Pall(50,Strategy1,10000)
#10 prisoners & strategy 1: prob success ~35% (n=5)
#100 prisoners & strategy 1: prob success ~31% (n=50)

Pall(5,Strategy2,10000)
Pall(50,Strategy2,10000)
#10 prisoners & strategy 2: prob success ~0.01%  (n=5)
#100 prisoners & strategy 2: prob success ~0% (smaller than precision)  (n=50)

Pall(5,Strategy3,10000)
Pall(50,Strategy3,10000)
#10 prisoners & strategy 3: prob success ~0.1%  (n=5)
#100 prisoners & strategy 3: prob success ~0% (smaller than precision) (n=50)


## Remarks

## Strategy 1 and Strategy 3 have similar individual success probabilities of 50%. 
## Strategy 1 has much higher joint success probability of above 30%, while Strategy 3 only has 0%.
## It demonstrates that there is something intelligent in Strategy 1's design.
## Strategy 2 has a lower marginal probability for success than the others, and a joint probability close to 0. 

## More explanations
## For Strategy 1, consider the distribution of maximum loop lengths amongst the boxes and cards. The cycle of box and card numbers 
## must eventually return the prisoner to open a box with his card number inside if he is allowed to open as many boxes as he wants, 
## as the numbers used are part of a finite sequence. Thus, the high joint success probability can be calculated as the cumulative 
## probability of the maximum loop length being less than n, which we have determined to be approximately 31%.

## For Strategy 2, the marginal probability is lower than 50% because the conditions for the prisoner's success are 1.the prisoner 
## select the correct loop with his card in it at random; 2.the loop he picks must also be shorter than n. 
## Our results show that the probability is about 37-40% (varies with n). The joint probability tends to 0 for the similar reason as strategy 3.

## For Strategy 3, the probability of finding their card every time they open a box is 1/2n and if he opens n boxes at random the 
## probability of finding his card is n/2n (=50%). For 2n prisoners all finding their cards in a simulation we multiply the independent 
## marginal probabilities to produce a joint probability of (0.5)^(2n), which tends to 0 as n increases. 


## The 'dloop' function has inputs: 'n' and 'nreps'. 
## It is a function which returns the probability distribution vector for each loop length occuring in a simulation.
## It takes the parameters n (making 2n boxes) and nreps (to make nreps simulations). In each simulation the boxes are 
## opened one by one while iterating through a loop, to find its length. The unique loop lengths from each simulation 
## are then tabulated and by nreps (the total number of simulations).
## This creates a probability vector (prob_vec_y = probability of each loop length occurring at least once),
## which is the output of the function. 
dloop <- function(n, nreps) {
  each_loop_lengths <- c() #create an empty list to store the loop lengths generated by all simulations
  
  for (i in 1:nreps){ 
    #simulate a new set of boxes for each simulation
    cards_num <- sample(1 : (2*n), 2*n , replace=FALSE) 
    #store a list of closed boxes in each simulation
    unopened_boxes <- 1:(2*n)
    
    #store the loop lengths found in each simulation 
    new_loop_lengths <- c()
    
    #scan for loops, (while more than 2 boxes are still closed, indexing breaks down for smaller loops)
    while (length(unopened_boxes) > 2 ){
      #pick the smallest number unopened box and store the value of the card inside 
      cards_picked <- cards_num[unopened_boxes[1]]
      
      #pick your next box according to your last card and scan each closed loop
      for (b in 2:length(unopened_boxes)){ #max loop length is the number of closed boxes
        #store the number of the latest card
        cards_picked <- c(cards_picked, cards_num[cards_picked[b-1]])
      }#end for 
            
      #scanning the last 2 unopened boxes:
      #if the card in box 1 matches the number of box 2
      if (length(unopened_boxes) == 2 && cards_num[unopened_boxes[1]] == unopened_boxes[2]) {
        #we have found a 2 item long loop 
        new_loop_lengths <- c(new_loop_lengths, 2) #save the loop length 
        #if the card in box 1 matches the number of box 1 
      } else if ((length(unopened_boxes) == 2) && (cards_num[unopened_boxes[1]] == unopened_boxes[1])) {
        #we have found one or more loops with length 1
        new_loop_lengths <- c(new_loop_lengths, 1) #save the loop length to simulation counter 
      }
      
      # add the last loop scanned to our list of loop lengths found 
      new_loop_lengths <- c(new_loop_lengths, length(unique(cards_picked))) 
      
      #remove the boxes we have scanned from the list of unopened boxes 
      unopened_boxes <- setdiff(unopened_boxes,unique(cards_picked))
    } #end while 
    
    #store the uniquely occuring loop lengths from each simulation in a global simulation list 
    each_loop_lengths <- c(each_loop_lengths, unique(new_loop_lengths))
    
  } #end for 
  #create a probability vector for the occurrence of each type of loop length 
  prob_vec_y <- tabulate(each_loop_lengths)/nreps #dividing frequency of occurences/total number of simulations
  #function outputs the probability distribution 
  return(prob_vec_y)
  
}#end function

#################### Alternative dloop function : 
## This version of dloop takes the same inputs n and nreps, and returns the same output, the probability distribution vector. 
## This version takes under 10 seconds on some devices but over 2 minuites on others (Cam's machine). We have included this version 
## because it uses the alternative interpretation of loops as nested objects and their lengths as the depth of nesting. 
##
## The method creates an empty array, 'loop_len' and uses simulation to count the occurrences of each 
## loop length ranging from 1 to 2n, which will be stored in the rows of 'loop_len'. Then we will convert the values to binary and give 1s to those 
## loop lengths which occur at least once. Based on the columns of 'loop_len', we can calculate the probability of each loop length occurring at least once.
## 
## dloop <- function(n,nreps) {
##  # create an empty array to store the number of occurrences of loops with lengths from 1 to 2n for each simulation
##  # default value is 1 as ...
##  loop_len <- array(rep(1,nreps*(2*n)), dim=c(nreps,2*n)) #each row referring to one simulation
##  prob <- rep(0,2*n) #create an empty vector to store the targeted probabilities
##  for (i in 1:nreps) {#for each simulation
##    cards_num <- sample(1:(2*n), 2*n, replace=FALSE)
##    cards_picked <- rep(0,2*n)
##    #calculate loop lengths for each prisoner's number
##    for (k in 1:(2*n)) {
##      cards_picked[1] <- cards_num[k]
##      for (b in 2:(2*n)) {
##        if (cards_picked[b-1] ==k) {
##          break #stop the loop if the loop lenth has been found
##        } else {
##          cards_picked[b] <- cards_num[cards_picked[b-1]]
##          loop_len[i,k] <- loop_len[i,k] + 1
##        }
##      }
##    }
##    #now we obtain a row which specifies the loop length for each k in 1:2n
##    #use tabulate function to count the occurrences of each loop length which is between 1 and 2n
##    len <- append(loop_len[i,], 2*n) #add a '2*n' to keep the length of the vector when using tabulate
##    len <- tabulate(len)
##    loop_len[i,] <- replace(len, length(len), len[length(len)]-1) #eliminate the impact of the added '2*n'
##    #convert the values to be binary
##    loop_len[i,] <- as.numeric(loop_len[i,] >= 1) # give 1s to those loop lengths which occur at least once
##  }#end for each simulation
##  #notice that each column of 'loop_len' shows whether the loop length appearing in each simulation
##  #calculate the probability of each loop length
##  return(colSums(loop_len)/nreps)
##}
################################################




#testing the distribution for n=50
n = 50 
#stores our probability distribution using function dloop
prob_vec <- dloop(n, 10000)

##plotting the probability distribution
#create an x axis 
x_axis <- 1:length(prob_vec)
#plot the scatter plot
plot(x_axis, prob_vec, xlab = "Loop Length", ylab = "Probability ", main = "Probability Distribution for the Occurrence \n of Loop Lengths (at least once) per Simulation", cex.main = 1)

#output the probability distribution vector: 
cat("your probability vector for producing at least one loop of length i: \n", prob_vec, "\n")

## Note that 'no loop length longer than 50' indicates that the max loop length is 50. 
## P(no loop longer than 50) = 1 - P(there exist loop lengths larger than 50)
## Since the total loop lengths must sum to 2n, the loops of length greater than n can only occur once.
## It implies that P(loop length >n occurs at least once ) = P(loop length >n occurs exactly once ).
## Thus, P(there exist loop lengths larger than 50) is equal to the sum of P(there exists loop length i) where i ranges from 51 to 100. 

cat( "Probability of Strategy 1 succeeding: \n", 1 - sum(prob_vec[(n+1):(2*n)]), "\n this is also the probability of the maximum loop length being no more than n")
