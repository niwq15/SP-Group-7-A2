#### Team Members:
# Fatima Kasenally (S2443602); Wenqi Ni (s1792412); Cameron Allan (S1748084)

#### address of github repo:
#https://github.com/niwq15/SP-Group-7-A2


#### Contributions

# Functions we each made versions of:
# Cameron - Strategy3, unused alternatives to each strategy using arrays, Pone, Pall optimization, unused dloop
# Wenqi - Strategy1, Strategy2, Strategy3, Pone, Pall, dloop 
# Fatima - Strategy2, Pone, Pall, dloop 

# each of us should add more here. we can say what we have done, involving different versions of functions above

# Wenqi created the final versions of the functions Strategy 1, Strategy 2, Pone, 
# and the probability of no loop longer than 50 in a random reshuffling of cards to boxes
# 


# Cameron added if statement to speed up the Pall function


# Other Tasks: (correct your own)
# Cameron - optimising stuff and array stuff
# Wenqi - Q3 - Q6 stuff 
# Fatima - Q3 - Q6 stuff 




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
## [As the boxes and cards comfiguration must remain the same between prisoner trials it cannot
## be defined within the function and must be called on and externally defined.] ?
## Uniform input parameters allows the implemented strategy to be variable in each simulation as each strategy 
## function is interchangable. This is required by functions 'Pone' and 'Pall'


## Strategy 1:
## Each prisoner with their number 'b' starts at the p-th box whose number on the card is k
## If k is not equal to b, then the prisoner goes to box number k and opens it
## The process will be repeated until they have either found the card with number p or opened n boxes without finding it

## Prisoner 'k' uses Strategy 1 to find their number amoungst '2n' boxes 'cards_num'
## where 'cards_num' is a order of card numbers
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
## This strategy follows an identicle procedure to strategy 1, except that the starting box is randombly selected
## The prisoner uses the card number in each box to pick his next box to open.
## The prisoner can open at most n boxes in order to find the card with their number on it

## Prisoner 'k' uses Strategy 2 to find their number amoungst '2n' boxes 'cards_num'
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
} #The function reutrns TRUE/FALSE expressing that the prisoner has FOUND/FAILED TO FIND his card 

## In Strategy 1 and Strategy 2, we use the idea that once prionsers find their numbers, they will stop
## We use 'break' to save the running time
## In Strategy 3, we show another idea of selecting n card numbers and then checking whether k is inside

## Strategy 3:
## In this strategy the prisoner opens n boxes at random, without replacement (check !!)
## The prisoner will open n different boxes

## Prisoner 'k' uses Strategy 3 to find their number amoungst '2n' boxes 'cards_num'
Strategy3 <- function(n,k,cards_num){ 
  cards_picked <- sample(cards_num,n,replace=FALSE) # Randomly select n boxes from cards_num to open
  any(k == cards_picked) # check whether the prisoner's number appears in the selected card_num
} # End function



## 'Pone' is a function which estimates the probability of a prisoner,k, successfully finding their card number (k)
## within the n tries amoungst 2n boxes (cards_num) in nreps simulated trials, using any one of the available strategies encoded above

Pone <- function(n,k,strategy,nreps) {
  # create an empty vector to store the outsome of each simulation as 1/0 = Success/ Failure to find his card
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

## 'Pall' is a function to find the probability of all the 2n prisoners finding their cards in a single simulation
## and thus being able to escape. All prisoners must use the same strategy and the probability 
## is calculated by analysing the results from nreps simulated trials 

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
#100 prisoners & strategy 1: prob success ~30% (n=50)

Pall(5,Strategy2,10000)
Pall(50,Strategy2,10000)
#10 prisoners & strategy 2: prob success ~0.01%  (n=5)
#100 prisoners & strategy 2: prob success ~0% (smaller than precision)  (n=50)

Pall(5,Strategy3,10000)
Pall(50,Strategy3,10000)
#10 prisoners & strategy 3: prob success ~1%  (n=5)
#100 prisoners & strategy 3: prob success ~0% (smaller than precision) (n=50)


## While stategies 1 and 3 are distinc, strategy 2 shares aspects with both of them 
## yet has distinct marginal and joint probabilities 

## Strategy 3's results are as expected. The probability of finding his card in a box is 1/2n
## therefore if he opens n boxes at random the probability of finding his card is n/2n i.e. 50%
## For 2n prisoners simultaneously finding their cards in a simulation we multiply the independent 
## marginal probabilities to produce a joint probability of (0.5)^(2n), which tends to 0 as n increases. 

## Strategy 1 uses an algorithm to select cards. For a single prisoner the marginal probability of success 
## is still 50%, and therefore does not improve on the random selection method of strategy 3. However, the 
## joint probability of success is significantly higher than that of strategy 3 - demonstrating that there is 
## something intelligent in the strategy's design. The reason for the higher probability of joint success is 
## in the distribution of maximum loop lengths amoungst the boxes and cards. The cycle of box and card numbers
## must eventually return the prisoner to open a box with his card number inside if he is allowed to open as
## many boxes as he wants, as the numbers used are continuous (integers) and finite. The only limitation to the 
## strategy is if the loop length is longer than n, the number of boxes the prisoner is allowed to open. Therefore,
## the probability of success is dependent of the probability distribution of loop lengths amoungst the boxes. 
## and the joint probability of success will be equal to the cumulative probability of the maximum loop length 
## being less than n, which we have determined to be approximately 30%


## Strategy 2 has a lower marginal probability for success than the others, and a joint probability close to 0. 
## Strategy 2 takes the worst aspects from both strategies 1 and 3. The fist box is selected at random, then the 
## prisoner is stuck looping through boxes that may or may not have his card in it. The marginal probability is 
## lower than 50% because in order to succeed not only must the prisoner select the correct loop with his card in it
## at random, the loop he picks must also be shorter than n. Our results show that meeting both these conditions at 
## once has a probability of 37-40% (varies with n). The joint probability tends to 0 for the same reason as strategy 3.
## Each of the 2n prisoners must independently select the correct loop with their number at random, this has a 
## probability (p) less than 1, therefore p^(2n) (the prob of all prisoners selecting the correct loop)
## tends to 0 as n increases, this is without including the additional requirement of the loop length also being less
## than n. 





## dloop is a function which returns the probability distribution for the loop lengths occurring in a simulation 
## It takes the parameters n (to create 2n boxes) and nreps (to analyse the results of nreps simulations)
dloop <- function(n, nreps) {
  each_loop_lengths <- c() #list to store the loop lengths generated by all simulations
  
  for (i in 1:nreps){ 
    #simulate a new set of boxes for each simulation
    boxes <- sample(1 : (2*n), 2*n , replace=FALSE) 
    #store a list of closed boxes in each simulation
    unopened_boxes <- 1:(2*n)
    
    #store the unique loop lengths generated from each simulation 
    new_loop_lengths <- c()
    
    #scan for loops, (while more than 2 boxes are still closed, indexing breaks down for smaller loops)
    while (length(unopened_boxes) > 2 ){
      #pick the smallest number unopened box and store the value of the card inside 
      cards_picked <- boxes[unopened_boxes[1]]
      
      #pick your next box according to your last card and scan each closed loop
      for (b in 2:length(unopened_boxes)){ #max loop length is the number of closed boxes
        #store the number of the latest card
        cards_picked <- c(cards_picked, boxes[cards_picked[b-1]])
      }#end for 
      
      
      #scanning the last 2 boxes
      if (length(unopened_boxes) == 2 && boxes[unopened_boxes[1]] == unopened_boxes[2]) {
        #we have found a 2 item long loop 
        new_loop_lengths <- c(new_loop_lengths, 2) 
      } else if ((length(unopened_boxes) == 2) && (boxes[unopened_boxes[1]] == unopened_boxes[1])) {
        #we have found one or more loops with length 1
        new_loop_lengths <- c(new_loop_lengths, 1) 
      }
      
      #add your new loop length to your list 
      new_loop_lengths <- c(new_loop_lengths, length(unique(cards_picked))) 
      
      #remove the boxes opened 
      unopened_boxes <- setdiff(unopened_boxes,unique(cards_picked))
    } #end while 
    
    #store the uniquely occuring loop lengths from each simulation in a global simulation list 
    each_loop_lengths <- c(each_loop_lengths, unique(new_loop_lengths))
    
    
  } #end for 
  prob_vec_y <- tabulate(each_loop_lengths)/nreps #probability vector for the occurrence of each type of loop length 
  #function outputs the probability distribution 
  return(prob_vec_y)
  
}#end function




#testing the distribution for n=50
n = 50 
#variable storing our probability distribution using function dloop
prob_vec <- dloop(n, 10000)

##plotting the probability distribution
#create an x axis 
x_axis <- 1:length(prob_vec_y)
plot(x_axis, prob_vec, xlab = "Loop Length", ylab = "Probability ", main = "Probability Distribution for the Occurance \n of one or more Loop Lengths per Simulation", cex.main = 1)

#output the probability distribution vector: 
cat("your probability vector for producing at least one loop of length i: \n", prob_vec_y, "\n")

## For loop lengths greater than n only one may occur at a time as the total loop lengths must sum to 2n. 
## Therefore P(loop length >n occurs at least once ) == P(loop length >n occurs once ) and is mutually exclusive with 
## all other loop lengths >n . If we take 1-sum(p(loop length >n)) we are left with the probability of no loops longer
## than n occuring in the simulation. In this case Strategy 1 is guaranteed success. 

cat( "Probability of Strategy 1 succeeding: \n", 1 - sum(prob_vec[(n+1):(2*n)]), "\n this is also the probability of the maximum loop length being less than n")


