#### Team Members ####
## Fatima Kasenally (S2443602); Wenqi Ni (s1792412); Cameron Allan (S1748084)

#### OVERVIEW ####

#### Team Member Contributions ####

#setwd("")

#strategy 2 - start with a random box - follow the chain 

#setting parameter values 
n <- 50 

#generate a list of integers between1 and 100 randmobly ordered - this simulates the cards in the box 
  #index of boxes is the box number 
  #number listed is the card number
boxes <- sample(1 : (2*n), 2*n , replace=FALSE)
#this makes the list of numbered prisoners
prisoners <- 1:(2*n) 


#Prisoner k uses  strategy 2 to find their card
Strat_2 <- function(k){
  #this makes a list to store which cards have been picked 
  cards_picked <- rep(0, n)
  #select the 1st box randomly and stores the value inside as the 1st card
  cards_picked[1] <- sample(boxes, 1)
  #select the remaining n-1 boxes according to the card inside 
  for (b in 2:n ) {
    #the number of the bth box picked takes the value from the (b-1)th card
    cards_picked[b] <- boxes[cards_picked[b-1]]
  }#end for loop
  
  #if prisoner k has picked card 'k' then they have successfully obtained the correct card
  if (k %in%  cards_picked) {
    correct_card <- TRUE
  }else{
    correct_card <- FALSE 
  }
  return(correct_card)
###Uncomment to see what's going on
# cat(correct_card, "\n")
# cat('prisoner picked cards: ', cards_picked, "\n")
#  cat('prisoner number: ' , k, "\n")
} #end function


Strat_2(5)



#finds the frequentist probability of a single prisoner finding his card amoungst the boxes
pone <- function (n, k, strategy, nreps) {
  # store the number of successful attempts 
  no_successes <- rep ( 0, nreps )
  
  for (i in 1:nreps) {
    #resimulate box set up
    boxes <- sample(1 : (2*n), 2*n , replace=FALSE)
    
    #stores a one/True for every successful attempt and zero/False for failure
    no_successes[i] <- strategy(k)
  }
  #function returns frequentist probability of success  
  return(sum(no_successes)/nreps)
}


#find the probability of all 2n prisoners finding their cards using each strategy
#prisoners do not communicate between turns therefore the probabilities are independent (and can be multiplied)
#pall <- function ( n, strategy ,nreps ) {
  
 # #set initial probability factor to 1
 # prob_success <- 1
 # 
 # #loop through each prisoner, applying pone each time 
 # for (k in 1:(2*n)) {
 #   #multiply the individual probs together to find the net probability of success
 #   prob_success <- prob_success*Pone(n,k,strategy,nreps)
#  }
 # #output the total probability 
 # return(prob_success)
#}
pone(100,50,Strat_2,1000)
pall(10, Strat_2, 100)




