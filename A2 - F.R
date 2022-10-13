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





