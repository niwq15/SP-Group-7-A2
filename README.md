# SP-Group-7-A2

This repository was created to enable collaborative work on an academic project for the class
Statistical Programming with the University of Edinburgh.

The project tasked us with creating a model of the famous 100 Prisoners Problem. To create our model
we used R as required by the class.

The 100 Prisoner Problem is an experiment regarding probabilties.
There are 100 Prisoners with numbers 1 through 100 and boxes (labelled 1 to 100) with cards in them
(also numbered 1 through to 100). Each prisoner searches 50 boxes individually and cannot communicate
with the other prisoners. If a prisoner finds their card within opening 50 boxes then they are freed.
Three strategies are commonly used to complete this problem.

Strategy 1:
Each prisoner starts with the box labelled with their number. If they find their number they are done.
If they don't find their number in that box then they go to the box with the number they found in the
box. Repeat until the prisoner finds their card or until the prisoner opens 50 boxes.

Strategy 2:
Like strategy 1 but we start with a random box.

Strategy 3:
Select 50 random boxes to open.

We generalise the 100 Prisoners Problem to the 2n Prisoners Problem in our model. With 2n boxes, cards
and opening up to n boxes. We simulate the problem nreps times (usually 10,000). We find the probabilties 
under each strategy for any one prisoner finding their card and the probabilties under each strategy for
all prisoners finding their cards.

For any n:
 - For strategy 1 we find that the probability of a prisoner finding their number is about 0.5 and that all 
   prisoners find their numbers is about 0.31.
 - For strategy 2 we find that the probability of a prisoner finding their number is about 0.4 and that all
   prisoners find their numbers is about 0.00.
 - For strategy 3 we find that the probability of a prisoner finding their number is about 0.5 and that all
   prisoners find their numbers is about 0.00.
   
 Strategy 1 works so well because of the presence of loops. If a prisoner starts with their number the loop
 created by the strategy guarantees their card is in a box in that loop. If all the loops in a set of boxes
 have length less than n then all prisoners find their cards. As such, the probabilities of all loops being
 less than n coincides with the probability that all prisoners escape.
 
 Strategy 2 is so poor for the same reason strategy 1 is so good. If the prisoner starts with a box that is
 part of a loop that does not contain their number, they will never find their number. They will loop through
 their chosen loop again and again until they open n boxes. They continue to open the same boxes if the loop
 has length less than n.
 
 Strategy 3 is okay for any one prisoner escaping but not for them all. Since the selection of boxes is random
 they have a 1/2 chance of finding their card. For 2n prisoners to all find their cards, the probability is
 (1/2)^2n. For n of any decent size this is incredibly low.
 
 We go on to investigate the probabilties of loop lengths as they are what makes the problem interesting.
 We find the probabilities that a loop of length k is present in any given simulation, and find the probability
 that the maximum loop length is n coincides exactly with the probability that all prisoners escape using
 strategy 1.
