#####
# In this project I revise a selection of the best examples from the book "R by Example" by J. Albert M. Rizzo (2012, Springer)
# Chapter 1 Introduction

##### 
# Example 1.2 
# The HEIGHT of the candidates of the US Presidency Elections has been noted as a (good) predictor of the candidate's success
# Source: wikipedia...

library (rvest)
library (tidyverse)
url <- "https://en.wikipedia.org/wiki/Heights_of_presidents_and_presidential_candidates_of_the_United_States"

temp <- url %>% 
  read_html %>%
  html_nodes("table")

class(temp)
candidates <- html_table(temp[5])     # You need to navigate the temp list to actually learn which is the index of the table you want
candidates <- data.frame(candidates)  # let's make a dataframe from this list
head(candidates)  # just to check that everything is allright

# redefine candidates, select only the columns that matter, renaming them and calculating the height differences
# warning : numeric data (height) behaves strangely

candidates <-  data.frame (Year = candidates$Election,
                           Winner = candidates$Winnerin.Electoral.College,
                           Height_W =  as.numeric(as.character(substr(candidates$Height.1,1,3))),
                           Loser = candidates$Main.opponent.s.during.election,
                           Height_L = as.numeric(as.character(substr(candidates$Height.3,1,3))),
                           Difference = as.numeric(as.character(substr(candidates$Height.1,1,3))) - as.numeric(as.character(substr(candidates$Height.3,1,3))), stringsAsFactors = FALSE)

head (candidates) # cool! it works

# making a sub_ dataframe with 1900 only elections (aka cheating)
candidates_1900 <- candidates %>% 
  filter (!is.na(Difference) & as.numeric(Year) >= 1900)

candidates_1900 %>%
    mutate (pos_Difference = Difference >=0) %>%                                                  # to have the option to color the bar according to winner\loser
      ggplot (aes (x = Year, y = Difference, fill = pos_Difference)) +
      geom_bar(stat = "identity") +
      geom_text (aes(label = Winner), colour = "black", size = 3, angle = 270, hjust = 0) +       # adding label with winner
      theme(axis.text.x=element_text(angle=90, hjust=1), legend.position = "")                    # flipping x axis and removing legend

  
taller.won <- candidates_1900$Height_W > candidates_1900$Height_L
table(taller.won)

#####
# Example 1.3
# Horse kicks Deaths recorded by 19th century Prussian Officers - 10 years periord - 20 cavalry corps

# entering the data
k <- c (0,1,2,3,4) # k = number of deaths
x <- c (109,65,22,3,1) # x = number of k fatalities

# plotting the data
barplot (x, names.arg = k)

# relative frequency distribution
p <- x / sum(x) 

# Calculating Mean and Sample Variance
r <- sum (p*k) # r = mean = summation of k times relative frequency
v <- sum (x*(k-r)^2)/199 # v = sample variance

# Mean and Variance are similar, so maybe the Poisson distribution could fit the data
# see https://it.wikipedia.org/wiki/Distribuzione_di_Poisson
f <- r^k*exp(-r)/factorial(k) # calculating probabilities using the formula...
dpois(k, r) #... or the R embedded dpois function

floor (200*f) # expected counts for k = 0,1,2,3,4 respectively
x # observed counts

cbind (k,p,f) # column bind

# using the random Poisson generator to simulate 200 random observation
y <- rpois(200, lambda = 0.61)
y 
kicks <- table (y) # table with sample frequencies
kicks

kicks / 200 # samole proportions

# comparing this data with the Theoretica Poisson frequencies
Theoretical <- dpois  (0:6, lambda = 0.61)
Sample <- kicks / 200
cbind (Theoretical, Sample)

mean (y)
var (y)

#####
# Example 1.6
# Use functions as arguments - example with the Beta and Integrate functions
# see https://it.wikipedia.org/wiki/Funzione_beta_di_Eulero for the Euler's Beta Function

# the beta function can be used to calculate the integral along a sequence of x values
f <- function (x, a = 1, b = 1) {
  return (x^(a-1) * (1-x)^(b-1))
}

x <- seq (0,1,0.2) # a a sequence of numbers from 0 to 1 with a 0.2 step
f(x, a = 2, b = 2)

# numerical integration of f using the integrate function and the f function as a parameter
integrate (f, lower = 0, upper = 1, a = 2,b = 2) #0.1666667

# this was made to demonstrate the use of a function f inside another function (integrate), R has an embeded function to calculate this integral
beta (2,2) #0.1666667 

# the plot of the curve (for a = 2, b = 2) can be obtained by
curve (x*(1-x), from = 0, to = 1, ylab = "f(x)") # need to study more math...

?curve
