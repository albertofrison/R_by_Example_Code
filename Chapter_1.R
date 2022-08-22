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

#####
# Example 1.8
# Social Class mobility and matrices to investigate the Class of a Child depending on the Class of his/her parents.
# Parents class are in the ROWS, the columns represent the probability of a child to transit to another class

probs <- c(.45,.05,.01,.48,.70,.50,.07,.25,.49)
P <- matrix (probs, nrow = 3, ncol = 3)

rownames(P) <- colnames(P) <- c("lower", "middle", "upper")
P # basically is easier to go up a class (or stay in that class) than to go down

rowSums(P) # sums the probability distribution by row, obtaining 1 in each ROW
apply (P, MARGIN =1, FUN=sum) # the same result can be obtained by using the apply function to P, specifiyng MARGING = 1 to tell that we are supplying the ROWS and the FUN=sum function

#what happens the next generation? P2 = P^2 = PP
P2 <- P %*% P # %*% matrix multiplication operator
P4 <- P2 %*% P2
P8 <- P4 %*% P4
P16 <- P8 %*% P8
P32 <- P16 %*% P16
P64 <- P32 %*% P32
P128 <- P64 %*% P64

#####
# Example 1.9
# US Arrests, a record of violent crimes arrests in the US - data per 100k residents for assault, murder, rape in the 50 States in 1973 - plus % of population living in urban areas. Used to learn about Data Frames
head(USArrests)
dim (USArrests) # dim tells you the number of rows and columns in the...
class(USArrests) # ...data frame

str(USArrests) # str gives you the dimension information as well as the type of data included in each column
any(is.na(USArrests)) # it makes sense to see if there is any NAs in the data frame

summary (USArrests) # There are no missing values. Median and Mean look similar across crimes except for Assault for which the Mean is larger than the Median indicating that data is positively skewed.

US_Assaults <- USArrests$Assault
hist (US_Assaults)

library(MASS) # needed to use truehist function
truehist(US_Assaults) # truehist makes easier to observe the skewed data
?truehist

hist (US_Assaults, probability = TRUE, breaks = "scott") # these parameters make hist look very similar to truehist

attach(USArrests) # makes easier to reference data frame variables and improves readability of the code
# with (USArrests, expr = {rape.pct = 100 * Rape / (Murder + Assault + Rape)}) # not 100% sure on how to use the with function

plot (UrbanPop, Murder) # plots a scatterplot Murder VS UrbanPop
pairs(USArrests) # plots a scatterplot for each pair of variables available within the data frame
# it looks that we have:
# a) a positive association between Murders and Assaults
# b) a weak association between Murders and Urban Population %
# c) a positive association between Rapes and Urban Population %
# we can investigate this further by calculating the COORELATION

cor (UrbanPop, Murder) # evaluates the correlation of a pair of variables
cor (USArrests)
# the analysis of the results tell us that:
# a) yes there is indeed a high correlation between Murders and Assaults 0.80
# b) a weak correlation betweeb Murders and Urban Population & 0.69
# c) a correlation between Rapes and Urban Population %  0.41
# d) also Rapes and Assaults have a strong correlation 0.66

detach(USArrests) # to detach

#####
# Example 1.13
# The url here below contains the first 5000 digits of PI - skip the first 60 rows to avoid to load
# we will use this dataset to calculate whether these 5000 digits are uniformly distributed

url <- "https://www.itl.nist.gov/div898/strd/univ/data/PiDigits.dat"
pidigits <- read.table (url, skip = 60)

table (pidigits)
pipropotions <- table (pidigits)/nrow(pidigits) # proportions are easier to read
class(pipropotions)

# the variance of a proportion is p * (1 - p) / n  - if the true proportions is 0.1 for each digit then the Standard Error se is:
sqrt(0.1 * 0.9 / 5000)

# since we are using the sample estimates of the proportions - substituting to 0.1 and 0.9 by the data in piproportions (a vector of 10, instead than a scalar)
# we can display the sample proportions plus / minus 2 standard errors

se.hat <- sqrt(pipropotions * (1 - pipropotions)/5000)
round (rbind (pipropotions, se.hat, pipropotions - 2 * se.hat, pipropotions + 2 * se.hat), 4) 

barplot(pipropotions, xlab="Digit", ylab = "Proportion")
abline(h  = 0.1) 

mean(pidigits$V1)
sd(pidigits$V1)
acf(pidigits$V1)

#####
# Exercises Section
# Ex 1,1
qnorm(0.90)

qnorm (c(1/4, 2/4, 3/4)) == qnorm(c(0.25, 0.5, 0.75))

a <- rnorm (100000)
mean (a)
sd(a)
plot (a)
truehist (a)


n.std <- function (x, mu=0, sd=1) {
  (1 / (sd * sqrt(2*pi))) *  exp ((-1/2 * ((x - mu)/sd )^2))
}

b <- seq(-4,4, by = 0.05)

c <- n.std(b)

plot(c)

# Ex 1.2
curve(dchisq(x,1),from = 0, to = 1)

# Ex 1.3
?dgamma
curve(dgamma(x,shape= 1, rate = 1), add = TRUE)
curve(dgamma(x,shape= 1, rate = 2), add = TRUE)
curve(dgamma(x,shape= 1, rate = 3), add = TRUE)

# Ex 1.4
k = 0:12 # from 0 ones to 12 ones
n = 12 # number of trials
p = 1/6 # probability to get a one in a single trow and not 1/3
q = 1-p
P_x1 = choose(n,k)*p^k*(1-p)^(n-k)
P_x2 = dbinom(k,n,p)
plot(k, P_x1)
plot(k, P_x2)

# test
1 - (5/6)^12
P_x2 = dbinom(0,n,p)
1-dbinom(0,n,p)

# Ex 1.5
plot(cumsum(P_x1), x  = k)
1 - pbinom(7,n,p)
1 - pbinom(k,n,p)[8]

# Ex 1.6
plot (candidates$Height_L, candidates$Height_W)

# Ex 1.7
n <- 10000
lambda <- 0.61

r_P1 <- rpois(n = n, lambda = lambda)

mean(r_P1)
var(r_P1)
sample <- table(r_P1)
sample_p <- sample / n
sample_p

#comparing with theoretical
theoretical_p <- dpois (0:6, lambda)
cbind (round(theoretical_p,5),sample_p)
?cbind
