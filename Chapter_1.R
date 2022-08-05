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
candidates <-  data.frame (Year = candidates$Election,
                           Winner = candidates$Winnerin.Electoral.College,
                           Height_W =  as.numeric(as.character(substr(candidates$Height.1,1,3))),
                           Loser = candidates$Main.opponent.s.during.election,
                           Height_L = as.numeric(as.character(substr(candidates$Height.3,1,3))),
                           Difference = as.numeric(as.character(substr(candidates$Height.1,1,3))) - as.numeric(as.character(substr(candidates$Height.3,1,3))), stringsAsFactors = FALSE)

typeof(as.numeric(candidates$Year))
as.numeric(candidates$Year) > 0

head (candidates) # cool! it works

mean(candidates$Height_W)

candidates %>%
  filter (!is.na(Difference) & as.numeric(Year) >= 1900) %>%
    mutate (pos_Difference = Difference >=0) %>%
      ggplot (aes (x = Year, y = Difference, fill = pos_Difference)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x=element_text(angle=90, hjust=1))
