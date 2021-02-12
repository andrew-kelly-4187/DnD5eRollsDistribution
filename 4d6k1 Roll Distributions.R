##---------------------------------------##
## Libraries
##---------------------------------------##

# I have already installed these packages, hence no instance of `install.packages`

library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(FSA)

##---------------------------------------##
## Standard array
##---------------------------------------##

std = c(15, 14, 13, 12, 10, 8)
sum_std = sum(std)   # 72
mean_std = mean(std) # 12

##---------------------------------------##
## Generating an array of rolls
##---------------------------------------##

ninter = 5000     # Number of iterations

roll  = rep(0,6)       # Vector initialise
sums  = rep(0,ninter)  # Vector initialise
stat6 = rep(0,ninter) # Vector initialise
stat5 = rep(0,ninter) # Vector initialise
stat4 = rep(0,ninter) # Vector initialise
stat3 = rep(0,ninter) # Vector initialise
stat2 = rep(0,ninter) # Vector initialise
stat1 = rep(0,ninter) # Vector initialise

for (j in 1:ninter){
  roll = rep(0,6)     # Vector initialise
  for (i in 1:6){
    
    a <- sample(1:6,4, replace = TRUE) # Roll 4 dice
    b <- sort(a,decreasing=TRUE)       # Sort highest to lowest
    c <- b[1:3]                        # Only keep the top 3
    roll[i]=sum(c)                     # Add this to the i-th element of the roll vector
    
  }
  sortedroll = sort(roll, decreasing = TRUE)
  
  sums[j]=sum(sortedroll)
  stat6[j]=sortedroll[6]
  stat5[j]=sortedroll[5]
  stat4[j]=sortedroll[4]
  stat3[j]=sortedroll[3]
  stat2[j]=sortedroll[2]
  stat1[j]=sortedroll[1]
}

##---------------------------------------##
## Plotting the distribution
##---------------------------------------##

data.frame('Stat 1'=stat1, 'Stat 2'=stat2, 'Stat 3'=stat3, 'Stat 4'=stat4, 'Stat 5'=stat5, 'Stat 6'=stat6) %>% 
  gather(stat, roll) %>% 
  ggplot(aes(roll))+geom_bar(aes(y = 100*(..count..)/sum(..count..)))+
  facet_grid(facets = stat~., scales = "free")+
  scale_x_continuous(breaks = 3:18 ,limits = c(2.4,18.6))+
  theme_bw()+
  theme(axis.text.y=element_blank(),axis.ticks=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())+
  labs(x = "Value of the rolled stat", y = "", title = "Distributions resulting from rolling 4d6 (drop lowest)")

##---------------------------------------##
## Summary statistics
##---------------------------------------##

# Here are the median figures for the 6 ordered statistics
median(stat1) #16
median(stat2) #14         
median(stat3) #13
median(stat4) #12
median(stat5) #10
median(stat6) #9

# Using `perc` we can get probability outputs from the observed cumulative distributions
perc(stat1,15,dir="geq") #79% of top rolls are greater than or equal to 15
perc(stat2,14,dir="geq") #69% of second rolls are greater than or equal to 14
perc(stat3,13,dir="geq") #63% of third rolls are greater than or equal to 13
perc(stat4,12,dir="geq") #58% of fourth rolls are greater than or equal to 12
perc(stat5,10,dir="geq") #72% of fifth rolls are greater than or equal to 10
perc(stat6,8,dir="geq")  #70% of sixth rolls are greater than or equal to 8

perc(stat1,15,dir="gt") #55% of top rolls are strictly greater than 15
perc(stat2,14,dir="gt") #41% of second rolls are strictly greater than 14
perc(stat3,13,dir="gt") #35% of third rolls are strictly greater than 13
perc(stat4,12,dir="gt") #32% of fourth rolls are strictly greater than 12
perc(stat5,10,dir="gt") #49% of fifth rolls are strictly greater than 10
perc(stat6,8,dir="gt")  #51% of sixth rolls are strictly greater than 8
