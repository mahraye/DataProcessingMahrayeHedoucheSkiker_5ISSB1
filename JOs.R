###############################################
###                Data processing            #######
### Abderrazzak Mahraye & Ilhame Hedouche & Hicham Skiker ###
###               5 ISS B1                 ###

################################
### Imports and Preparation ###
##############################

library(RCurl)
library(dplyr)    
library(plyr)
library(tidyverse)
library(ggpubr)

library(gridExtra)
library(dummies)
library(webshot)
library(highcharter) 
library(reshape2)

# Load the CSV file
MyDataSetOG <- read.csv("/Users/abdelmahraye/Documents/BigData/dataSet.csv")

# Get the Winter Olympic Games from 1980 to nowadays
WinterOGs <- dplyr::filter(MyDataSetOG,  Season == "Winter")
WinterOGDataSet80 <- dplyr::filter(WinterOGs, Year >= 1980)


#----------Find the 5 sport which got the more medals--------------------------------------------------------
#Number of participants for each sport
Participants <- count(WinterOGDataSet80$Sport)
#number of medals by sports
Medals<- count(filter(WinterOGDataSet80, Medal != 'NA')$Sport)

# plotting the medals by sports
ggplot(Medals, aes(x = x, y = freq, fill = x, label = round(freq))) + 
  geom_histogram(stat = "identity") +
  xlab("Sports") +
  ylab("number of medals") +
  guides(fill=guide_legend("Sports")) +
  geom_text(size = 3, position = position_stack(vjust = 0.85)) +
  ggsave(filename = "/Users/abdelmahraye/Documents/BigData/plots/medals.pdf")

