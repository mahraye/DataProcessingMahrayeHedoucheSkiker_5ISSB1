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


#----------Find the 5 sport which got the more medals--------------------------------------------------------#
  #number of medals by sports
  Medals<- count(filter(WinterOGDataSet80, Medal != 'NA')$Sport)
  
  # plotting the medals by sports
  ggplot(Medals, aes(x = x, y = freq, fill = x, label = round(freq))) + 
    geom_histogram(stat = "identity") +
    xlab("Sports") +
    ylab("number of medals") +
    guides(fill=guide_legend("Sports")) +
    ggsave(filename = "/Users/abdelmahraye/Documents/BigData/plots/medals.pdf")




#--------- Calculation  medals/ participants ratio (are the number of medals influented by the number of participants ?) ----------#
  #Number of participants for each sport
  Participants <- count(WinterOGDataSet80$Sport)
  #number of medals by sports
  Medals<- count(filter(WinterOGDataSet80, Medal != 'NA')$Sport)
  #Ratio medals/participant
  percent_medal <- (Medals$freq/Participants$freq)*100 
  print(percent_medal)
  # bind the percentages and the sports (percentage by sports)
  bind <- cbind(Participants, percent_medal) 
  print(bind)
  # top five
  topFive <- arrange(bind, -df$percent_medal)[1:5,] 
  
  
  ggplot(topFive, aes(x = x, y = percent_medal, fill = x, label = round(percent_medal))) + 
    geom_histogram(stat = "identity") +
    xlab("Sports") +
    ylab("% medals/participants") +
    guides(fill=guide_legend("Sports")) +
    ggsave(filename = "/Users/abdelmahraye/Documents/BigData/plots/ratioMedalParticipants.pdf")

  
  #--------- What are the ages in top sports for the women and the ages ? ----------#
  
  #men mean age:
  topFiveM<- filter(filter(WinterOGDataSet80, Sport %in% topFive$x), Sex == "M")
  groupby <- topFiveM %>%  #group by sports
    group_by(Sport) %>%
    select(Age)
  meanAgesM <- ddply(na.omit(groupby), .(Sport), summarise, mean = mean(Age))
  
  ggplot(meanAgesM, aes(x = Sport, y = mean, fill = Sport, label = round(mean))) + 
    geom_histogram(stat = "identity") +
    xlab("Sports") +
    ylab("% medals/participants") +
    guides(fill=guide_legend("Sports")) +
    ggsave(filename = "/Users/abdelmahraye/Documents/BigData/plots/AgesM.pdf")
  
  #women mean age:
  topFiveF<- filter(filter(WinterOGDataSet80, Sport %in% topFive$x), Sex == "F")
  groupby <- topFiveF %>%  #group by sports
    group_by(Sport) %>%
    select(Age)
  meanAgesF <- ddply(na.omit(groupby), .(Sport), summarise, mean = mean(Age))
  
  ggplot(meanAgesF, aes(x = Sport, y = mean, fill = Sport, label = round(mean))) + 
    geom_histogram(stat = "identity") +
    xlab("Sports") +
    ylab("% medals/participants") +
    guides(fill=guide_legend("Sports")) +
    ggsave(filename = "/Users/abdelmahraye/Documents/BigData/plots/AgesF.pdf")
  
  
  