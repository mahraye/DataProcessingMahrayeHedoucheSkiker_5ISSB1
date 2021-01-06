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
    ggsave(filename = "/Users/abdelmahraye/Documents/BigData/plots/medalsWinter.pdf")




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
    ggsave(filename = "/Users/abdelmahraye/Documents/BigData/plots/ratioMedalParticipantsWinter.pdf")


    
  
#--------- What are the ages in top sports for the women and men ? ----------#
  
    #men mean age:
    topFiveM<- filter(filter(WinterOGDataSet80, Sport %in% topFive$x), Sex == "M")
    groupby <- topFiveM %>%  #group by sports
      group_by(Sport) %>%
      select(Age)
    meanAgesM <- ddply(na.omit(groupby), .(Sport), summarise, mean = mean(Age))
    
    ggplot(meanAgesM, aes(x = Sport, y = mean, fill = Sport, label = round(mean))) + 
      geom_bar(width = 3, stat = "identity") +
      coord_polar("y", start=0) +
      xlab("Sports") +
      ylab("% mean age men") +
      guides(fill=guide_legend("Sports")) +
      ggsave(filename = "/Users/abdelmahraye/Documents/BigData/plots/AgesMWinter.pdf")
    
    #women mean age:
    topFiveF<- filter(filter(WinterOGDataSet80, Sport %in% topFive$x), Sex == "F")
    groupby <- topFiveF %>%  #group by sports
      group_by(Sport) %>%
      select(Age)
    meanAgesF <- ddply(na.omit(groupby), .(Sport), summarise, mean = mean(Age))
    
    ggplot(meanAgesF, aes(x = Sport, y = mean, fill = Sport, label = round(mean))) + 
      geom_bar(width = 3, stat = "identity") +
     coord_polar("y", start=0) +
      xlab("Sports") +
      ylab("mean age women") +
      guides(fill=guide_legend("Sports")) +
      ggsave(filename = "/Users/abdelmahraye/Documents/BigData/plots/AgesFWinter.pdf")
  
  
    #--------- women/men participant evolution by year ----------#
    
    #Number of participants for each sport
    ParticipantsT <- count(filter(WinterOGDataSet80, (Sex == 'F'||Sex == 'M'))$Year) #Total Participants by Year
    #number of woman by year
    Women<-count(filter(WinterOGDataSet80, Sex == 'F')$Year)
    #percentages
    percent_Woman <- (Women$freq/ParticipantsT$freq)*100 
    # bind the percentages and the years (percentage by years)
    WomanPart <- cbind(ParticipantsT, percent_Woman) 
    
    men<-count(filter(WinterOGDataSet80, Sex == 'M')$Year)
    #percentages
    percent_man <- (men$freq/ParticipantsT$freq)*100 
    # bind the percentages and the years (percentage by years)
    manPart <- cbind(ParticipantsT, percent_man) 
    
    plot<- list()
    
    p <- ggplot(data = WomanPart, aes(x = x, y = percent_Woman, color=percent_Woman, group = 1)) 
    
    p2 <- ggplot(data = manPart, aes(x = x, y = percent_man,color=percent_man ,group = 1)) 
    # Line plot basique avec des points
    
    
    plot[[1]] <- p + geom_line() + geom_point()  + xlab("Year") + ylab("Woman Percentage")
    plot[[2]] <-  p2 + geom_line() + geom_point()  + xlab("Year") + ylab("man Percentage")
    
    
    
    figure <- ggarrange(plot[[1]], plot[[2]],
                        ncol = 1, nrow = 3)
    ggsave(file="/Users/abdelmahraye/Documents/BigData/plots/Women_menWinter.pdf",figure)

    
    
    
    
    
    
#-------Summer Olympic Games--------------#
    
    
    # Get the Winter Olympic Games from 1980 to nowadays
    SummerOGs <- dplyr::filter(MyDataSetOG,  Season == "Summer")
    SummerDataSet80 <- dplyr::filter(WinterOGs, Year >= 1980)
    
    
    #----------Find the 5 sport which got the more medals--------------------------------------------------------#
    #number of medals by sports
    Medals<- count(filter(SummerDataSet80, Medal != 'NA')$Sport)
    
    # plotting the medals by sports
    ggplot(Medals, aes(x = x, y = freq, fill = x, label = round(freq))) + 
      geom_histogram(stat = "identity") +
      xlab("Sports") +
      ylab("number of medals") +
      guides(fill=guide_legend("Sports")) +
      ggsave(filename = "/Users/abdelmahraye/Documents/BigData/plots/medalsWinter.pdf")
    
    
    
    
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
      ggsave(filename = "/Users/abdelmahraye/Documents/BigData/plots/ratioMedalParticipantsWinter.pdf")
    
    
    
    
    #--------- What are the ages in top sports for the women and men ? ----------#
    
    #men mean age:
    topFiveM<- filter(filter(WinterOGDataSet80, Sport %in% topFive$x), Sex == "M")
    groupby <- topFiveM %>%  #group by sports
      group_by(Sport) %>%
      select(Age)
    meanAgesM <- ddply(na.omit(groupby), .(Sport), summarise, mean = mean(Age))
    
    ggplot(meanAgesM, aes(x = Sport, y = mean, fill = Sport, label = round(mean))) + 
      geom_bar(width = 3, stat = "identity") +
      coord_polar("y", start=0) +
      xlab("Sports") +
      ylab("% mean age men") +
      guides(fill=guide_legend("Sports")) +
      ggsave(filename = "/Users/abdelmahraye/Documents/BigData/plots/AgesMWinter.pdf")
    
    #women mean age:
    topFiveF<- filter(filter(WinterOGDataSet80, Sport %in% topFive$x), Sex == "F")
    groupby <- topFiveF %>%  #group by sports
      group_by(Sport) %>%
      select(Age)
    meanAgesF <- ddply(na.omit(groupby), .(Sport), summarise, mean = mean(Age))
    
    ggplot(meanAgesF, aes(x = Sport, y = mean, fill = Sport, label = round(mean))) + 
      geom_bar(width = 3, stat = "identity") +
      coord_polar("y", start=0) +
      xlab("Sports") +
      ylab("mean age women") +
      guides(fill=guide_legend("Sports")) +
      ggsave(filename = "/Users/abdelmahraye/Documents/BigData/plots/AgesFWinter.pdf")
    
    
    #--------- women/men participant evolution by year ----------#
    
    #Number of participants for each sport
    ParticipantsT <- count(filter(WinterOGDataSet80, (Sex == 'F'||Sex == 'M'))$Year) #Total Participants by Year
    #number of woman by year
    Women<-count(filter(WinterOGDataSet80, Sex == 'F')$Year)
    #percentages
    percent_Woman <- (Women$freq/ParticipantsT$freq)*100 
    # bind the percentages and the years (percentage by years)
    WomanPart <- cbind(ParticipantsT, percent_Woman) 
    
    men<-count(filter(WinterOGDataSet80, Sex == 'M')$Year)
    #percentages
    percent_man <- (men$freq/ParticipantsT$freq)*100 
    # bind the percentages and the years (percentage by years)
    manPart <- cbind(ParticipantsT, percent_man) 
    
    plot<- list()
    
    p <- ggplot(data = WomanPart, aes(x = x, y = percent_Woman, color=percent_Woman, group = 1)) 
    
    p2 <- ggplot(data = manPart, aes(x = x, y = percent_man,color=percent_man ,group = 1)) 
    # Line plot basique avec des points
    
    
    plot[[1]] <- p + geom_line() + geom_point()  + xlab("Year") + ylab("Woman Percentage")
    plot[[2]] <-  p2 + geom_line() + geom_point()  + xlab("Year") + ylab("man Percentage")
    
    
    
    figure <- ggarrange(plot[[1]], plot[[2]],
                        ncol = 1, nrow = 3)
    ggsave(file="/Users/abdelmahraye/Documents/BigData/plots/Women_menWinter.pdf",figure)
    
  