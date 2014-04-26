#usefull_commands to request information in the DF
#profile for a certain position
position <- 87
c <- subset(DF,as.numeric(levels(Data$Pos))[Data$Pos] == as.numeric(position))
c

#profile for a given name (Regex)
a <- subset(Data, grepl("EPP", Data$Name))
a
#profile for a given team
b <- subset(Data, grepl("OBERHOFFEN", Data$Team))
b #EPP was alone on his team


#best runner for a given gategory
#to access the different catagories
Categories <- Data$Cat_Name[!duplicated(Data$Cat_Name)]
Ranking_cat <- subset(Data, Data$Cat_name == "JUM")
for (i in 1:length(Categories)){
  Ranking_cat <- subset(Data, Data$Cat_name == Categories[i])
  
}

#3best runners per category
Categories <- Data$Cat_Name[!duplicated(Data$Cat_name)]
Bests <- NULL
for (i in 1:length(Categories)){
  Ranking_cat <- subset(Data, Data$Cat_name == Categories[i])
  Bests <- rbind(Bests, Ranking_cat[1:3,]) 
}
#if you now want to isolate a certain category
Categories #to check which ones are avaible
subset(Bests, Bests$Cat_Name == "V1M")

#3best runners per ages
Ages <- Data$Age[!duplicated(Data$Age)]
Bests_Ages <- NULL
for (i in 1:length(Ages)){
  Ranking_Age <- subset(Data, Data$Age == Ages[i])
  Bests_Ages <- rbind(Bests_Ages, Ranking_Age[1:3,]) 
  #attention we should ensure there's 3 best cases for each age
}
#if you now want to isolate a certain category
Ages #to check which ones are avaible
subset(Bests_Ages, as.numeric(levels(Bests_Ages$Age))[Bests_Ages$Age] == as.numeric(23))

#to plot performances by category by ages
library(lattice)
xyplot(levels(Data$Time) ~ as.numeric(levels(Data$Age)[Data$Age]) | Data$Cat_name, data = Data)