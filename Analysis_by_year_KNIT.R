###############
#Getting ready#
###############

rm(list=ls())
setwd("Y:/Clinical Services/Paul/Youth Health Councils project/Schools survey/R/")
raw.data <- read.csv("All_schools.csv", header=F)

require(ggplot2)
require(grid)
require(stringr)
require(xtable)
require(pander)
###############


################
##Schools list##
################

#"Alfriston College"
#"Auckland Seventh-day Adventist High School"
#"Edgewater College"
#"James Cook High School"
#"Mangere College"
#"St Kentigern College"

# OR

#"All schools"

################


################
#School chooser#
################

#Enter the school for analysis here (exactly as in list above, without the starting #)
the.school <- "Edgewater College"


#Grab school data
if (the.school=="All schools") {
  school.data <- raw.data[(raw.data$V10!="Which school do you attend?"),10:102]
  
} else {school.data <- raw.data[(raw.data$V10==the.school),10:102] }
rm(raw.data)

school.data$V11 <- as.numeric(str_sub(school.data$V11,start=5, end=-1))

school.data$V11[school.data$V11==9|school.data$V11==10] <- NA 

#Optional limiter on years (e.g. St Kents Yrs 10-12, ASDAH, Yrs 9,10,12)
#school.data <- school.data[school.data$V11==11|school.data$V11==12|school.data$V11==13,]

#Name school data columns
colnames(school.data) <- c("School","Year", "UID1","UID2","UID3","UID4","UID5","General.health.good",
                           "Understand.be.healthy","Eat.nutritious.diet","Right.amount.of.food",
                           "Am.happy","Exercise.regularly","Understand.emotions","Spiritual.beliefs.important",
                           "Feel.valued.in.community","Connected.to.culture","Family.healthy.food",
                           "Family.healthy.living","Family.connected.culture","Family.understand.emotions",
                           "Family.regular.exercise","Friends.healthy.food","Friends.regular.exercise",
                           "Friends.understand.emotions","Friends.be.happy","Friends.healthy.living",
                           "School.connected.culture","School.healthy.food","School.healthy.living",
                           "School.regular.exercise","School.be.happy","YHC.awareness","YHC.remember.project",
                           "YHC.school.healthier","HC.counsellor","HC.doctor","HC.school","HC.medical.centre","HC.hospital","HC.YOSS",
                           "HC.traditional","HC.alternative","HC.nowhere","HC.other",
                           "HC.last.visit","Barriers.knowledge","Barriers.appointment","Barriers.hoped.get.better",
                           "Barriers.person.unavailable","Barriers.not.bothered","Barriers.privacy","Barriers.uncomfortable",
                           "Barriers.unfriendly","Barriers.no.support.person","Barriers.no.transport","Barriers.scared",
                           "Barriers.cost.too.much","Barriers.no.fuss","Barriers.embarrassed","Barriers.other",
                           "Exercise.important","Exercise.dont","Exercise.not.last.7.days","Exercise.once",
                           "Exercise.twice","Exercise.three.times","Exercise.four.times","Exercise.five.times",
                           "Exercise.six.times","Exercise.seven.plus","Got.drunk","Driven.drunk","Been.driven.drunk.driver",
                           "Smoked.cigarettes","Used.drugs","Beaten.someone.up","Had.unprotected.sex",
                           "Concerned.someone.hurt.me","Bullied.by.peers","Harmed.self.on.purpose",
                           "Seriously.thought.suicide","People.look.up.to","Talk.family.about.feelings",
                           "Range.of.coping.strategies","Supported.by.friends","Know.where.help.in.community",
                           "Belong.at.my.school","Family.stands.by.me","Know.who.ask.about.health","Know.where.to.get.healthcare",
                           "Comfortable.about.getting.healthcare","Quick.to.seek.help")
  
attach(school.data)

#Graph info: colour schemes
yhc_15cols1<- c("#A50026","#D73027","#F46D43","#FDAE61","#FEE08B","#D9EF8B","#A6D96A","#66BD63","#1A9850","#A6DBA0","#D9F0D3","#E7D4E8","#C2A5CF","#9970AB","#762A83")
yhc_15cols2<- c("#F5F6CE","#F5ECCE","#F6E3CE","#F5D0A9","#F7BE81","#FAAC58","#FE9A2E","#FF8000","#DF7401","#B45F04","#8A4B08","#61380B","#61210B","#610B0B","#3B0B0B")
yhc_9cols2<- c("#E6EDEB","#CCEEEE","#A7DFE8","#77B9C4","#5D959E","#447D86","#2C585F","#143E44","#023D46")
yhc_9cols1<- c("#CEE3F6","#CED8F6","#CECEF6","#A9A9F5","#8181F7","#5858FA","#2E2EFE","#4000FF","#7401DF")
yhc_7cols1<- c("F0F9E8","CCEBC5","A8DDB5","7BCCC4","4EB3D3","2B8CBE","08589E")
yhc_5cols1<- c("#FFFFCC","#A1DAB4","#41B6C4","#2C7FB8","#253494")
yhc_5cols2<- c("#FFFFD4","#FED98E","#FE9929","#D95F0E","#993404")
yhc_5cols3<- c("#EDDDBE","#D6B98C","#C1835E","#872A28","#4A0018")
yhc_5cols4<- c("#F6EFF7","#BDC9E1","#67A9CF","#1C9099","#016C59")
yhc_5cols5<- c("#FEEBE2","#FBB4B9","#F768A1","#C51B8A","#7A0177")
yhc_4cols1<- c("#EDF8FB","#B3CDE3","#8C96C6","#88419D")
yhc_4cols2<- c("#FFFFD4","#FED98E","#FE9929","#CC4C02")
yhc_4cols3<- c("#F1EEF6","#BDC9E1","#74A9CF","#0570B0")
yhc_4cols4<- c("#F6EFF7","#BDC9E1","#67A9CF","#02818A")
yhc_4cols5<- c("#FFFFCC","#C2E699","#78C679","#238443")
yhc_3colsplus1 <- c("#DEEBF7","#9ECAE1","#3182BD","#636363")
yhc_3cols1 <- c("#FDE0DD","#FA9FB5","#C51B8A")
yhc_3cols2 <- c("#DEEBF7","#9ECAE1","#3182BD")
yhc_2cols1<- c("#FFEDA0","#FEB24C")
yhc_2cols2<- c("#ECE7F2","#A6BDDB")


###########
#SECTION 1# --> Personal statements about health
###########

#0101/General health
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little","Somewhat","Quite a bit","A lot"), levels= c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.General.health.good <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.General.health.good) <- "Agreement"
DF.General.health.good$Agreement <- ordered(levels(DF.General.health.good$Agreement), levels=c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.General.health.good$Year <- rep(DF.yrs,each=5)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$General.health.good <- as.character(temp.school.data$General.health.good)
  temp.school.data$General.health.good[temp.school.data$General.health.good==""] <- NA
  temp.school.data$General.health.good[temp.school.data$General.health.good=="N/A"] <- NA
  
    if (DF.yrs[i]=="All") { 
      temp.school.data <- school.data 
      temp.school.data$General.health.good <- as.character(temp.school.data$General.health.good)
      temp.school.data$General.health.good[temp.school.data$General.health.good==""] <- NA
      temp.school.data$General.health.good[temp.school.data$General.health.good=="N/A"] <- NA
      temp.school.data$General.health.good <- as.character(temp.school.data$General.health.good)
      }
  for (j in 1:length(levels(DF.General.health.good$Agreement))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.General.health.good$Agreement[j])
    DF.General.health.good$count[spot.count] <- length(na.omit(temp.school.data$General.health.good[curr.level==temp.school.data$General.health.good]))
    DF.General.health.good$sum[spot.count] <- length(na.omit(match(temp.school.data$General.health.good,levs)))
  }
}

DF.General.health.good$percent <- 100*(DF.General.health.good$count/DF.General.health.good$sum)
sums.DF.General.health.good <- DF.General.health.good$sum[seq(from=1,to=length(DF.General.health.good$sum), by=5)]

#0102/I understand how to be healthy 
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little","Somewhat","Quite a bit","A lot"), levels= c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Understand.be.healthy <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Understand.be.healthy) <- "Agreement"
DF.Understand.be.healthy$Agreement <- ordered(levels(DF.Understand.be.healthy$Agreement), levels=c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Understand.be.healthy$Year <- rep(DF.yrs,each=5)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Understand.be.healthy <- as.character(temp.school.data$Understand.be.healthy)
  temp.school.data$Understand.be.healthy[temp.school.data$Understand.be.healthy==""] <- NA
  temp.school.data$Understand.be.healthy[temp.school.data$Understand.be.healthy=="N/A"] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Understand.be.healthy <- as.character(temp.school.data$Understand.be.healthy)
    temp.school.data$Understand.be.healthy[temp.school.data$Understand.be.healthy==""] <- NA
    temp.school.data$Understand.be.healthy <- as.character(temp.school.data$Understand.be.healthy)
  }
  for (j in 1:length(levels(DF.Understand.be.healthy$Agreement))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Understand.be.healthy$Agreement[j])
    DF.Understand.be.healthy$count[spot.count] <- length(na.omit(temp.school.data$Understand.be.healthy[curr.level==temp.school.data$Understand.be.healthy]))
    DF.Understand.be.healthy$sum[spot.count] <- length(na.omit(match(temp.school.data$Understand.be.healthy,levs)))
  }
}

DF.Understand.be.healthy$percent <- 100*(DF.Understand.be.healthy$count/DF.Understand.be.healthy$sum)
sums.DF.Understand.be.healthy <- DF.Understand.be.healthy$sum[seq(from=1,to=length(DF.Understand.be.healthy$sum), by=5)]

#0103/I eat a nutritious diet Eat.nutritious.diet
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little","Somewhat","Quite a bit","A lot"), levels= c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Eat.nutritious.diet <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Eat.nutritious.diet) <- "Agreement"
DF.Eat.nutritious.diet$Agreement <- ordered(levels(DF.Eat.nutritious.diet$Agreement), levels=c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Eat.nutritious.diet$Year <- rep(DF.yrs,each=5)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Eat.nutritious.diet <- as.character(temp.school.data$Eat.nutritious.diet)
  temp.school.data$Eat.nutritious.diet[temp.school.data$Eat.nutritious.diet==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Eat.nutritious.diet <- as.character(temp.school.data$Eat.nutritious.diet)
    temp.school.data$Eat.nutritious.diet[temp.school.data$Eat.nutritious.diet==""] <- NA
    temp.school.data$Eat.nutritious.diet <- as.character(temp.school.data$Eat.nutritious.diet)
  }
  for (j in 1:length(levels(DF.Eat.nutritious.diet$Agreement))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Eat.nutritious.diet$Agreement[j])
    DF.Eat.nutritious.diet$count[spot.count] <- length(na.omit(temp.school.data$Eat.nutritious.diet[curr.level==temp.school.data$Eat.nutritious.diet]))
    DF.Eat.nutritious.diet$sum[spot.count] <- length(na.omit(match(temp.school.data$Eat.nutritious.diet,levs)))
  }
}

DF.Eat.nutritious.diet$percent <- 100*(DF.Eat.nutritious.diet$count/DF.Eat.nutritious.diet$sum)
sums.DF.Eat.nutritious.diet <- DF.Eat.nutritious.diet$sum[seq(from=1,to=length(DF.Eat.nutritious.diet$sum), by=5)]

#0104/I eat the right amount of food for my lifestyle
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little","Somewhat","Quite a bit","A lot"), levels= c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Right.amount.of.food <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Right.amount.of.food) <- "Agreement"
DF.Right.amount.of.food$Agreement <- ordered(levels(DF.Right.amount.of.food$Agreement), levels=c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Right.amount.of.food$Year <- rep(DF.yrs,each=5)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Right.amount.of.food <- as.character(temp.school.data$Right.amount.of.food)
  temp.school.data$Right.amount.of.food[temp.school.data$Right.amount.of.food==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Right.amount.of.food <- as.character(temp.school.data$Right.amount.of.food)
    temp.school.data$Right.amount.of.food[temp.school.data$Right.amount.of.food==""] <- NA
    temp.school.data$Right.amount.of.food <- as.character(temp.school.data$Right.amount.of.food)
  }
  for (j in 1:length(levels(DF.Right.amount.of.food$Agreement))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Right.amount.of.food$Agreement[j])
    DF.Right.amount.of.food$count[spot.count] <- length(na.omit(temp.school.data$Right.amount.of.food[curr.level==temp.school.data$Right.amount.of.food]))
    DF.Right.amount.of.food$sum[spot.count] <- length(na.omit(match(temp.school.data$Right.amount.of.food,levs)))
  }
}

DF.Right.amount.of.food$percent <- 100*(DF.Right.amount.of.food$count/DF.Right.amount.of.food$sum)
sums.DF.Right.amount.of.food <- DF.Right.amount.of.food$sum[seq(from=1,to=length(DF.Right.amount.of.food$sum), by=5)]


#0105/I am happy
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little","Somewhat","Quite a bit","A lot"), levels= c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Am.happy <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Am.happy) <- "Agreement"
DF.Am.happy$Agreement <- ordered(levels(DF.Am.happy$Agreement), levels=c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Am.happy$Year <- rep(DF.yrs,each=5)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Am.happy <- as.character(temp.school.data$Am.happy)
  temp.school.data$Am.happy[temp.school.data$Am.happy==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Am.happy <- as.character(temp.school.data$Am.happy)
    temp.school.data$Am.happy[temp.school.data$Am.happy==""] <- NA
    temp.school.data$Am.happy <- as.character(temp.school.data$Am.happy)
  }
  for (j in 1:length(levels(DF.Am.happy$Agreement))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Am.happy$Agreement[j])
    DF.Am.happy$count[spot.count] <- length(na.omit(temp.school.data$Am.happy[curr.level==temp.school.data$Am.happy]))
    DF.Am.happy$sum[spot.count] <- length(na.omit(match(temp.school.data$Am.happy,levs)))
  }
}

DF.Am.happy$percent <- 100*(DF.Am.happy$count/DF.Am.happy$sum)
sums.DF.Am.happy <- DF.Am.happy$sum[seq(from=1,to=length(DF.Am.happy$sum), by=5)]

#0106/I exercise regularly
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little","Somewhat","Quite a bit","A lot"), levels= c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Exercise.regularly <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Exercise.regularly) <- "Agreement"
DF.Exercise.regularly$Agreement <- ordered(levels(DF.Exercise.regularly$Agreement), levels=c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Exercise.regularly$Year <- rep(DF.yrs,each=5)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Exercise.regularly <- as.character(temp.school.data$Exercise.regularly)
  temp.school.data$Exercise.regularly[temp.school.data$Exercise.regularly==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Exercise.regularly <- as.character(temp.school.data$Exercise.regularly)
    temp.school.data$Exercise.regularly[temp.school.data$Exercise.regularly==""] <- NA
    temp.school.data$Exercise.regularly <- as.character(temp.school.data$Exercise.regularly)
  }
  for (j in 1:length(levels(DF.Exercise.regularly$Agreement))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Exercise.regularly$Agreement[j])
    DF.Exercise.regularly$count[spot.count] <- length(na.omit(temp.school.data$Exercise.regularly[curr.level==temp.school.data$Exercise.regularly]))
    DF.Exercise.regularly$sum[spot.count] <- length(na.omit(match(temp.school.data$Exercise.regularly,levs)))
  }
}

DF.Exercise.regularly$percent <- 100*(DF.Exercise.regularly$count/DF.Exercise.regularly$sum)
sums.DF.Exercise.regularly <- DF.Exercise.regularly$sum[seq(from=1,to=length(DF.Exercise.regularly$sum), by=5)]

#0107/I understand my emotions
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little","Somewhat","Quite a bit","A lot"), levels= c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Understand.emotions <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Understand.emotions) <- "Agreement"
DF.Understand.emotions$Agreement <- ordered(levels(DF.Understand.emotions$Agreement), levels=c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Understand.emotions$Year <- rep(DF.yrs,each=5)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Understand.emotions <- as.character(temp.school.data$Understand.emotions)
  temp.school.data$Understand.emotions[temp.school.data$Understand.emotions==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Understand.emotions <- as.character(temp.school.data$Understand.emotions)
    temp.school.data$Understand.emotions[temp.school.data$Understand.emotions==""] <- NA
    temp.school.data$Understand.emotions <- as.character(temp.school.data$Understand.emotions)
  }
  for (j in 1:length(levels(DF.Understand.emotions$Agreement))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Understand.emotions$Agreement[j])
    DF.Understand.emotions$count[spot.count] <- length(na.omit(temp.school.data$Understand.emotions[curr.level==temp.school.data$Understand.emotions]))
    DF.Understand.emotions$sum[spot.count] <- length(na.omit(match(temp.school.data$Understand.emotions,levs)))
  }
}

DF.Understand.emotions$percent <- 100*(DF.Understand.emotions$count/DF.Understand.emotions$sum)
sums.DF.Understand.emotions <- DF.Understand.emotions$sum[seq(from=1,to=length(DF.Understand.emotions$sum), by=5)]

#0108/Sprirtual beliefs are important to me
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little","Somewhat","Quite a bit","A lot"), levels= c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Spiritual.beliefs.important <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Spiritual.beliefs.important) <- "Agreement"
DF.Spiritual.beliefs.important$Agreement <- ordered(levels(DF.Spiritual.beliefs.important$Agreement), levels=c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Spiritual.beliefs.important$Year <- rep(DF.yrs,each=5)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Spiritual.beliefs.important <- as.character(temp.school.data$Spiritual.beliefs.important)
  temp.school.data$Spiritual.beliefs.important[temp.school.data$Spiritual.beliefs.important==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Spiritual.beliefs.important <- as.character(temp.school.data$Spiritual.beliefs.important)
    temp.school.data$Spiritual.beliefs.important[temp.school.data$Spiritual.beliefs.important==""] <- NA
    temp.school.data$Spiritual.beliefs.important <- as.character(temp.school.data$Spiritual.beliefs.important)
  }
  for (j in 1:length(levels(DF.Spiritual.beliefs.important$Agreement))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Spiritual.beliefs.important$Agreement[j])
    DF.Spiritual.beliefs.important$count[spot.count] <- length(na.omit(temp.school.data$Spiritual.beliefs.important[curr.level==temp.school.data$Spiritual.beliefs.important]))
    DF.Spiritual.beliefs.important$sum[spot.count] <- length(na.omit(match(temp.school.data$Spiritual.beliefs.important,levs)))
  }
}

DF.Spiritual.beliefs.important$percent <- 100*(DF.Spiritual.beliefs.important$count/DF.Spiritual.beliefs.important$sum)
sums.DF.Spiritual.beliefs.important <- DF.Spiritual.beliefs.important$sum[seq(from=1,to=length(DF.Spiritual.beliefs.important$sum), by=5)]

#0109/I feel valued in my community
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little","Somewhat","Quite a bit","A lot"), levels= c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Feel.valued.in.community <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Feel.valued.in.community) <- "Agreement"
DF.Feel.valued.in.community$Agreement <- ordered(levels(DF.Feel.valued.in.community$Agreement), levels=c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Feel.valued.in.community$Year <- rep(DF.yrs,each=5)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Feel.valued.in.community <- as.character(temp.school.data$Feel.valued.in.community)
  temp.school.data$Feel.valued.in.community[temp.school.data$Feel.valued.in.community==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Feel.valued.in.community <- as.character(temp.school.data$Feel.valued.in.community)
    temp.school.data$Feel.valued.in.community[temp.school.data$Feel.valued.in.community==""] <- NA
    temp.school.data$Feel.valued.in.community <- as.character(temp.school.data$Feel.valued.in.community)
  }
  for (j in 1:length(levels(DF.Feel.valued.in.community$Agreement))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Feel.valued.in.community$Agreement[j])
    DF.Feel.valued.in.community$count[spot.count] <- length(na.omit(temp.school.data$Feel.valued.in.community[curr.level==temp.school.data$Feel.valued.in.community]))
    DF.Feel.valued.in.community$sum[spot.count] <- length(na.omit(match(temp.school.data$Feel.valued.in.community,levs)))
  }
}

DF.Feel.valued.in.community$percent <- 100*(DF.Feel.valued.in.community$count/DF.Feel.valued.in.community$sum)
sums.DF.Feel.valued.in.community <- DF.Feel.valued.in.community$sum[seq(from=1,to=length(DF.Feel.valued.in.community$sum), by=5)]

#0110/I am connected to my culture
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little","Somewhat","Quite a bit","A lot"), levels= c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Connected.to.culture <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Connected.to.culture) <- "Agreement"
DF.Connected.to.culture$Agreement <- ordered(levels(DF.Connected.to.culture$Agreement), levels=c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Connected.to.culture$Year <- rep(DF.yrs,each=5)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Connected.to.culture <- as.character(temp.school.data$Connected.to.culture)
  temp.school.data$Connected.to.culture[temp.school.data$Connected.to.culture==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Connected.to.culture <- as.character(temp.school.data$Connected.to.culture)
    temp.school.data$Connected.to.culture[temp.school.data$Connected.to.culture==""] <- NA
    temp.school.data$Connected.to.culture <- as.character(temp.school.data$Connected.to.culture)
  }
  for (j in 1:length(levels(DF.Connected.to.culture$Agreement))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Connected.to.culture$Agreement[j])
    DF.Connected.to.culture$count[spot.count] <- length(na.omit(temp.school.data$Connected.to.culture[curr.level==temp.school.data$Connected.to.culture]))
    DF.Connected.to.culture$sum[spot.count] <- length(na.omit(match(temp.school.data$Connected.to.culture,levs)))
  }
}

DF.Connected.to.culture$percent <- 100*(DF.Connected.to.culture$count/DF.Connected.to.culture$sum)
sums.DF.Connected.to.culture <- DF.Connected.to.culture$sum[seq(from=1,to=length(DF.Connected.to.culture$sum), by=5)]



###########
#SECTION 2# ---> Influence of family, friends and schools
###########

#How much does your family encourage you to...
#0201/Family/Eat healthy food
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little bit","Some","Very much"), levels= c("Not at all","A little bit","Some","Very much"))
DF.Family.healthy.food <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Family.healthy.food) <- "Response"
DF.Family.healthy.food$Response <- ordered(levels(DF.Family.healthy.food$Response), levels=c("Not at all","A little bit","Some","Very much"))
DF.Family.healthy.food$Year <- rep(DF.yrs,each=4)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Family.healthy.food <- as.character(temp.school.data$Family.healthy.food)
  temp.school.data$Family.healthy.food[temp.school.data$Family.healthy.food==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Family.healthy.food <- as.character(temp.school.data$Family.healthy.food)
    temp.school.data$Family.healthy.food[temp.school.data$Family.healthy.food==""] <- NA
    temp.school.data$Family.healthy.food <- as.character(temp.school.data$Family.healthy.food)
  }
  for (j in 1:length(levels(DF.Family.healthy.food$Response))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Family.healthy.food$Response[j])
    DF.Family.healthy.food$count[spot.count] <- length(na.omit(temp.school.data$Family.healthy.food[curr.level==temp.school.data$Family.healthy.food]))
    DF.Family.healthy.food$sum[spot.count] <- length(na.omit(match(temp.school.data$Family.healthy.food,levs)))
  }
}

DF.Family.healthy.food$percent <- 100*(DF.Family.healthy.food$count/DF.Family.healthy.food$sum)
sums.DF.Family.healthy.food <- DF.Family.healthy.food$sum[seq(from=1,to=length(DF.Family.healthy.food$sum), by=4)]


#Family.healthy.food <- factor(Family.healthy.food, levels=c("Not at all","A little bit","Some","Very much"))
#DF.Family.healthy.food <-data.frame(tapply(Family.healthy.food,Family.healthy.food,length))
#colnames(DF.Family.healthy.food) <- "Count"
#DF.Family.healthy.food$levels <- factor(levels(Family.healthy.food), levels=c("Not at all","A little bit","Some","Very much"))
#DF.Family.healthy.food$sum <- as.numeric(sum(DF.Family.healthy.food$Count))
#DF.Family.healthy.food$percent <- 100*(DF.Family.healthy.food$Count/DF.Family.healthy.food$sum)
#graph.file <- paste("graphs/",the.school,"_0201family_eat.healthy.png",sep='')
#the.plot<-ggplot(DF.Family.healthy.food, aes(x=levels,y=percent,fill=levels))
#png(graph.file, width=6, height=5, units='in',res=300)
#print(the.plot + 
#        theme_classic()+      
#        theme(panel.margin = unit(c(0,0,0,0), "cm")) +
#        theme(plot.title = element_text(size = 14))+
#        ggtitle("How much does your family encourage you to eat healthy food?") + 
#        scale_fill_hue(l=40) +
#        geom_bar() + guides(fill=F) +
#        xlab("") +
#        ylab("Percent") +
#        ylim(-1,70) 
#)
#dev.off()
#sum.graphs <- c(sum.graphs,graph.file)

#0202/Family/Understand healthy ways of living
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little bit","Some","Very much"), levels= c("Not at all","A little bit","Some","Very much"))
DF.Family.healthy.living <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Family.healthy.living) <- "Response"
DF.Family.healthy.living$Response <- ordered(levels(DF.Family.healthy.living$Response), levels=c("Not at all","A little bit","Some","Very much"))
DF.Family.healthy.living$Year <- rep(DF.yrs,each=4)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Family.healthy.living <- as.character(temp.school.data$Family.healthy.living)
  temp.school.data$Family.healthy.living[temp.school.data$Family.healthy.living==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Family.healthy.living <- as.character(temp.school.data$Family.healthy.living)
    temp.school.data$Family.healthy.living[temp.school.data$Family.healthy.living==""] <- NA
    temp.school.data$Family.healthy.living <- as.character(temp.school.data$Family.healthy.living)
  }
  for (j in 1:length(levels(DF.Family.healthy.living$Response))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Family.healthy.living$Response[j])
    DF.Family.healthy.living$count[spot.count] <- length(na.omit(temp.school.data$Family.healthy.living[curr.level==temp.school.data$Family.healthy.living]))
    DF.Family.healthy.living$sum[spot.count] <- length(na.omit(match(temp.school.data$Family.healthy.living,levs)))
  }
}

DF.Family.healthy.living$percent <- 100*(DF.Family.healthy.living$count/DF.Family.healthy.living$sum)
sums.DF.Family.healthy.living <- DF.Family.healthy.living$sum[seq(from=1,to=length(DF.Family.healthy.living$sum), by=4)]


#0203/Family/Be connected to your culture
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little bit","Some","Very much"), levels= c("Not at all","A little bit","Some","Very much"))
DF.Family.connected.culture <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Family.connected.culture) <- "Response"
DF.Family.connected.culture$Response <- ordered(levels(DF.Family.connected.culture$Response), levels=c("Not at all","A little bit","Some","Very much"))
DF.Family.connected.culture$Year <- rep(DF.yrs,each=4)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Family.connected.culture <- as.character(temp.school.data$Family.connected.culture)
  temp.school.data$Family.connected.culture[temp.school.data$Family.connected.culture==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Family.connected.culture <- as.character(temp.school.data$Family.connected.culture)
    temp.school.data$Family.connected.culture[temp.school.data$Family.connected.culture==""] <- NA
    temp.school.data$Family.connected.culture <- as.character(temp.school.data$Family.connected.culture)
  }
  for (j in 1:length(levels(DF.Family.connected.culture$Response))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Family.connected.culture$Response[j])
    DF.Family.connected.culture$count[spot.count] <- length(na.omit(temp.school.data$Family.connected.culture[curr.level==temp.school.data$Family.connected.culture]))
    DF.Family.connected.culture$sum[spot.count] <- length(na.omit(match(temp.school.data$Family.connected.culture,levs)))
  }
}

DF.Family.connected.culture$percent <- 100*(DF.Family.connected.culture$count/DF.Family.connected.culture$sum)
sums.DF.Family.connected.culture <- DF.Family.connected.culture$sum[seq(from=1,to=length(DF.Family.connected.culture$sum), by=4)]


#0204/Family/Understand your emotions?
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little bit","Some","Very much"), levels= c("Not at all","A little bit","Some","Very much"))
DF.Family.understand.emotions <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Family.understand.emotions) <- "Response"
DF.Family.understand.emotions$Response <- ordered(levels(DF.Family.understand.emotions$Response), levels=c("Not at all","A little bit","Some","Very much"))
DF.Family.understand.emotions$Year <- rep(DF.yrs,each=4)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Family.understand.emotions <- as.character(temp.school.data$Family.understand.emotions)
  temp.school.data$Family.understand.emotions[temp.school.data$Family.understand.emotions==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Family.understand.emotions <- as.character(temp.school.data$Family.understand.emotions)
    temp.school.data$Family.understand.emotions[temp.school.data$Family.understand.emotions==""] <- NA
    temp.school.data$Family.understand.emotions <- as.character(temp.school.data$Family.understand.emotions)
  }
  for (j in 1:length(levels(DF.Family.understand.emotions$Response))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Family.understand.emotions$Response[j])
    DF.Family.understand.emotions$count[spot.count] <- length(na.omit(temp.school.data$Family.understand.emotions[curr.level==temp.school.data$Family.understand.emotions]))
    DF.Family.understand.emotions$sum[spot.count] <- length(na.omit(match(temp.school.data$Family.understand.emotions,levs)))
  }
}

DF.Family.understand.emotions$percent <- 100*(DF.Family.understand.emotions$count/DF.Family.understand.emotions$sum)
sums.DF.Family.understand.emotions <- DF.Family.understand.emotions$sum[seq(from=1,to=length(DF.Family.understand.emotions$sum), by=4)]


#0205/Family/Do regular exercise?
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little bit","Some","Very much"), levels= c("Not at all","A little bit","Some","Very much"))
DF.Family.regular.exercise <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Family.regular.exercise) <- "Response"
DF.Family.regular.exercise$Response <- ordered(levels(DF.Family.regular.exercise$Response), levels=c("Not at all","A little bit","Some","Very much"))
DF.Family.regular.exercise$Year <- rep(DF.yrs,each=4)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Family.regular.exercise <- as.character(temp.school.data$Family.regular.exercise)
  temp.school.data$Family.regular.exercise[temp.school.data$Family.regular.exercise==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Family.regular.exercise <- as.character(temp.school.data$Family.regular.exercise)
    temp.school.data$Family.regular.exercise[temp.school.data$Family.regular.exercise==""] <- NA
    temp.school.data$Family.regular.exercise <- as.character(temp.school.data$Family.regular.exercise)
  }
  for (j in 1:length(levels(DF.Family.regular.exercise$Response))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Family.regular.exercise$Response[j])
    DF.Family.regular.exercise$count[spot.count] <- length(na.omit(temp.school.data$Family.regular.exercise[curr.level==temp.school.data$Family.regular.exercise]))
    DF.Family.regular.exercise$sum[spot.count] <- length(na.omit(match(temp.school.data$Family.regular.exercise,levs)))
  }
}

DF.Family.regular.exercise$percent <- 100*(DF.Family.regular.exercise$count/DF.Family.regular.exercise$sum)
sums.DF.Family.regular.exercise <- DF.Family.regular.exercise$sum[seq(from=1,to=length(DF.Family.regular.exercise$sum), by=4)]


#How much do your friends encourage you to...
#0206/Friends/Eat healthy food
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little bit","Some","Very much"), levels= c("Not at all","A little bit","Some","Very much"))
DF.Friends.healthy.food <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Friends.healthy.food) <- "Response"
DF.Friends.healthy.food$Response <- ordered(levels(DF.Friends.healthy.food$Response), levels=c("Not at all","A little bit","Some","Very much"))
DF.Friends.healthy.food$Year <- rep(DF.yrs,each=4)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Friends.healthy.food <- as.character(temp.school.data$Friends.healthy.food)
  temp.school.data$Friends.healthy.food[temp.school.data$Friends.healthy.food==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Friends.healthy.food <- as.character(temp.school.data$Friends.healthy.food)
    temp.school.data$Friends.healthy.food[temp.school.data$Friends.healthy.food==""] <- NA
    temp.school.data$Friends.healthy.food <- as.character(temp.school.data$Friends.healthy.food)
  }
  for (j in 1:length(levels(DF.Friends.healthy.food$Response))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Friends.healthy.food$Response[j])
    DF.Friends.healthy.food$count[spot.count] <- length(na.omit(temp.school.data$Friends.healthy.food[curr.level==temp.school.data$Friends.healthy.food]))
    DF.Friends.healthy.food$sum[spot.count] <- length(na.omit(match(temp.school.data$Friends.healthy.food,levs)))
  }
}

DF.Friends.healthy.food$percent <- 100*(DF.Friends.healthy.food$count/DF.Friends.healthy.food$sum)
sums.DF.Friends.healthy.food <- DF.Friends.healthy.food$sum[seq(from=1,to=length(DF.Friends.healthy.food$sum), by=4)]



#0207/Friends/Do regular exercise
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little bit","Some","Very much"), levels= c("Not at all","A little bit","Some","Very much"))
DF.Friends.regular.exercise <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Friends.regular.exercise) <- "Response"
DF.Friends.regular.exercise$Response <- ordered(levels(DF.Friends.regular.exercise$Response), levels=c("Not at all","A little bit","Some","Very much"))
DF.Friends.regular.exercise$Year <- rep(DF.yrs,each=4)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Friends.regular.exercise <- as.character(temp.school.data$Friends.regular.exercise)
  temp.school.data$Friends.regular.exercise[temp.school.data$Friends.regular.exercise==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Friends.regular.exercise <- as.character(temp.school.data$Friends.regular.exercise)
    temp.school.data$Friends.regular.exercise[temp.school.data$Friends.regular.exercise==""] <- NA
    temp.school.data$Friends.regular.exercise <- as.character(temp.school.data$Friends.regular.exercise)
  }
  for (j in 1:length(levels(DF.Friends.regular.exercise$Response))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Friends.regular.exercise$Response[j])
    DF.Friends.regular.exercise$count[spot.count] <- length(na.omit(temp.school.data$Friends.regular.exercise[curr.level==temp.school.data$Friends.regular.exercise]))
    DF.Friends.regular.exercise$sum[spot.count] <- length(na.omit(match(temp.school.data$Friends.regular.exercise,levs)))
  }
}

DF.Friends.regular.exercise$percent <- 100*(DF.Friends.regular.exercise$count/DF.Friends.regular.exercise$sum)
sums.DF.Friends.regular.exercise <- DF.Friends.regular.exercise$sum[seq(from=1,to=length(DF.Friends.regular.exercise$sum), by=4)]


#0208/Friends/Understand your emotions
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little bit","Some","Very much"), levels= c("Not at all","A little bit","Some","Very much"))
DF.Friends.understand.emotions <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Friends.understand.emotions) <- "Response"
DF.Friends.understand.emotions$Response <- ordered(levels(DF.Friends.understand.emotions$Response), levels=c("Not at all","A little bit","Some","Very much"))
DF.Friends.understand.emotions$Year <- rep(DF.yrs,each=4)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Friends.understand.emotions <- as.character(temp.school.data$Friends.understand.emotions)
  temp.school.data$Friends.understand.emotions[temp.school.data$Friends.understand.emotions==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Friends.understand.emotions <- as.character(temp.school.data$Friends.understand.emotions)
    temp.school.data$Friends.understand.emotions[temp.school.data$Friends.understand.emotions==""] <- NA
    temp.school.data$Friends.understand.emotions <- as.character(temp.school.data$Friends.understand.emotions)
  }
  for (j in 1:length(levels(DF.Friends.understand.emotions$Response))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Friends.understand.emotions$Response[j])
    DF.Friends.understand.emotions$count[spot.count] <- length(na.omit(temp.school.data$Friends.understand.emotions[curr.level==temp.school.data$Friends.understand.emotions]))
    DF.Friends.understand.emotions$sum[spot.count] <- length(na.omit(match(temp.school.data$Friends.understand.emotions,levs)))
  }
}

DF.Friends.understand.emotions$percent <- 100*(DF.Friends.understand.emotions$count/DF.Friends.understand.emotions$sum)
sums.DF.Friends.understand.emotions <- DF.Friends.understand.emotions$sum[seq(from=1,to=length(DF.Friends.understand.emotions$sum), by=4)]

#0209/Friends/Be happy
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little bit","Some","Very much"), levels= c("Not at all","A little bit","Some","Very much"))
DF.Friends.be.happy <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Friends.be.happy) <- "Response"
DF.Friends.be.happy$Response <- ordered(levels(DF.Friends.be.happy$Response), levels=c("Not at all","A little bit","Some","Very much"))
DF.Friends.be.happy$Year <- rep(DF.yrs,each=4)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Friends.be.happy <- as.character(temp.school.data$Friends.be.happy)
  temp.school.data$Friends.be.happy[temp.school.data$Friends.be.happy==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Friends.be.happy <- as.character(temp.school.data$Friends.be.happy)
    temp.school.data$Friends.be.happy[temp.school.data$Friends.be.happy==""] <- NA
    temp.school.data$Friends.be.happy <- as.character(temp.school.data$Friends.be.happy)
  }
  for (j in 1:length(levels(DF.Friends.be.happy$Response))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Friends.be.happy$Response[j])
    DF.Friends.be.happy$count[spot.count] <- length(na.omit(temp.school.data$Friends.be.happy[curr.level==temp.school.data$Friends.be.happy]))
    DF.Friends.be.happy$sum[spot.count] <- length(na.omit(match(temp.school.data$Friends.be.happy,levs)))
  }
}

DF.Friends.be.happy$percent <- 100*(DF.Friends.be.happy$count/DF.Friends.be.happy$sum)
sums.DF.Friends.be.happy <- DF.Friends.be.happy$sum[seq(from=1,to=length(DF.Friends.be.happy$sum), by=4)]


#0210/Friends/Understand healthy ways of living
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little bit","Some","Very much"), levels= c("Not at all","A little bit","Some","Very much"))
DF.Friends.healthy.living <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Friends.healthy.living) <- "Response"
DF.Friends.healthy.living$Response <- ordered(levels(DF.Friends.healthy.living$Response), levels=c("Not at all","A little bit","Some","Very much"))
DF.Friends.healthy.living$Year <- rep(DF.yrs,each=4)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Friends.healthy.living <- as.character(temp.school.data$Friends.healthy.living)
  temp.school.data$Friends.healthy.living[temp.school.data$Friends.healthy.living==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Friends.healthy.living <- as.character(temp.school.data$Friends.healthy.living)
    temp.school.data$Friends.healthy.living[temp.school.data$Friends.healthy.living==""] <- NA
    temp.school.data$Friends.healthy.living <- as.character(temp.school.data$Friends.healthy.living)
  }
  for (j in 1:length(levels(DF.Friends.healthy.living$Response))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Friends.healthy.living$Response[j])
    DF.Friends.healthy.living$count[spot.count] <- length(na.omit(temp.school.data$Friends.healthy.living[curr.level==temp.school.data$Friends.healthy.living]))
    DF.Friends.healthy.living$sum[spot.count] <- length(na.omit(match(temp.school.data$Friends.healthy.living,levs)))
  }
}

DF.Friends.healthy.living$percent <- 100*(DF.Friends.healthy.living$count/DF.Friends.healthy.living$sum)
sums.DF.Friends.healthy.living <- DF.Friends.healthy.living$sum[seq(from=1,to=length(DF.Friends.healthy.living$sum), by=4)]

#How much does your school encourage you to...
#0211/School/Be connected with your culture
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little bit","Some","Very much"), levels= c("Not at all","A little bit","Some","Very much"))
DF.School.connected.culture <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.School.connected.culture) <- "Response"
DF.School.connected.culture$Response <- ordered(levels(DF.School.connected.culture$Response), levels=c("Not at all","A little bit","Some","Very much"))
DF.School.connected.culture$Year <- rep(DF.yrs,each=4)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$School.connected.culture <- as.character(temp.school.data$School.connected.culture)
  temp.school.data$School.connected.culture[temp.school.data$School.connected.culture==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$School.connected.culture <- as.character(temp.school.data$School.connected.culture)
    temp.school.data$School.connected.culture[temp.school.data$School.connected.culture==""] <- NA
    temp.school.data$School.connected.culture <- as.character(temp.school.data$School.connected.culture)
  }
  for (j in 1:length(levels(DF.School.connected.culture$Response))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.School.connected.culture$Response[j])
    DF.School.connected.culture$count[spot.count] <- length(na.omit(temp.school.data$School.connected.culture[curr.level==temp.school.data$School.connected.culture]))
    DF.School.connected.culture$sum[spot.count] <- length(na.omit(match(temp.school.data$School.connected.culture,levs)))
  }
}

DF.School.connected.culture$percent <- 100*(DF.School.connected.culture$count/DF.School.connected.culture$sum)
sums.DF.School.connected.culture <- DF.School.connected.culture$sum[seq(from=1,to=length(DF.School.connected.culture$sum), by=4)]

#0212/School/Eat healthy food
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little bit","Some","Very much"), levels= c("Not at all","A little bit","Some","Very much"))
DF.School.healthy.food <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.School.healthy.food) <- "Response"
DF.School.healthy.food$Response <- ordered(levels(DF.School.healthy.food$Response), levels=c("Not at all","A little bit","Some","Very much"))
DF.School.healthy.food$Year <- rep(DF.yrs,each=4)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$School.healthy.food <- as.character(temp.school.data$School.healthy.food)
  temp.school.data$School.healthy.food[temp.school.data$School.healthy.food==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$School.healthy.food <- as.character(temp.school.data$School.healthy.food)
    temp.school.data$School.healthy.food[temp.school.data$School.healthy.food==""] <- NA
    temp.school.data$School.healthy.food <- as.character(temp.school.data$School.healthy.food)
  }
  for (j in 1:length(levels(DF.School.healthy.food$Response))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.School.healthy.food$Response[j])
    DF.School.healthy.food$count[spot.count] <- length(na.omit(temp.school.data$School.healthy.food[curr.level==temp.school.data$School.healthy.food]))
    DF.School.healthy.food$sum[spot.count] <- length(na.omit(match(temp.school.data$School.healthy.food,levs)))
  }
}

DF.School.healthy.food$percent <- 100*(DF.School.healthy.food$count/DF.School.healthy.food$sum)
sums.DF.School.healthy.food <- DF.School.healthy.food$sum[seq(from=1,to=length(DF.School.healthy.food$sum), by=4)]

#0213/School/Understand healthy ways of living
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little bit","Some","Very much"), levels= c("Not at all","A little bit","Some","Very much"))
DF.School.healthy.living <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.School.healthy.living) <- "Response"
DF.School.healthy.living$Response <- ordered(levels(DF.School.healthy.living$Response), levels=c("Not at all","A little bit","Some","Very much"))
DF.School.healthy.living$Year <- rep(DF.yrs,each=4)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$School.healthy.living <- as.character(temp.school.data$School.healthy.living)
  temp.school.data$School.healthy.living[temp.school.data$School.healthy.living==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$School.healthy.living <- as.character(temp.school.data$School.healthy.living)
    temp.school.data$School.healthy.living[temp.school.data$School.healthy.living==""] <- NA
    temp.school.data$School.healthy.living <- as.character(temp.school.data$School.healthy.living)
  }
  for (j in 1:length(levels(DF.School.healthy.living$Response))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.School.healthy.living$Response[j])
    DF.School.healthy.living$count[spot.count] <- length(na.omit(temp.school.data$School.healthy.living[curr.level==temp.school.data$School.healthy.living]))
    DF.School.healthy.living$sum[spot.count] <- length(na.omit(match(temp.school.data$School.healthy.living,levs)))
  }
}

DF.School.healthy.living$percent <- 100*(DF.School.healthy.living$count/DF.School.healthy.living$sum)
sums.DF.School.healthy.living <- DF.School.healthy.living$sum[seq(from=1,to=length(DF.School.healthy.living$sum), by=4)]


#0214/School/Do regular exercise
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little bit","Some","Very much"), levels= c("Not at all","A little bit","Some","Very much"))
DF.School.regular.exercise <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.School.regular.exercise) <- "Response"
DF.School.regular.exercise$Response <- ordered(levels(DF.School.regular.exercise$Response), levels=c("Not at all","A little bit","Some","Very much"))
DF.School.regular.exercise$Year <- rep(DF.yrs,each=4)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$School.regular.exercise <- as.character(temp.school.data$School.regular.exercise)
  temp.school.data$School.regular.exercise[temp.school.data$School.regular.exercise==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$School.regular.exercise <- as.character(temp.school.data$School.regular.exercise)
    temp.school.data$School.regular.exercise[temp.school.data$School.regular.exercise==""] <- NA
    temp.school.data$School.regular.exercise <- as.character(temp.school.data$School.regular.exercise)
  }
  for (j in 1:length(levels(DF.School.regular.exercise$Response))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.School.regular.exercise$Response[j])
    DF.School.regular.exercise$count[spot.count] <- length(na.omit(temp.school.data$School.regular.exercise[curr.level==temp.school.data$School.regular.exercise]))
    DF.School.regular.exercise$sum[spot.count] <- length(na.omit(match(temp.school.data$School.regular.exercise,levs)))
  }
}

DF.School.regular.exercise$percent <- 100*(DF.School.regular.exercise$count/DF.School.regular.exercise$sum)
sums.DF.School.regular.exercise <- DF.School.regular.exercise$sum[seq(from=1,to=length(DF.School.regular.exercise$sum), by=4)]

#0215/School/Be happy
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little bit","Some","Very much"), levels= c("Not at all","A little bit","Some","Very much"))
DF.School.be.happy <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.School.be.happy) <- "Response"
DF.School.be.happy$Response <- ordered(levels(DF.School.be.happy$Response), levels=c("Not at all","A little bit","Some","Very much"))
DF.School.be.happy$Year <- rep(DF.yrs,each=4)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$School.be.happy <- as.character(temp.school.data$School.be.happy)
  temp.school.data$School.be.happy[temp.school.data$School.be.happy==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$School.be.happy <- as.character(temp.school.data$School.be.happy)
    temp.school.data$School.be.happy[temp.school.data$School.be.happy==""] <- NA
    temp.school.data$School.be.happy <- as.character(temp.school.data$School.be.happy)
  }
  for (j in 1:length(levels(DF.School.be.happy$Response))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.School.be.happy$Response[j])
    DF.School.be.happy$count[spot.count] <- length(na.omit(temp.school.data$School.be.happy[curr.level==temp.school.data$School.be.happy]))
    DF.School.be.happy$sum[spot.count] <- length(na.omit(match(temp.school.data$School.be.happy,levs)))
  }
}

DF.School.be.happy$percent <- 100*(DF.School.be.happy$count/DF.School.be.happy$sum)
sums.DF.School.be.happy <- DF.School.be.happy$sum[seq(from=1,to=length(DF.School.be.happy$sum), by=4)]

###########
#SECTION 3# ---> Youth Health Councils
###########

#0301/I know about the Youth Health Council in my school 
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("No","Yes"), levels= c("No","Yes"))
DF.YHC.awareness <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.YHC.awareness) <- "Level"
DF.YHC.awareness$Level <- ordered(levels(DF.YHC.awareness$Level), levels=c("No","Yes"))
DF.YHC.awareness$Year <- rep(DF.yrs,each=2)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$YHC.awareness <- as.character(temp.school.data$YHC.awareness)
  temp.school.data$YHC.awareness[temp.school.data$YHC.awareness==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$YHC.awareness <- as.character(temp.school.data$YHC.awareness)
    temp.school.data$YHC.awareness[temp.school.data$YHC.awareness==""] <- NA
    temp.school.data$YHC.awareness <- as.character(temp.school.data$YHC.awareness)
  }
  for (j in 1:length(levels(DF.YHC.awareness$Level))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.YHC.awareness$Level[j])
    DF.YHC.awareness$count[spot.count] <- length(na.omit(temp.school.data$YHC.awareness[curr.level==temp.school.data$YHC.awareness]))
    DF.YHC.awareness$sum[spot.count] <- length(na.omit(match(temp.school.data$YHC.awareness,levs)))
  }
}

DF.YHC.awareness$percent <- 100*(DF.YHC.awareness$count/DF.YHC.awareness$sum)
sums.DF.YHC.awareness <- DF.YHC.awareness$sum[seq(from=1,to=length(DF.YHC.awareness$sum), by=2)]


#0302/I can remember a project that the Youth Health Council has run in my school 
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("No","Yes"), levels= c("No","Yes"))
DF.YHC.remember.project <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.YHC.remember.project) <- "Level"
DF.YHC.remember.project$Level <- ordered(levels(DF.YHC.remember.project$Level), levels=c("No","Yes"))
DF.YHC.remember.project$Year <- rep(DF.yrs,each=2)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$YHC.remember.project <- as.character(temp.school.data$YHC.remember.project)
  temp.school.data$YHC.remember.project[temp.school.data$YHC.remember.project==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$YHC.remember.project <- as.character(temp.school.data$YHC.remember.project)
    temp.school.data$YHC.remember.project[temp.school.data$YHC.remember.project==""] <- NA
    temp.school.data$YHC.remember.project <- as.character(temp.school.data$YHC.remember.project)
  }
  for (j in 1:length(levels(DF.YHC.remember.project$Level))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.YHC.remember.project$Level[j])
    DF.YHC.remember.project$count[spot.count] <- length(na.omit(temp.school.data$YHC.remember.project[curr.level==temp.school.data$YHC.remember.project]))
    DF.YHC.remember.project$sum[spot.count] <- length(na.omit(match(temp.school.data$YHC.remember.project,levs)))
  }
}

DF.YHC.remember.project$percent <- 100*(DF.YHC.remember.project$count/DF.YHC.remember.project$sum)
sums.DF.YHC.remember.project <- DF.YHC.remember.project$sum[seq(from=1,to=length(DF.YHC.remember.project$sum), by=2)]


#0303/Having a Youth Health Council helps students at my school to be healthier  
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not really","Some","A lot","Not sure"), levels= c("Not really","Some","A lot","Not sure"))
DF.YHC.school.healthier <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.YHC.school.healthier) <- "Agreement"
DF.YHC.school.healthier$Agreement <- ordered(levels(DF.YHC.school.healthier$Agreement), levels=c("Not really","Some","A lot","Not sure"))
DF.YHC.school.healthier$Year <- rep(DF.yrs,each=4)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$YHC.school.healthier <- as.character(temp.school.data$YHC.school.healthier)
  temp.school.data$YHC.school.healthier[temp.school.data$YHC.school.healthier==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$YHC.school.healthier <- as.character(temp.school.data$YHC.school.healthier)
    temp.school.data$YHC.school.healthier[temp.school.data$YHC.school.healthier==""] <- NA
    temp.school.data$YHC.school.healthier <- as.character(temp.school.data$YHC.school.healthier)
  }
  for (j in 1:length(levels(DF.YHC.school.healthier$Agreement))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.YHC.school.healthier$Agreement[j])
    DF.YHC.school.healthier$count[spot.count] <- length(na.omit(temp.school.data$YHC.school.healthier[curr.level==temp.school.data$YHC.school.healthier]))
    DF.YHC.school.healthier$sum[spot.count] <- length(na.omit(match(temp.school.data$YHC.school.healthier,levs)))
  }
}

DF.YHC.school.healthier$percent <- 100*(DF.YHC.school.healthier$count/DF.YHC.school.healthier$sum)
sums.DF.YHC.school.healthier <- DF.YHC.school.healthier$sum[seq(from=1,to=length(DF.YHC.school.healthier$sum), by=4)]

###########
#SECTION 4# ---> Health care
###########

#0401/Where do you usually go for health care? (Tick as many as apply to you)
#HC.doctor <- factor(HC.doctor)
DF.healthcare <- data.frame(c('Family doctor or GP','School clinic','24hr clinic','Hospital A-E','Youth one-stop shop','Traditional healer','Alternative health','No healthcare','Other'))
colnames(DF.healthcare) <- "Places"
DF.healthcare$count <- 0
DF.healthcare$count[1] <- length(HC.doctor[HC.doctor!=""])
DF.healthcare$count[2] <- length(HC.school[HC.school!=""])
DF.healthcare$count[3] <- length(HC.medical.centre[HC.medical.centre!=""])
DF.healthcare$count[4] <- length(HC.hospital[HC.hospital!=""])
DF.healthcare$count[5] <- length(HC.YOSS[HC.YOSS!=""])
DF.healthcare$count[6] <- length(HC.traditional[HC.traditional!=""])
DF.healthcare$count[7] <- length(HC.alternative[HC.alternative!=""])
DF.healthcare$count[8] <- length(HC.nowhere[HC.nowhere!=""])
DF.healthcare$count[9] <- length(HC.other[HC.other!=''])+length(HC.counsellor[HC.counsellor!=""])
healthcare.blank <- 0
for (i in 1:length(school.data[,1])) {
  if (HC.counsellor[i]=='' & HC.doctor[i]=='' & HC.school[i]=='' & HC.medical.centre[i]=='' & HC.hospital[i]=='' & HC.YOSS[i]=='' & HC.traditional[i]=='' & HC.alternative[i]=='' & HC.nowhere[i]=='' & HC.other[i]=='') {
        healthcare.blank <- healthcare.blank+1
  }
}
DF.healthcare$everyone <- length(school.data[,1])-healthcare.blank
DF.healthcare$percent <- 100*(DF.healthcare$count/DF.healthcare$everyone)
DF.healthcare$Places <- c("Family doctor\n or GP","School clinic","24hr clinic","Hospital\n A and E","Youth one\n stop shop","Traditional\n healer","Alternative\n health","No healthcare","Other")
DF.healthcare$Places <- ordered(DF.healthcare$Places, levels=c("Family doctor\n or GP","School clinic","24hr clinic","Hospital\n A and E","Youth one\n stop shop","Traditional\n healer","Alternative\n health","No healthcare","Other"))
#barplot(DF.healthcare$percent, ylim=c(0,100), ylab="Percent", main="Where do you usually go for health care? (Multiple responses)")
#bar.label <- paste("n =",DF.healthcare$everyone[1])
#axis(1, at=seq(from=0.75, to=10.25, length.out=9), labels=DF.healthcare$Places, las=2, cex.axis=.7)
#text(seq(from=0.75, to=10.25, length.out=9),par("usr")[3] - 0.2, labels=DF.healthcare$Places,srt = 90,offset=3,cex=0.7, pos=1, xpd = TRUE)
#legend("topright",bar.label, bty='n')
#rm(DF.healthcare)


#0402/When was the last time you went for health care? 
#HC.last.visit<- factor(HC.last.visit, levels=c("0 - 12 months ago","12 - 24 months ago","More than 2 years ago"))
#DF.HC.last.visit<-data.frame(tapply(HC.last.visit,HC.last.visit,length))
#colnames(DF.HC.last.visit) <- "Count"
#DF.HC.last.visit$levels <- factor(levels(HC.last.visit), levels=c("0 - 12 months ago","12 - 24 months ago","More than 2 years ago"))
#DF.HC.last.visit$sum <- as.numeric(sum(DF.HC.last.visit$Count))
#DF.HC.last.visit$percent <- 100*(DF.HC.last.visit$Count/DF.HC.last.visit$sum)
#barplot(DF.HC.last.visit$percent, ylim=c(0,70), ylab="Percent", main="When was the last time you went for health care?",xaxt='n')
#bar.label <- paste("n =",DF.HC.last.visit$sum[1])
#axis(1,at=c(0.75,1.9,3.1),labels=c("0 - 12 months ago","12 - 24 months ago","\n More than \n 2 years ago"),lwd.ticks=0)
#legend("topright",bar.label, bty='n')
#rm(DF.HC.last.visit,HC.last.visit)
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("0 - 12 months ago","12 - 24 months ago","More than 2 years ago"), levels= c("0 - 12 months ago","12 - 24 months ago","More than 2 years ago"))
DF.HC.last.visit <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.HC.last.visit) <- "Levels"
DF.HC.last.visit$Levels <- ordered(levels(DF.HC.last.visit$Levels), levels=c("0 - 12 months ago","12 - 24 months ago","More than 2 years ago"))
DF.HC.last.visit$Year <- rep(DF.yrs,each=3)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$HC.last.visit <- as.character(temp.school.data$HC.last.visit)
  temp.school.data$HC.last.visit[temp.school.data$HC.last.visit==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$HC.last.visit <- as.character(temp.school.data$HC.last.visit)
    temp.school.data$HC.last.visit[temp.school.data$HC.last.visit==""] <- NA
    temp.school.data$HC.last.visit <- as.character(temp.school.data$HC.last.visit)
  }
  for (j in 1:length(levels(DF.HC.last.visit$Levels))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.HC.last.visit$Levels[j])
    DF.HC.last.visit$count[spot.count] <- length(na.omit(temp.school.data$HC.last.visit[curr.level==temp.school.data$HC.last.visit]))
    DF.HC.last.visit$sum[spot.count] <- length(na.omit(match(temp.school.data$HC.last.visit,levs)))
  }
}

DF.HC.last.visit$percent <- 100*(DF.HC.last.visit$count/DF.HC.last.visit$sum)
sums.DF.HC.last.visit <- DF.HC.last.visit$sum[seq(from=1,to=length(DF.HC.last.visit$sum), by=3)]


#0403/Barriers to accessing health care
DF.barriers <- data.frame(c('Knowledge','Appointment','Hoped_get_better','Person_unavailable','Not_bothered','Privacy','Uncomfortable','Unfriendly','No_support_person','No_transport','Scared','Cost_too_much','No.fuss','Embarrassed','Other'))
colnames(DF.barriers) <- "Barriers"
DF.barriers$count <- 0
DF.barriers$count[1] <- length(Barriers.knowledge[Barriers.knowledge!=""])
DF.barriers$count[2] <- length(Barriers.appointment[Barriers.appointment!=""])
DF.barriers$count[3] <- length(Barriers.hoped.get.better[Barriers.hoped.get.better!=""])
DF.barriers$count[4] <- length(Barriers.person.unavailable[Barriers.person.unavailable!=""])
DF.barriers$count[5] <- length(Barriers.not.bothered[Barriers.not.bothered!=""])
DF.barriers$count[6] <- length(Barriers.privacy[Barriers.privacy!=""])
DF.barriers$count[7] <- length(Barriers.uncomfortable[Barriers.uncomfortable!=""])
DF.barriers$count[8] <- length(Barriers.unfriendly[Barriers.unfriendly!=""])
DF.barriers$count[9] <- length(Barriers.no.support.person[Barriers.no.support.person!=""])
DF.barriers$count[10] <- length(Barriers.no.transport[Barriers.no.transport!=""])
DF.barriers$count[11] <- length(Barriers.scared[Barriers.scared!=""])
DF.barriers$count[12] <- length(Barriers.cost.too.much[Barriers.cost.too.much!=""])
DF.barriers$count[13] <- length(Barriers.no.fuss[Barriers.no.fuss!=""])
DF.barriers$count[14] <- length(Barriers.embarrassed[Barriers.embarrassed!=""])
DF.barriers$count[15] <- length(Barriers.other[Barriers.other!=''])

barriers.blank <- 0
for (i in 1:length(school.data[,1])) {
if (Barriers.knowledge[i]=='' & Barriers.appointment[i]=='' & Barriers.hoped.get.better[i]=='' & Barriers.person.unavailable[i]=='' & Barriers.not.bothered[i]=='' & Barriers.privacy[i]=='' & Barriers.uncomfortable[i]=='' & Barriers.unfriendly[i]=='' & Barriers.no.support.person[i]=='' & Barriers.no.transport[i]=='' & Barriers.scared[i]=='' & Barriers.cost.too.much[i]=='' & Barriers.no.fuss[i]=='' & Barriers.embarrassed[i]=='' & Barriers.other[i]=='')  {
    barriers.blank <- barriers.blank+1
  }
}
DF.barriers$everyone <- length(school.data[,1])-barriers.blank
DF.barriers$percent <- 100*(DF.barriers$count/DF.barriers$everyone)
DF.barriers$Barriers <- ordered(c("Lack of knowledge\nof available services","Could not get\n appointment","Hoped I would\n get better","The person\n was unavailable","I could not\n be bothered","Worried\n about privacy","Felt uncomfortable","Staff were\n unfriendly","No support person","No transport","Was scared","Cost too much","Did not want\n to make a fuss","Felt embarrassed","Other"), levels=c("Lack of knowledge\nof available services","Could not get\n appointment","Hoped I would\n get better","The person\n was unavailable","I could not\n be bothered","Worried\n about privacy","Felt uncomfortable","Staff were\n unfriendly","No support person","No transport","Was scared","Cost too much","Did not want\n to make a fuss","Felt embarrassed","Other"))

#par(mar=c(7, 4, 4, 1) + 0.1)
#barplot(DF.barriers$percent, ylim=c(0,60), ylab="Percent", main="What barriers stop you accessing health care? (Multiple responses)", xlab='',xaxt='n')
#bar.label <- paste("n =",DF.barriers$everyone[1])
#axis(1, at=seq(from=0.75, to=17.5, length.out=15), labels=DF.barriers$Barriers, cex.axis=.6)
#text(seq(from=0.5, to=17.5, length.out=15),par("usr")[3] - 0.2, labels=c('Lack of knowledge \n of services','Could not get \n appointment','Hoped I would \n get better','The person was \n unavailable','Could not \n be bothered','Worried about \n privacy','I felt \n ncomfortable','The people \n were unfriendly','Had no one \n to go with','Had no transport','I was scared','Services cost \n too much','I did not want \n to make a fuss','I felt \n embarrassed','Other'),srt = 90,offset=2.5,cex=0.7, pos=1, xpd = TRUE)
#legend("topright",bar.label, bty='n')
#rm(DF.barriers)

###########
#SECTION 5# ---> Exercise
###########

#0501/Is physical exercise an important part of your life?
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c('Not really','A little bit','A lot'), levels= c('Not really','A little bit','A lot'))
DF.Exercise.important <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Exercise.important) <- "Response"
DF.Exercise.important$Response <- ordered(levels(DF.Exercise.important$Response), levels=c('Not really','A little bit','A lot'))
DF.Exercise.important$Year <- rep(DF.yrs,each=3)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Exercise.important <- as.character(temp.school.data$Exercise.important)
  temp.school.data$Exercise.important[temp.school.data$Exercise.important==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Exercise.important <- as.character(temp.school.data$Exercise.important)
    temp.school.data$Exercise.important[temp.school.data$Exercise.important==""] <- NA
    temp.school.data$Exercise.important <- as.character(temp.school.data$Exercise.important)
  }
  for (j in 1:length(levels(DF.Exercise.important$Response))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Exercise.important$Response[j])
    DF.Exercise.important$count[spot.count] <- length(na.omit(temp.school.data$Exercise.important[curr.level==temp.school.data$Exercise.important]))
    DF.Exercise.important$sum[spot.count] <- length(na.omit(match(temp.school.data$Exercise.important,levs)))
  }
}

DF.Exercise.important$percent <- 100*(DF.Exercise.important$count/DF.Exercise.important$sum)
sums.DF.Exercise.important <- DF.Exercise.important$sum[seq(from=1,to=length(DF.Exercise.important$sum), by=3)]

#0502/In the last 7 days, how many times have you done physical exercise?
DF.exercise.frequency <- data.frame(c("I do not\nexercise","I have not\n exercised in \n the last 7 days","Once","Twice","Three times","Four times","Five times","Six times","Seven or\n more times"))
colnames(DF.exercise.frequency) <- "Frequency"
DF.exercise.frequency$count <- 0
DF.exercise.frequency$count[1] <- length(Exercise.dont[Exercise.dont!=''])
DF.exercise.frequency$count[2] <- length(Exercise.not.last.7.days[Exercise.not.last.7.days != ''])
DF.exercise.frequency$count[3] <- length(Exercise.once[Exercise.once!=''])
DF.exercise.frequency$count[4] <- length(Exercise.twice[Exercise.twice!=''])
DF.exercise.frequency$count[5] <- length(Exercise.three.times[Exercise.three.times!=''])
DF.exercise.frequency$count[6] <- length(Exercise.four.times[Exercise.four.times!=''])
DF.exercise.frequency$count[7] <- length(Exercise.five.times[Exercise.five.times!=''])
DF.exercise.frequency$count[8] <- length(Exercise.six.times[Exercise.six.times!=''])
DF.exercise.frequency$count[9] <- length(Exercise.seven.plus[Exercise.seven.plus!=''])
DF.exercise.frequency$Response <- factor(DF.exercise.frequency$Frequency, levels=c("I do not\nexercise","I have not\n exercised in \n the last 7 days","Once","Twice","Three times","Four times","Five times","Six times","Seven or\n more times"))
DF.exercise.frequency$sum <- as.numeric(sum(DF.exercise.frequency$count))
DF.exercise.frequency$percent <- 100*(DF.exercise.frequency$count/DF.exercise.frequency$sum)

###########
#SECTION 6# ---> Risk factors
###########

# 0601/Got drunk
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Never","Once","A couple of times","Three or 4 times","More than five times"), levels= c("Never","Once","A couple of times","Three or 4 times","More than five times"))
DF.Got.drunk <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Got.drunk) <- "Frequency"
DF.Got.drunk$Frequency <- ordered(levels(DF.Got.drunk$Frequency), levels=c("Never","Once","A couple of times","Three or 4 times","More than five times"))
DF.Got.drunk$Year <- rep(DF.yrs,each=5)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Got.drunk <- as.character(temp.school.data$Got.drunk)
  temp.school.data$Got.drunk[temp.school.data$Got.drunk==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Got.drunk <- as.character(temp.school.data$Got.drunk)
    temp.school.data$Got.drunk[temp.school.data$Got.drunk==""] <- NA
    temp.school.data$Got.drunk <- as.character(temp.school.data$Got.drunk)
  }
  for (j in 1:length(levels(DF.Got.drunk$Frequency))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Got.drunk$Frequency[j])
    DF.Got.drunk$count[spot.count] <- length(na.omit(temp.school.data$Got.drunk[curr.level==temp.school.data$Got.drunk]))
    DF.Got.drunk$sum[spot.count] <- length(na.omit(match(temp.school.data$Got.drunk,levs)))
  }
}

DF.Got.drunk$percent <- 100*(DF.Got.drunk$count/DF.Got.drunk$sum)
sums.DF.Got.drunk <- DF.Got.drunk$sum[seq(from=1,to=length(DF.Got.drunk$sum), by=5)]

# 0602/Driven whilst drunk
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Never","Once","A couple of times","Three or 4 times","More than five times"), levels= c("Never","Once","A couple of times","Three or 4 times","More than five times"))
DF.Driven.drunk <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Driven.drunk) <- "Frequency"
DF.Driven.drunk$Frequency <- ordered(levels(DF.Driven.drunk$Frequency), levels=c("Never","Once","A couple of times","Three or 4 times","More than five times"))
DF.Driven.drunk$Year <- rep(DF.yrs,each=5)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Driven.drunk <- as.character(temp.school.data$Driven.drunk)
  temp.school.data$Driven.drunk[temp.school.data$Driven.drunk==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Driven.drunk <- as.character(temp.school.data$Driven.drunk)
    temp.school.data$Driven.drunk[temp.school.data$Driven.drunk==""] <- NA
    temp.school.data$Driven.drunk <- as.character(temp.school.data$Driven.drunk)
  }
  for (j in 1:length(levels(DF.Driven.drunk$Frequency))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Driven.drunk$Frequency[j])
    DF.Driven.drunk$count[spot.count] <- length(na.omit(temp.school.data$Driven.drunk[curr.level==temp.school.data$Driven.drunk]))
    DF.Driven.drunk$sum[spot.count] <- length(na.omit(match(temp.school.data$Driven.drunk,levs)))
  }
}

DF.Driven.drunk$percent <- 100*(DF.Driven.drunk$count/DF.Driven.drunk$sum)
sums.DF.Driven.drunk <- DF.Driven.drunk$sum[seq(from=1,to=length(DF.Driven.drunk$sum), by=5)]



# 0603/Been in a car when the driver was drunk
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Never","Once","A couple of times","Three or 4 times","More than five times"), levels= c("Never","Once","A couple of times","Three or 4 times","More than five times"))
DF.Been.driven.drunk.driver <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Been.driven.drunk.driver) <- "Frequency"
DF.Been.driven.drunk.driver$Frequency <- ordered(levels(DF.Been.driven.drunk.driver$Frequency), levels=c("Never","Once","A couple of times","Three or 4 times","More than five times"))
DF.Been.driven.drunk.driver$Year <- rep(DF.yrs,each=5)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Been.driven.drunk.driver <- as.character(temp.school.data$Been.driven.drunk.driver)
  temp.school.data$Been.driven.drunk.driver[temp.school.data$Been.driven.drunk.driver==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Been.driven.drunk.driver <- as.character(temp.school.data$Been.driven.drunk.driver)
    temp.school.data$Been.driven.drunk.driver[temp.school.data$Been.driven.drunk.driver==""] <- NA
    temp.school.data$Been.driven.drunk.driver <- as.character(temp.school.data$Been.driven.drunk.driver)
  }
  for (j in 1:length(levels(DF.Been.driven.drunk.driver$Frequency))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Been.driven.drunk.driver$Frequency[j])
    DF.Been.driven.drunk.driver$count[spot.count] <- length(na.omit(temp.school.data$Been.driven.drunk.driver[curr.level==temp.school.data$Been.driven.drunk.driver]))
    DF.Been.driven.drunk.driver$sum[spot.count] <- length(na.omit(match(temp.school.data$Been.driven.drunk.driver,levs)))
  }
}

DF.Been.driven.drunk.driver$percent <- 100*(DF.Been.driven.drunk.driver$count/DF.Been.driven.drunk.driver$sum)
sums.DF.Been.driven.drunk.driver <- DF.Been.driven.drunk.driver$sum[seq(from=1,to=length(DF.Been.driven.drunk.driver$sum), by=5)]


# 0604/Smoked cigarettes
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Never","Once","A couple of times","Three or 4 times","More than five times"), levels= c("Never","Once","A couple of times","Three or 4 times","More than five times"))
DF.Smoked.cigarettes <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Smoked.cigarettes) <- "Frequency"
DF.Smoked.cigarettes$Frequency <- ordered(levels(DF.Smoked.cigarettes$Frequency), levels=c("Never","Once","A couple of times","Three or 4 times","More than five times"))
DF.Smoked.cigarettes$Year <- rep(DF.yrs,each=5)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Smoked.cigarettes <- as.character(temp.school.data$Smoked.cigarettes)
  temp.school.data$Smoked.cigarettes[temp.school.data$Smoked.cigarettes==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Smoked.cigarettes <- as.character(temp.school.data$Smoked.cigarettes)
    temp.school.data$Smoked.cigarettes[temp.school.data$Smoked.cigarettes==""] <- NA
    temp.school.data$Smoked.cigarettes <- as.character(temp.school.data$Smoked.cigarettes)
  }
  for (j in 1:length(levels(DF.Smoked.cigarettes$Frequency))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Smoked.cigarettes$Frequency[j])
    DF.Smoked.cigarettes$count[spot.count] <- length(na.omit(temp.school.data$Smoked.cigarettes[curr.level==temp.school.data$Smoked.cigarettes]))
    DF.Smoked.cigarettes$sum[spot.count] <- length(na.omit(match(temp.school.data$Smoked.cigarettes,levs)))
  }
}

DF.Smoked.cigarettes$percent <- 100*(DF.Smoked.cigarettes$count/DF.Smoked.cigarettes$sum)
sums.DF.Smoked.cigarettes <- DF.Smoked.cigarettes$sum[seq(from=1,to=length(DF.Smoked.cigarettes$sum), by=5)]

# 0605/Used drugs
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Never","Once","A couple of times","Three or 4 times","More than five times"), levels= c("Never","Once","A couple of times","Three or 4 times","More than five times"))
DF.Used.drugs <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Used.drugs) <- "Frequency"
DF.Used.drugs$Frequency <- ordered(levels(DF.Used.drugs$Frequency), levels=c("Never","Once","A couple of times","Three or 4 times","More than five times"))
DF.Used.drugs$Year <- rep(DF.yrs,each=5)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Used.drugs <- as.character(temp.school.data$Used.drugs)
  temp.school.data$Used.drugs[temp.school.data$Used.drugs==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Used.drugs <- as.character(temp.school.data$Used.drugs)
    temp.school.data$Used.drugs[temp.school.data$Used.drugs==""] <- NA
    temp.school.data$Used.drugs <- as.character(temp.school.data$Used.drugs)
  }
  for (j in 1:length(levels(DF.Used.drugs$Frequency))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Used.drugs$Frequency[j])
    DF.Used.drugs$count[spot.count] <- length(na.omit(temp.school.data$Used.drugs[curr.level==temp.school.data$Used.drugs]))
    DF.Used.drugs$sum[spot.count] <- length(na.omit(match(temp.school.data$Used.drugs,levs)))
  }
}

DF.Used.drugs$percent <- 100*(DF.Used.drugs$count/DF.Used.drugs$sum)
sums.DF.Used.drugs <- DF.Used.drugs$sum[seq(from=1,to=length(DF.Used.drugs$sum), by=5)]


# 0606/Beaten someone up
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Never","Once","A couple of times","Three or 4 times","More than five times"), levels= c("Never","Once","A couple of times","Three or 4 times","More than five times"))
DF.Beaten.someone.up <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Beaten.someone.up) <- "Frequency"
DF.Beaten.someone.up$Frequency <- ordered(levels(DF.Beaten.someone.up$Frequency), levels=c("Never","Once","A couple of times","Three or 4 times","More than five times"))
DF.Beaten.someone.up$Year <- rep(DF.yrs,each=5)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Beaten.someone.up <- as.character(temp.school.data$Beaten.someone.up)
  temp.school.data$Beaten.someone.up[temp.school.data$Beaten.someone.up==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Beaten.someone.up <- as.character(temp.school.data$Beaten.someone.up)
    temp.school.data$Beaten.someone.up[temp.school.data$Beaten.someone.up==""] <- NA
    temp.school.data$Beaten.someone.up <- as.character(temp.school.data$Beaten.someone.up)
  }
  for (j in 1:length(levels(DF.Beaten.someone.up$Frequency))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Beaten.someone.up$Frequency[j])
    DF.Beaten.someone.up$count[spot.count] <- length(na.omit(temp.school.data$Beaten.someone.up[curr.level==temp.school.data$Beaten.someone.up]))
    DF.Beaten.someone.up$sum[spot.count] <- length(na.omit(match(temp.school.data$Beaten.someone.up,levs)))
  }
}

DF.Beaten.someone.up$percent <- 100*(DF.Beaten.someone.up$count/DF.Beaten.someone.up$sum)
sums.DF.Beaten.someone.up <- DF.Beaten.someone.up$sum[seq(from=1,to=length(DF.Beaten.someone.up$sum), by=5)]

# 0607/Had unprotected sex
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Never","Once","A couple of times","Three or 4 times","More than five times"), levels= c("Never","Once","A couple of times","Three or 4 times","More than five times"))
DF.Had.unprotected.sex <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Had.unprotected.sex) <- "Frequency"
DF.Had.unprotected.sex$Frequency <- ordered(levels(DF.Had.unprotected.sex$Frequency), levels=c("Never","Once","A couple of times","Three or 4 times","More than five times"))
DF.Had.unprotected.sex$Year <- rep(DF.yrs,each=5)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Had.unprotected.sex <- as.character(temp.school.data$Had.unprotected.sex)
  temp.school.data$Had.unprotected.sex[temp.school.data$Had.unprotected.sex==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Had.unprotected.sex <- as.character(temp.school.data$Had.unprotected.sex)
    temp.school.data$Had.unprotected.sex[temp.school.data$Had.unprotected.sex==""] <- NA
    temp.school.data$Had.unprotected.sex <- as.character(temp.school.data$Had.unprotected.sex)
  }
  for (j in 1:length(levels(DF.Had.unprotected.sex$Frequency))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Had.unprotected.sex$Frequency[j])
    DF.Had.unprotected.sex$count[spot.count] <- length(na.omit(temp.school.data$Had.unprotected.sex[curr.level==temp.school.data$Had.unprotected.sex]))
    DF.Had.unprotected.sex$sum[spot.count] <- length(na.omit(match(temp.school.data$Had.unprotected.sex,levs)))
  }
}

DF.Had.unprotected.sex$percent <- 100*(DF.Had.unprotected.sex$count/DF.Had.unprotected.sex$sum)
sums.DF.Had.unprotected.sex <- DF.Had.unprotected.sex$sum[seq(from=1,to=length(DF.Had.unprotected.sex$sum), by=5)]

# 0608/Felt concerned that someone was going to hurt me
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Never","Once","A couple of times","Three or 4 times","More than five times"), levels= c("Never","Once","A couple of times","Three or 4 times","More than five times"))
DF.Concerned.someone.hurt.me <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Concerned.someone.hurt.me) <- "Frequency"
DF.Concerned.someone.hurt.me$Frequency <- ordered(levels(DF.Concerned.someone.hurt.me$Frequency), levels=c("Never","Once","A couple of times","Three or 4 times","More than five times"))
DF.Concerned.someone.hurt.me$Year <- rep(DF.yrs,each=5)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Concerned.someone.hurt.me <- as.character(temp.school.data$Concerned.someone.hurt.me)
  temp.school.data$Concerned.someone.hurt.me[temp.school.data$Concerned.someone.hurt.me==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Concerned.someone.hurt.me <- as.character(temp.school.data$Concerned.someone.hurt.me)
    temp.school.data$Concerned.someone.hurt.me[temp.school.data$Concerned.someone.hurt.me==""] <- NA
    temp.school.data$Concerned.someone.hurt.me <- as.character(temp.school.data$Concerned.someone.hurt.me)
  }
  for (j in 1:length(levels(DF.Concerned.someone.hurt.me$Frequency))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Concerned.someone.hurt.me$Frequency[j])
    DF.Concerned.someone.hurt.me$count[spot.count] <- length(na.omit(temp.school.data$Concerned.someone.hurt.me[curr.level==temp.school.data$Concerned.someone.hurt.me]))
    DF.Concerned.someone.hurt.me$sum[spot.count] <- length(na.omit(match(temp.school.data$Concerned.someone.hurt.me,levs)))
  }
}

DF.Concerned.someone.hurt.me$percent <- 100*(DF.Concerned.someone.hurt.me$count/DF.Concerned.someone.hurt.me$sum)
sums.DF.Concerned.someone.hurt.me <- DF.Concerned.someone.hurt.me$sum[seq(from=1,to=length(DF.Concerned.someone.hurt.me$sum), by=5)]

# 0609/Been bullied by my peers
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Never","Once","A couple of times","Three or 4 times","More than five times"), levels= c("Never","Once","A couple of times","Three or 4 times","More than five times"))
DF.Bullied.by.peers <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Bullied.by.peers) <- "Frequency"
DF.Bullied.by.peers$Frequency <- ordered(levels(DF.Bullied.by.peers$Frequency), levels=c("Never","Once","A couple of times","Three or 4 times","More than five times"))
DF.Bullied.by.peers$Year <- rep(DF.yrs,each=5)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Bullied.by.peers <- as.character(temp.school.data$Bullied.by.peers)
  temp.school.data$Bullied.by.peers[temp.school.data$Bullied.by.peers==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Bullied.by.peers <- as.character(temp.school.data$Bullied.by.peers)
    temp.school.data$Bullied.by.peers[temp.school.data$Bullied.by.peers==""] <- NA
    temp.school.data$Bullied.by.peers <- as.character(temp.school.data$Bullied.by.peers)
  }
  for (j in 1:length(levels(DF.Bullied.by.peers$Frequency))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Bullied.by.peers$Frequency[j])
    DF.Bullied.by.peers$count[spot.count] <- length(na.omit(temp.school.data$Bullied.by.peers[curr.level==temp.school.data$Bullied.by.peers]))
    DF.Bullied.by.peers$sum[spot.count] <- length(na.omit(match(temp.school.data$Bullied.by.peers,levs)))
  }
}

DF.Bullied.by.peers$percent <- 100*(DF.Bullied.by.peers$count/DF.Bullied.by.peers$sum)
sums.DF.Bullied.by.peers <- DF.Bullied.by.peers$sum[seq(from=1,to=length(DF.Bullied.by.peers$sum), by=5)]

# 0610/Harmed myself on purpose
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Never","Once","A couple of times","Three or 4 times","More than five times"), levels= c("Never","Once","A couple of times","Three or 4 times","More than five times"))
DF.Harmed.self.on.purpose <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Harmed.self.on.purpose) <- "Frequency"
DF.Harmed.self.on.purpose$Frequency <- ordered(levels(DF.Harmed.self.on.purpose$Frequency), levels=c("Never","Once","A couple of times","Three or 4 times","More than five times"))
DF.Harmed.self.on.purpose$Year <- rep(DF.yrs,each=5)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Harmed.self.on.purpose <- as.character(temp.school.data$Harmed.self.on.purpose)
  temp.school.data$Harmed.self.on.purpose[temp.school.data$Harmed.self.on.purpose==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Harmed.self.on.purpose <- as.character(temp.school.data$Harmed.self.on.purpose)
    temp.school.data$Harmed.self.on.purpose[temp.school.data$Harmed.self.on.purpose==""] <- NA
    temp.school.data$Harmed.self.on.purpose <- as.character(temp.school.data$Harmed.self.on.purpose)
  }
  for (j in 1:length(levels(DF.Harmed.self.on.purpose$Frequency))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Harmed.self.on.purpose$Frequency[j])
    DF.Harmed.self.on.purpose$count[spot.count] <- length(na.omit(temp.school.data$Harmed.self.on.purpose[curr.level==temp.school.data$Harmed.self.on.purpose]))
    DF.Harmed.self.on.purpose$sum[spot.count] <- length(na.omit(match(temp.school.data$Harmed.self.on.purpose,levs)))
  }
}

DF.Harmed.self.on.purpose$percent <- 100*(DF.Harmed.self.on.purpose$count/DF.Harmed.self.on.purpose$sum)
sums.DF.Harmed.self.on.purpose <- DF.Harmed.self.on.purpose$sum[seq(from=1,to=length(DF.Harmed.self.on.purpose$sum), by=5)]
   
# 0611/Seriously thought about killing myself
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Never","Once","A couple of times","Three or 4 times","More than five times"), levels= c("Never","Once","A couple of times","Three or 4 times","More than five times"))
DF.Seriously.thought.suicide <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Seriously.thought.suicide) <- "Frequency"
DF.Seriously.thought.suicide$Frequency <- ordered(levels(DF.Seriously.thought.suicide$Frequency), levels=c("Never","Once","A couple of times","Three or 4 times","More than five times"))
DF.Seriously.thought.suicide$Year <- rep(DF.yrs,each=5)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Seriously.thought.suicide <- as.character(temp.school.data$Seriously.thought.suicide)
  temp.school.data$Seriously.thought.suicide[temp.school.data$Seriously.thought.suicide==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Seriously.thought.suicide <- as.character(temp.school.data$Seriously.thought.suicide)
    temp.school.data$Seriously.thought.suicide[temp.school.data$Seriously.thought.suicide==""] <- NA
    temp.school.data$Seriously.thought.suicide <- as.character(temp.school.data$Seriously.thought.suicide)
  }
  for (j in 1:length(levels(DF.Seriously.thought.suicide$Frequency))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Seriously.thought.suicide$Frequency[j])
    DF.Seriously.thought.suicide$count[spot.count] <- length(na.omit(temp.school.data$Seriously.thought.suicide[curr.level==temp.school.data$Seriously.thought.suicide]))
    DF.Seriously.thought.suicide$sum[spot.count] <- length(na.omit(match(temp.school.data$Seriously.thought.suicide,levs)))
  }
}

DF.Seriously.thought.suicide$percent <- 100*(DF.Seriously.thought.suicide$count/DF.Seriously.thought.suicide$sum)
sums.DF.Seriously.thought.suicide <- DF.Seriously.thought.suicide$sum[seq(from=1,to=length(DF.Seriously.thought.suicide$sum), by=5)]

###########
#SECTION 7# ---> Resiliency factors
###########

#0701/I have people I look up to
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little","Somewhat","Quite a bit","A lot"), levels= c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.People.look.up.to <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.People.look.up.to) <- "Agreement"
DF.People.look.up.to$Agreement <- ordered(levels(DF.People.look.up.to$Agreement), levels=c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.People.look.up.to$Year <- rep(DF.yrs,each=5)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$People.look.up.to <- as.character(temp.school.data$People.look.up.to)
  temp.school.data$People.look.up.to[temp.school.data$People.look.up.to==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$People.look.up.to <- as.character(temp.school.data$People.look.up.to)
    temp.school.data$People.look.up.to[temp.school.data$People.look.up.to==""] <- NA
    temp.school.data$People.look.up.to <- as.character(temp.school.data$People.look.up.to)
  }
  for (j in 1:length(levels(DF.People.look.up.to$Agreement))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.People.look.up.to$Agreement[j])
    DF.People.look.up.to$count[spot.count] <- length(na.omit(temp.school.data$People.look.up.to[curr.level==temp.school.data$People.look.up.to]))
    DF.People.look.up.to$sum[spot.count] <- length(na.omit(match(temp.school.data$People.look.up.to,levs)))
  }
}

DF.People.look.up.to$percent <- 100*(DF.People.look.up.to$count/DF.People.look.up.to$sum)
sums.DF.People.look.up.to <- DF.People.look.up.to$sum[seq(from=1,to=length(DF.People.look.up.to$sum), by=5)]


#People.look.up.to <- factor(People.look.up.to, levels=c("Not at all","A little","Somewhat","Quite a bit","A lot"))
#DF.People.look.up.to <-data.frame(tapply(People.look.up.to,People.look.up.to,length))
#colnames(DF.People.look.up.to) <- "Count"
#DF.People.look.up.to$Count[is.na(DF.People.look.up.to$Count)] <- 0
#DF.People.look.up.to$levels <- levels(People.look.up.to)
#DF.People.look.up.to$sum <- as.numeric(sum(DF.People.look.up.to$Count))
#DF.People.look.up.to$percent <- 100*(DF.People.look.up.to$Count/DF.People.look.up.to$sum)
#barplot(DF.People.look.up.to$percent, ylim=c(0,60), ylab="Percent", main="I have people I look up to",xaxt='n')
#bar.label <- paste("n =",DF.People.look.up.to$sum[1])
#axis(1,at=seq(from=0.7, to=5.5, length.out=5),labels=DF.People.look.up.to$levels,lwd.ticks=1)
#legend("topleft",bar.label, bty='n')
#rm(DF.People.look.up.to,People.look.up.to)

#0702/I talk to my family/caregiver(s) about how I feel
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little","Somewhat","Quite a bit","A lot"), levels= c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Talk.family.about.feelings <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Talk.family.about.feelings) <- "Agreement"
DF.Talk.family.about.feelings$Agreement <- ordered(levels(DF.Talk.family.about.feelings$Agreement), levels=c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Talk.family.about.feelings$Year <- rep(DF.yrs,each=5)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Talk.family.about.feelings <- as.character(temp.school.data$Talk.family.about.feelings)
  temp.school.data$Talk.family.about.feelings[temp.school.data$Talk.family.about.feelings==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Talk.family.about.feelings <- as.character(temp.school.data$Talk.family.about.feelings)
    temp.school.data$Talk.family.about.feelings[temp.school.data$Talk.family.about.feelings==""] <- NA
    temp.school.data$Talk.family.about.feelings <- as.character(temp.school.data$Talk.family.about.feelings)
  }
  for (j in 1:length(levels(DF.Talk.family.about.feelings$Agreement))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Talk.family.about.feelings$Agreement[j])
    DF.Talk.family.about.feelings$count[spot.count] <- length(na.omit(temp.school.data$Talk.family.about.feelings[curr.level==temp.school.data$Talk.family.about.feelings]))
    DF.Talk.family.about.feelings$sum[spot.count] <- length(na.omit(match(temp.school.data$Talk.family.about.feelings,levs)))
  }
}

DF.Talk.family.about.feelings$percent <- 100*(DF.Talk.family.about.feelings$count/DF.Talk.family.about.feelings$sum)
sums.DF.Talk.family.about.feelings <- DF.Talk.family.about.feelings$sum[seq(from=1,to=length(DF.Talk.family.about.feelings$sum), by=5)]

#0703/I have a range of positive ways I cope with my problems
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little","Somewhat","Quite a bit","A lot"), levels= c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Range.of.coping.strategies <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Range.of.coping.strategies) <- "Agreement"
DF.Range.of.coping.strategies$Agreement <- ordered(levels(DF.Range.of.coping.strategies$Agreement), levels=c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Range.of.coping.strategies$Year <- rep(DF.yrs,each=5)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Range.of.coping.strategies <- as.character(temp.school.data$Range.of.coping.strategies)
  temp.school.data$Range.of.coping.strategies[temp.school.data$Range.of.coping.strategies==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Range.of.coping.strategies <- as.character(temp.school.data$Range.of.coping.strategies)
    temp.school.data$Range.of.coping.strategies[temp.school.data$Range.of.coping.strategies==""] <- NA
    temp.school.data$Range.of.coping.strategies <- as.character(temp.school.data$Range.of.coping.strategies)
  }
  for (j in 1:length(levels(DF.Range.of.coping.strategies$Agreement))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Range.of.coping.strategies$Agreement[j])
    DF.Range.of.coping.strategies$count[spot.count] <- length(na.omit(temp.school.data$Range.of.coping.strategies[curr.level==temp.school.data$Range.of.coping.strategies]))
    DF.Range.of.coping.strategies$sum[spot.count] <- length(na.omit(match(temp.school.data$Range.of.coping.strategies,levs)))
  }
}

DF.Range.of.coping.strategies$percent <- 100*(DF.Range.of.coping.strategies$count/DF.Range.of.coping.strategies$sum)
sums.DF.Range.of.coping.strategies <- DF.Range.of.coping.strategies$sum[seq(from=1,to=length(DF.Range.of.coping.strategies$sum), by=5)]


#0704/I feel supported by my friends
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little","Somewhat","Quite a bit","A lot"), levels= c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Supported.by.friends <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Supported.by.friends) <- "Agreement"
DF.Supported.by.friends$Agreement <- ordered(levels(DF.Supported.by.friends$Agreement), levels=c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Supported.by.friends$Year <- rep(DF.yrs,each=5)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Supported.by.friends <- as.character(temp.school.data$Supported.by.friends)
  temp.school.data$Supported.by.friends[temp.school.data$Supported.by.friends==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Supported.by.friends <- as.character(temp.school.data$Supported.by.friends)
    temp.school.data$Supported.by.friends[temp.school.data$Supported.by.friends==""] <- NA
    temp.school.data$Supported.by.friends <- as.character(temp.school.data$Supported.by.friends)
  }
  for (j in 1:length(levels(DF.Supported.by.friends$Agreement))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Supported.by.friends$Agreement[j])
    DF.Supported.by.friends$count[spot.count] <- length(na.omit(temp.school.data$Supported.by.friends[curr.level==temp.school.data$Supported.by.friends]))
    DF.Supported.by.friends$sum[spot.count] <- length(na.omit(match(temp.school.data$Supported.by.friends,levs)))
  }
}

DF.Supported.by.friends$percent <- 100*(DF.Supported.by.friends$count/DF.Supported.by.friends$sum)
sums.DF.Supported.by.friends <- DF.Supported.by.friends$sum[seq(from=1,to=length(DF.Supported.by.friends$sum), by=5)]


#0705/I know where to go in my community to get help
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little","Somewhat","Quite a bit","A lot"), levels= c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Know.where.help.in.community <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Know.where.help.in.community) <- "Agreement"
DF.Know.where.help.in.community$Agreement <- ordered(levels(DF.Know.where.help.in.community$Agreement), levels=c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Know.where.help.in.community$Year <- rep(DF.yrs,each=5)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Know.where.help.in.community <- as.character(temp.school.data$Know.where.help.in.community)
  temp.school.data$Know.where.help.in.community[temp.school.data$Know.where.help.in.community==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Know.where.help.in.community <- as.character(temp.school.data$Know.where.help.in.community)
    temp.school.data$Know.where.help.in.community[temp.school.data$Know.where.help.in.community==""] <- NA
    temp.school.data$Know.where.help.in.community <- as.character(temp.school.data$Know.where.help.in.community)
  }
  for (j in 1:length(levels(DF.Know.where.help.in.community$Agreement))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Know.where.help.in.community$Agreement[j])
    DF.Know.where.help.in.community$count[spot.count] <- length(na.omit(temp.school.data$Know.where.help.in.community[curr.level==temp.school.data$Know.where.help.in.community]))
    DF.Know.where.help.in.community$sum[spot.count] <- length(na.omit(match(temp.school.data$Know.where.help.in.community,levs)))
  }
}

DF.Know.where.help.in.community$percent <- 100*(DF.Know.where.help.in.community$count/DF.Know.where.help.in.community$sum)
sums.DF.Know.where.help.in.community <- DF.Know.where.help.in.community$sum[seq(from=1,to=length(DF.Know.where.help.in.community$sum), by=5)]


#0706/I feel I belong at my school
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little","Somewhat","Quite a bit","A lot"), levels= c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Belong.at.my.school <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Belong.at.my.school) <- "Agreement"
DF.Belong.at.my.school$Agreement <- ordered(levels(DF.Belong.at.my.school$Agreement), levels=c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Belong.at.my.school$Year <- rep(DF.yrs,each=5)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Belong.at.my.school <- as.character(temp.school.data$Belong.at.my.school)
  temp.school.data$Belong.at.my.school[temp.school.data$Belong.at.my.school==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Belong.at.my.school <- as.character(temp.school.data$Belong.at.my.school)
    temp.school.data$Belong.at.my.school[temp.school.data$Belong.at.my.school==""] <- NA
    temp.school.data$Belong.at.my.school <- as.character(temp.school.data$Belong.at.my.school)
  }
  for (j in 1:length(levels(DF.Belong.at.my.school$Agreement))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Belong.at.my.school$Agreement[j])
    DF.Belong.at.my.school$count[spot.count] <- length(na.omit(temp.school.data$Belong.at.my.school[curr.level==temp.school.data$Belong.at.my.school]))
    DF.Belong.at.my.school$sum[spot.count] <- length(na.omit(match(temp.school.data$Belong.at.my.school,levs)))
  }
}

DF.Belong.at.my.school$percent <- 100*(DF.Belong.at.my.school$count/DF.Belong.at.my.school$sum)
sums.DF.Belong.at.my.school <- DF.Belong.at.my.school$sum[seq(from=1,to=length(DF.Belong.at.my.school$sum), by=5)]


#0707/My family stands by me during difficult times
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little","Somewhat","Quite a bit","A lot"), levels= c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Family.stands.by.me <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Family.stands.by.me) <- "Agreement"
DF.Family.stands.by.me$Agreement <- ordered(levels(DF.Family.stands.by.me$Agreement), levels=c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Family.stands.by.me$Year <- rep(DF.yrs,each=5)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Family.stands.by.me <- as.character(temp.school.data$Family.stands.by.me)
  temp.school.data$Family.stands.by.me[temp.school.data$Family.stands.by.me==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Family.stands.by.me <- as.character(temp.school.data$Family.stands.by.me)
    temp.school.data$Family.stands.by.me[temp.school.data$Family.stands.by.me==""] <- NA
    temp.school.data$Family.stands.by.me <- as.character(temp.school.data$Family.stands.by.me)
  }
  for (j in 1:length(levels(DF.Family.stands.by.me$Agreement))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Family.stands.by.me$Agreement[j])
    DF.Family.stands.by.me$count[spot.count] <- length(na.omit(temp.school.data$Family.stands.by.me[curr.level==temp.school.data$Family.stands.by.me]))
    DF.Family.stands.by.me$sum[spot.count] <- length(na.omit(match(temp.school.data$Family.stands.by.me,levs)))
  }
}

DF.Family.stands.by.me$percent <- 100*(DF.Family.stands.by.me$count/DF.Family.stands.by.me$sum)
sums.DF.Family.stands.by.me <- DF.Family.stands.by.me$sum[seq(from=1,to=length(DF.Family.stands.by.me$sum), by=5)]


#0708/If I have questions about health I know who to ask
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little","Somewhat","Quite a bit","A lot"), levels= c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Know.who.ask.about.health  <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Know.who.ask.about.health ) <- "Agreement"
DF.Know.who.ask.about.health $Agreement <- ordered(levels(DF.Know.who.ask.about.health $Agreement), levels=c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Know.who.ask.about.health $Year <- rep(DF.yrs,each=5)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Know.who.ask.about.health  <- as.character(temp.school.data$Know.who.ask.about.health )
  temp.school.data$Know.who.ask.about.health [temp.school.data$Know.who.ask.about.health ==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Know.who.ask.about.health  <- as.character(temp.school.data$Know.who.ask.about.health )
    temp.school.data$Know.who.ask.about.health [temp.school.data$Know.who.ask.about.health ==""] <- NA
    temp.school.data$Know.who.ask.about.health  <- as.character(temp.school.data$Know.who.ask.about.health )
  }
  for (j in 1:length(levels(DF.Know.who.ask.about.health $Agreement))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Know.who.ask.about.health $Agreement[j])
    DF.Know.who.ask.about.health $count[spot.count] <- length(na.omit(temp.school.data$Know.who.ask.about.health [curr.level==temp.school.data$Know.who.ask.about.health ]))
    DF.Know.who.ask.about.health $sum[spot.count] <- length(na.omit(match(temp.school.data$Know.who.ask.about.health ,levs)))
  }
}

DF.Know.who.ask.about.health $percent <- 100*(DF.Know.who.ask.about.health $count/DF.Know.who.ask.about.health $sum)
sums.DF.Know.who.ask.about.health  <- DF.Know.who.ask.about.health $sum[seq(from=1,to=length(DF.Know.who.ask.about.health $sum), by=5)]


#0709/When I need health care I know where to go
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little","Somewhat","Quite a bit","A lot"), levels= c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Know.where.to.get.healthcare  <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Know.where.to.get.healthcare ) <- "Agreement"
DF.Know.where.to.get.healthcare $Agreement <- ordered(levels(DF.Know.where.to.get.healthcare $Agreement), levels=c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Know.where.to.get.healthcare $Year <- rep(DF.yrs,each=5)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Know.where.to.get.healthcare  <- as.character(temp.school.data$Know.where.to.get.healthcare )
  temp.school.data$Know.where.to.get.healthcare [temp.school.data$Know.where.to.get.healthcare ==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Know.where.to.get.healthcare  <- as.character(temp.school.data$Know.where.to.get.healthcare )
    temp.school.data$Know.where.to.get.healthcare [temp.school.data$Know.where.to.get.healthcare ==""] <- NA
    temp.school.data$Know.where.to.get.healthcare  <- as.character(temp.school.data$Know.where.to.get.healthcare )
  }
  for (j in 1:length(levels(DF.Know.where.to.get.healthcare $Agreement))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Know.where.to.get.healthcare $Agreement[j])
    DF.Know.where.to.get.healthcare $count[spot.count] <- length(na.omit(temp.school.data$Know.where.to.get.healthcare [curr.level==temp.school.data$Know.where.to.get.healthcare ]))
    DF.Know.where.to.get.healthcare $sum[spot.count] <- length(na.omit(match(temp.school.data$Know.where.to.get.healthcare ,levs)))
  }
}

DF.Know.where.to.get.healthcare $percent <- 100*(DF.Know.where.to.get.healthcare $count/DF.Know.where.to.get.healthcare $sum)
sums.DF.Know.where.to.get.healthcare <- DF.Know.where.to.get.healthcare $sum[seq(from=1,to=length(DF.Know.where.to.get.healthcare $sum), by=5)]


#0710/I feel comfortable getting help about health
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little","Somewhat","Quite a bit","A lot"), levels= c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Comfortable.about.getting.healthcare <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Comfortable.about.getting.healthcare) <- "Agreement"
DF.Comfortable.about.getting.healthcare$Agreement <- ordered(levels(DF.Comfortable.about.getting.healthcare$Agreement), levels=c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Comfortable.about.getting.healthcare$Year <- rep(DF.yrs,each=5)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Comfortable.about.getting.healthcare <- as.character(temp.school.data$Comfortable.about.getting.healthcare)
  temp.school.data$Comfortable.about.getting.healthcare[temp.school.data$Comfortable.about.getting.healthcare==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Comfortable.about.getting.healthcare <- as.character(temp.school.data$Comfortable.about.getting.healthcare)
    temp.school.data$Comfortable.about.getting.healthcare[temp.school.data$Comfortable.about.getting.healthcare==""] <- NA
    temp.school.data$Comfortable.about.getting.healthcare <- as.character(temp.school.data$Comfortable.about.getting.healthcare)
  }
  for (j in 1:length(levels(DF.Comfortable.about.getting.healthcare$Agreement))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Comfortable.about.getting.healthcare$Agreement[j])
    DF.Comfortable.about.getting.healthcare$count[spot.count] <- length(na.omit(temp.school.data$Comfortable.about.getting.healthcare[curr.level==temp.school.data$Comfortable.about.getting.healthcare]))
    DF.Comfortable.about.getting.healthcare$sum[spot.count] <- length(na.omit(match(temp.school.data$Comfortable.about.getting.healthcare,levs)))
  }
}

DF.Comfortable.about.getting.healthcare$percent <- 100*(DF.Comfortable.about.getting.healthcare$count/DF.Comfortable.about.getting.healthcare$sum)
sums.DF.Comfortable.about.getting.healthcare <- DF.Comfortable.about.getting.healthcare$sum[seq(from=1,to=length(DF.Comfortable.about.getting.healthcare$sum), by=5)]


#0711/I am quick to seek out help about health when I need it
DF.yrs <- ordered(c(sort(unique(na.omit(Year))), "All"), levels=c(sort(unique(na.omit(Year))), "All"))
spot.count <- 0
curr.level <- c()
levs <- factor(c("Not at all","A little","Somewhat","Quite a bit","A lot"), levels= c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Quick.to.seek.help <- data.frame(rep(levels(levs),each=1, times=(1+length(unique(na.omit(Year))))))
colnames(DF.Quick.to.seek.help) <- "Agreement"
DF.Quick.to.seek.help$Agreement <- ordered(levels(DF.Quick.to.seek.help$Agreement), levels=c("Not at all","A little","Somewhat","Quite a bit","A lot"))
DF.Quick.to.seek.help$Year <- rep(DF.yrs,each=5)
for (i in 1:length(DF.yrs)) {
  temp.school.data <- school.data[Year==DF.yrs[i],]
  temp.school.data$Quick.to.seek.help <- as.character(temp.school.data$Quick.to.seek.help)
  temp.school.data$Quick.to.seek.help[temp.school.data$Quick.to.seek.help==""] <- NA
  if (DF.yrs[i]=="All") { 
    temp.school.data <- school.data 
    temp.school.data$Quick.to.seek.help <- as.character(temp.school.data$Quick.to.seek.help)
    temp.school.data$Quick.to.seek.help[temp.school.data$Quick.to.seek.help==""] <- NA
    temp.school.data$Quick.to.seek.help <- as.character(temp.school.data$Quick.to.seek.help)
  }
  for (j in 1:length(levels(DF.Quick.to.seek.help$Agreement))) {
    spot.count <- spot.count +1
    curr.level <- as.character(DF.Quick.to.seek.help$Agreement[j])
    DF.Quick.to.seek.help$count[spot.count] <- length(na.omit(temp.school.data$Quick.to.seek.help[curr.level==temp.school.data$Quick.to.seek.help]))
    DF.Quick.to.seek.help$sum[spot.count] <- length(na.omit(match(temp.school.data$Quick.to.seek.help,levs)))
  }
}

DF.Quick.to.seek.help$percent <- 100*(DF.Quick.to.seek.help$count/DF.Quick.to.seek.help$sum)
sums.DF.Quick.to.seek.help <- DF.Quick.to.seek.help$sum[seq(from=1,to=length(DF.Quick.to.seek.help$sum), by=5)]


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-

#Export the data for the school
anon.columns <- c(1,2,8:93)
export.data <- school.data[,anon.columns]
#^^^
#Needs to be formatted for out [col. names and some responses compressed]

#Write a word doc
library(knitr)
the.school.mod <- str_replace_all(the.school, " ", "_")
knit(paste0("schools.Rhtml"), encoding = "utf-8")
system(paste0("pandoc -o ", the.school.mod, ".docx ", "schools.html"))

#chisq.test(matrix(nrow=5,ncol=3,data=c(DF.General.health.good$count[1:5],DF.General.health.good$count[6:10],DF.General.health.good$count[11:15]),dimnames=list(DF.General.health.good$Agreement[1:5],c(10,11,12))))
#chisq.test(matrix(nrow=5,ncol=3,data=c(DF.Understand.be.healthy$count[1:5],DF.Understand.be.healthy$count[6:10],DF.Understand.be.healthy$count[11:15]),dimnames=list(DF.Understand.be.healthy$Agreement[1:5],c(10,11,12))))
#chisq.test(matrix(nrow=5,ncol=3,data=c(DF.Eat.nutritious.diet$count[1:5],DF.Eat.nutritious.diet$count[6:10],DF.Eat.nutritious.diet$count[11:15]),dimnames=list(DF.Eat.nutritious.diet$Agreement[1:5],c(10,11,12))))
#chisq.test(matrix(nrow=5,ncol=3,data=c(DF.Right.amount.of.food$count[1:5],DF.Right.amount.of.food$count[6:10],DF.Right.amount.of.food$count[11:15]),dimnames=list(DF.Right.amount.of.food$Agreement[1:5],c(10,11,12))))
#chisq.test(matrix(nrow=5,ncol=3,data=c(DF.Am.happy$count[1:5],DF.Am.happy$count[6:10],DF.Am.happy$count[11:15]),dimnames=list(DF.Am.happy$Agreement[1:5],c(10,11,12))))
#chisq.test(matrix(nrow=5,ncol=3,data=c(DF.Exercise.regularly$count[1:5],DF.Exercise.regularly$count[6:10],DF.Exercise.regularly$count[11:15]),dimnames=list(DF.Exercise.regularly$Agreement[1:5],c(10,11,12))))
#chisq.test(matrix(nrow=5,ncol=3,data=c(DF.Understand.emotions$count[1:5],DF.Understand.emotions$count[6:10],DF.Understand.emotions$count[11:15]),dimnames=list(DF.Understand.emotions$Agreement[1:5],c(10,11,12))))$stdres
#chisq.test(matrix(nrow=5,ncol=3,data=c(DF.Spiritual.beliefs.important$count[1:5],DF.Spiritual.beliefs.important$count[6:10],DF.Spiritual.beliefs.important$count[11:15]),dimnames=list(DF.Spiritual.beliefs.important$Agreement[1:5],c(10,11,12))))
#chisq.test(matrix(nrow=5,ncol=3,data=c(DF.Feel.valued.in.community$count[1:5],DF.Feel.valued.in.community$count[6:10],DF.Feel.valued.in.community$count[11:15]),dimnames=list(DF.Feel.valued.in.community$Agreement[1:5],c(10,11,12))))
#chisq.test(matrix(nrow=5,ncol=3,data=c(DF.Connected.to.culture$count[1:5],DF.Connected.to.culture$count[6:10],DF.Connected.to.culture$count[11:15]),dimnames=list(DF.Connected.to.culture$Agreement[1:5],c(10,11,12))))

#require(polycor)
#require(psych)
#context2[context2[,]==""] <- NA
#c2cor <- hetcor(context2[,1:36],use="complete.obs")
#c2mat <- c2cor$correlations 

#scree(c2mat)

#fa.c2 <- factanal(c2mat,covmat=c2mat,rotation="oblimin",factors=5)
#print(fa.c2,digits=3)

#psych.fa <- fa(c2mat,nfactors=4, fm="ml",n.obs=1386)
#print.psych(psych.fa,digits=3,cut=0.2,sort=T)


#require(polycor)

require(psych)
require(vcd)
YHCknow <- school.data[,33:35]
YHCknow[is.na(YHCknow)] <- ""
YHCknow[YHCknow=="N/A"] <- ""
YHCknowlim <- YHCknow[YHCknow$YHC.school.healthier!="Not sure",]
YHCknowlim <- YHCknowlim[YHCknowlim$YHC.school.healthier!="",]
YHCknowlim <- YHCknowlim[YHCknowlim$YHC.remember.project!="",]
YHCknowlim <- data.frame(lapply(YHCknowlim, as.character), stringsAsFactors=FALSE)

sieve(table(YHCknowlim[,2:3]),shade=T)


resil <- school.data[,83:93]
risks <- school.data[,72:82]
health <- school.data[,8:17]
context <- school.data[,18:32]


for (i in 1:11) {
resil[,i] <- as.numeric(factor(resil[,i], levels=c("Not at all","A little","Somewhat","Quite a bit","A lot")))
risks[,i] <- as.numeric(factor(risks[,i], levels=c("Never","Once","A couple of times","Three or 4 times","More than five times")))
}
for (i in 1:10) {
  health[,i] <- as.numeric(factor(health[,i], levels=c("Not at all","A little","Somewhat","Quite a bit","A lot")))
}
for (i in 1:15) {
  context[,i] <- as.numeric(factor(context[,i], levels=c("Not at all","A little bit","Some","Very much")))
}

context$Family.avg <- 0
context$Friends.avg <- 0
context$School.avg <- 0

for (i in 1:nrow(context)) {
context$Family.avg[i] <- sum(context[i,1:5])/length(na.omit(context[i,1:5]))
context$Friends.avg[i] <- sum(context[i,6:10])/length(na.omit(context[i,6:10]))
context$School.avg[i] <- sum(context[i,11:15])/length(na.omit(context[i,11:15]))
}
  
health.means <- data.frame (c(0,unique(na.omit(Year))),0,0,0,0,0,0,0,0,0,0)

for (i in 1:10) {
  health.means[1,(i+1)] <- mean(health[,i],na.rm=T)
  for (j in 1:(length(unique(na.omit(Year))))) {  
    
  health.means[(j+1),(i+1)] <- mean(health[Year==unique(na.omit(Year))[j],i],na.rm=T)
  }
}
colnames(health.means) <- c("Year",colnames(health))

health.means <- health.means[order(health.means[,1]),]
health.means[1,1] <- "All"

risks.means <- data.frame (c(0,unique(na.omit(Year))),0,0,0,0,0,0,0,0,0,0,0)
resil.means  <- data.frame (c(0,unique(na.omit(Year))),0,0,0,0,0,0,0,0,0,0,0)
for (i in 1:11) {
  risks.means[1,(i+1)] <- mean(risks[,i],na.rm=T)
  resil.means[1,(i+1)] <- mean(resil[,i],na.rm=T)
  for (j in 1:(length(unique(na.omit(Year))))) {  
        risks.means[(j+1),(i+1)] <- mean(risks[Year==unique(na.omit(Year))[j],i],na.rm=T)
        resil.means[(j+1),(i+1)] <- mean(resil[Year==unique(na.omit(Year))[j],i],na.rm=T)
  }
}
colnames(risks.means) <- c("Year",colnames(risks))
colnames(resil.means) <- c("Year",colnames(resil))

risks.means <- risks.means[order(risks.means[,1]),]
risks.means[1,1] <- "All"  
resil.means <- resil.means[order(resil.means[,1]),]
resil.means[1,1] <- "All"  
  
#all.df <- data.frame(resil,risks,health)
#scree(all.df)
#all.fa <- fa(all.df,nfactors=5, fm="ml",n.obs=2088)
#print.psych(all.fa,digits=3,cut=0.2,sort=T)


general.health.df <- data.frame(health$General.health.good,context)

scree(general.health.df)
gh.fa <- fa(general.health.df,nfactors=3, fm="minres",n.obs=93, rotate="oblimin",scores="Barlett")
print.psych(gh.fa,digits=3,cut=0.2,sort=T)


risk1df <- data.frame(risks[,3],context,resil)
scree(risk1df)
r1.fa <- factanal(as.matrix(risk1df),factors=4)
print.psych(r1.fa,digits=3,cut=0.2,sort=T)

fpca(data=risk1df,risks...3.~Family.healthy.food+Family.healthy.living+Family.connected.culture+Family.understand.emotions+Family.regular.exercise+Friends.healthy.food+Friends.regular.exercise+Friends.understand.emotions+Friends.be.happy+Friends.healthy.living+School.connected.culture+School.healthy.food+School.healthy.living+School.regular.exercise+School.be.happy+Family.avg+Friends.avg+School.avg+People.look.up.to+Talk.family.about.feelings+Range.of.coping.strategies+Supported.by.friends+Know.where.help.in.community+Belong.at.my.school+Family.stands.by.me+Know.who.ask.about.health+Know.where.to.get.healthcare+Comfortable.about.getting.healthcare+Quick.to.seek.help)



s1 <- c()
s2 <- c()
s3 <- c()
s4 <- c()
s5 <- c()
st1 <- c()
st2 <- c()
st3 <- c()
st4 <- c()
st5 <- c()

for (l in 1:32) {
  st1 <- mean(all.df[all.df$Seriously.thought.suicide==1,l], na.rm=T)
  st2 <- mean(all.df[all.df$Seriously.thought.suicide==2,l], na.rm=T)
  st3 <- mean(all.df[all.df$Seriously.thought.suicide==3,l], na.rm=T)
  st4 <- mean(all.df[all.df$Seriously.thought.suicide==4,l], na.rm=T)
  st5 <- mean(all.df[all.df$Seriously.thought.suicide==5,l], na.rm=T)
  s1 <- c(s1,st1)
  s2 <- c(s2,st2)
  s3 <- c(s3,st3)
  s4 <- c(s4,st4)
  s5 <- c(s5,st5)
  }

s.df <- matrix(nrow=5,ncol=32)
for (l in 1:32) {
s.df[,l] <- c(s1[l],s2[l],s3[l],s4[l],s5[l])
}
s.df <- as.data.frame(s.df)
colnames(s.df) <- colnames(all.df)
for (i in 1:nrow(health)) {
health$avg[i] <- mean(as.numeric(health[i,1:10]), na.rm=T)
risks$avg[i] <- mean(as.numeric(risks[i,1:11]), na.rm=T)
resil$avg[i] <- mean(as.numeric(resil[i,1:11]), na.rm=T)
}