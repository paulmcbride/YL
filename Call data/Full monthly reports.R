###Need to make sure there are no double ups in terms of counting the same 
# call twice when transfers are involved. (i.e. the outgoing call to triage or HL as well as 
# the original queue call)
# Is the new system reliable? i.e. for triage.match and helpline.match?

##############################
##BEFORE STARTING, READ THIS:##############################################################################
##R:\Youthline Research\Youthline call data\Monthly reports\HOW TO Run call data reports for Youthline.docx#
###########################################################################################################

#Remove old data and call in current data from .xls 
rm(list=ls())

#DEFINE MONTH#
#Input the name of the file exported from IPFX reports
#This is the exact .xls data file name (but without the .xls ending)
month.name <- "June13"

#SET UP QUEUES AND EXTENSIONS#
#Queue numbers - in any order inside the (brackets) and separated by commas
#Helpline/triage
triage <- c(223,222,789,797)
helpline <- c(710,791,793,792,796,795,794,240,340)
triagehl <- c(triage,helpline)
#SAS
miscarriage.support <- 770
yl.waitakere <- 233
yl.north.shore <- 234
yl.pregnancy <- 231
yl.f2f <-232
action.education <- 235
youth.services <- 241

#Helpline/triage extensions  - in any order inside (brackets) separated by commas
all.triage <- c(751,717,851,762,752,998,996,977,748,749) #Includes overnight extensions
GL.helpline <- c(718,719,720)
mnk.helpline <- c(753,754,750)
PN.helpline <- c(758,759)
wel.helpline <- c(755,756)
chch.helpline <- c(760,761)
inv.helpline <- c(961,962)
dun.helpline <- c(951,952)
all.helpline <- c(GL.helpline,mnk.helpline,PN.helpline,wel.helpline,
                  chch.helpline,inv.helpline,dun.helpline)

####Office extensions#### (excludes SAS & Youth services extensions)
#PONSONBY
GL.queue <- (700)
GL.ext <- c(701,702,703,704,705,707,708,709,711,713,721,722,725,726,728,
               729,730,731,734,736,737,738,739,745,746,747,790,901,963,964,965,
               966,967,970,971,973,974,978)
GL.office <- c(GL.queue,GL.ext)  
#MANUKAU
mnk.office <- c(840,841,842,843,844,845,846,847)
#NORTH SHORE
NS.office <- c(610,611,612)
  
  
##############  
#GET THE DATA#  
##############
  
#Data location
setwd("R:/Youthline Research/Youthline call data/Monthly reports/")

#Get libraries
require(knitr)
require(stringr)
require(ggplot2)
require(xtable)
require(pander)
require(RODBC)

#Handy time converter
naskipper <- function(q) {
  
  if (is.na(q)) { q=10 }
  return (q)
}

minmaker <- function(x) {
  hours <- trunc(x/60)
  xy <- x-60*hours
  mins <- trunc(xy)
  secs <- round((xy-mins)*60,0)
  towmins <- c()
  towhours <- c()
  towsecs <- c()
  for (j in 1:length(x)) {
    towmins <- c(towmins,naskipper(mins[j]))
    towhours <- c(towhours,naskipper(hours[j]))
    towsecs <- c(towsecs,naskipper(secs[j]))
  }
  for (l in 1:length(x)) {
    if (towhours[l] < 10) { 
      hours[l] <- paste("0",hours[l],sep='') 
    } 
    if (towmins[l] < 10) { 
      mins[l] <- paste("0",mins[l],sep='') 
    } 
    if (towsecs[l] < 10) { 
      secs[l] <- paste("0",secs[l],sep='') 
    } 
  }
  y <- paste(hours,":",mins,":",secs,sep='') 
  return(y)
}

minmaker2 <- function(x) {
  mins <- trunc(x)
  secs <- round((x-mins)*60,0)
  towmins <- c()
  towhours <- c()
  towsecs <- c()
  for (j in 1:length(x)) {
    towmins <- c(towmins,naskipper(mins[j]))
    towsecs <- c(towsecs,naskipper(secs[j]))
  }
  if (towmins[l] < 10) { 
    mins[l] <- paste("0",mins[l],sep='') 
  } 
  if (towsecs[l] < 10) { 
    secs[l] <- paste("0",secs[l],sep='') 
  } 
y <- paste(mins,":",secs,sep='') 
return(y)
}

#Access XLS as database
file.loc <- paste("R:/Youthline Research/Youthline call data/Monthly reports/Data files/",month.name,".xls", sep='')
ch1 <- odbcConnectExcel(file.loc, readOnly = T)
raw.data <- sqlFetch(ch1, "Sheet1$")
close(ch1)

colnames(raw.data) <- c("Start.Time","Queue.Number","Extension.Number","Media.Type","Call.Type.","Dialled.Number..Out","Dialled.Number..In","Calling.Line.Identification","Caller.Details","Group.Count","Time.In..Queue...OffHook","Time...Ringing.at.Extension","Time...Talking","Time..On.Hold.","Time..In.Wrapup.","Total.Handle.Time","Release..Type")
raw.data <- data.frame(lapply(raw.data, as.character), stringsAsFactors=FALSE)
raw.data$Media.Type[is.na(raw.data$Media.Type)] <- "destruct"
raw.data <- raw.data[raw.data$Media.Type=="Call",]
raw.data[is.na(raw.data)] <- ""
raw.data$Calling.Line.Identification[raw.data$Calling.Line.Identification=="anonymous"] <- "+645555555"

###################
##DATA PROCESSING##
###################

#Identify duplicates
newdate.raw <- as.POSIXlt(raw.data$Start.Time, format='%Y-%m-%d %H:%M:%S')

totalcalltime.raw <- as.POSIXlt(raw.data$Total.Handle.Time, format='%H:%M:%S')

#Remove duplicated calls bouncing around in VM system
add <- (totalcalltime.raw$hour*3600)+(totalcalltime.raw$min*60)+(totalcalltime.raw$sec)
endtime.raw <- as.POSIXlt(newdate.raw + add)
raw.data$num.start <- as.numeric(newdate.raw)
raw.data$num.end <- as.numeric(endtime.raw)
starttime.raw <- as.POSIXlt(newdate.raw)
index.dup.raw <- 1:nrow(raw.data)
raw.data.dup <- c()
start.dup.raw <- c()
end.dup.raw <- c()



raw.data$Original.Queue <- raw.data$Queue.Number
raw.data$CLI <- as.character(str_sub(raw.data$Calling.Line.Identification, start=-7, end=-1))
raw.data$CLI[is.na(raw.data$CLI)] <- 5555555
unique.CLIs.for.dup.raw <- unique(raw.data$CLI)

for (l in 1:length(unique.CLIs.for.dup.raw)) {
  index.dup.raw <- 1:nrow(raw.data)
  index.dup.raw <- index.dup.raw[(raw.data$CLI==unique.CLIs.for.dup.raw[l])]
  raw.data.dup <- raw.data[index.dup.raw,]
  if (length(raw.data.dup[,1])>1) {
    for (j in 1:(length(raw.data.dup[,1])-1)) {
      if (raw.data.dup$num.end[j]==raw.data.dup$num.start[j+1]) {
        thei <- index.dup.raw[j]
        raw.data$Release..Type[thei] <- "I"
        raw.data$Original.Queue[(thei+1)] <- raw.data$Queue.Number[thei]
      }
    }
  }
}

raw.data$Queue.Number <- as.numeric(as.character(raw.data$Queue.Number))
newdate.raw <- as.POSIXlt(raw.data$Start.Time, format='%Y-%m-%d %H:%M:%S')


#Grab only helpline/triage data
#call.data <- data.frame(raw.data[raw.data$Call.Type.=="Queue",], stringsAsFactors=FALSE)
#call.data <- call.data[call.data$Caller.Details==""|call.data$Caller.Details=="ID Withheld",]
#cdmatch <- match(call.data$Queue.Number,triagehl)
#cdmatch[is.na(cdmatch)] <- 0
#call.data <- call.data[cdmatch>0,]

#call.data$CLI <- as.character(str_sub(call.data$Calling.Line.Identification, start=-7, end=-1))
#raw.data$CLI <- as.character(str_sub(raw.data$Calling.Line.Identification, start=-7, end=-1))
#call.data$CLI[is.na(call.data$CLI)] <- 5555555
#call.data <- call.data[call.data$CLI!="3766645",]

#Format date for use in R
#newdate <- as.POSIXlt(call.data$Start.Time, format='%Y-%m-%d %H:%M:%S')
#newdate.raw <- as.POSIXlt(raw.data$Start.Time, format='%Y-%m-%d %H:%M:%S')
#totalcalltime <- as.POSIXlt(call.data$Total.Handle.Time, format='%H:%M:%S')

cdmatch <- match(raw.data$Original.Queue,triagehl)
cdmatch[is.na(cdmatch)] <- 0
call.data <- raw.data[cdmatch>0,]
call.data <- data.frame(call.data[call.data$Release..Type!="I",], stringsAsFactors=FALSE)

newdate <- as.POSIXlt(call.data$Start.Time, format='%Y-%m-%d %H:%M:%S')

#Sort out days, add nicely formatted date and DOW
the.day <- c()
for (t in 1:length(newdate$mon)) {
if (str_length(newdate$mday[t])==1) {
  the.day <- c(the.day, paste(0,newdate$mday[t],sep=''))
}
if (str_length(newdate$mday[t])==2) {
  the.day <- c(the.day, paste(newdate$mday[t],sep=''))
}
}
the.date <- as.numeric(paste((newdate$mon+1),the.day,sep=''))
call.data$the.date <- the.date
newdate.unique <- the.date[!duplicated(the.date)]
newdate.unique <- as.numeric(newdate.unique)

unique.days.mon <- unique( newdate.raw$mday)
newdate.unique.mon <- newdate[!duplicated(newdate.raw$mday)]

call.data$weekday <- weekdays(newdate)
call.data$talk.time.mins <- (as.numeric(str_sub(call.data$Time...Talking, -2,-1))+(60*(as.numeric(str_sub(call.data$Time...Talking, -5,-4))))+(3600*(as.numeric(str_sub(call.data$Time...Talking, -8,-7)))))/60

#Pull out the days of the month that have data in the report
unique.days <- newdate.unique

#Identify CLI withheld calls from ID withheld, anonymous and phone box calls 
withheld.list <- c()
for (p in 1:length(call.data$Calling.Line.Identification)) {
  if (call.data$Calling.Line.Identification[p]=="+645555555" | call.data$Calling.Line.Identification[p]=="anonymous" | call.data$Calling.Line.Identification[p]=="000101" ) { withheld.list <- c(withheld.list, 1) } else withheld.list <- c(withheld.list, 0)
}
call.data$Withheld <- withheld.list

#Variables for later use
CLIs <- c() 

for (j in 1:length(call.data$Withheld)) {
  CLIs[j] <- as.numeric(as.character(call.data$Calling.Line.Identification[j]))
  }
call.day <- c()
n.day <- c()
v.day <- c()
q.day <- c()
triage.n.day <- c()
triage.v.day <- c()
triage.q.day <- c()
helpline.n.day <- c()
helpline.v.day <- c()
helpline.q.day <- c()
CLIWithheld.day <- c()
unique.callers <- c()
triage.unique.callers <- c()
helpline.unique.callers <- c()
unique.answered.callers <- c()
triage.unique.answered.callers <- c()
helpline.unique.answered.callers <- c()

####

#Cellphone, landline, and CLI blocked calls and callers
call.data$Calling.Line.Identification[call.data$Calling.Line.Identification=="anonymous"] <- 645555555
call.data$Calling.Line.Identification <- as.numeric(as.character(call.data$Calling.Line.Identification))
call.data$caller.type <- "landline"
for (l in 1:nrow(call.data)) {
  if (str_sub(call.data$Calling.Line.Identification[l], 1,3)=="642") { call.data$caller.type[l] <- "cell" }
  if ((str_sub(call.data$Calling.Line.Identification[l], 1,9)=="645555555") | str_sub(call.data$Calling.Line.Identification[l],1,9)=="anonymous") { call.data$caller.type[l] <- "anon" }
}

cellphone.calls <- length(na.omit(str_match(call.data$caller.type, "cell")))
anonymous.calls <- length(na.omit(str_match(call.data$Calling.Line.Identification, "+645555555")))+length(na.omit(str_match(call.data$Calling.Line.Identification, "anonymous")))
landline.calls <- length(call.data[,1]) - (cellphone.calls+anonymous.calls)
cellphone.calls.PA <- paste(100*(round(length(call.data$caller.type[call.data$caller.type=="cell"&call.data$Release..Type=="N"])/length(call.data$caller.type[call.data$caller.type=="cell"]),3)),"%")
landline.calls.PA <- paste(100*(round(length(call.data$caller.type[call.data$caller.type=="landline"&call.data$Release..Type=="N"])/length(call.data$caller.type[call.data$caller.type=="landline"]),3)),"%")
anonymous.calls.PA <- paste(100*(round(length(call.data$caller.type[call.data$caller.type=="anon"&call.data$Release..Type=="N"])/length(call.data$caller.type[call.data$caller.type=="anon"]),3)),"%")


cellphone.callers <- length(unique(as.character(call.data$Calling.Line.Identification[str_detect(call.data$Calling.Line.Identification, "+642")])))
landline.callers <- length(unique(as.character(call.data$Calling.Line.Identification[!(str_detect(call.data$Calling.Line.Identification, "+642")|str_detect(call.data$Calling.Line.Identification, "+64555")|str_detect(call.data$Calling.Line.Identification, "anon"))])))
callcallerratio <- (cellphone.callers+landline.callers)/(cellphone.calls+landline.calls)
anonymous.callers <- round(callcallerratio*anonymous.calls)
cellphone.callers.PA <- paste(100*round(length(unique(as.character(call.data$Calling.Line.Identification[str_detect(call.data$Calling.Line.Identification, "+642")&call.data$Release..Type=="N"])))/cellphone.callers,3),"%")
landline.callers.PA <- paste(100*round(length(unique(as.character(call.data$Calling.Line.Identification[!(str_detect(call.data$Calling.Line.Identification, "+642")|str_detect(call.data$Calling.Line.Identification, "+64555")|str_detect(call.data$Calling.Line.Identification, "anon"))&call.data$Release..Type=="N"])))/landline.callers,3),"%")
calls.and.callers <- c("Total calls","% Total calls answered","Daily unique callers","% unique callers answered")

###

#Calls incoming to and answered by Youthline centres

GL.calls <- call.data[call.data$Queue.Number=="710"&call.data$caller.type=="landline",]
mnk.calls <- call.data[call.data$Queue.Number=="791"&call.data$caller.type=="landline",]
PN.calls <- call.data[call.data$Queue.Number=="793"&call.data$caller.type=="landline",]
wel.calls <- call.data[call.data$Queue.Number=="792"&call.data$caller.type=="landline",]
chch.calls <- call.data[call.data$Queue.Number=="796"&call.data$caller.type=="landline",]
dun.calls <- call.data[call.data$Queue.Number=="795"&call.data$caller.type=="landline",]
inv.calls <- call.data[call.data$Queue.Number=="794"&call.data$caller.type=="landline",]
CI.calls <- call.data[call.data$Queue.Number=="240"|call.data$Queue.Number=="340",]
#afterhours.calls <- call.data[call.data$Queue.Number=="797",]
triage.calls <- call.data[call.data$Queue.Number=="222"|call.data$Queue.Number=="223"|call.data$Queue.Number=="789"|call.data$Queue.Number=="797",]

GL.calls.N <- GL.calls[GL.calls$Release..Type=="N"& str_sub(GL.calls$Extension.Number,1,3)!="500",]
mnk.calls.N <-  mnk.calls[mnk.calls$Release..Type=="N"& str_sub(mnk.calls$Extension.Number,1,3)!="500",]
PN.calls.N <- PN.calls[PN.calls$Release..Type=="N"& str_sub(PN.calls$Extension.Number,1,3)!="500",]
wel.calls.N <- wel.calls[wel.calls$Release..Type=="N"& str_sub(wel.calls$Extension.Number,1,3)!="500",]
chch.calls.N <- chch.calls[chch.calls$Release..Type=="N"& str_sub(chch.calls$Extension.Number,1,3)!="500",]
dun.calls.N <- dun.calls[dun.calls$Release..Type=="N"& str_sub(dun.calls$Extension.Number,1,3)!="500",]
inv.calls.N <- inv.calls[inv.calls$Release..Type=="N"& str_sub(inv.calls$Extension.Number,1,3)!="500",]
CI.calls.N <- CI.calls[CI.calls$Release..Type=="N"& str_sub(CI.calls$Extension.Number,1,3)!="500",]
#afterhours.calls.N <- afterhours.calls[afterhours.calls$Release..Type=="N",]
triage.calls.N1 <- triage.calls[triage.calls$Release..Type=="N"& str_sub(triage.calls$Extension.Number,1,3)!="500",]

all.answered.calls <- call.data[call.data$Release..Type=="N"& str_sub(call.data$Extension.Number,1,3)!="500",]

GL.helpline.GL <- length(na.omit(match(GL.calls.N$Extension.Number,GL.helpline)))
GL.helpline.triage <- length(na.omit(match(GL.calls.N$Extension.Number,all.triage)))
GL.helpline.other <- length(GL.calls.N$Extension.Number) - (GL.helpline.GL+GL.helpline.triage)

mnk.helpline.mnk <- length(na.omit(match(mnk.calls.N$Extension.Number,mnk.helpline)))
mnk.helpline.triage <- length(na.omit(match(mnk.calls.N$Extension.Number,all.triage)))
mnk.helpline.other <- length(mnk.calls.N$Extension.Number) - (mnk.helpline.mnk+mnk.helpline.triage)

PN.helpline.PN <- length(na.omit(match(PN.calls.N$Extension.Number,PN.helpline)))
PN.helpline.triage <- length(na.omit(match(PN.calls.N$Extension.Number,all.triage)))
PN.helpline.other <- length(PN.calls.N$Extension.Number) - (PN.helpline.PN+PN.helpline.triage)

wel.helpline.wel <- length(na.omit(match(wel.calls.N$Extension.Number,wel.helpline)))
wel.helpline.triage <- length(na.omit(match(wel.calls.N$Extension.Number,all.triage)))
wel.helpline.other <- length(wel.calls.N$Extension.Number) - (wel.helpline.wel+wel.helpline.triage)

chch.helpline.chch <- length(na.omit(match(chch.calls.N$Extension.Number,chch.helpline)))
chch.helpline.triage <- length(na.omit(match(chch.calls.N$Extension.Number,all.triage)))
chch.helpline.other <- length(chch.calls.N$Extension.Number) - (chch.helpline.chch+chch.helpline.triage)

inv.helpline.inv <- length(na.omit(match(inv.calls.N$Extension.Number,inv.helpline)))
inv.helpline.triage <- length(na.omit(match(inv.calls.N$Extension.Number,all.triage)))
inv.helpline.other <- length(inv.calls.N$Extension.Number) - (inv.helpline.inv+inv.helpline.triage)

dun.helpline.dun <- length(na.omit(match(dun.calls.N$Extension.Number,dun.helpline)))
dun.helpline.triage <- length(na.omit(match(dun.calls.N$Extension.Number,all.triage)))
dun.helpline.other <- length(dun.calls.N$Extension.Number) - (dun.helpline.dun+dun.helpline.triage)

triage.triage <- length(na.omit(match(triage.calls.N1$Extension.Number,all.triage)))
#afterhours.afterhours <- length(na.omit(match(afterhours.calls.N$Extension.Number,afterhours.helpline)))

##----##----##----##----##----##----##----##----##----##----##----##----##

###################
#Other YL Services#
###################

#Miscarriage Support
ms.data <- raw.data[raw.data$Queue.Number==miscarriage.support,]
ms.total <- nrow(ms.data)
ms.answered <- length(ms.data$Release..Type[ms.data$Release..Type=="N"])
ms.missed <- ms.total-ms.answered
ms.vmx <- length(ms.data$Release..Type[ms.data$Release..Type=="V"])
ms.data$Talking.time <- (as.numeric(str_sub(ms.data$Time...Talking, -2,-1))+(60*(as.numeric(str_sub(ms.data$Time...Talking, -5,-4))))+(3600*(as.numeric(str_sub(ms.data$Time...Talking, -8,-7)))))/60
average.ms.talk.time <- mean(ms.data$Talking.time[ms.data$Release..Type=="N"])
average.ms.talk.time <- minmaker(average.ms.talk.time)

##MISCARRIAGE TABLE
miscarriage <- data.frame(ms.total,ms.answered,ms.missed,average.ms.talk.time,ms.vmx)
colnames(miscarriage) <- c("Total calls","Answered","Missed","Average call duration","Message left")

#YL SAS services
raw.data.BH <- raw.data[newdate.raw$hour>=9&newdate.raw$hour<17,]
raw.data.AH <- raw.data[newdate.raw$hour<9|newdate.raw$hour>=17,]

north.shore.calls.BH <- c(length(raw.data.BH$Queue.Number[raw.data.BH$Queue.Number==yl.north.shore]),length(raw.data.BH$Queue.Number[raw.data.BH$Queue.Number==yl.north.shore & raw.data.BH$Release..Type=="N"]))
waitakere.calls.BH <- c(length(raw.data.BH$Queue.Number[raw.data.BH$Queue.Number==yl.waitakere]),length(raw.data.BH$Queue.Number[raw.data.BH$Queue.Number==yl.waitakere & raw.data.BH$Release..Type=="N"]))
pregnancy.calls.BH <- c(length(raw.data.BH$Queue.Number[raw.data.BH$Queue.Number==yl.pregnancy]),length(raw.data.BH$Queue.Number[raw.data.BH$Queue.Number==yl.pregnancy & raw.data.BH$Release..Type=="N"]))
action.education.calls.BH <- c(length(raw.data.BH$Queue.Number[raw.data.BH$Queue.Number==action.education]),length(raw.data.BH$Queue.Number[raw.data.BH$Queue.Number==action.education & raw.data.BH$Release..Type=="N"]))
f2f.calls.BH <- c(length(raw.data.BH$Queue.Number[raw.data.BH$Queue.Number==yl.f2f]),length(raw.data.BH$Queue.Number[raw.data.BH$Queue.Number==yl.f2f & raw.data.BH$Release..Type=="N"]))
youth.services.calls.BH <- c(length(raw.data.BH$Queue.Number[raw.data.BH$Queue.Number==youth.services]),length(raw.data.BH$Queue.Number[raw.data.BH$Queue.Number==youth.services & raw.data.BH$Release..Type=="N"]))

north.shore.calls.AH <- c(length(raw.data.AH$Queue.Number[raw.data.AH$Queue.Number==yl.north.shore]),length(raw.data.AH$Queue.Number[raw.data.AH$Queue.Number==yl.north.shore & raw.data.AH$Release..Type=="N"]))
waitakere.calls.AH <- c(length(raw.data.AH$Queue.Number[raw.data.AH$Queue.Number==yl.waitakere]),length(raw.data.AH$Queue.Number[raw.data.AH$Queue.Number==yl.waitakere & raw.data.AH$Release..Type=="N"]))
pregnancy.calls.AH <- c(length(raw.data.AH$Queue.Number[raw.data.AH$Queue.Number==yl.pregnancy]),length(raw.data.AH$Queue.Number[raw.data.AH$Queue.Number==yl.pregnancy & raw.data.AH$Release..Type=="N"]))
action.education.calls.AH <- c(length(raw.data.AH$Queue.Number[raw.data.AH$Queue.Number==action.education]),length(raw.data.AH$Queue.Number[raw.data.AH$Queue.Number==action.education & raw.data.AH$Release..Type=="N"]))
f2f.calls.AH <- c(length(raw.data.AH$Queue.Number[raw.data.AH$Queue.Number==yl.f2f]),length(raw.data.AH$Queue.Number[raw.data.AH$Queue.Number==yl.f2f & raw.data.AH$Release..Type=="N"]))
youth.services.calls.AH <- c(length(raw.data.AH$Queue.Number[raw.data.AH$Queue.Number==youth.services]),length(raw.data.AH$Queue.Number[raw.data.AH$Queue.Number==youth.services & raw.data.AH$Release..Type=="N"]))


##SAS TABLES
sas.table.BH <- data.frame(c("YL North Shore","YL Waitakere","Pregnancy Centre","Action Education","Counselling","Youth Services"),c(north.shore.calls.BH[1],waitakere.calls.BH[1],pregnancy.calls.BH[1],action.education.calls.BH[1],f2f.calls.BH[1],youth.services.calls.BH[1]),c(north.shore.calls.BH[2],waitakere.calls.BH[2],pregnancy.calls.BH[2],action.education.calls.BH[2],f2f.calls.BH[2],youth.services.calls.BH[2]))
colnames(sas.table.BH) <- c("Service","Incoming","Answered")
sas.table.BH$Missed <- sas.table.BH$Incoming-sas.table.BH$Answered
sas.table.BH$"Percent answered" <- paste(round(100*sas.table.BH$Answered/sas.table.BH$Incoming,2),"%")

sas.table.AH <- data.frame(c("YL North Shore","YL Waitakere","Pregnancy Centre","Action Education","Counselling","Youth Services"),c(north.shore.calls.AH[1],waitakere.calls.AH[1],pregnancy.calls.AH[1],action.education.calls.AH[1],f2f.calls.AH[1],youth.services.calls.AH[1]),c(north.shore.calls.AH[2],waitakere.calls.AH[2],pregnancy.calls.AH[2],action.education.calls.AH[2],f2f.calls.AH[2],youth.services.calls.AH[2]))
colnames(sas.table.AH) <- c("Service","Incoming","Answered")
sas.table.AH$Missed <- sas.table.AH$Incoming-sas.table.AH$Answered
sas.table.AH$"Percent answered" <- paste(round(100*sas.table.AH$Answered/sas.table.AH$Incoming,2),"%")

##Helpline calls summary##
all.calls <- nrow(call.data)
all.calls.V <- length(call.data$Release[call.data$Release..Type=="V"])
all.calls.N <- length(call.data$Release[call.data$Release..Type=="N"])
all.calls.Q <- length(call.data$Release[call.data$Release..Type=="Q"])

helpline.all.calls <- length(na.omit(match(call.data$Queue.Number,helpline)))
call.data$helpline.match <- as.numeric(match(call.data$Original.Queue,helpline,0))
#call.data$helpline.match[call.data$helpline.match==0] <- as.numeric(match(call.data$Queue.Number[call.data$helpline.match==0],helpline,0))
helpline.calls.V <- length(na.omit(match(call.data$Queue.Number[call.data$Release..Type=="V"],helpline)))
helpline.calls.N <- length(na.omit(match(call.data$Queue.Number[call.data$Release..Type=="N"],helpline)))
helpline.calls.Q <- length(na.omit(match(call.data$Queue.Number[call.data$Release..Type=="Q"],helpline)))
  
triage.all.calls <- length(na.omit(match(call.data$Queue.Number,triage)))
call.data$triage.match <- as.numeric(match(call.data$Original.Queue,triage,0))
#call.data$triage.match[call.data$triage.match==0] <- as.numeric(match(call.data$Queue.Number[call.data$triage.match==0],triage,0))
triage.calls.V <- length(na.omit(match(call.data$Queue.Number[call.data$Release..Type=="V"],triage)))
triage.calls.N <- length(na.omit(match(call.data$Queue.Number[call.data$Release..Type=="N"],triage)))
triage.calls.Q <- length(na.omit(match(call.data$Queue.Number[call.data$Release..Type=="Q"],triage)))

answered.calls <- call.data[call.data$Release..Type=="N" & str_sub(call.data$Extension.Number,1,3)!="500",]
mean.call.length <- mean(answered.calls$talk.time.mins)
median.call.length <- median(answered.calls$talk.time.mins)
total.call.length <- sum(answered.calls$talk.time.mins)
mean.helpline.call.length <- as.character(round(mean(call.data$talk.time.mins[call.data$Release..Type=="N" & call.data$helpline.match>0]),1))
median.helpline.call.length <- as.character(round(median(call.data$talk.time.mins[call.data$Release..Type=="N" & call.data$helpline.match>0]),1))
mean.triage.call.length <- as.character(round(mean(call.data$talk.time.mins[call.data$Release..Type=="N" & call.data$triage.match>0]),1))
median.triage.call.length <- as.character(round(median(call.data$talk.time.mins[call.data$Release..Type=="N" & call.data$triage.match>0]),1))

helpline.calls.for.length <- (call.data$talk.time.mins[call.data$Release..Type=="N" & call.data$helpline.match>0])
triage.calls.for.length <- (call.data$talk.time.mins[call.data$Release..Type=="N" & call.data$triage.match>0])

h05 <- length(helpline.calls.for.length[helpline.calls.for.length<5])
h530 <- length(helpline.calls.for.length[helpline.calls.for.length<30 & helpline.calls.for.length>5])
h30plus <- length(helpline.calls.for.length[helpline.calls.for.length>30])

h5.prop <- round(100*h05/sum(h05,h530,h30plus),1)
h530.prop <- round(100*h530/sum(h05,h530,h30plus),1)
h30plus.prop <- round(100*h30plus/sum(h05,h530,h30plus),1)

t05 <- length(triage.calls.for.length[triage.calls.for.length<5])
t530 <- length(triage.calls.for.length[triage.calls.for.length<30 & triage.calls.for.length>5])
t30plus <- length(triage.calls.for.length[triage.calls.for.length>30])

t5.prop <- round(100*t05/sum(t05,t530,t30plus),1)
t530.prop <- round(100*t530/sum(t05,t530,t30plus),1)
t30plus.prop <- round(100*t30plus/sum(t05,t530,t30plus),1)


call.length.table <- data.frame(c("0-5 mins","5-30mins","30+ mins"," ","Mean length (mins)","Median length (mins)"),c(h05,h530,h30plus," ",mean.helpline.call.length,median.helpline.call.length),c(h5.prop,h530.prop,h30plus.prop," "," "," "),c(t05,t530,t30plus," ",mean.triage.call.length,median.triage.call.length),c(t5.prop,t530.prop,t30plus.prop," "," "," "))
colnames(call.length.table) <- c("Length","Helpline","Helpline %","Triage","Triage %")

averages.table <- call.length.table[5:6,c(1,2,4)]
colnames(averages.table) <- c(" ","Helpline","Triage")

#Urgent
urgent <- call.data[call.data$Queue.Number=="789",]
urgent.table <- data.frame(c("Total calls","Answered calls","Missed calls","Percent answered"))
urgent.table$Count <- c(as.character(length(urgent[,1])),as.character(length(urgent$Release..Type[urgent$Release..Type=="N"])),as.character(length(urgent$Release..Type[urgent$Release..Type!="N"])),paste(round(100*length(urgent$Release..Type[urgent$Release..Type=="N"])/length(urgent[,1]),2),"%"))
colnames(urgent.table) <- c("","")

#YOUTHLINE OFFICE
#Auckland offices contact totals

pomatch <- match(raw.data.BH$Queue.Number,GL.office)
pomatch[is.na(pomatch)] <- 0
nsomatch <- match(raw.data.BH$Queue.Number,NS.office)
nsomatch[is.na(nsomatch)] <- 0
mnkomatch <- match(raw.data.BH$Queue.Number,mnk.office)
mnkomatch[is.na(mnkomatch)] <- 0

PO <- raw.data.BH[pomatch>0,]
PO <- PO[str_length(PO$Calling.Line.Identification)>4,]
PO <- PO[(PO$Call.Type.=="Incoming"|PO$Call.Type.=="Queue"),]
POall <- nrow(PO)
POans <- nrow(PO[PO$Release..Type=="N",])

NSO <- raw.data.BH[nsomatch>0,]
NSO <- NSO[str_length(NSO$Calling.Line.Identification)>4,]
NSO <- NSO[(NSO$Call.Type.=="Incoming"|NSO$Call.Type.=="Queue"),]
NSOall <- nrow(NSO)
NSOans <-nrow(NSO[NSO$Release..Type=="N",])

MNKO <- raw.data.BH[mnkomatch>0,]
MNKO <- MNKO[str_length(MNKO$Calling.Line.Identification)>4,]
MNKO <- MNKO[(MNKO$Call.Type.=="Incoming"|MNKO$Call.Type.=="Queue"),]
MNKOall <- nrow(MNKO)
MNKOans <-nrow(MNKO[MNKO$Release..Type=="N",])

office.table <- data.frame("Office"=c("Ponsonby","Manukau","North Shore"),"Incoming"=c(POall,MNKOall,NSOall),"Answered"=c(POans,MNKOans,NSOans))

##----##----##----##----##----##----##----##----##----##----##----##----##

raw.data$num.start <- as.numeric(as.POSIXlt(raw.data$Start.Time, format='%Y-%m-%d %H:%M:%S'))
raw.length <- as.POSIXlt(raw.data$Total.Handle.Time, format='%H:%M:%S')
raw.add <- (raw.length$hour*3600)+(raw.length$min*60)+(raw.length$sec)
raw.data$num.end <- raw.data$num.start+raw.add

raw.data$nmbr <- 1:nrow(raw.data)
poss.transfers <- raw.data[(raw.data$Queue.Number=="222"|raw.data$Queue.Number=="223"|raw.data$Queue.Number=="224"),]
poss.transfers <- poss.transfers[poss.transfers$Release..Type=="N",]

GL.trans <- 0
mnk.trans <- 0
PN.trans <- 0
wel.trans <- 0
chch.trans <- 0
inv.trans <- 0
dun.trans <- 0
ids <- c()

for (i in poss.transfers$nmbr) {
  if(nrow(raw.data)>=(i+2)) {
    raw.data.cut <- raw.data[i:nrow(raw.data),]
    raw.data.cut <-raw.data.cut[raw.data.cut$num.start<raw.data.cut$num.end[1],]
    extn.nmbr <- as.numeric(as.character(raw.data.cut$Extension.Number[1]))
    raw.data.cut$Queue.Number <- as.numeric(as.character(raw.data.cut$Queue.Number))
    if(nrow(raw.data.cut)>2) {
      if(any(raw.data.cut$Queue.Number==extn.nmbr)) {
        rowno <- which(raw.data.cut$Queue.Number==extn.nmbr)
        if(raw.data.cut$Call.Type.[rowno]=="Outgoing") {
          dialled.extn <- na.omit(as.numeric(as.character(raw.data.cut$Dialled.Number..Out[rowno])))
          if(any(raw.data.cut$Queue.Number==dialled.extn)) {
            lastrow <- which(raw.data.cut$Queue.Number==dialled.extn)
            identifier <- raw.data.cut$nmbr[lastrow]
            ids <- c(ids, identifier)
              if(any(raw.data$Queue.Number[identifier]==all.helpline)) {
                if(raw.data$num.end[i]>=raw.data$num.start[identifier]) {
                  if(raw.data$Release..Type[identifier]=="N") {
                  if(any(raw.data$Queue.Number[identifier]==GL.helpline)) { GL.trans<- GL.trans+1 }
                  if(any(raw.data$Queue.Number[identifier]==mnk.helpline)) { mnk.trans<- mnk.trans+1 }
                  if(any(raw.data$Queue.Number[identifier]==PN.helpline)) { PN.trans<- PN.trans+1 }
                  if(any(raw.data$Queue.Number[identifier]==wel.helpline)) { wel.trans<- wel.trans+1 }
                  if(any(raw.data$Queue.Number[identifier]==chch.helpline)) { chch.trans<- chch.trans+1 }
                  if(any(raw.data$Queue.Number[identifier]==inv.helpline)) { inv.trans<- inv.trans+1 }
                  if(any(raw.data$Queue.Number[identifier]==dun.helpline)) { dun.trans<- dun.trans+1 }
                }
              }
            }
          }
        }
      }
    }
  }
}


GL.answered <- length(na.omit(match(all.answered.calls$Extension.Number,GL.helpline)))+GL.trans
mnk.answered <-length(na.omit(match(all.answered.calls$Extension.Number,mnk.helpline)))+mnk.trans
PN.answered <-length(na.omit(match(all.answered.calls$Extension.Number,PN.helpline)))+PN.trans
wel.answered <-length(na.omit(match(all.answered.calls$Extension.Number,wel.helpline)))+wel.trans
chch.answered <-length(na.omit(match(all.answered.calls$Extension.Number,chch.helpline)))+chch.trans
dun.answered <-length(na.omit(match(all.answered.calls$Extension.Number,dun.helpline)))+dun.trans
inv.answered <-length(na.omit(match(all.answered.calls$Extension.Number,inv.helpline)))+inv.trans
triage.answered <- length(na.omit(match(all.answered.calls$Extension.Number,all.triage)))
#afterhours.answered <- length(na.omit(match(all.answered.calls$Extension.Number,afterhours.helpline)))



######################
#HELPLINE CALL TABLES#
######################

##CALLERS FROM CELLPHONES AND LANDLINES
caller.table <- data.frame(calls.and.callers, c(cellphone.calls,cellphone.calls.PA,cellphone.callers,cellphone.callers.PA),c(landline.calls,landline.calls.PA,landline.callers,landline.callers.PA),c(anonymous.calls,anonymous.calls.PA,anonymous.callers,"-"))
colnames(caller.table) <- c(".","Cellphone","Landline","Anonymous")

##MAIN TABLE
centres.table <- data.frame(c("Ponsonby","Manukau","Palmerston North","Wellington","Chch","Otago","Southland","Triage"))
colnames(centres.table) <- "Centre"
centres.table$"Local calls" <- c(length(GL.calls[,1]),length(mnk.calls[,1]),length(PN.calls[,1]),length(wel.calls[,1]),length(chch.calls[,1]),length(dun.calls[,1]),length(inv.calls[,1]),length(triage.calls[,1]))
centres.table$"Answered locally" <- c(GL.helpline.GL,mnk.helpline.mnk,PN.helpline.PN,wel.helpline.wel,chch.helpline.chch,dun.helpline.dun,inv.helpline.inv,triage.triage)
centres.table$"Answered by triage" <- c(GL.helpline.triage,mnk.helpline.triage,PN.helpline.triage,wel.helpline.triage,chch.helpline.triage,dun.helpline.triage,inv.helpline.triage,NA)
centres.table$"Answered by others" <- c(GL.helpline.other,mnk.helpline.other,PN.helpline.other,wel.helpline.other,chch.helpline.other,dun.helpline.other,inv.helpline.other,NA)
centres.table$"Missed calls" <- c(nrow(GL.calls)-nrow(GL.calls.N),nrow(mnk.calls)-nrow(mnk.calls.N),nrow(PN.calls)-nrow(PN.calls.N),nrow(wel.calls)-nrow(wel.calls.N),nrow(chch.calls)-nrow(chch.calls.N),nrow(dun.calls)-nrow(dun.calls.N),nrow(inv.calls)-nrow(inv.calls.N),nrow(triage.calls)-nrow(triage.calls.N1))
centres.table$"Total calls answered by centre" <- c(GL.answered,mnk.answered,PN.answered,wel.answered,chch.answered,dun.answered,inv.answered,triage.answered)

triage.table <- data.frame(length(triage.calls[,1]),nrow(triage.calls.N1), length(triage.calls[,1])-nrow(triage.calls.N1),(triage.answered-nrow(triage.calls.N1)))
colnames(triage.table) <- c("Triage calls","Triage calls answered","Triage calls missed","Helpine calls answered by Triage")

##COOK ISLANDS TABLE
cook.islands <- data.frame(length(CI.calls[,1]),length(CI.calls.N[,1]),length(CI.calls[,1])-length(CI.calls.N[,1]))
colnames(cook.islands) <- c("Total calls","Answered calls","Missed calls")

##TRIAGE UNIQUE ROUTED CALLERS
triage.unique.routed <- length(unique(call.data$Calling.Line.Identification[call.data$Queue.Number==223|call.data$Queue.Number==224|call.data$Queue.Number==225|call.data$Queue.Number==222&call.data$Calling.Line.Identification!="anonymous"&call.data$Calling.Line.Identification!="+645555555"]))

##CALL LENGTH
length.order <- order(call.data$talk.time.mins, decreasing=T)
length.order <- length.order[1:10]
longest.calls <- round(call.data$talk.time.mins[length.order],0)

##BUSIEST DAY
calls.mon <- length(call.data$weekday[call.data$weekday=="Monday"])/length(unique(call.data$the.date[call.data$weekday=="Monday"]))
callers.mon <- length(unique(call.data$Calling.Line.Identification[call.data$weekday=="Monday"]))-2
anonymous.mon <- length(na.omit(str_match(call.data$Calling.Line.Identification[call.data$weekday=="Monday"], "+645555555")))+length(na.omit(str_match(call.data$Calling.Line.Identification[call.data$weekday=="Monday"], "anonymous")))
anonymous.mon <- round(anonymous.mon*callcallerratio,0)
callers.mon <- (callers.mon+anonymous.mon)/length(unique(call.data$the.date[call.data$weekday=="Monday"]))

calls.tue <- length(call.data$weekday[call.data$weekday=="Tuesday"])/length(unique(call.data$the.date[call.data$weekday=="Tuesday"]))
callers.tue <- length(unique(call.data$Calling.Line.Identification[call.data$weekday=="Tuesday"]))-2
anonymous.tue <- length(na.omit(str_match(call.data$Calling.Line.Identification[call.data$weekday=="Tuesday"], "+645555555")))+length(na.omit(str_match(call.data$Calling.Line.Identification[call.data$weekday=="Tuesday"], "anonymous")))
anonymous.tue <- round(anonymous.tue*callcallerratio,0)
callers.tue <- (callers.tue+anonymous.tue)/length(unique(call.data$the.date[call.data$weekday=="Tuesday"]))

calls.wed <- length(call.data$weekday[call.data$weekday=="Wednesday"])/length(unique(call.data$the.date[call.data$weekday=="Wednesday"]))
callers.wed <- length(unique(call.data$Calling.Line.Identification[call.data$weekday=="Wednesday"]))-2
anonymous.wed <- length(na.omit(str_match(call.data$Calling.Line.Identification[call.data$weekday=="Wednesday"], "+645555555")))+length(na.omit(str_match(call.data$Calling.Line.Identification[call.data$weekday=="Wednesday"], "anonymous")))
anonymous.wed <- round(anonymous.wed*callcallerratio,0)
callers.wed <- (callers.wed+anonymous.wed)/length(unique(call.data$the.date[call.data$weekday=="Wednesday"]))

calls.thu <- length(call.data$weekday[call.data$weekday=="Thursday"])/length(unique(call.data$the.date[call.data$weekday=="Thursday"]))
callers.thu <- length(unique(call.data$Calling.Line.Identification[call.data$weekday=="Thursday"]))-2
anonymous.thu <- length(na.omit(str_match(call.data$Calling.Line.Identification[call.data$weekday=="Thursday"], "+645555555")))+length(na.omit(str_match(call.data$Calling.Line.Identification[call.data$weekday=="Thursday"], "anonymous")))
anonymous.thu <- round(anonymous.thu*callcallerratio,0)
callers.thu <- (callers.thu+anonymous.thu)/length(unique(call.data$the.date[call.data$weekday=="Thursday"]))

calls.fri <- length(call.data$weekday[call.data$weekday=="Friday"])/length(unique(call.data$the.date[call.data$weekday=="Friday"]))
callers.fri <- length(unique(call.data$Calling.Line.Identification[call.data$weekday=="Friday"]))-2
anonymous.fri <- length(na.omit(str_match(call.data$Calling.Line.Identification[call.data$weekday=="Friday"], "+645555555")))+length(na.omit(str_match(call.data$Calling.Line.Identification[call.data$weekday=="Friday"], "anonymous")))
anonymous.fri <- round(anonymous.fri*callcallerratio,0)
callers.fri <- (callers.fri+anonymous.fri)/length(unique(call.data$the.date[call.data$weekday=="Friday"]))

calls.sat <- length(call.data$weekday[call.data$weekday=="Saturday"])/length(unique(call.data$the.date[call.data$weekday=="Saturday"]))
callers.sat <- length(unique(call.data$Calling.Line.Identification[call.data$weekday=="Saturday"]))-2
anonymous.sat <- length(na.omit(str_match(call.data$Calling.Line.Identification[call.data$weekday=="Saturday"], "+645555555")))+length(na.omit(str_match(call.data$Calling.Line.Identification[call.data$weekday=="Saturday"], "anonymous")))
anonymous.sat <- round(anonymous.sat*callcallerratio,0)
callers.sat <- (callers.sat+anonymous.sat)/length(unique(call.data$the.date[call.data$weekday=="Saturday"]))

calls.sun <- length(call.data$weekday[call.data$weekday=="Sunday"])/length(unique(call.data$the.date[call.data$weekday=="Sunday"]))
callers.sun <- length(unique(call.data$Calling.Line.Identification[call.data$weekday=="Sunday"]))-2
anonymous.sun <- length(na.omit(str_match(call.data$Calling.Line.Identification[call.data$weekday=="Sunday"], "+645555555")))+length(na.omit(str_match(call.data$Calling.Line.Identification[call.data$weekday=="Sunday"], "anonymous")))
anonymous.sun <- round(anonymous.sun*callcallerratio,0)
callers.sun <- (callers.sun+anonymous.sun)/length(unique(call.data$the.date[call.data$weekday=="Sunday"]))

DOW.table <- data.frame(c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"), c(calls.mon,calls.tue,calls.wed,calls.thu,calls.fri,calls.sat,calls.sun),c(callers.mon,callers.tue,callers.wed,callers.thu,callers.fri,callers.sat,callers.sun))
colnames(DOW.table) <- c("Day of week","Average call volume","Average number of unique callers")


#BUSIEST HOUR OF THE DAY
hours.table <- data.frame(newdate$hour,1:length(newdate$hour))
colnames(hours.table) <- c("HOD","index")
hours.index <- 0
busiest.hour <- data.frame(c("12am-1am","1am-2am","2am-3am","3am-4am","4am-5am","5am-6am","6am-7am","7am-8am","8am-9am","9am-10am","10am-11am","11am-12pm","12pm-1pm","1pm-2pm","2pm-3pm","3pm-4pm","4pm-5pm","5pm-6pm","6pm-7pm","7pm-8pm","8pm-9pm","9pm-10pm","10pm-11pm","11pm-12am"))
busiest.hour$calls <- 0
busiest.hour$callers <- 0
for (f in 1:24) {
  busiest.hour$calls[f] <- length(hours.table$HOD[hours.table$HOD==(f-1)])/max(newdate$mday)
  hours.index <- hours.table$index[hours.table$HOD==(f-1)]
  hours.unique <- length(unique(call.data$Calling.Line.Identification[hours.index]))-2
  anonymous.hours <- length(na.omit(str_match(call.data$Calling.Line.Identification[hours.index], "+645555555")))+length(na.omit(str_match(call.data$Calling.Line.Identification[hours.index], "anonymous")))
  anonymous.hours <- round(anonymous.sun*callcallerratio,0)
  hours.unique <- hours.unique + anonymous.hours
  busiest.hour$callers[f] <- hours.unique/max(newdate$mday)
}
colnames(busiest.hour) <- c("Hour","Average call volume","Average number of unique callers")

####MFC
call.data$Release..Type <- as.character(call.data$Release..Type)
all.callers <- str_sub(call.data$Calling.Line.Identification, start=1)
call.data$CLI <- str_sub(call.data$Calling.Line.Identification, start=1)
all.callers <- all.callers[all.callers!="645555555"]
all.callers <- all.callers[all.callers!="nonymous"]
all.callers <- as.numeric(all.callers)
call.numbers.per.CLI <- c()
working.callers <- 0
MFC.call.sec <- as.numeric(str_sub(call.data$Time...Talking, start=-2,end=-1))
MFC.call.min <- as.numeric(str_sub(call.data$Time...Talking, start=-5,end=-4))
MFC.call.hr <- as.numeric(str_sub(call.data$Time...Talking, start=1,end=2))
call.data$MFC.call.time <- (MFC.call.sec/60) + MFC.call.min + (MFC.call.hr*60) 

all.unique.callers <- data.frame(as.character(unique(all.callers)))
colnames(all.unique.callers) <- "CLI"
all.unique.callers$CLI <- as.numeric(as.character(all.unique.callers$CLI))
call.data$CLI <- as.numeric(call.data$CLI)
for (u in 1:length(all.unique.callers$CLI)) {
  working.callers <- all.callers[all.unique.callers$CLI[u]==all.callers]
  all.unique.callers$calls[u] <- length(working.callers)
  all.unique.callers$answered.calls[u] <- length(call.data$Release..Type[call.data$Release..Type=="N" & call.data$CLI==all.unique.callers$CLI[u]])
  all.unique.callers$total.call.time[u] <- sum(call.data$MFC.call.time[call.data$CLI==all.unique.callers$CLI[u]])
}

all.unique.callers$average.call.time <- all.unique.callers$total.call.time/all.unique.callers$answered.calls

sort.MFC.calls <- order(all.unique.callers$calls, decreasing=T)
sort.MFC.answered <- order(all.unique.callers$answered.calls, decreasing=T)
sort.MFC.time <- order(all.unique.callers$total.call.time, decreasing=T)
sort.MFC.calls <- sort.MFC.calls[1:10]
sort.MFC.answered <- sort.MFC.answered[1:10]
sort.MFC.time <- sort.MFC.time[1:10]
all.unique.callers$total.call.time <- minmaker(all.unique.callers$total.call.time)
all.unique.callers$average.call.time <- minmaker(all.unique.callers$average.call.time)
all.unique.callers$CLI <- as.character(all.unique.callers$CLI)

#MFC TABLES
MFC.calls <- data.frame(all.unique.callers[sort.MFC.calls,])
colnames(MFC.calls) <- c("CLI","Calls","Answered calls","Total call time","Average call time")
MFC.answered <- data.frame(all.unique.callers[sort.MFC.answered,])
MFC.time <- data.frame(all.unique.callers[sort.MFC.time,])
colnames(MFC.answered) <- c("CLI","Calls","Answered calls","Total call time","Average call time")
colnames(MFC.time) <- c("CLI","Calls","Answered calls","Total call time","Average call time")



#######################
##BEGIN DATA TRAWLING##
#######################

for (day in unique.days) {
  day.call.data <- call.data[the.date==day,]
  call.count <- 0
  v.count <- 0
  n.count <- 0 
  q.count <- 0
  triage.v.count <- 0
  triage.n.count <- 0 
  triage.q.count <- 0
  helpline.v.count <- 0
  helpline.n.count <- 0 
  helpline.q.count <- 0
  CLIWithheld <- 0
  uniques <- c()
  triage.unique <- c()
  helpline.unique <- c()
  helpline.unique.answered <- c()
  unique.answered <- c()
  triage.unique.answered <- c()
  current.release <- "V"
  
  #Start the cycle for each day (through all lines of data)
  for (d in 1:length(day.call.data$Start.Time)) {
    
    #Find matches for each day and assign to v,n and q (and same for triage)
    if (day.call.data$the.date[d]==day) {
      call.count <-  call.count +1
      current.release <- day.call.data$Release..Type[d]
      
      if (current.release=="V") { 
        v.count <- v.count +1 
        if(any(day.call.data$Queue.Number[d]==triage|day.call.data$Original.Queue[d]==triage)) {
          triage.v.count <- triage.v.count +1 } 
        if (any(day.call.data$Queue.Number[d]==helpline|day.call.data$Original.Queue[d]==helpline)) {
            helpline.v.count <- helpline.v.count +1 
          }
      }
      if (current.release=="Q") { 
        q.count <- q.count +1 
        if(any(day.call.data$Queue.Number[d]==triage|day.call.data$Original.Queue[d]==triage)) {
          triage.q.count <- triage.q.count +1 } 
        if (any(day.call.data$Queue.Number[d]==helpline|day.call.data$Original.Queue[d]==helpline)) {
            helpline.q.count <- helpline.q.count +1 
          }
      }
      if (current.release=="N") { 
        n.count <- n.count +1 
        if(any(day.call.data$Queue.Number[d]==triage|day.call.data$Original.Queue[d]==triage)) {
          triage.n.count <- triage.n.count +1 }         
        if (any(day.call.data$Queue.Number[d]==helpline|day.call.data$Original.Queue[d]==helpline)) {
            helpline.n.count <- helpline.n.count +1 
        }
      }
      if (day.call.data$Withheld[d]==1) { CLIWithheld <- CLIWithheld +1 }
      if (day.call.data$Withheld[d]==0) { 
        uniques <- c(uniques, day.call.data$CLI[d])
        uniques <- unique(uniques)
        if (any(day.call.data$Queue.Number[d]==triage|day.call.data$Original.Queue[d]==triage)) {
          triage.unique <- c(triage.unique,day.call.data$CLI[d])
          triage.unique <- unique(triage.unique)
        }
        if (any(day.call.data$Queue.Number[d]==helpline|day.call.data$Original.Queue[d]==helpline)) {
          helpline.unique <- c(helpline.unique, day.call.data$CLI[d])
          helpline.unique <- unique(helpline.unique)
        }
        if (current.release=="N") {
          unique.answered <- c(unique.answered, day.call.data$CLI[d])
          unique.answered <- unique(unique.answered)
          if (any(day.call.data$Queue.Number[d]==triage)) {
            triage.unique.answered <- c(triage.unique.answered, day.call.data$CLI[d])
            triage.unique.answered <- unique(triage.unique.answered)
          }
          if (any(day.call.data$Queue.Number[d]==helpline|day.call.data$Original.Queue[d]==helpline)) {
            helpline.unique.answered <- c(helpline.unique.answered, day.call.data$CLI[d])
            helpline.unique.answered <- unique(helpline.unique.answered)
          }
        }
      }
    }
  }
  
  
  
  #Add daily data to lists
  call.day <- c(call.day, call.count)
  v.day <- c(v.day, v.count)
  n.day <- c(n.day, n.count)
  q.day <- c(q.day, q.count)
  triage.v.day <- c(triage.v.day, triage.v.count)
  triage.n.day <- c(triage.n.day, triage.n.count)
  triage.q.day <- c(triage.q.day, triage.q.count)
  helpline.v.day <- c(helpline.v.day, helpline.v.count)
  helpline.n.day <- c(helpline.n.day, helpline.n.count)
  helpline.q.day <- c(helpline.q.day, helpline.q.count)
  CLIWithheld.day <- c(CLIWithheld.day, CLIWithheld)
  unique.callers <- c(unique.callers, length(uniques))
  unique.answered.callers <- c(unique.answered.callers, length(unique.answered))
  triage.unique.callers <- c(triage.unique.callers, length(triage.unique))
  triage.unique.answered.callers <- c(triage.unique.answered.callers, length(triage.unique.answered))
  helpline.unique.callers <- c(helpline.unique.callers, length(helpline.unique))
  helpline.unique.answered.callers <- c(helpline.unique.answered.callers, length(helpline.unique.answered))
  
  #Visual output to see that something's happening
  cat("\n Day: ", day, " ", call.count, "\n \n")
}

###################################
#:GET THE PROCESSED DATA IN SHAPE:#
###################################

#Output to a dataframe and export as .csv 
by.day <- data.frame(unique.days,as.numeric(str_sub(unique.days,-2,-1)), weekdays(newdate.unique.mon),call.day, v.day, n.day,q.day,triage.v.day,triage.n.day,triage.q.day,helpline.v.day,helpline.n.day,helpline.q.day, unique.callers, triage.unique.callers, helpline.unique.callers, CLIWithheld.day, unique.answered.callers, triage.unique.answered.callers,helpline.unique.answered.callers)
by.day$unique.withheld.callers <- round((by.day$unique.callers/by.day$call.day)*CLIWithheld.day)
by.day$total.unique.callers <- by.day$unique.withheld.callers + unique.callers
by.day$total.triage.unique.callers <- by.day$unique.withheld.callers + triage.unique.callers
#by.day$helpline.unique.callers <- by.day$total.unique.callers - by.day$total.triage.unique.callers
#by.day$helpline.unique.answered.callers <- by.day$unique.answered.callers - by.day$triage.unique.answered.callers
by.day$proportion.triage <- (by.day$triage.v.day+by.day$triage.n.day+by.day$triage.q.day)/by.day$call.day
by.day$proportion.triage.answered <- by.day$triage.n.day/(by.day$triage.v.day+by.day$triage.n.day+by.day$triage.q.day)
by.day$proportion.helpline.answered <- by.day$helpline.n.day/(by.day$helpline.v.day+by.day$helpline.n.day+by.day$helpline.q.day)
by.day$proportion.answered <- by.day$n.day/(by.day$call.day)
by.day$proportion.missed <- (by.day$v.day+by.day$q.day)/(by.day$call.day)
by.day$proportion.unique.answ <- (by.day$unique.answered.callers)/(by.day$unique.callers)
by.day$proportion.triage.unique.answ <- (by.day$triage.unique.answered.callers)/(by.day$triage.unique.callers)
by.day$proportion.helpline.unique.answ <- by.day$helpline.unique.answered.callers/by.day$helpline.unique.callers
#by.day$people.no.service <- round((1-by.day$proportion.unique.answ)*by.day$unique.withheld.callers)+(by.day$unique.callers-by.day$unique.answered.callers)
by.day$triage.people.no.service <- round((1-by.day$proportion.triage.unique.answ)*by.day$total.triage.unique.callers)
by.day$helpline.people.no.service <- by.day$helpline.unique.callers-by.day$helpline.unique.answered.callers
by.day$people.no.service <- by.day$triage.people.no.service+by.day$helpline.people.no.service

colnames(by.day) <- c("Mon.day","Day.of.the.month","Weekdays","Total.calls","V","N","Q","Triage.V","Triage.N","Triage.Q","Helpline.V","Helpline.N","Helpline.Q","Unique.callers.with.CLI","Triage.unique.w.CLI","Helpline.unique.callers","CLIWitheld.total","Unique.answered.callers","Triage.unique.answered.callers","Helpline.unique.answered.callers","Unique.Withheld.Callers","Total.unique.callers","Total.triage.unique.callers","proportion.triage", "proportion.triage.answered",	"proportion.helpline.answered",  "proportion.answered",	"proportion.missed",	"proportion.unique.answ","proportion.triage.unique.answ","proportion.helpine.unique.answ","triage.people.no.service","helpline.people.no.service","people.no.service")

labs <- c("","Monthly cell calls","Avg call duration (mins)","","calls answered by agent","Abandoned hangugs or VM","Total calls","Percentage of calls not answered","Cook Is total","cook Is answered","Cook is missed","","Total calls by centre","Total calls answered Ponsonby","Total calls answered Manukau","Total calls answered PN","Total calls answered Wel","Total calls answered CSI","Total calls answered Otago","Total calls answered southland","Total calls answered Triage","overnight","TOTAL","","","","","","","","","","","","","Directly answered by centre","Ponsonby","manukau","PN","Wel","Chch","otago","Southland","Triage","overnight","total","transferred from triage","otago","southland","chch","wel","PN","ponsonby","manukau","total","calls only managed by triage","","","","","","","","","Miscarriage Support","Total calls","Calls answered","hang ups or lost calls","average duration","messages left")
outs <- c("",cellphone.calls, round(mean.call.length,2),"",all.calls.N,(all.calls.V+all.calls.Q),all.calls,paste(100*round(((all.calls.V+all.calls.Q)/all.calls),2),"%",sep=''),cook.islands[1,1],cook.islands[1,2],cook.islands[1,3],"","",centres.table$"Total calls answered by centre"[1],centres.table$"Total calls answered by centre"[2],centres.table$"Total calls answered by centre"[3],centres.table$"Total calls answered by centre"[4],centres.table$"Total calls answered by centre"[5],centres.table$"Total calls answered by centre"[6],centres.table$"Total calls answered by centre"[7],centres.table$"Total calls answered by centre"[8],"included in triage",sum(centres.table$"Total calls answered by centre"),"","","","","","","","","","","","","",centres.table$"Total calls answered by centre"[1]-GL.trans,centres.table$"Total calls answered by centre"[2]-mnk.trans,centres.table$"Total calls answered by centre"[3]-PN.trans,centres.table$"Total calls answered by centre"[4]-wel.trans,centres.table$"Total calls answered by centre"[5]-chch.trans,centres.table$"Total calls answered by centre"[6]-dun.trans,centres.table$"Total calls answered by centre"[7]-inv.trans,centres.table$"Total calls answered by centre"[8],"included in triage",sum(centres.table$"Total calls answered by centre"[1]-GL.trans,centres.table$"Total calls answered by centre"[2]-mnk.trans,centres.table$"Total calls answered by centre"[3]-PN.trans,centres.table$"Total calls answered by centre"[4]-wel.trans,centres.table$"Total calls answered by centre"[5]-chch.trans,centres.table$"Total calls answered by centre"[6]-dun.trans,centres.table$"Total calls answered by centre"[7]-inv.trans,centres.table$"Total calls answered by centre"[8]),"",dun.trans,inv.trans,chch.trans,wel.trans,PN.trans,GL.trans,mnk.trans,sum(dun.trans,inv.trans,chch.trans,wel.trans,PN.trans,GL.trans,mnk.trans),centres.table$"Total calls answered by centre"[8]-sum(dun.trans,inv.trans,chch.trans,wel.trans,PN.trans,GL.trans,mnk.trans),"","","","","","","","","",miscarriage[1,1],miscarriage[1,2],miscarriage[1,3],as.character(miscarriage[1,4]),miscarriage[1,5])

stats.table <- data.frame(labs,outs)
colnames(stats.table) <- c("Stats",month.name)


###############
# MAKE OUTPUTS#
###############

#LOCATION FOR OUTPUT
write.table(stats.table,file=paste("Outputs/",month.name,".csv", sep=''), quote=F, sep=",", eol = "\n", col.names=T, row.names=F, na="NA")
#write.table(by.day, file=paste("Outputs/",month.name,"_day-by-day.csv", sep=''), quote=F, sep=",", eol = "\n", col.names=T, row.names=F, na="NA")

centres.table.no7 <- as.character(centres.table[,7])
dfct <- data.frame(centres.table[,1],centres.table.no7)
colnames(dfct) <- c("Centre","Total calls answered by centre")

#Write a word doc
knit(paste0("monthly.Rhtml"), encoding = "utf-8")
system(paste0("pandoc -o ", month.name, ".docx ", "monthly.html"),intern=T)

knit(paste0("monthly_national.Rhtml"), encoding = "utf-8")
system(paste0("pandoc -o ", month.name, "_national.docx ", "monthly_national.html"),intern=T)
  
#YS hours

#YS.call.data <- raw.data[raw.data$Queue.Number==241,]
#YS.hr <- c()
#YS.table <- data.frame(c("9am","10am","11am","12pm","1pm","2pm","3pm","4pm"))
#YS.newdate <- as.POSIXlt(YS.call.data$Start.Time, format='%Y-%m-%d %I:%M:%S %p')
#for (YShrs in 9:16) {
#  YS.hr <- YS.call.data[YS.newdate$hour==YShrs,]
#  YS.table$calls[(YShrs-8)] <- length(YS.call.data[YS.newdate$hour==YShrs,1])
#  YS.table$ans[(YShrs-8)] <- length(YS.call.data[YS.newdate$hour==YShrs&YS.call.data$Release..Type=="N",1])
#}  
#YS.table$missed <- YS.table$calls-YS.table$ans
#YS.table$percent <- paste(round(100*YS.table$ans/YS.table$calls,2),"%")
#colnames(YS.table) <- c("Hour starting","Calls","Answered","Missed","Percent answered")




          #              #              #              #
          #              #              #              #
          #              #              #              #
          #              #              #              #
          #              #              #              #
      #########      #########      #########      #########        
       ## D ##        ## O ##        ## N ##        ## E ##   
        ## ##          ## ##          ## ##          ## ##
         ###            ###            ###            ###
          #              #              #              #
  
##################################################################
##################################################################
###------------------------------------------------------------###
###        REPORTS COMPLETED --- PLEASE VIEW BY GOING TO       ###
###  R:\Youthline Research\Youthline call data\Monthly reports ###
###------------------------------------------------------------###
##################################################################
##################################################################