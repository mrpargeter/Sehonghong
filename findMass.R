library(caret)
library(foreach)
library(data.table)
library(stringr)

#Import Data, create ID column and create map to get map tool number to ID, clean up data
#based on walking through database with Justin to determine missing, incorrect or unlabeled
#categorial data

cleanData<- function(){
DT = read.csv("Pilot data03.23.14.csv",header=T, sep=",",na.string='NA', stringsAsFactors=F)
DT[,1]<-c(1:length(DT[,2])) #Creates an id in the first column that is a unique number

#Changes to Rock type
DT$Rock[DT$Rock=='']<-"quartzite" #Fill in missing rock types

#Changes to Flake Portion for 'core' or 'core on flake'
changeToCore = c(28,79,263,288,321,328,637,697,999,1033,1175,1178,1382,1399,1403,1439,
           1440,1444,1536,1538,1947,2465,2537,2543)
DT$Flake.portion[DT$X. %in% changeToCore]<-"core"
DT$Flake.portion[DT$X. %in% c(271,868,997,1053,1066,1114,1334)]<-"core on flake"

#Changes to Platform Preparation on Dorsal
DT$P.prep.on.dorsal.[DT$X. == 585]<- "None (A1);"
DT$P.prep.on.dorsal.[DT$X. == 657]<- "small removals following scar ridges (A3)"
DT$P.prep.on.dorsal.[DT$X. %in% c(241,694)]<- "Other"

#Changes to Cone
DT$Cone[DT$X. %in% c(30,37,86,106,107,803,2246)]<- "none"

#Changes Cort of 1 to 100%
DT$Cort[DT$Cort == "1"]<-"100%"
DT$Cort[DT$Cort == "1-9%"]<-"1-10%"

#Changes Dorsal Scar Pattern
DT$Dorsal.scar.pattern[DT$X. == 2102]<-"NA"

#Changes Frac.initiation.point
DT$Frac.initiation.point[DT$X. == 1397]<-"NA"

#Changes to Type
DT$Type<-str_trim(DT$Type)
DT$Type[DT$Type==""]<-"Other"

#Changes to Termination
DT$Type[DT$Termination==""]<-"NA"

colnames(DT)[1]<-"ID"
Data.Table<-DT[!DT$X. %in% c(86, 1439, 2299,2303,2308,2310),]

Data.Map <-Data.Table[,c(1:2)] #Creates a map of Original ID to new ID
Data.Table[2]<-NULL
Data.Table$Flake.portion = factor(Data.Table$Flake.portion)
colnames(Data.Table)[7:11]<-c("L.max","L.tech","W.mid","T.mid","T.bulb")
colnames(Data.Table)[20:23]<-c("P.morph","P.prep","P.prep.dorsal","P.abr")
Data.Table
}

CalculateMass<-function(Data.Table){
Data.Table_Sub<-Data.Table[Data.Table$Flake.portion == "complete" | 
                          Data.Table$Flake.portion == "proximal but nearly complete" |
                          Data.Table$ID == 1363,]
Data.Table_Sub[Data.Table_Sub ==""]<-NA
Data.Table_Sub<-Data.Table_Sub[!is.na(Data.Table_Sub$L.max.),]
for (i in c(7:11,17)){Data.Table_Sub[,i]<-as.numeric(Data.Table_Sub[,i])}


Data.Table_Sub$MaxL = apply(Data.Table_Sub[7:8], 1, max, na.rm = TRUE)
Data.Table_Sub$MaxT = apply(Data.Table_Sub[10:11], 1, max, na.rm = TRUE)
Data.Table_Sub$VolumeMax = (Data.Table_Sub$W.mid*Data.Table_Sub$MaxL*Data.Table_Sub$MaxT)
  
#Split Data into training and testing for mass calculations
Data.training = Data.Table_Sub[!is.na(Data.Table_Sub$MASS),]
Training.ID<-Data.training[,1]
Data.test = Data.Table_Sub[!Data.Table_Sub$ID %in% Training.ID,]
training<-Data.training[,c(35,19,18,17)]

training$Rock<-factor(training$Rock)
SplitRockType<-split(training,training$Rock)
map<-data.frame("Rock"=character(),"Edge"=character(),"Density" = numeric())
foreach (data = SplitRockType) %do% {
  rock = as.character(data$Rock[1])
  data$Edge.profile<-factor(data$Edge.profile)
  splitData<-split(data,data$Edge.profile)
  foreach (s.data = splitData) %do% {
    edge<-as.character(s.data$Edge.profile[1])
    s.data$density = s.data$MASS/s.data$VolumeMax
   map.d<-data.table(Rock = rock, Edge = edge, Density = mean(s.data$density))
    map<-rbind(map,map.d)
  }
}
colnames(Data.test)[18]<-c("Edge")
colnames(Data.training)[18]<-c("Edge")
mappedData.test<-merge(Data.test,map,by=c("Rock","Edge"))
mappedData.test$set<-'test'
mappedData.train<-merge(Data.training,map,by=c("Rock","Edge"))
mappedData.train$set<-'train'
mappedData.test$MASS<-mappedData.test$VolumeMax*mappedData.test$Density

AllData_Mass<-rbind(mappedData.test,mappedData.train)
PlotMass<-(AllData_Mass[,c(19,37)])
AllData_Mass
}

#Check of mass distribution
PlotMass<- function(Mass){
ggplot(Mass,aes(x = MASS)) + 
  geom_histogram(data=subset(Mass,set == 'train'),fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(Mass,set == 'test'),fill = "blue", alpha = 0.2)
}
