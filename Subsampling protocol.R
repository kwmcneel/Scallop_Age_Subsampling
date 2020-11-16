#Scallop Age Subsampling####
#Based on Williams, B. 2018. Scallop age sampling. 

#Coggins et al. 2013 suggest a sample size of 500-1,000 and 10 aged scallops per shell height bin. Based upon
#this and the general size structure observed in our current samples I recommend that 10 shells be aged from
#each of the following bins.
#0,25,50,75,77,79,81,83,85,87,89,91,93,95 97 99 101 103 105 107 109 111 113 115 117 119 121 123 125 127 129
#131 133 135 137 139 141 143 145 147 149 151 153 155 157 159 161 163 165 167 169 171 173 175 177 179 181
#183 185 187 189 191 193 195 197 199
#So a bin from 0-25, 26-50, 77-78, etc.
#This will amount to a sample size of 660 if all bins have 10 individuals aged in them. If the target sample
#size ~550 is regularly being missed then the number of samples in each bin can be increased.
#I recommend this sample size for each district for the time being. If we find that there are unique areas that
#need more information we can increase the sample size from our archived, but unaged, collection.

#Load data####
while(!require(readr)){install.packages("readr")}
while(!require(ggplot2)){install.packages("ggplot2")}
while(!require(dplyr)){install.packages("dplyr")}

#Make Bins
{rm(list = ls())
  Bins<-as.data.frame(c(0,25,50,75,77,79,81,83,85,87,89,91,93,95,97,99,101,103,105,107,109,111,113,115,117,119,121,123,125,
131,133,135,137,139,141,143,145,147,149,151,153,155,157,159,161,163,165,167,169,171,173,175,177,179,181,
183,185,187,189,191,193,195,197,199))
names(Bins)<-"Bin.Start"}

#load and format data
ScallopData<- read_csv(file=file.choose(), 
  col_types = cols(date_sampled = col_date(format = "%m-%d-%Y"), 
  effort_no = col_character(), field_species_code = col_character(), 
  gear_code = col_character(), management_area_code = col_character(), 
  maturity_code = col_character(), 
  sample_date = col_date(format = "%m-%d-%Y"), 
  specimen_comment = col_character(), 
  submitter_sample_id = col_character()))

#Assign Bin and clean data####
###This script removes shell that have no length, length of 0, weight of 0 for survey scallops, and
###large outliers in a weight:length model for survey scallops. All removed rows are put into 
###dataframe "Thrownout" and are exported to thrownout.csv


#Clean Data####
#Clean data of NAs and 0s for length

{Thrownout<-ScallopData[!complete.cases(ScallopData[,c(8,15)]),] #pull out rows where length and location =NA
Thrownout<-rbind(Thrownout, ScallopData[ScallopData$length == 0,]) #remove rows where length =0

ScallopData<- ScallopData[complete.cases(ScallopData[,c(8,15)]),] #remove rows where length =NA
ScallopData<-ScallopData[ScallopData$length!=0,] #remove rows where length =0
}

#Run a Weight:Length model and remove outliers for survey data
if (unique(ScallopData$fishery_code)=="SU"){ScallopData<-ScallopData[ScallopData$weight!=0,] #remove rows where weight=0 for survey scallops
  LWmod<-lm(log(weight)~log(length),data=ScallopData)
  res<-LWmod$residuals
  upper<-mean(res)+(10*sd(res)) 
  ScallopData$res<-LWmod$residuals
  filtered<-ScallopData[abs(ScallopData$res)>upper,]
  ScallopData<-ScallopData[abs(ScallopData$res)<=upper,]
  Thrownout<-rbind(Thrownout,filtered[,-22]) #add data from outlier model to thrownout
  #plot results from outlier model. removed values should be in red and can be pulled up in dataframe "filtered"
  plot(ScallopData$weight~ScallopData$length,
       xlim=c(min(ScallopData$length,Thrownout$length,na.rm=TRUE),max(ScallopData$length,Thrownout$length,na.rm=TRUE)),
       ylim=c(min(ScallopData$weight,Thrownout$weight,na.rm=TRUE),max(ScallopData$weight,Thrownout$weight,na.rm=TRUE)),
       xlab="Shell Height (mm)",
       ylab="Whole Weight (g)")
    points(Thrownout$weight~Thrownout$length, col="red")
    legend("topleft", inset=.05, legend=c("Scallop Data","Removed Data"),col=c("black","red"),pch=c(1,1),box.lty = 0)
}

Thrownout<- Thrownout[complete.cases(Thrownout[,6]),] #removes blank rows
write.csv(Thrownout, paste("Thrownout",paste(unique(ScallopData$sample_year),collapse = "."),
                           paste(unique(ScallopData$fishery_code),collapse = "."), ".csv",
                           sep="_")) #exports thrownout data into csv

#Assign Bins and subsample data####

ScallopData$Bin<- cut(ScallopData$length, Bins$Bin.Start) #assign scallops to bin
ScallopData$BinLocation<-mapply(paste,ScallopData$location_code,ScallopData$Bin) #set unique bins by location
scallop.invoice<- as.data.frame(matrix(ncol = length(names(ScallopData)), nrow=0)) #make dummy dataframe
colnames(scallop.invoice)=names(ScallopData) #rename variables
#sample 6 scallops from each location:bin
for (i in unique(ScallopData$BinLocation)){
  
  subset<-ScallopData[ScallopData$BinLocation == i,]
  subset<-sample_n(subset,6, replace = TRUE)
  scallop.invoice[nrow(scallop.invoice)+1:nrow(subset),]<-subset
}

scallop.invoice<-unique(scallop.invoice) #remove redundancies

ggplot()+
  geom_histogram(ScallopData,mapping= aes(x=length),fill="lightblue",binwidth = 2)+
  geom_histogram(scallop.invoice, mapping= aes(x=length),fill="grey5",binwidth = 2)+
  facet_grid(location_code~.,scales="free_y")+
  theme_bw()+
  labs(fill="Sample")

write.csv(scallop.invoice, paste("Scallop.invoive",paste(unique(ScallopData$sample_year),collapse = "."),
                           paste(unique(ScallopData$fishery_code),collapse = "."), ".csv",
                           sep="_")) #exports thrownout data into csv


