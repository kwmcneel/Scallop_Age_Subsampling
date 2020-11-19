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
while(!require(openxlsx)){install.packages("openxlsx")}

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
Thrownout<-rbind(Thrownout, ScallopData[ScallopData$length == 0,]) #pull rows where length =0
Thrownout<-rbind(Thrownout, ScallopData[ScallopData$weight == 0,]) #pull rows where weight =0

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
ScallopData$Bin<- as.character(cut(ScallopData$length, Bins$Bin.Start)) #assign scallops to bin
scallop.invoice<- as.data.frame(matrix(ncol = length(names(ScallopData)), nrow=0)) #make dummy dataframe
colnames(scallop.invoice)=names(ScallopData) #rename variables
scallop.invoice$sample_date<-as.Date(scallop.invoice$sample_date)
scallop.invoice$date_sampled<-as.Date(scallop.invoice$date_sampled)

#sample 6 scallops from each location:bin
for (i in unique(ScallopData$BinLocation)){
  subset<-ScallopData[ScallopData$BinLocation == i,]
  if (subset>6){
    subset<-sample_n(subset,5, replace = TRUE)
    } 
  scallop.invoice[nrow(scallop.invoice)+1:nrow(subset),]<-subset
}

scallop.invoice<-unique(scallop.invoice) #remove redundancies

#Subsample for profile measurement

scallop.measured<- as.data.frame(matrix(ncol = length(names(scallop.invoice)), nrow=0)) #make dummy dataframe
colnames(scallop.measured)=names(scallop.invoice) #rename variables
scallop.measured$sample_date<-as.Date(scallop.measured$sample_date)
scallop.measured$date_sampled<-as.Date(scallop.measured$date_sampled)

#sample 2 scallops from each location:bin
#sample 30 scallops from each location:bin
for (i in unique(scallop.invoice$location_code)){
  subset<-scallop.invoice[scallop.invoice$location_code == i,]
  if (subset>30){
    subset<-sample_n(subset,30, replace = TRUE)
  } 
  scallop.measured[nrow(scallop.measured)+1:nrow(subset),]<-subset
}

scallop.measured<-unique(scallop.measured) #remove redundancies


#Track shell excluded
ScallopData<-dplyr::bind_rows(ScallopData,Thrownout)
ScallopData<-unique(ScallopData) #remove redundancies
scallop.exclude<-anti_join(ScallopData,scallop.invoice, by=c("submitter_sample_id","submitter_specimen_id"))

#Plot invoiced shell histogram
ggplot()+
  geom_histogram(ScallopData,mapping= aes(x=length, fill="Collection"), binwidth = 2)+
  geom_histogram(scallop.invoice, mapping= aes(x=length, fill="Subsample"), binwidth = 2)+
  geom_histogram(scallop.measured, mapping= aes(x=length, fill="to measure"), binwidth = 2)+
  facet_grid(location_code~.,scales="free_y")+
  labs(x="Shell Height", y="Count",fill="")+
  theme_bw()+
  scale_fill_manual(values=c("grey","black","red"))

#Convert ADU sample ID from Observer samples
if(unique(scallop.invoice$fishery_code)== "CO"){
  scallop.invoice$ADUID<-paste(substr(scallop.invoice$sample_year,start = 3, stop = 4),
                                scallop.invoice$management_area_code,
                                substr(scallop.invoice$effort_no,start=6,stop=9),"~",
                                substr(formatC(scallop.invoice$trip_no, width=2, flag="0"),start=2,stop=4),sep = "")
} else {
  scallop.invoice$ADUID<-scallop.invoice$submitter_sample_id   
}

if(unique(scallop.exclude$fishery_code)== "CO"){
  scallop.exclude$ADUID<-paste(substr(scallop.exclude$sample_year,start = 3, stop = 4),
                               scallop.exclude$management_area_code,
                               substr(scallop.exclude$effort_no,start=6,stop=9),"~",
                               substr(formatC(scallop.exclude$trip_no, width=2, flag="0"),start=2,stop=4),sep = "")
} else {
  scallop.exclude$ADUID<-scallop.exclude$submitter_sample_id   
}



scallop.field<-cbind(scallop.invoice$ADUID,
                     as.numeric(substr(scallop.invoice$submitter_specimen_id,start=24,stop=26)),
                     scallop.invoice$sample_date,scallop.invoice[,c(14:21)])
colnames(scallop.field)=c("ADU SAMPLE ID",	"ADU SPECIMEN NUMBER",	"SAMPLE DATE",	
                          "FIELD SPECIES CODE",	"FISH LENGTH mm",	"FISH LENGTH TYPE",
                          "FISH WEIGHT g",	"FISH WEIGHT TYPE",	"GENDER",	"REGIONAL_MATURITY",
                          "FIELD SPECIMEN COMMENT")
scallop.field$`FISH WEIGHT TYPE`<-ifelse(scallop.field$`FISH WEIGHT g`>0,"WH","")
                              
scallop.invoicing<-cbind(scallop.invoice[,c(1:12,12,14)],
                         as.data.frame(rep("Valve",length(scallop.invoice$sample_year),colnames="AGE STRUCTURE")),
                         scallop.invoice$ADUID,
                         as.numeric(substr(scallop.invoice$submitter_specimen_id,start=24,stop=26)),
                         as.numeric(substr(scallop.invoice$submitter_specimen_id,start=24,stop=26)))
if(unique(scallop.invoice$fishery_code)== "SU"){
    scallop.invoicing[,c(17,18)]<-cbind(scallop.invoice$submitter_specimen_id,scallop.invoice$submitter_specimen_id)
}


scallop.invoicing.exclude<-cbind(scallop.exclude[,c(1:12,12,14)],
                         as.data.frame(rep("Valve",length(scallop.exclude$sample_year),colnames="AGE STRUCTURE")),
                         scallop.exclude$ADUID,
                         as.numeric(substr(scallop.exclude$submitter_specimen_id,start=24,stop=26)),
                         as.numeric(substr(scallop.exclude$submitter_specimen_id,start=24,stop=26)))
if(unique(scallop.invoicing.exclude$fishery_code)== "SU"){
  scallop.invoicing.exclude[,c(17,18)]<-cbind(scallop.exclude$submitter_specimen_id,scallop.exclude$submitter_specimen_id)
}

write.csv(scallop.invoice, paste("Scallop.invoive",paste(unique(ScallopData$sample_year),collapse = "."),
                           paste(unique(ScallopData$fishery_code),collapse = "."), ".csv",
                           sep="_")) #exports thrownout data into csv

scallop.field.exclude<-cbind(scallop.exclude$ADUID,
                     as.numeric(substr(scallop.exclude$submitter_specimen_id,start=24,stop=26)),
                     scallop.exclude$sample_date,scallop.exclude[,c(14:21)])

colnames(scallop.field.exclude)=c("ADU SAMPLE ID",	"ADU SPECIMEN NUMBER",	"SAMPLE DATE",	
                          "FIELD SPECIES CODE",	"FISH LENGTH mm",	"FISH LENGTH TYPE",
                          "FISH WEIGHT g",	"FISH WEIGHT TYPE",	"GENDER",	"REGIONAL_MATURITY",
                          "FIELD SPECIMEN COMMENT")

scallop.field.exclude$`FISH WEIGHT TYPE`<-ifelse(scallop.field.exclude$`FISH WEIGHT g`>0,"WH","")

#create subdirectory for .xlsx
{dir.create(file.path(getwd(), "Invoices"))
dir.create(file.path(getwd(), "Field_Data"))
dir.create(file.path(getwd(), "Excluded_Invoices"))
dir.create(file.path(getwd(), "Excluded_Field_Data"))}

#Export Sample Invoices####
wb <- loadWorkbook("Scallop_Invoice_template.xlsx")
writeData(wb,x=scallop.invoicing,sheet="Sheet1", startRow = 8, startCol = 1, colNames = FALSE, rowNames = FALSE)
writeData(wb,x=format(Sys.time(), "%m/%d/%Y"),sheet="Sheet1", startRow = 4, startCol = 14, colNames = FALSE, rowNames = FALSE)
saveWorkbook(wb,paste("Invoices/Scallop.invoive",paste(unique(ScallopData$sample_year),collapse = "."),
                      paste(unique(ScallopData$fishery_code),collapse = "."), ".xlsx",
                      sep="_"),overwrite = T)
#Export Sample Invoice of excluded specimens
wb <- loadWorkbook("Scallop_Invoice_template.xlsx")
writeData(wb,x=scallop.invoicing.exclude,sheet="Sheet1", startRow = 8, startCol = 1, colNames = FALSE, rowNames = FALSE)
writeData(wb,x=format(Sys.time(), "%m/%d/%Y"),sheet="Sheet1", startRow = 4, startCol = 14, colNames = FALSE, rowNames = FALSE)
saveWorkbook(wb,paste("Excluded_Invoices/Scallop.invoive_excluded",paste(unique(ScallopData$sample_year),collapse = "."),
                      paste(unique(ScallopData$fishery_code),collapse = "."), ".xlsx",
                      sep="_"),overwrite = T)
#Export Field Data####
wb <- loadWorkbook("Scallop_Field_Data_template.xlsx")
writeData(wb,x=scallop.field,sheet="FIELD DATA IMPORT", startRow = 2, startCol = 1, colNames = FALSE, rowNames = FALSE)
saveWorkbook(wb,paste("Field_Data/Scallop.Field_Data",paste(unique(ScallopData$sample_year),collapse = "."),
                      paste(unique(ScallopData$fishery_code),collapse = "."), ".xlsx",
                      sep="_"),overwrite = T)
#Export Field Data of excluded specimens
wb <- loadWorkbook("Scallop_Field_Data_template.xlsx")
writeData(wb,x=scallop.field.exclude,sheet="FIELD DATA IMPORT", startRow = 2, startCol = 1, colNames = FALSE, rowNames = FALSE)
saveWorkbook(wb,paste("Excluded_Field_Data/Scallop.Field_Data.Excluded",paste(unique(ScallopData$sample_year),collapse = "."),
                      paste(unique(ScallopData$fishery_code),collapse = "."), ".xlsx",
                      sep="_"),overwrite = T)

