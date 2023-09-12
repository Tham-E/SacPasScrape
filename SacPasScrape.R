library(rvest)
library(httr)
library(stringr)
library(dplyr)
library(janitor)
library(readxl)
library(writexl)

# Establish link to SacPas Website ----------------------------
link<-"https://www.cbr.washington.edu/sacramento/workgroups/delta_smelt.html"

# Work around from directly using rvest::read_html (would produce error 407) ----------------------------
download.file(link,destfile = "SacPas.html")
SacPas<-rvest::read_html("SacPas.html")
unlink("SacPas.html")


# Isolate and import Table in Sacpas link ----------------------------
table_css<-SacPas%>%html_nodes("table")
link_css<-SacPas%>%html_nodes("a")%>%.[8]%>%html_attr('href')

csv_link<-paste0("https://www.cbr.washington.edu",link_css)
download.file(csv_link,"ds_daily.csv")

# Read the Daily Data table  ----------------------------
DailyDataTable<-read.csv("ds_daily.csv")%>%mutate(
  Date= as.POSIXct(Date),
  Water.Temperature.SJR.at.Antioch..C.=as.double(Water.Temperature.SJR.at.Antioch..C.),
  Water.Temperature.SR.at.Rio.Vista.Br..C.=as.double(Water.Temperature.SR.at.Rio.Vista.Br..C.),
  Water.Temperature.SJR.at.Mossdale.Br..C.=as.double(Water.Temperature.SJR.at.Mossdale.Br..C.),
  Water.Temperature.3.Station.Avg..C.=as.double(Water.Temperature.3.Station.Avg..C.),
  River.Discharge.Flow.3.day.Freeport..CFS.=as.double(River.Discharge.Flow.3.day.Freeport..CFS.),
  Turbidity.3.day.Freeport..FNU.=as.double(Turbidity.3.day.Freeport..FNU.),
  Water.Temperature.3.day.Jersey.Point..C.=as.double(Water.Temperature.3.day.Jersey.Point..C.),
  Water.Turbidity.1.day.Prisoner.s.Point..NTU.=as.double(Water.Turbidity.1.day.Prisoner.s.Point..NTU.),
  Water.Turbidity.1.day.SJR.Holland.Cut..FNU.=as.double(Water.Turbidity.1.day.SJR.Holland.Cut..FNU.),
  Water.Turbidity.1.day.Victoria.Canal..NTU.=as.double(Water.Turbidity.1.day.Victoria.Canal..NTU.),
  Water.Turbidity.1.day.Old.River.at.Bacon.Island..USGS...FNU.=as.double(Water.Turbidity.1.day.Old.River.at.Bacon.Island..USGS...FNU.),
  Water.Turbidity.1.day.Old.River.at.Franks.Tract..NTU.=as.double(Water.Turbidity.1.day.Old.River.at.Franks.Tract..NTU.),
  Water.Turbidity.1.day.Bethel.Island..NTU.=as.double(Water.Turbidity.1.day.Bethel.Island..NTU.),
  QWEST..CFS.=as.double(QWEST..CFS.),
  NDOI..CFS.=as.double(NDOI..CFS.),
  E.I.Ratio.3.day....=as.double(E.I.Ratio.3.day....),
  X2.Position..KM.=as.double(X2.Position..KM.),
  Water.Temperature.Clifton.Court..C.=as.double(Water.Temperature.Clifton.Court..C.),
  OMR.Index..CFS.=as.double(OMR.Index..CFS.),
  OMR.Index.5.day..CFS.=as.double(OMR.Index.5.day..CFS.),
  OMR.Index.14.day..CFS.=as.double(OMR.Index.14.day..CFS.),
  USGS.Tidally.Filtered.OMR..CFS.=as.double(USGS.Tidally.Filtered.OMR..CFS.),
  USGS.Tidally.Filtered.OMR.5.day.mean..CFS.=as.double(USGS.Tidally.Filtered.OMR.5.day.mean..CFS.),
  USGS.Tidally.Filtered.OMR.14.day.mean..CFS.=as.double(USGS.Tidally.Filtered.OMR.14.day.mean..CFS.),
  Sum.Pumping.Discharge.HRO.TRP..CFS.=as.double(Sum.Pumping.Discharge.HRO.TRP..CFS.),
  Pumping.Discharge.Harvey.O.Banks.Pumping.Plant..CFS.=as.double(Pumping.Discharge.Harvey.O.Banks.Pumping.Plant..CFS.),
  Pumping.Discharge.SJR.Tracy.Pumping.Plant..CFS.=as.double(Pumping.Discharge.SJR.Tracy.Pumping.Plant..CFS.),
  River.Discharge.Flow.SR.at.Freeport..CFS.=as.double(River.Discharge.Flow.SR.at.Freeport..CFS.),
  River.Discharge.Flow.SJR.nr.Vernalis..CFS.=as.double(River.Discharge.Flow.SJR.nr.Vernalis..CFS.),
  Water.Temperature.C.Barker.Slough..C.=as.double(Water.Temperature.C.Barker.Slough..C.),
  Turbidity.Barker.Slough..NTU.=as.double(Turbidity.Barker.Slough..NTU.),
  Pumping.Discharge.Barker.Slough..NTU.=as.double(Pumping.Discharge.Barker.Slough..NTU.)
)
# Adjust naming scheme for Sac Pas Scrape table  ----------------------------
colnames(DailyDataTable)[1]<-"Date"
colnames(DailyDataTable)[2]<-"Water Temperature SJR at Antioch (C)"
colnames(DailyDataTable)[3]<-"Water Temperature SR at Rio Vista Br (C)"
colnames(DailyDataTable)[4]<-"Water Temperature SJR at Mossdale Br (C)"
colnames(DailyDataTable)[5]<-"Water Temperature 3 Station Avg (C)"
colnames(DailyDataTable)[6]<-"River Discharge Flow 3-day Freeport (CFS)"
colnames(DailyDataTable)[7]<-"Turbidity 3-day Freeport (FNU)"
colnames(DailyDataTable)[8]<-"Water Temperature 3-day Jersey Point (C)"
colnames(DailyDataTable)[9]<-"Water Turbidity 1-day Prisoner's Point (NTU)"
colnames(DailyDataTable)[10]<-"Water Turbidity 1-day SJR Holland Cut (FNU)"
colnames(DailyDataTable)[11]<-"Water Turbidity 1-day Victoria Canal (NTU)"
colnames(DailyDataTable)[12]<-"Water Turbidity 1-day Old River at Bacon Island (USGS) (FNU)"
colnames(DailyDataTable)[13]<-"Water Turbidity 1-day Old River at Franks Tract (NTU)"
colnames(DailyDataTable)[14]<-"Water Turbidity 1-day Bethel Island (NTU)"
colnames(DailyDataTable)[15]<-"QWEST (CFS)"
colnames(DailyDataTable)[16]<-"NDOI (CFS)"
colnames(DailyDataTable)[17]<-"E/I Ratio 3 day (%)"
colnames(DailyDataTable)[18]<-"X2 Position (KM)"
colnames(DailyDataTable)[19]<-"Water Temperature Clifton Court (C)"
colnames(DailyDataTable)[20]<-"OMR Index (CFS)"
colnames(DailyDataTable)[21]<-"OMR Index 5-day (CFS)"
colnames(DailyDataTable)[22]<-"OMR Index 14-day (CFS)"
colnames(DailyDataTable)[23]<-"USGS Tidally Filtered OMR (CFS)"
colnames(DailyDataTable)[24]<-"USGS Tidally Filtered OMR 5-day mean (CFS)"
colnames(DailyDataTable)[25]<-"USGS Tidally Filtered OMR 14-day mean (CFS)"
colnames(DailyDataTable)[26]<-"Sum Pumping Discharge HRO+TRP (CFS)"
colnames(DailyDataTable)[27]<-"Pumping Discharge Harvey O Banks Pumping Plant (CFS)"
colnames(DailyDataTable)[28]<-"Pumping Discharge SJR Tracy Pumping Plant (CFS)"
colnames(DailyDataTable)[29]<-"River Discharge Flow SR at Freeport (CFS)"
colnames(DailyDataTable)[30]<-"River Discharge Flow SJR nr Vernalis (CFS)"
colnames(DailyDataTable)[31]<-"Water Temperature C Barker Slough (C)"
colnames(DailyDataTable)[32]<-"Turbidity Barker Slough (NTU)"
colnames(DailyDataTable)[33]<-"Pumping Discharge Barker Slough (NTU)"
unlink("ds_daily.csv")

str(DailyDataTable)


# IGNORE: Keeping for record ----------------------------


#SacpasTables<-SacPas%>%html_nodes("table")%>%.[1:12]%>%html_table()

## Reading Tables into Environment
#releases_DS<-SacpasTables%>%.[[4]] # Info about DS experimental releases
#smeltMonitoring_2weeks<-SacpasTables%>%.[[5]] # Info about DS catches in the past two weeks
#smeltMonitoring_WY<-SacpasTables%>%.[[6]] # Info about DS catches for the Water Year (Begins Oct)
#smeltSalvage<-SacpasTables%>%.[[7]] # Total number of Smelt observed in salvage for this water year.
#chippsCatch_WY<-SacpasTables%>%.[[8]]%>%mutate(Date=as.Date.character(Date, tryFormats="%Y-%m-%d")) # Info about Smelt catches @ Chipps Island
#edsmCatch_WY<-SacpasTables%>%.[[9]]%>%mutate(Date=as.Date.character(Date, tryFormats="%Y-%m-%d")) # Info about Smelt catch in EDSM
#salvageCatch_WY<-SacpasTables%>%.[[10]]%>%
#  mutate(samptimestamp=as.POSIXct(samptimestamp, Formats="%Y-%m-%d %H:%M:%S"))

##Reading Daily Values Table
#DailyDataTable<-SacpasTables%>%.[[11]]%>%
#  row_to_names(row_number = 1)%>%.[-1,]%>%
#  mutate(Date=as.Date.character(Date, tryFormats="%Y-%m-%d"))%>%
#  mutate_if(is.character,as.numeric)

# Pulling from Daily Table for days w/out OMRI and OMR tidally filtered ----------------------------
DailyDataTable2<-DailyDataTable%>%filter(is.na(`OMR Index (CFS)`))
# Adding updated data to new Daily Table Record ----------------------------
getwd()
tmp_snapshot<-fileSnapshot("~/SacPasScrape/SacPasRecords")
test1<-rownames(tmp_snapshot$info[-which.max(tmp_snapshot$info$size),])
print(test1)

test2<-rownames(tmp_snapshot$info[-which.max(tmp_snapshot$info$ctime),])
print(test2)

latestData<-read_excel(paste0("~/SacPasScrape/SacPasRecords/",rownames(tmp_snapshot$info[which.max(tmp_snapshot$info$mtime),])))
latestData$`Water Turbidity 1-day Bethel Island (NTU)`<-as.double(latestData$`Water Turbidity 1-day Bethel Island (NTU)`)

tmp<-unique(rbind(DailyDataTable%>%filter(!is.na(`River Discharge Flow 3-day Freeport (CFS)`)),latestData)%>%#adds latest pull with last compiled data
              #filters out NA"s to limit duplicates in NA's later
              filter(!is.na(`OMR Index 14-day (CFS)`) & c(!is.na(`USGS Tidally Filtered OMR 14-day mean (CFS)`)|
                                                            c(is.na(`USGS Tidally Filtered OMR 14-day mean (CFS)`)&!is.na(`QWEST (CFS)`))))%>%
              filter(!is.na(`USGS Tidally Filtered OMR 14-day mean (CFS)`)&Date<=Sys.Date()-3)%>%
              rbind(DailyDataTable2)
)%>%arrange(Date)


# write updated data table ----------------------------
write_xlsx(tmp,path=paste0("~/SacPasScrape/SacPasRecords/SacPasDailyDataTable",Sys.Date(),".xlsx"))
write_xlsx(tmp,path=paste0("~/SacPasScrape/SacPasRecords/SacPasDailyDataTable_LastUpdate",Sys.Date(),".xlsx"))
# remove extra data tables/elements ----------------------------


file_path<-"~/SacPasScrape/SacPasRecords"
f<-list.files(file_path,include.dirs = F,full.names = T,recursive = T)

test<-list.files(file_path,include.dirs = F,full.names = T,recursive = T)
print(f)
rm(DailyDataTable,DailyDataTable2,latestData,SacPas,table_css,tmp_snapshot,csv_link,link,link_css)
# Develop graphs to show SacPasDailyTable Data ----------------------------

library(ggplot2)

ggplot(data=tmp)+
  geom_line(aes(x=Date,y=`OMR Index (CFS)`,color="OMRI"))+
  geom_line(aes(x=Date,y=`USGS Tidally Filtered OMR (CFS)`,color="OMR"))

ggplot(data=tmp)+
  geom_point(aes(x=`OMR Index (CFS)`,y=`River Discharge Flow SJR nr Vernalis (CFS)`))

ggplot(data=tmp)+
  geom_point(aes(x=`USGS Tidally Filtered OMR (CFS)`,y=`River Discharge Flow SJR nr Vernalis (CFS)`))

ggplot(data=tmp)+
  geom_point(aes(x=`USGS Tidally Filtered OMR (CFS)`,y=`OMR Index (CFS)`))

ggplot(data=tmp)+
  geom_point(aes(x=`USGS Tidally Filtered OMR (CFS)`,y=`Sum Pumping Discharge HRO+TRP (CFS)`))

ggplot(data=tmp)+
  geom_point(aes(x=`USGS Tidally Filtered OMR (CFS)`,y=`Sum Pumping Discharge HRO+TRP (CFS)`))

ggplot(data=tmp)+
  geom_point(aes(x=`USGS Tidally Filtered OMR (CFS)`,y=`QWEST (CFS)`))
  

ggplot(data=tmp)+
  geom_line(aes(x=Date,y=`X2 Position (KM)`))
