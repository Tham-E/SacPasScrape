library(cder)
library(lubridate)
library(dplyr)
dateIndex <- data.frame(date = seq(as.Date("2021-12-31"), as.Date("2022-12-31"), by = "1 day")) %>% 
  transmute(monthDay = format(date, format = "%b-%d")) %>% 
  filter(!monthDay %in% c("Dec-31","Feb-29")) %>% 
  mutate(xIndex = 1:n())

PRI_event_turb<-cdec_query("PRI",221,"E",as.Date("2000-01-01"),Sys.Date())%>%dplyr::select(DateTime,Value)%>%
  group_by(DateTime=date(DateTime))%>%
  summarize(avgTurbPRI=mean(Value,na.rm=T))
PRI_event_temp<-cdec_query("PRI",25,"E",as.Date("2000-01-01"),Sys.Date())%>%dplyr::select(DateTime,Value)%>%
  group_by(DateTime=date(DateTime))%>%
  summarize(avgTempPRI=mean(Value,na.rm=T))
PRI_event_flow<-cdec_query("PRI",20,"E",as.Date("2000-01-01"),Sys.Date())%>%dplyr::select(DateTime,Value)%>%
  group_by(DateTime=date(DateTime))%>%
  summarize(avgFlowPRI=mean(Value,na.rm=T))
PRI_event<-PRI<-PRI_event_flow%>%
  left_join(PRI_event_temp,by="DateTime")%>%
  left_join(PRI_event_turb,by="DateTime")%>%
  mutate(Date=as.character(DateTime),
         Date=as.POSIXct(Date,format="%Y-%m-%d"),
         year=year(DateTime),
         month=month(DateTime),
         wYear=ifelse(month<10,year,year+1),
         day_wYear=julian(DateTime)-julian(as.Date(paste(year,"-01-01",sep=""))),
         monthDay=format(Date,format="%b-%d"))%>%left_join(dateIndex, by = "monthDay")%>%filter(year>=2008)

rm(PRI_event_flow,PRI_event_temp,PRI_event_turb,PRI_event)
# This is an indication that PRI is only being used for flow. 
# Historically, it had included data for FNU turbidity, but the data set had ended 2020-05-28 (right before memorial day weekend)
# am curious about why Turb was discontinued after then?

PPT<-cdec_query("PPT",27,"E",as.Date("2000-01-01"),Sys.Date())%>%dplyr::select(DateTime,Value)%>%
  group_by(DateTime=date(DateTime))%>%
  summarize(avgTurbPPT=mean(Value,na.rm=T))%>%
  mutate(Date=as.character(DateTime),
         Date=as.POSIXct(Date,format="%Y-%m-%d"),
         year=year(DateTime),
         month=month(DateTime),
         wYear=ifelse(month<10,year,year+1),
         day_wYear=julian(DateTime)-julian(as.Date(paste(year,"-01-01",sep=""))),
         monthDay=format(Date,format="%b-%d")
  )%>%left_join(dateIndex, by = "monthDay")%>%filter(year>=2008)

OSJ<-cdec_query("OSJ",221,"E",as.Date("2000-01-01"),Sys.Date())%>%dplyr::select(DateTime,Value)%>%
  group_by(DateTime=date(DateTime))%>%
  summarize(avgTurbOSJ=mean(Value,na.rm=T))%>%
  mutate(Date=as.character(DateTime),
         Date=as.POSIXct(Date,format="%Y-%m-%d"),
         year=year(DateTime),
         month=month(DateTime),
         wYear=ifelse(month<10,year,year+1),
         day_wYear=julian(DateTime)-julian(as.Date(paste(year,"-01-01",sep=""))),
         monthDay=format(Date,format="%b-%d")
  )%>%left_join(dateIndex, by = "monthDay")%>%filter(year>=2008)


cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                   "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot()+
  geom_line(data=PPT,aes(x=xIndex,y=avgTurbPPT,color="PPT"),size=1.5)+
  geom_line(data=OSJ,aes(x=xIndex,y=avgTurbOSJ,color="OSJ"),size=1.5)+
  geom_line(data=PRI,aes(x=xIndex,y=avgTurbPRI,color="PRI"),size=1.5)+
  scale_x_continuous(breaks = seq(min(1),max(365),by=31), 
                     labels = filter(dateIndex, xIndex %in% seq(min(1),max(365),by=31))$monthDay, 
                     expand = c(0.01,0.01))+
  coord_cartesian(ylim=c(0,20))+
  scale_color_manual(name="Turbidity Station",values=c("PPT"="#E69F00","OSJ"="#0072B2","PRI"="#000000"))+
  facet_wrap(~year)+
  guides(x=guide_axis(angle=90))+
  theme_bw()
  

ggplot()+
  geom_line(data=PPT%>%filter(year(DateTime)==2015),aes(x=DateTime,y=avgTurbPPT,color="RawStation_PPT"))+
  geom_line(data=tmp%>%filter(year(Date)==2015),aes(x=date(Date),y=`Water Turbidity 1-day Prisoner's Point (NTU)`,color="SacPas"))+
  geom_line(data=PRI%>%filter(year(DateTime)==2015),aes(x=DateTime,y=avgTurbPRI,color="RawStation_PRI"))
  

