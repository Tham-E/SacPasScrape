library(cder)

VNS<-cdec_query("VNS",41,"D",as.Date("2001-01-01"),Sys.Date())
MSD<-cdec_query("MSD",41,"D",as.Date("2001-01-01"),Sys.Date())
SJR<-cdec_query("SJR",20,"E",as.Date("2001-01-01"),Sys.Date())%>%
  mutate(Date=format(x = DateTime, format="%Y-%m-%d"))
SJR <-  aggregate(Value ~ Date, data=SJR, FUN=mean)


