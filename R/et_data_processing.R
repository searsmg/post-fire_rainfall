#setwd("N:/Research/Kampf/Private/field_data/east_troublesome")
setwd('/Volumes/Kampf/Private/field_data/east_troublesome')

library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)
library(dataRetrieval)

#####################################################################
#precipitation
#####################################################################

willow_upper = readNWISuv(siteNumber = 401642106051601, parameterCd = "00045")

willow_upper = willow_upper[,3:4]
colnames(willow_upper)=c('datetime','P_in')
willow_upper$datenumeric=as.numeric(willow_upper$datetime)
willow_upper$P_mm=willow_upper$P_in*25.4

willow_upper_short=filter(willow_upper, P_mm>0)

#calculate time between tips
for (i in 2:nrow(willow_upper_short)) {
  willow_upper_short[i,'dt']=willow_upper_short[i,'datenumeric']-willow_upper_short[i-1,'datenumeric']
}
willow_upper_short$dt_hr=as.numeric(willow_upper_short$dt)/60/60

#start new event if time between tips >=6
willow_upper_short$event=1
for (i in 2:nrow(willow_upper_short)) {
  willow_upper_short[i,'event']=ifelse(willow_upper_short[i,'dt_hr']<6,willow_upper_short[i-1,'event'],willow_upper_short[i-1,'event']+1)
}

#summarize by event
willow_upper_short_rain_event=group_by(willow_upper_short,event) %>%
  summarize(P=sum(P_mm),start=min(datenumeric),end=max(datenumeric),duration=end-start)
willow_upper_short_rain_event$duration_hr=willow_upper_short_rain_event$duration/60/60

#resample each event to 5, 15, 30, 60 min intervals
for (i in 1:nrow(willow_upper_short_rain_event)) {
  start=as.numeric(willow_upper_short_rain_event[i,'start'])
  rowstart=as.numeric(which(willow_upper$datenumeric==start))
  end=as.numeric(willow_upper_short_rain_event[i,'end'])
  rowend=as.numeric(which(willow_upper$datenumeric==end))
  
  event=willow_upper[rowstart:rowend,]
  willow_upper_short_rain_event[i,'MI5']=max(event[,'P_mm'],na.rm=T)*12
  
  if (nrow(event)>3) {
    for (j in 3:nrow(event)) {
      event[j,'P15']=event[j,'P_mm']+event[j-1,'P_mm']+event[j-2,'P_mm']
    }
    willow_upper_short_rain_event[i,'MI15']=max(event[,'P15'],na.rm=T)*4
  } else {
    willow_upper_short_rain_event[i,'MI15']=sum(event[,'P_mm'],na.rm=T)*4
  }
  
  if (nrow(event)>6) {
    for (j in 6:nrow(event)) {
      event[j,'P30']=event[j,'P_mm']+event[j-1,'P_mm']+event[j-2,'P_mm']+
        event[j-3,'P_mm']+event[j-4,'P_mm']+event[j-5,'P_mm']
      }
    willow_upper_short_rain_event[i,'MI30']=max(event[,'P30'],na.rm=T)*2
  } else  {
    willow_upper_short_rain_event[i,'MI30']=sum(event[,'P_mm'],na.rm=T)*2
  }
  
  if (nrow(event)>12) {
    for (j in 12:nrow(event)) {
      event[j,'P60']=event[j,'P_mm']+event[j-1,'P_mm']+event[j-2,'P_mm']+
        event[j-3,'P_mm']+event[j-4,'P_mm']+event[j-5,'P_mm']+event[j-6,'P_mm']+
        event[j-7,'P_mm']+event[j-8,'P_mm']+event[j-9,'P_mm']+event[j-10,'P_mm']+
        event[j-11,'P_mm']
    }
    willow_upper_short_rain_event[i,'MI60']=max(event[,'P60'],na.rm=T)
  } else {
    willow_upper_short_rain_event[i,'MI60']=sum(event[,'P_mm'],na.rm=T)
  }
}
willow_upper_short_rain_event$starttime=as_datetime(willow_upper_short_rain_event$start)
willow_upper_short_rain_event$endtime=as_datetime(willow_upper_short_rain_event$end)

write_csv(willow_upper_short_rain_event,'willow_upper_rain_event.csv')

willow_upper$date=date(willow_upper$datetime)
willow_upper_daily=group_by(willow_upper,date) %>%
  summarize(P_mm=sum(P_mm))
willow_upper_daily$site='willow upper'

#############################
drowsy = readNWISuv(siteNumber = 400912106031201, parameterCd = "00045")
drowsy = drowsy[,3:4]
colnames(drowsy)=c('datetime','P_in')
drowsy$P_mm=drowsy$P_in*25.4
drowsy$datenumeric=as.numeric(drowsy$datetime)
drowsy_short=filter(drowsy, P_mm>0)

#calculate time between tips
for (i in 2:nrow(drowsy_short)) {
  drowsy_short[i,'dt']=drowsy_short[i,'datenumeric']-drowsy_short[i-1,'datenumeric']
}
drowsy_short$dt_hr=as.numeric(drowsy_short$dt)/60/60

#start new event if time between tips >=6
drowsy_short$event=1
for (i in 2:nrow(drowsy_short)) {
  drowsy_short[i,'event']=ifelse(drowsy_short[i,'dt_hr']<6,drowsy_short[i-1,'event'],drowsy_short[i-1,'event']+1)
}

#summarize by event
drowsy_short_rain_event=group_by(drowsy_short,event) %>%
  summarize(P=sum(P_mm),start=min(datenumeric),end=max(datenumeric),duration=end-start)
drowsy_short_rain_event$duration_hr=drowsy_short_rain_event$duration*60*60

#resample each event to 5, 15, 30, 60 min intervals
for (i in 1:nrow(drowsy_short_rain_event)) {
  start=as.numeric(drowsy_short_rain_event[i,'start'])
  rowstart=as.numeric(which(drowsy$datenumeric==start))
  end=as.numeric(drowsy_short_rain_event[i,'end'])
  rowend=as.numeric(which(drowsy$datenumeric==end))
  
  event=drowsy[rowstart:rowend,]
  drowsy_short_rain_event[i,'MI5']=max(event[,'P_mm'],na.rm=T)*12
  
  if (nrow(event)>3) {
    for (j in 3:nrow(event)) {
      event[j,'P15']=event[j,'P_mm']+event[j-1,'P_mm']+event[j-2,'P_mm']
    }
    drowsy_short_rain_event[i,'MI15']=max(event[,'P15'],na.rm=T)*4
  } else {
    drowsy_short_rain_event[i,'MI15']=sum(event[,'P_mm'],na.rm=T)*4
  }
  
  if (nrow(event)>6) {
    for (j in 6:nrow(event)) {
      event[j,'P30']=event[j,'P_mm']+event[j-1,'P_mm']+event[j-2,'P_mm']+
        event[j-3,'P_mm']+event[j-4,'P_mm']+event[j-5,'P_mm']
    }
    drowsy_short_rain_event[i,'MI30']=max(event[,'P30'],na.rm=T)*2
  } else  {
    drowsy_short_rain_event[i,'MI30']=sum(event[,'P_mm'],na.rm=T)*2
  }
  
  if (nrow(event)>12) {
    for (j in 12:nrow(event)) {
      event[j,'P60']=event[j,'P_mm']+event[j-1,'P_mm']+event[j-2,'P_mm']+
        event[j-3,'P_mm']+event[j-4,'P_mm']+event[j-5,'P_mm']+event[j-6,'P_mm']+
        event[j-7,'P_mm']+event[j-8,'P_mm']+event[j-9,'P_mm']+event[j-10,'P_mm']+
        event[j-11,'P_mm']
    }
    drowsy_short_rain_event[i,'MI60']=max(event[,'P60'],na.rm=T)
  } else {
    drowsy_short_rain_event[i,'MI60']=sum(event[,'P_mm'],na.rm=T)
  }
}
drowsy_short_rain_event$starttime=as_datetime(drowsy_short_rain_event$start)
drowsy_short_rain_event$endtime=as_datetime(drowsy_short_rain_event$end)

write_csv(drowsy_short_rain_event,'drowsy_rain_event.csv')

drowsy$date=date(drowsy$datetime)
drowsy_daily=group_by(drowsy,date) %>%
  summarize(P_mm=sum(P_mm))
drowsy_daily$site='drowsy'


##########################################
p1_rain=read_csv('p1_rain_composite.csv')
p1_rain$datetime = as.POSIXct(p1_rain$datetime,tz="MST", format="%m/%d/%Y %H:%M")
p1_rain$datenumeric=as.numeric(p1_rain$datetime)

#calculate time between tips
for (i in 2:nrow(p1_rain)) {
  p1_rain[i,'dt']=p1_rain[i,'datenumeric']-p1_rain[i-1,'datenumeric']
}
p1_rain$dt_hr=as.numeric(p1_rain$dt)/60/60

#start new event if time between tips >=6
p1_rain$event=1
for (i in 2:nrow(p1_rain)) {
  p1_rain[i,'event']=ifelse(p1_rain[i,'dt_hr']<6,p1_rain[i-1,'event'],p1_rain[i-1,'event']+1)
}
p1_rain$P_in = 0.01
p1_rain$P_mm = 0.254

#summarize by event
p1_rain_event=group_by(p1_rain,event) %>%
  summarize(P=sum(P_mm),start=min(datenumeric),end=max(datenumeric),duration=end-start)
p1_rain_event$duration_hr=p1_rain_event$duration/60/60

#resample each event to 5, 15, 30, 60 min intervals
for (i in 1:nrow(p1_rain_event)) {
  t5_rows=as.numeric(round(p1_rain_event[i,'duration_hr']*60/5))+1
  start=as.numeric(p1_rain_event[i,'start'])
  end=as.numeric(p1_rain_event[i,'end'])
  t5_time=as.data.frame(seq(from=start,to=end,by=300))
  event=filter(p1_rain,event==i)
  for (k in 1:t5_rows) {
    sub=filter(event,datetime>=t5_time[k,1]&datetime<t5_time[k+1,1])
    t5_time[k,'P_mm']=nrow(sub)*0.254
  }
  p1_rain_event[i,'MI5']=max(t5_time[,'P_mm'],na.rm=T)*12
  
  t15_rows=as.numeric(round(p1_rain_event[i,'duration_hr']*60/15))+1
  t15_time=as.data.frame(seq(from=start,to=end,by=900))
  event=filter(p1_rain,event==i)
  if (p1_rain_event[i,'duration_hr']>0.25) {
    for (k in 1:t15_rows) {
      sub=filter(event,datetime>=t15_time[k,1]&datetime<t15_time[k+1,1])
      t15_time[k,'P_mm']=nrow(sub)*0.254
    }
    p1_rain_event[i,'MI15']=max(t15_time[,'P_mm'],na.rm=T)*4
  } else p1_rain_event[i,'MI15']=sum(event[,'P_mm'])*4

  t30_rows=as.numeric(round(p1_rain_event[i,'duration_hr']*2))+1
  t30_time=as.data.frame(seq(from=start,to=end,by=1800))
  event=filter(p1_rain,event==i)
  if (p1_rain_event[i,'duration_hr']>0.5) {
    for (k in 1:t30_rows) {
      sub=filter(event,datetime>=t30_time[k,1]&datetime<t30_time[k+1,1])
      t30_time[k,'P_mm']=nrow(sub)*0.254
    }
    p1_rain_event[i,'MI30']=max(t30_time[,'P_mm'],na.rm=T)*2
  } else { p1_rain_event[i,'MI30']=sum(event[,'P_mm'])*2 }

  t60_rows=as.numeric(round(p1_rain_event[i,'duration_hr']))+1
  t60_time=as.data.frame(seq(from=start,to=end,by=3600))
  event=filter(p1_rain,event==i)
  if (p1_rain_event[i,'duration_hr']>1) {
    for (k in 1:t60_rows) {
      sub=filter(event,datetime>=t60_time[k,1]&datetime<t60_time[k+1,1])
      t60_time[k,'P_mm']=nrow(sub)*0.254
    } 
    p1_rain_event[i,'MI60']=max(t60_time[,'P_mm'],na.rm=T)
  } else {p1_rain_event[i,'MI60']=sum(event[,'P_mm'])}
}
p1_rain_event$starttime=as_datetime(p1_rain_event$start)
p1_rain_event$endtime=as_datetime(p1_rain_event$end)

write_csv(p1_rain_event,'p1_rain_event.csv')

p1_rain$date=date(p1_rain$datetime)
p1_daily=group_by(p1_rain,date) %>%
  summarize(P_mm=sum(P_mm))
p1_daily$site='p1'


######################################
mub_rain=read_csv('mub_rain_composite.csv')
mub_rain$datetime = as_datetime(mub_rain$datetime,tz="MST", format="%m/%d/%Y %H:%M")
mub_rain$datenumeric=as.numeric(mub_rain$datetime)

#calculate time between tips
for (i in 2:nrow(mub_rain)) {
  mub_rain[i,'dt']=mub_rain[i,'datenumeric']-mub_rain[i-1,'datenumeric']
}
mub_rain$dt_hr=as.numeric(mub_rain$dt)/60/60

#start new event if time between tips >=6
mub_rain$event=1
for (i in 2:nrow(mub_rain)) {
  mub_rain[i,'event']=ifelse(mub_rain[i,'dt_hr']<6,mub_rain[i-1,'event'],mub_rain[i-1,'event']+1)
}
mub_rain$P_in = 0.01
mub_rain$P_mm = 0.254

#summarize by event
mub_rain_event=group_by(mub_rain,event) %>%
  summarize(P=sum(P_mm),start=min(datenumeric),end=max(datenumeric),duration=end-start)
mub_rain_event$duration_hr=mub_rain_event$duration/60/60

#resample each event to 5, 15, 30, 60 min intervals
for (i in 1:nrow(mub_rain_event)) {
  t5_rows=as.numeric(round(mub_rain_event[i,'duration_hr']*60/5))+1
  start=as.numeric(mub_rain_event[i,'start'])
  end=as.numeric(mub_rain_event[i,'end'])
  t5_time=as.data.frame(seq(from=start,to=end,by=300))
  event=filter(mub_rain,event==i)
  for (k in 1:t5_rows) {
    sub=filter(event,datetime>=t5_time[k,1]&datetime<t5_time[k+1,1])
    t5_time[k,'P_mm']=nrow(sub)*0.254
  }
  mub_rain_event[i,'MI5']=max(t5_time[,'P_mm'],na.rm=T)*12
  
  t15_rows=as.numeric(round(mub_rain_event[i,'duration_hr']*60/15))+1
  t15_time=as.data.frame(seq(from=start,to=end,by=900))
  event=filter(mub_rain,event==i)
  if (mub_rain_event[i,'duration_hr']>0.25) {
    for (k in 1:t15_rows) {
      sub=filter(event,datetime>=t15_time[k,1]&datetime<t15_time[k+1,1])
      t15_time[k,'P_mm']=nrow(sub)*0.254
    }
    mub_rain_event[i,'MI15']=max(t15_time[,'P_mm'],na.rm=T)*4
  } else mub_rain_event[i,'MI15']=sum(event[,'P_mm'])*4
  
  t30_rows=as.numeric(round(mub_rain_event[i,'duration_hr']*2))+1
  t30_time=as.data.frame(seq(from=start,to=end,by=1800))
  event=filter(mub_rain,event==i)
  if (mub_rain_event[i,'duration_hr']>0.5) {
    for (k in 1:t30_rows) {
      sub=filter(event,datetime>=t30_time[k,1]&datetime<t30_time[k+1,1])
      t30_time[k,'P_mm']=nrow(sub)*0.254
    }
    mub_rain_event[i,'MI30']=max(t30_time[,'P_mm'],na.rm=T)*2
  } else { mub_rain_event[i,'MI30']=sum(event[,'P_mm'])*2 }
  
  t60_rows=as.numeric(round(mub_rain_event[i,'duration_hr']))+1
  t60_time=as.data.frame(seq(from=start,to=end,by=3600))
  event=filter(mub_rain,event==i)
  if (mub_rain_event[i,'duration_hr']>1) {
    for (k in 1:t60_rows) {
      sub=filter(event,datetime>=t60_time[k,1]&datetime<t60_time[k+1,1])
      t60_time[k,'P_mm']=nrow(sub)*0.254
    } 
    mub_rain_event[i,'MI60']=max(t60_time[,'P_mm'],na.rm=T)
  } else {mub_rain_event[i,'MI60']=sum(event[,'P_mm'])}
}
mub_rain_event$starttime=as_datetime(mub_rain_event$start)
mub_rain_event$endtime=as_datetime(mub_rain_event$end)

write_csv(mub_rain_event,'mub_rain_event.csv')

mub_rain$date=date(mub_rain$datetime)
mub_daily=group_by(mub_rain,date) %>%
  summarize(P_mm=sum(P_mm))
mub_daily$site='mub'


#################################
lpm_rain=read_csv('lpm_rain_composite.csv')
lpm_rain$datetime = as_datetime(lpm_rain$datetime,tz="MST", format="%m/%d/%Y %H:%M")
lpm_rain$datenumeric=as.numeric(lpm_rain$datetime)

#calculate time between tips
for (i in 2:nrow(lpm_rain)) {
  lpm_rain[i,'dt']=lpm_rain[i,'datenumeric']-lpm_rain[i-1,'datenumeric']
}
lpm_rain$dt_hr=as.numeric(lpm_rain$dt)/60/60

#start new event if time between tips >=6
lpm_rain$event=1
for (i in 2:nrow(lpm_rain)) {
  lpm_rain[i,'event']=ifelse(lpm_rain[i,'dt_hr']<6,lpm_rain[i-1,'event'],lpm_rain[i-1,'event']+1)
}
lpm_rain$P_in = 0.01
lpm_rain$P_mm = 0.254

#summarize by event
lpm_rain_event=group_by(lpm_rain,event) %>%
  summarize(P=sum(P_mm),start=min(datenumeric),end=max(datenumeric),duration=end-start)
lpm_rain_event$duration_hr=lpm_rain_event$duration/60/60

#resample each event to 5, 15, 30, 60 min intervals
for (i in 1:nrow(lpm_rain_event)) {
  t5_rows=as.numeric(round(lpm_rain_event[i,'duration_hr']*60/5))+1
  start=as.numeric(lpm_rain_event[i,'start'])
  end=as.numeric(lpm_rain_event[i,'end'])
  t5_time=as.data.frame(seq(from=start,to=end,by=300))
  event=filter(lpm_rain,event==i)
  for (k in 1:t5_rows) {
    sub=filter(event,datetime>=t5_time[k,1]&datetime<t5_time[k+1,1])
    t5_time[k,'P_mm']=nrow(sub)*0.254
  }
  lpm_rain_event[i,'MI5']=max(t5_time[,'P_mm'],na.rm=T)*12
  
  t15_rows=as.numeric(round(lpm_rain_event[i,'duration_hr']*60/15))+1
  t15_time=as.data.frame(seq(from=start,to=end,by=900))
  event=filter(lpm_rain,event==i)
  if (lpm_rain_event[i,'duration_hr']>0.25) {
    for (k in 1:t15_rows) {
      sub=filter(event,datetime>=t15_time[k,1]&datetime<t15_time[k+1,1])
      t15_time[k,'P_mm']=nrow(sub)*0.254
    }
    lpm_rain_event[i,'MI15']=max(t15_time[,'P_mm'],na.rm=T)*4
  } else lpm_rain_event[i,'MI15']=sum(event[,'P_mm'])*4
  
  t30_rows=as.numeric(round(lpm_rain_event[i,'duration_hr']*2))+1
  t30_time=as.data.frame(seq(from=start,to=end,by=1800))
  event=filter(lpm_rain,event==i)
  if (lpm_rain_event[i,'duration_hr']>0.5) {
    for (k in 1:t30_rows) {
      sub=filter(event,datetime>=t30_time[k,1]&datetime<t30_time[k+1,1])
      t30_time[k,'P_mm']=nrow(sub)*0.254
    }
    lpm_rain_event[i,'MI30']=max(t30_time[,'P_mm'],na.rm=T)*2
  } else { lpm_rain_event[i,'MI30']=sum(event[,'P_mm'])*2 }
  
  t60_rows=as.numeric(round(lpm_rain_event[i,'duration_hr']))+1
  t60_time=as.data.frame(seq(from=start,to=end,by=3600))
  event=filter(lpm_rain,event==i)
  if (lpm_rain_event[i,'duration_hr']>1) {
    for (k in 1:t60_rows) {
      sub=filter(event,datetime>=t60_time[k,1]&datetime<t60_time[k+1,1])
      t60_time[k,'P_mm']=nrow(sub)*0.254
    } 
    lpm_rain_event[i,'MI60']=max(t60_time[,'P_mm'],na.rm=T)
  } else {lpm_rain_event[i,'MI60']=sum(event[,'P_mm'])}
}

lpm_rain_event$starttime=as_datetime(lpm_rain_event$start)
lpm_rain_event$endtime=as_datetime(lpm_rain_event$end)

write_csv(lpm_rain_event,'lpm_rain_event.csv')

lpm_rain$date=date(lpm_rain$datetime)
lpm_daily=group_by(lpm_rain,date) %>%
  summarize(P_mm=sum(P_mm))
lpm_daily$site='lpm'

################################
precip=rbind(willow_upper_daily,drowsy_daily,p1_daily,mub_daily,lpm_daily)

hyetograph=ggplot()+geom_line(data=precip, aes(x=date,y=P_mm,color=site))
ggplotly(hyetograph)

#####################################################################
#stream stage & discharge
#####################################################################

p1_stage=read_csv('p1_stage_composite.csv')
p1_stage$datetime = as_datetime(p1_stage$datetime,tz="MST", format="%m/%d/%Y %H:%M")

p1_baro=read_csv('p1_baro_composite.csv')
p1_baro$datetime = as_datetime(p1_baro$datetime,tz="MST", format="%m/%d/%y %H:%M")

p2_stage=read_csv('p2_stage_composite.csv')
p2_stage$datetime = as_datetime(p2_stage$datetime,tz="MST", format="%m/%d/%Y %H:%M")

hum_stage=read_csv('hum_stage_composite.csv')
hum_stage$datetime = as_datetime(hum_stage$datetime,tz="MST", format="%m/%d/%Y %H:%M")

hm_stage=read_csv('hm_stage_composite.csv')
hm_stage$datetime = as_datetime(hm_stage$datetime,tz="MST", format="%m/%d/%Y %H:%M")

hum_baro=read_csv('hum_baro_composite.csv')
hum_baro$datetime = as_datetime(hum_baro$datetime,tz="MST", format="%m/%d/%Y %H:%M")

mum_stage=read_csv('mum_stage_composite.csv')
mum_stage$datetime = as_datetime(mum_stage$datetime,tz="MST", format="%m/%d/%Y %H:%M")

mpm_stage=read_csv('mpm_stage_composite.csv')
mpm_stage$datetime = as_datetime(mpm_stage$datetime,tz="MST", format="%m/%d/%Y %H:%M")

mub_baro=read_csv('mub_baro_composite.csv')
mub_baro$datetime = as_datetime(mub_baro$datetime,tz="MST", format="%m/%d/%Y %H:%M")

mub_stage=read_csv('mub_stage_composite.csv')
mub_stage$datetime = as_datetime(mub_stage$datetime,tz="MST", format="%m/%d/%Y %H:%M")


# Set your start and end datetime
start_datetime <- as.POSIXct("2023-07-19 08:10:00", tz = 'MST')
end_datetime <- as.POSIXct("2023-07-27 08:00:00", tz = 'MST')

# Generate a sequence of datetime values
new_datetimes <- data.frame(datetime = seq(from = start_datetime, to = end_datetime, by = "5 mins")) %>%
  mutate(Pa_kPa = NA,
         Ta_C = NA)

# Fill in the time series of p1_baro
p1_baro_update <- new_datetimes %>%
  bind_rows(., p1_baro) %>%
  arrange(datetime) %>%
  left_join(., mub_baro, by = 'datetime') %>%
  mutate(Pa_kPa = if_else(is.na(Pa_kPa.x),
                          -3.03 + 1.01*Pa_kPa.y,
                          Pa_kPa.x)) %>%
  select(datetime, Pa_kPa, Ta_C = Ta_C.x)

#baro_lm <- lm(p1_baro_update$Pa_kPa.x ~ p1_baro_update$Pa_kPa.y)
#coefficients <- coef(baro_lm)


p1_stage=merge(p1_stage,p1_baro_update)
p2_stage=merge(p2_stage,p1_baro_update)
hum_stage=merge(hum_stage,p1_baro_update)
hm_stage=merge(hm_stage,p1_baro_update)
mum_stage=merge(mum_stage,mub_baro) 
mpm_stage=merge(mpm_stage,mub_baro) 
mub_stage=merge(mub_stage,mub_baro) 

p1_stage$Stage_cm = (p1_stage$Pw_kPa - p1_stage$Pa_kPa)*10.1972
p2_stage$Stage_cm = (p2_stage$Pw_kPa - p2_stage$Pa_kPa)*10.1972
hum_stage$Stage_cm = (hum_stage$Pw_kPa - hum_stage$Pa_kPa)*10.1972
hm_stage$Stage_cm = (hm_stage$Pw_kPa - hm_stage$Pa_kPa)*10.1972
mum_stage$Stage_cm = (mum_stage$Pw_kPa - mum_stage$Pa_kPa)*10.1972 
mpm_stage$Stage_cm = (mpm_stage$Pw_kPa - mpm_stage$Pa_kPa)*10.1972 
mub_stage$Stage_cm = (mub_stage$Pw_kPa - mub_stage$Pa_kPa)*10.1972 

p1_stage$site='p1'
p2_stage$site='p2'
hum_stage$site='hum'
hm_stage$site='hm'
mum_stage$site='mum'
mpm_stage$site='mpm'
mub_stage$site='mub'

willow_stage = readNWISuv(siteNumber = '09019850', parameterCd = "00065")
willow_res = read_csv('willowres.csv')

willow_stage = willow_stage[,3:4]
colnames(willow_stage)=c('datetime','Stage_ft')
willow_stage$stage_cm=willow_stage$Stage_ft*30.48
willow_stage$site='willow'

willow_res$datetime = as.POSIXct(willow_res$datetime,tz=Sys.timezone())

combined=rbind(p1_stage, p2_stage, hum_stage, hm_stage, mum_stage, mpm_stage, mub_stage)

write_csv(combined,'et_stage_sensors.csv')

hydrograph=ggplot()+geom_line(data=combined, aes(x=datetime,y=Stage_cm,color=site))+
  geom_line(data=willow_stage,aes(x=datetime,y=stage_cm/3))
ggplotly(hydrograph)

# Offset stage
# hm
hm_stage <- hm_stage %>%
  mutate(Stage_cm_fix = if_else(datetime > '2022-07-12 08:20:00',
                                Stage_cm + 0.79,
                                Stage_cm))

hm <- ggplot(hm_stage) + geom_line(aes(datetime, Stage_cm_fix))
ggplotly(hm)



high_stage=filter(combined,site=='p1' | site=='p2' | site=='hm' | site=='hum')
high=ggplot()+geom_line(data=high_stage, aes(x=datetime,y=Stage_cm, color=site))+
  scale_color_manual(values=c('orange','red','green4','green2'))+
  geom_line(data=willow_stage,aes(x=datetime,y=stage_cm-100))+ggtitle('High elevation')
ggplotly(high)

mid_stage=filter(combined,site=='mum' | site=='mpm' | site=='mub')
mid=ggplot()+geom_line(data=mid_stage, aes(x=datetime,y=Stage_cm, color=site))+
  scale_color_manual(values=c('orange','green4','red'))+
  geom_line(data=willow_stage,aes(x=datetime,y=stage_cm-100))+ggtitle('Mid elevation')
ggplotly(mid)

ggarrange(high,mid,ncol=1,nrow=2)

ggplot()+geom_line(data=willow_stage,aes(x=datetime,y=stage_cm))+
  geom_line(data=willow_res,aes(x=datetime,y=Q_cfs/9+150),color='blue')+ggtitle('Low elevation')


