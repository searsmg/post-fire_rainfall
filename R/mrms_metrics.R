# functiosn for mrms events, intensities, and tb comparisons

# function for mrms events
get_mrms_events <- function(df, datetime) {
  df$datetime = as.POSIXct(df$datetime,tz="MST", format="%m/%d/%Y %H:%M")
  df$datenumeric=as.numeric(df$datetime)
  
  for (i in 2:nrow(df)) {
    df[i,'dt']=df[i,'datenumeric']-df[i-1,'datenumeric']
  }
  df$dt_hr=as.numeric(df$dt)/60/60
  
  #start new event if time between rows is  >=6
  df$event=1
  for (i in 2:nrow(df)) {
    df[i,'event']=ifelse(df[i,'dt_hr']<6,df[i-1,'event'],df[i-1,'event']+1)
  }
  return(df)
}

# get rqi for events
get_rqi_events <- function(ID_name) {
  rqi_events <- pixel10 %>%
    filter(site == ID_name,
           rain_mm >= 0.1) %>%
    get_mrms_events(., datetime)
  
  return(rqi_events)
}

# get events
get_mrms_all_events <- function(all, ID_name) {
  
  events <- all %>%
    filter(site == ID_name,
           rain_mm >= 0.1) %>%
    get_mrms_events(., datetime) %>%
    group_by(event) %>%
    mutate(start_time = min(datetime),
           end_time = max(datetime)) %>%
    dplyr::select(-c(datenumeric, dt, dt_hr)) %>%
    complete(datetime = seq(min(datetime), max(datetime), by = "10 mins")) %>%
    arrange(event, datetime) %>%
    fill(start_time, end_time, site) %>%
    ungroup() %>%
    mutate(rain_mm = ifelse(is.na(rain_mm), 0, rain_mm))
  
  return(events)
}

# get intensities
get_mrms_intensities <- function(all_events, ID_name) {
  
  intens <- all_events %>%
    filter(site == ID_name) %>%
    group_by(event) %>%
    mutate(max10_mm = max(rain_mm),
           sum20_mm = rollapplyr(rain_mm, width = 2, FUN = sum, partial = TRUE),
           max20_mm = max(sum20_mm),
           sum30_mm = rollapplyr(rain_mm, width = 3, FUN = sum, partial = TRUE),
           max30_mm = max(sum30_mm),
           sum60_mm = rollapplyr(rain_mm, width = 6, FUN = sum, partial = TRUE),
           max60_mm = max(sum60_mm),
           event_sum_mm = sum(rain_mm),
           MI10_mmhr = max10_mm*6,
           MI20_mmhr = max20_mm*3,
           MI30_mmhr = max30_mm*2,
           MI60_mmhr = max60_mm) %>%
    dplyr::select(-c(max10_mm, max20_mm, max30_mm, max60_mm,
                     sum20_mm, sum30_mm, sum60_mm, datetime,
                     rain_mm)) %>%
    ungroup() %>%
    distinct(start_time, .keep_all = T) %>%
    mutate(duration_hr = difftime(end_time, start_time, unit = 'hour')) %>%
    filter(event_sum_mm > 1)
  
}

# filter tb based on mrms event windows +/- 60 mins
filter_tb <- function(site_name) {
  pixel_site <- pixel10_intens %>% 
    filter(site == site_name)
  
  tb_site <- tb %>% 
    filter(site == site_name)
  
  tb_site_fil <- map2(pixel_site$start_time, pixel_site$end_time, ~ {
    start_time <- .x - as.difftime(60, units = "mins")
    end_time <- .y + as.difftime(60, units = "mins")
    event <- pixel_site$event[pixel_site$start_time == .x]  # Get corresponding event value 
    
    # Filter tb_site for rows where datetime falls between start_time and end_time
    tb_site %>%
      filter(between(datetime, start_time, end_time)) %>%
      mutate(event_mrms = event)  # Add the event column
  }) %>%
    bind_rows()  # Combine the filtered results
  
  return(tb_site_fil)
}
