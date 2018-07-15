##############################
# Load and transform data for heatmap
###############################

#load ofo library
if (!require(ofobikeR)) {
  install.packages('ofobikeR') 
  require(ofobikeR)
}

#unload all previously loaded packages
detachAllPackages()

#load required libraries
if (!require(tidyverse)) {
  install.packages('tidyverse') # load tidyverse packages
  require(tidyverse)
}
if (!require(here)) {
  install.packages('here') # for better directory references
  require(here)
}
if (!require(googlesheets)) {
  install.packages('googlesheets') # read data from google sheets
  require(googlesheets)
}



## clear the console of all variables
rm(list = ls())
## free up unused memory
gc()

computeTrips<-function(prior_weeks=0,full_week=0) {
  week_trips<-US_trips_cleaned %>% 
    arrange(trip_time) %>% 
    filter(between(trip_time,
                   lubridate::floor_date(max(US_trips_cleaned$trip_time)-lubridate::days(prior_weeks*7),
                                         unit='week',week_start=1),
                   ifelse(full_week==0,
                          max(US_trips_cleaned$trip_time)-lubridate::days(prior_weeks*7),
                          lubridate::ceiling_date(max(US_trips_cleaned$trip_time)-lubridate::days(prior_weeks*7),
                                                  unit='week',week_start=1)))) %>%  
    summarise(n())
  return(week_trips) 
}

computeRevenue<-function(prior_weeks=0,full_week=0) {
  week_revenue<-US_trips_cleaned %>% 
    arrange(trip_time) %>% 
    filter(between(trip_time,
                   lubridate::floor_date(max(US_trips_cleaned$trip_time)-lubridate::days(prior_weeks*7),
                                         unit='week',week_start=1),
                   ifelse(full_week==0,
                          max(US_trips_cleaned$trip_time)-lubridate::days(prior_weeks*7),
                          lubridate::ceiling_date(max(US_trips_cleaned$trip_time)-lubridate::days(prior_weeks*7),
                                                  unit='week',week_start=1)))) %>%  
    summarise(sum(balance,na.rm=TRUE)+sum(credit_balance,na.rm=TRUE)+sum(receipts,na.rm=TRUE))
  
  pass_revenue<-ofo_Pass_Clean %>% 
    arrange(pay_date) %>% 
    filter(between(pay_date,
                   lubridate::floor_date(max(ofo_Pass_Clean$pay_date)-lubridate::days(prior_weeks*7),
                                         unit='week',week_start=1),
                   ifelse(full_week==0,
                          max(ofo_Pass_Clean$pay_date)-lubridate::days(prior_weeks*7),
                          lubridate::ceiling_date(max(ofo_Pass_Clean$pay_date)-lubridate::days(prior_weeks*7),
                                                  unit='week',week_start=1)))) %>%
    summarise(sum(money,na.rm=TRUE))
    
  
  return(week_revenue+pass_revenue) 
} 

computeDownloads<-function(prior_weeks=0,full_week=0) {
  week_downloads<-US_Users_Summary_Clean %>% 
    filter(between(download_date,
                   lubridate::floor_date(max(US_Users_Summary_Clean$download_date)-lubridate::days(prior_weeks*7),  
                                         unit='week',week_start=1),
                   ifelse(full_week==0,
                          max(US_Users_Summary_Clean$download_date)-lubridate::days(prior_weeks*7),
                          lubridate::ceiling_date(max(US_Users_Summary_Clean$download_date)-lubridate::days(prior_weeks*7),
                                                  unit='week',week_start=1)))) %>% 
    summarise(n())
  return(week_downloads) 
} 

#US_Users_Summary <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/data/users/users_us_most_recent.csv",
US_Users_Summary <- read_csv("~/Downloads/US ofo Users (daily) [use this].csv",
                             ,col_types = cols(tr.total_trips = col_integer(),
                                               gu.bindtime = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                                               gu.lasttime = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                               gu.last_usedtime = col_datetime(format = "%Y-%m-%d %H:%M:%S"),                                               
                                               tr.first_trip = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                                               tr.last_trip = col_datetime(format = "%Y-%m-%d %H:%M:%S")))
US_Users_Summary_Clean<-US_Users_Summary %>%
  rename_all(~sub('(gu|tr).','',.x)) %>%
  rename(user_id=userid) %>% 
  group_by(user_id) %>%
  arrange(desc(createtime)) %>% 
  mutate(n=row_number()) %>% 
  ungroup(.) %>% 
  mutate(first_trip=lubridate::as_date(first_trip),
         last_trip=lubridate::as_date(last_trip),
         total_trips=ifelse(is.na(total_trips),0,total_trips),
#         download_date=lubridate::as_date(createtime),
         download_date=lubridate::as_datetime(createtime-lubridate::hours(12)),
         join_date=lubridate::as_date(download_date),
         start_week=lubridate::as_date(lubridate::floor_date(download_date,unit='week',1)),
         start_month=lubridate::as_date(lubridate::floor_date(download_date,unit='month')),
         start_year=lubridate::as_date(lubridate::floor_date(download_date,unit='year')),
 #         signup_date = lubridate::as_date(applytime),
         cc_date = lubridate::as_date(bindtime),
         has_multiple_trips=if_else(total_trips>1,1,0),
         has_trip=if_else(total_trips>0,1,0),
         has_cc=is_bind_card,
         #         has_registered=ifelse(is.na(signup_date),0,1),
         has_registered=ifelse(state %in% c(1,2,11),1,0),
         has_downloaded=1) %>% 
  filter(n==1) %>% 
#  select(user_id,user_city,pushtoken,first_trip,last_trip,total_trips,download_date,join_date,start_week,start_month,start_year,has_multiple_trips,has_trip,has_cc,has_registered,has_downloaded)
  select(user_id,user_city,first_trip,last_trip,total_trips,download_date,join_date,start_week,start_month,start_year,has_multiple_trips,has_trip,has_cc,has_registered,has_downloaded)

Users_join<-US_Users_Summary_Clean %>% 
  select(user_id,join_date,user_city) %>% 
#  rename(join_date=download_date_adjusted) %>% 
  mutate(user_city=ifelse(is.na(user_city),"None",user_city))


rides_2018_Q1 <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/data/trips/2018Q1_US_res_detail.csv",
                          col_types = cols(ord.local_time = col_datetime(),
                                           ord_pay.balance = col_double(),
                                           ord_pay.coupon_amount = col_double(),
                                           ord_pay.coupon_id=col_integer(),
                                           ord_pay.credit_balance = col_double(),
                                           ord_pay.payment_method = col_integer(),
                                           coup_act.coupon_type = col_integer(), 
                                           receipts = col_double()))

rides_2018_Q2 <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/data/trips/2018Q2_US_res_detail.csv",
#rides_2018_Q2_3 <- read_csv("~/Downloads/US Reservation Details with coupons [use this]-jun.csv",                        
                           col_types = cols(#ord.local_time = col_character(),
                                            ord.local_time = col_datetime(),
                                            ord_pay.balance = col_double(),
                                            ord_pay.coupon_amount = col_double(),
                                            ord_pay.coupon_id=col_integer(),
                                            ord_pay.credit_balance = col_double(),
                                            ord_pay.payment_method = col_integer(),
                                           coup_act.coupon_type = col_integer(), 
                                           receipts = col_double()))

rides_most_recent <- read_csv("~/Downloads/US Reservation Details with coupons [use this].csv",
                          col_types = cols(#ord.local_time = col_character(),
                                           ord.local_time = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                                           ord_pay.balance = col_double(),
                                           ord_pay.coupon_amount = col_double(),
                                           ord_pay.coupon_id=col_integer(),
                                           ord_pay.credit_balance = col_double(),
                                           ord_pay.payment_method = col_integer(),
                                           coup_act.coupon_type = col_integer(), 
                                           receipts = col_double()))

US_rides<-bind_rows(rides_2018_Q1,rides_2018_Q2,rides_most_recent)
#US_rides<-bind_rows(rides_2018_Q1,rides_2018_Q2)
US_rides<-distinct(US_rides)
 rm(rides_2018_Q1,rides_2018_Q2,rides_most_recent)
#rm(rides_2018_Q1,rides_2018_Q2)

US_trips_cleaned<-US_rides %>%
  rename_all(~sub('^(ord|ord_pay|coup|coup_act)\\.','',.x)) %>%
  #  na.omit(.) %>% 
  rename(trip_time=local_time) %>% 
  filter(lubridate::year(trip_time)==2018) %>% 
  mutate(start_hour=lubridate::hour(trip_time),
         start_day=lubridate::wday(trip_time,week_start=1),
         trip_iso_week=lubridate::isoweek(trip_time),
         trip_week=lubridate::as_date(lubridate::floor_date(trip_time,unit='week',1)),
         trip_month=lubridate::as_date(lubridate::floor_date(trip_time,unit='month')),
         trip_year=lubridate::as_date(lubridate::floor_date(trip_time,unit='year')),
         trip_date=lubridate::as_date(trip_time),
         trip_payment_type = case_when(payment_method == 0 ~ "Free",
                                              payment_method == 4 ~ "ofo Pass",
                                              is.na(payment_method) ~ "not captured",
                                              TRUE ~ 'Pay')) %>% 
  left_join(.,Users_join,by="user_id") %>% 
  group_by(user_id) %>% 
  arrange(trip_time) %>% 
  # mutate(trip_type=case_when(
  #   difftime(trip_date,join_date,units='days')<=7 ~ 'New User',
  #   difftime(trip_date,lag(trip_date,1),units='days')<=28 ~ 'Returning User',
  #   difftime(trip_date,lag(trip_date,1),units='days')>28 ~ 'Lapsed User',
  #   difftime(trip_date,join_date,units='days')>7 ~ 'First Time User',
  #   TRUE ~ 'Other')) %>% 
  mutate(time_since_join=difftime(trip_date,join_date,units='days'),
         time_since_last_trip=difftime(trip_date,lag(trip_date,1),units='days')) %>% 
  ungroup(.)
    



#ofo_markets<-as.data.frame.matrix(ofo_get_Markets(),row.names=NULL,stringsAsFactors = FALSE)


#passes_us_most_recent <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/data/passes/passes_us_most_recent.csv",
passes_us_most_recent <- read_csv("~/Downloads/ofo Pass purchases.csv",
                                  col_types = cols(arw.pass_id = col_integer(),
                                                   arw.policy_id = col_integer(), 
                                                   arw.run_count = col_integer(), 
                                                   arw.status = col_integer(), 
                                                   dp.money = col_double(), 
                                                   pp.price = col_double(), 
                                                   pp.discount_price=col_double(),
                                                   pr.create_time = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                                   pr.end_time = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                                   pr.start_time = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
                                                   pr.update_time = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                                                   dp.money = col_double(),
                                                   payment_time = col_datetime(format = "%Y-%m-%d %H:%M:%S")))

ofo_Pass_Clean<-passes_us_most_recent %>% 
  rename(autorenew_status=arw.status,
         autorenew_policy=arw.policy_id) %>% 
  rename_all(~sub('^(pr|pp|dp|arw).','',.x)) %>%
  mutate(user_id=ifelse(user_id<0,user_id*-1,user_id),
         start_date=lubridate::as_date(start_time),
#         start_week=lubridate::floor_date(start_date,unit="week",week_start = 1),
#         start_month=lubridate::floor_date(start_date,unit="month"),
         end_date=lubridate::as_date(end_time),
         create_date=lubridate::as_date(create_time),
         update_date=lubridate::as_date(update_time),
         pay_date=lubridate::as_date(payment_time),
         pay_week=lubridate::floor_date(pay_date,unit="week",week_start=1),
         pay_month=lubridate::floor_date(pay_date,unit="month"),
         pass_type=ifelse(content=='Campus Pass',paste0(trimws(title),':',content),trimws(title)),
         auto_renew=ifelse(auto_type==1,'Yes','No')) %>% 
  replace_na(list(price = 0, 
                  discount_price= 0,
                  money=0,
                  autorenew_status=0)) %>% 
  left_join(.,US_Users_Summary_Clean,by='user_id') %>% 
  select(user_id,user_city,start_date,end_date,create_time,pay_date,pay_week,pay_month,freeweek,status,pass_type,auto_renew,title,content,price,discount_price,money,autorenew_status,autorenew_policy) %>% 
  mutate(start_week=lubridate::floor_date(start_date,unit="weeks",week_start=1),
         start_month=lubridate::floor_date(start_date,unit='month'),
         start_year=lubridate::floor_date(start_date,unit='year')) %>% 
  filter(pass_type %in% c('Monthly Pass','Monthly Pass:Campus Pass','2-day Pass','Quarterly Pass','Semester Pass','Yearly Pass','Group Pass'))
  
#US_feedback <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/data/ride_feedback/feedback_us_most_recent.csv",
US_feedback <- read_csv("~/Downloads/ofo US User feedback.csv",
                        col_types = cols(stg_global_t_feedback.time = col_datetime(format = "%Y-%m-%d %H:%M:%S")))

US_feedback_cleaned<-US_feedback %>% 
  rename_all(~sub('stg_global_t_feedback.','',.x)) %>%
  rename(user_id=userid,
         feedback_time=time) %>% 
  mutate(feedback_date=lubridate::as_date(feedback_time)) %>% 
  left_join(.,US_Users_Summary_Clean,by='user_id') %>%
  select(id,user_id,orderno,grade,options,comment,feedback_date,feedback_time,user_city)



#get weekly ad report 
US_ofo_Markets<-gs_title("Market Mappings")
#gs_ws_ls(US_Operations)

#read the online campaign sheet
market_map<-US_ofo_Markets %>% 
  gs_read(ws='market mappings') %>% 
  na.omit(.) 

#market_map <- read_csv("~/Downloads/market_map.csv")
#market_map<-unique(market_map)

US_trips_cleaned<-left_join(US_trips_cleaned,market_map, by=c("start_city"="City"))
US_Users_Summary_Clean<-left_join(US_Users_Summary_Clean,market_map, by=c("user_city"="City"))
ofo_Pass_Clean<-left_join(ofo_Pass_Clean,market_map, by=c("user_city"="City"))
US_feedback_cleaned<-left_join(US_feedback_cleaned,market_map,by=c("user_city"="City"))

#we will have some Users who do not have a market mapped, since having a city isn't a requirement, especially when using country code to filter
#we will fill as Unknown for now
US_Users_Summary_Clean<-US_Users_Summary_Clean %>% 
  mutate(Market==is.na(Market),"Unknown",Market)



dashboardTrips<-setNames(cbind(as_tibble(c('WTD','PWTD','PW')),as_tibble(rep('Trips',3)),rbind(computeTrips(),computeTrips(1),computeTrips(1,1))),c('TimeFrame','dashboard_Measure','dashboard_Value'))
dashboardRevenue<-setNames(cbind(as_tibble(c('WTD','PWTD','PW')),as_tibble(rep('Revenue',3)),rbind(computeRevenue(),computeRevenue(1),computeRevenue(1,1))),c('TimeFrame','dashboard_Measure','dashboard_Value'))
dashboardDownloads<-setNames(cbind(as_tibble(c('WTD','PWTD','PW')),as_tibble(rep('Downloads',3)),rbind(computeDownloads(),computeDownloads(1),computeDownloads(1,1))),c('TimeFrame','dashboard_Measure','dashboard_Value'))

Topline_dashboard<-rbind(dashboardDownloads,dashboardTrips,dashboardRevenue)

save(US_trips_cleaned,US_Users_Summary_Clean,ofo_Pass_Clean,US_feedback_cleaned,Topline_dashboard,file=here('app_data','marketing_dashboard_data.RData'))
rm(market_map,passes_us_most_recent,US_feedback,US_rides,US_Users_Summary,dashboardTrips,dashboardRevenue,dashboardDownloads,Users_join)


