library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)
library(rhandsontable)

login_info <- read_rds("static_files/login.rds")
delaycodecategories <- content(GET("https://pascan-api.intelisys.ca/RESTv1/delayCodeCategories",
                           authenticate(login_info[1],
                                        login_info[2],
                                        type = "basic")))

delaycodes <- content(GET("https://pascan-api.intelisys.ca/RESTv1/delayCodes",
                                   authenticate(login_info[1],
                                                login_info[2],
                                                type = "basic"))) %>% 
  map_df(~enframe(unlist(.)) %>% 
           pivot_wider(names_from = name,values_from = value))






source("static_files/functions/functions.R")
date_margin <- c(as.character(today() - ddays(2)),
                 as.character(today() + ddays(1)))
date_selected <- today()-1
date_selected2 <- today()

main_url <- 'https://pascan-api.intelisys.ca/RESTv1/'
ressources <- str_c("flightStatuses?earliestDeparture=",
                    date_selected,
#                    date_margin[[1]],
                    "&latestDeparture=",
#                    date_margin[[2]])
                     date_selected2)


flightstatuses <- content(GET(str_c(main_url,
                                    ressources),
                              authenticate(login_info[1],
                                           login_info[2],
                                           type = "basic")
))



cols_selection <- c(
#  "flightLeg.departure.scheduledTime",
#  "departure.estimatedTime",
#  "flightLeg.arrival.scheduledTime",
#  "arrival.estimatedTime",
#  "flight.flightNumber",
#  "tail.identifier",
#  "departure.airport.code",
#  "arrival.airport.code",
  "delayMinutes",
  "delayCode.code",
  "delayCode.name",
  "note",
  "included_delay",
  "departure.airport.href")


test26 <- map(flightstatuses,fct_legs) %>% 
  map_df(fct_complete_flt) %>% 
  select(depart_zulu_prevu =flightLeg.departure.scheduledTime,
         depart_zulu_reel = departure.estimatedTime,
         arrivee_zulu_prevu = flightLeg.arrival.scheduledTime,
         arrivee_zulu_reel = arrival.estimatedTime,
         flt_numb = flight.flightNumber,
         Dep = departure.airport.code,
         Arr = arrival.airport.code,
         legNumber,
         tail = tail.identifier,
         starts_with("flightLegStatus"),
         any_of(cols_selection)) %>% #############################################################
#  filter(date(depart_zulu_prevu) == date_selected)    %>% 
  mutate(depart_zulu_prevu = ymd_hms(depart_zulu_prevu, tz = "UTC"),
         depart_zulu_reel = ymd_hms(depart_zulu_reel, tz = "UTC"),
         arrivee_zulu_reel = ymd_hms(arrivee_zulu_reel, tz = "UTC"),
         arrivee_zulu_prevu = ymd_hms(arrivee_zulu_prevu, tz = "UTC"),
         flt_numb = str_c(flt_numb," leg #",legNumber),
         delay_dep = difftime(depart_zulu_reel,depart_zulu_prevu, units = "mins"),
         delay_arr =difftime(arrivee_zulu_reel,arrivee_zulu_prevu, units = "mins")) %>% 
  arrange(tail,depart_zulu_reel) %>% 
  
  mutate(note = if_else(included_delay == "" | is.na(included_delay),
                        str_c("<b>",note,"</b>"),
                        str_c("<b>",note,"</b>","<br><u>included delay</u><br>",
                              '<small style="color:red;">',included_delay,"</small>"))) 


                          

cancel_flts <- test26 %>% 
  filter(flightLegStatus.cancelled == TRUE) %>% 
  select(depart_zulu_prevu,flt_numb,Dep,Arr)

column_selected <- c("depart_zulu_prevu",
                     "d_vol_prevu",
                     "d_vol_reel",
                     "flt_numb",
                     "Dep",
                     "Arr",
                     "tail",
                     "Delay",
                     "diff_blk_T",
                     "delayCode.code,delayCode.name",
                     "note",
                     "included_delay",
                     "departure.airport.href")

test27 <- test26 %>%
  filter(flightLegStatus.cancelled == FALSE,
         difftime(depart_zulu_reel,depart_zulu_prevu,units = "mins") %>% as.numeric() >= 9) %>% 
  mutate(d_vol_prevu = arrivee_zulu_prevu - depart_zulu_prevu,
         d_vol_reel = arrivee_zulu_reel-depart_zulu_reel,
         diff_blk_T = d_vol_reel - d_vol_prevu)

test28 <- test27 %>% 
  select(
    depart_zulu_prevu,
    d_vol_prevu,
    d_vol_reel,
    flt_numb,
    Dep,
    Arr,
    tail,
    delay_dep,
    delay_arr,
    diff_blk_T,
any_of(c("delayCode.code",###########################################
         "delayCode.name",
         "note")),
#         "included_delay")),
    departure.airport.href)
    
test29 <- test28 %>% 
    group_by(tail) %>% 
    group_split() %>% 
    map_df(fct_code93)


test28 %>% 
  select(flt_numb,delayCode.code,delayCode.name,note) %>% 
  rhandsontable(allowedTags = "<em><b><br><u><big>") %>% 
  hot_col("note",renderer = "html") %>% 
  hot_col("note",renderer = htmlwidgets::JS("safeHtmlRenderer")) %>% 
  hot_col("flt_numb", colHeader = "test")

