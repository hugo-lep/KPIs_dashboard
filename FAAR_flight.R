library(httr)
library(tidyverse)
library(lubridate)

login_info <- read_rds("static_files/login.rds")


flights <- content(GET(str_c("https://pascan-api.intelisys.ca/RESTv1/flightstatuses?",
                                  "earliestDeparture=",
                                  as.character(today()),
                                  "&latestDeparture=",
                                  as.character(today()+ ddays(30))),
                       authenticate(login_info[1],
                                    login_info[2],
                                    type = "basic")))

flights2 <- flights %>%  map_df(~enframe(unlist(.)) %>% 
                              pivot_wider(names_from = name,values_from = value)) %>% 
  select(date = departure.scheduledTime,airport = departure.airport.code) %>% 
  mutate(date = date(date)) %>% 
  distinct()



#############################################################################################
#############################################################################################
#############################################################################################
                            












                                    
2022-05-02&",
#                                  "latestDeparture=2022-05-02"),
                                   authenticate(login_info[1],
                                                login_info[2],
                                                type = "basic")))


RESERVATIONS_ <- GET(str_c("https://pascan-api.intelisys.ca/RESTv1/reservations?",
                                  "earliestDeparture=2022-05-01&",
                                  "latestDeparture=2022-05-10"),
                            authenticate(login_info[1],
                                         login_info[2],
                                         type = "basic"))

RESERVATIONS <- content(RESERVATIONS_)