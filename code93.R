library(tidyverse)
library(rhandsontable)


code93 <- test26 %>% 
  filter(flightLegStatus.cancelled != TRUE) %>% 
  select(depart_zulu_prevu,depart_zulu_reel,delay_dep,delay_arr,flt_numb,tail,delayCode.code,delayCode.name,note,Arr,Dep) %>% 
  filter(tail == "C-FKSL")

code93_1 <- code93 %>% group_by(tail) %>% 
  group_split() %>% 
  map_df(fct_code93)



fct_code93 <- function(data){
  data %>% 
    select(-delay_arr) %>% 
    bind_cols(
      data %>% add_row(.before = 1) %>% 
        slice_head(n = -1) %>% 
        select(Arr2 = Arr,flt_numb2 = flt_numb,delay_arr) %>% 
        mutate(delay_arr = as.character(delay_arr)),
      
      data %>% select(next_flight_code = delayCode.code) %>% 
        add_row() %>% 
        slice(-1)
    ) %>% 
    
    
    #################################
  mutate(initial_cause = if_else(next_flight_code == "93" & (delayCode.code != 93 | is.na(delayCode.code)),
                                 flt_numb,
                                 NA_character_)) %>% 
    fill(initial_cause) %>% 
    mutate(initial_cause = if_else(delayCode.code == "93",
                                   initial_cause,
                                   NA_character_)) %>% 
    
    mutate(delayCode.name = if_else(delayCode.code == "93",
                                    str_c(delayCode.name,
                                          '<br><small style="color:red;">Previous flight: ',flt_numb2," was: ",delay_arr," mins late","</small>",
                                          '<br><small style="color:red;">Rotational cause by: flight ', initial_cause,"</small>"),
                                    delayCode.name))%>% 
    select(Dep,Arr,1:8) %>% 
    mutate(delay_dep = as.character(delay_dep))
}







code93_1 %>% rhandsontable(
  allowedTags = "<br><p><small><h3><u>") %>% 
  hot_col(c("delayCode.name","note"),renderer = "html") %>% 
  hot_col(c("delayCode.name","note"),renderer = htmlwidgets::JS("safeHtmlRenderer"))








