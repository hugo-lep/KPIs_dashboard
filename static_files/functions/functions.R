
fct_legs <- function(my_data){
  my_data[["legs"]]
}

#########################################################
#test


#########################################################
#data <- test26[[1]][[1]]


fct_complete_flt <- function(data){
  data %>% map_df(fct_leg_details) #%>% 
#    arrange(flightLeg.departure.scheduledTime)
}

fct_leg_details <- function(data){
  unlist_data <- enframe(unlist(data))
  
  leg_data <- unlist_data %>% 
    filter(!grepl("^delays",name)) %>% 
    pivot_wider(names_from = name,values_from = value)
  
  delays_data <- unlist_data %>% 
    filter(grepl("^delays",name)) 
  
  delays_data2 <- 
    if(nrow(delays_data) == 0){NULL
    }else{
      data$delays %>% 
        map_df(~enframe(unlist(.)) %>% 
                 pivot_wider(names_from = name,values_from = value)) %>% 
        filter(delayApplicability.departure == TRUE) %>% 
        select(delayMinutes,delayCode.code,delayCode.name,note) %>%
        mutate(delayMinutes = as.numeric(delayMinutes)) %>% 
        arrange(desc(delayMinutes))
    }
  
  delays_data3 <- if(is.null(delays_data2)){NULL
  }else if(nrow(delays_data2) == 0){NULL
    }else{
      delays_data2}
  
  delays_data4 <- 
    if(is.null(delays_data3)){NULL
    }else{
      delays_data3 %>% 
        slice(1)
    }  
  
  
  previous_delay <- 
    if(is.null(delays_data3)){NULL
    } else {
      delays_data3 %>%
        arrange(desc(delayMinutes)) %>% 
        slice(-1) %>%
        mutate(note_previous_delay = str_c("#",delayCode.code,"(",delayMinutes,"min): ",note)) %>% 
        select(note_previous_delay) %>% 
        as_vector() %>% 
        paste0(collapse = "<br>")
    }
  
#previous_delay %>% as_vector() %>%  paste0(collapse = "<br>")
  
  
  
  
  output <-  bind_cols(leg_data,delays_data4) %>% 
    mutate(included_delay = previous_delay)
  
  
  
#  output <- if(nrow(delays_data3) == 0){leg_data
#  } else {
#    bind_cols(leg_data,delays_data3) %>% 
#      mutate(included_delay = previous_delay)
#  }
#}
}


#########################################################################################
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
    select(Dep,Arr,1:8) #%>% 
#    mutate(delay_dep = as.character(delay_dep))
}




