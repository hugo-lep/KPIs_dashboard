
fct_legs <- function(my_data){
  my_data[["legs"]]
}

#########################################################



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
