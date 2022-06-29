library(shiny)
library(shinydashboard)
library(tidyverse)
library(httr)
#library(reactable)
#install.packages("devtools")
library(lubridate)
library(rhandsontable)
library(jsonlite)
library(hms)
library(shinymanager)

#https://stla.github.io/stlapblog/posts/shiny_editTable.html



source("static_files/functions/functions.R")
login_info <- read_rds("static_files/login.rds")
main_url <- 'https://pascan-api.intelisys.ca/RESTv1/'

cols_selection <- c(
  "flightLeg.departure.scheduledTime",
  "departure.estimatedTime",
  "flightLeg.arrival.scheduledTime",
  "arrival.estimatedTime",
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

time_delay <- c("Tous","9 min. et +")

delaycodes <- content(GET("https://pascan-api.intelisys.ca/RESTv1/delayCodes",
                          authenticate(login_info[1],
                                       login_info[2],
                                       type = "basic"))) %>% 
  map_df(~enframe(unlist(.)) %>% 
           pivot_wider(names_from = name,values_from = value)) %>% 
  select(code,name,category.name) %>% 
  mutate(code_name = str_c(code," - ",name))

credentials <- data.frame(
  user = c("Julian", "Mathieu","dispatch"), # mandatory
  password = c("Pascan2022", "Pascan2022","Pascan2022"), # mandatory
  start = c("2019-04-15"), # optinal (all others)
  expire = c(NA,NA,NA),
  admin = c(FALSE, FALSE,TRUE),
  comment = "Simple and secure authentification mechanism 
  for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)


#########################################################################################################
#########################################################################################################
#########################################################################################################

ui <- fluidPage(
  tags$h2("My secure application"),
  verbatimTextOutput("auth_output")
)


ui <- secure_app(
  fluidPage(
    titlePanel("Page pour réviser les retards"),
    fluidRow(
      column(4,
             dateInput("date_selected",
                       label = "select a date",
                       max = today(tzone = "UTC")- ddays(1),
                       value = today() - ddays(1))),
      column(4,
             checkboxInput("if_delay",
                           label = "afficher les vols en retard seulement",
                           value = TRUE)),
      column(4,
             conditionalPanel(
               condition = "input.if_delay == true",
               selectInput("min_retard",
                           label = "nombre de minutes de délai pour être considéré comme un retard",
                           choices = time_delay,
                           selected = "9 min. et +"))
      )),
    #  fluidRow("Nombre de segment(s) prévu(s): ",),
    h2("Tableau d'évaluation des retards (Délais au départ)"),
    rHandsontableOutput("test"),
    
    h2("Tableau de vols annulés"),
    rHandsontableOutput("cancel_tab")
    
  )
)


#########################################################################################################
#########################################################################################################
#########################################################################################################


server <- function(input, output, session) {
  
  # check_credentials returns a function to authenticate users
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  ressources <- reactive(
    str_c("flightStatuses?earliestDeparture=",
          #          "2022-05-06",
          input$date_selected,
          "&latestDeparture=",
          #          "2022-05-07")
          input$date_selected + ddays(1))
  )
  
  flightstatuses <- reactive(
    content(GET(str_c(main_url,
                      ressources()),
                authenticate(login_info[1],
                             login_info[2],
                             type = "basic")
    )
    )
  )
  
  test26 <- reactive({
    out <- map(flightstatuses(),fct_legs) %>% 
      map_df(fct_complete_flt)
    out <- out %>% 
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
             any_of(cols_selection)) %>%                                         #################
    filter(date(depart_zulu_prevu) == input$date_selected) %>%
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
                            str_c("<b>",note,"</b>","<br><u>included delay</u><br>",included_delay)))
    
    #      out <-  if(input$if_delay == FALSE){out
    #        }else{
    #          out %>% filter(
    #            difftime(departure.estimatedTime,depart_zulu_prevu,units = "mins") %>% as.numeric() >= time_delay2())
    #     }
  })
  
#  test27 <- reactive({
#    out <-  if(input$if_delay == FALSE){test26()
#    }else{
#      test26() %>% filter(
#        difftime(depart_zulu_reel,depart_zulu_prevu,units = "mins") %>% as.numeric() >= time_delay2())
#    }
#  })
  
  test28 <- reactive({
    out <- test26() %>% 
      filter(flightLegStatus.cancelled != TRUE) %>% 
      group_by(tail) %>% 
      group_split() %>% 
      map_df(fct_code93)
    
    out <-  if(input$if_delay == FALSE){out
    }else{
      out %>% filter(
        difftime(depart_zulu_reel,depart_zulu_prevu,units = "mins") %>% as.numeric() >= time_delay2())
    }
  })
  
  
  
  output$test <- renderRHandsontable(
    test28() %>% 
      #        select(-starts_with("flightLegStatus")) %>% 
      mutate(Delay = difftime(depart_zulu_reel,depart_zulu_prevu,units = "mins"),
             Delay = str_c(as.character(Delay)," mins"),
#             delay_arr =difftime(arrivee_zulu_reel,arrivee_zulu_prevu, units = "mins"),
             d_vol_prevu = difftime(arrivee_zulu_prevu,depart_zulu_prevu, units = "mins"),
             d_vol_reel = difftime(arrivee_zulu_reel,depart_zulu_reel, units = "min"),
             diff_blk_T = as.character(d_vol_reel - d_vol_prevu),
             d_vol_prevu = as.character(d_vol_prevu),
             d_vol_reel = as.character(d_vol_reel),
             depart_zulu_prevu = str_sub(as.character(depart_zulu_prevu),-8,-4)) %>% #,
      #                depart_zulu_prevu = as.date(depart_zulu_prevu, )) %>%   #car rhandsometable ne semble pas accepter class difftime
      select(
        depart_zulu_prevu,
        flt_numb,
        Dep,
        Arr,
        tail,
        Delay,
        #          d_vol_prevu,
        #          d_vol_reel,
        diff_blk_T,
        any_of(c("delayCode.code",
                 "delayCode.name",
                 "note"))) %>% #,
      #         "included_delay"))) %>% 
      #          Delay_code = delayCode.code,
      #          Delay_name = delayCode.name,
      #          note,
      #          included_delay) %>% 
      rename("flight #" = flt_numb,
             "Depart (Zulu)" = depart_zulu_prevu,
             "Diff blk time" = diff_blk_T,
             "Delay Code" = delayCode.code,
             "Delay Name" = delayCode.name) %>% 
      #         "Diff blk time" = diff_blk_T) %>% 
      
      rhandsontable(
        #            rename("flight #" = flt_numb),
        col_highlight = 5,
        #            rowHeaders = FALSE,
        readOnly = TRUE,
        allowedTags = "<br><p><small><h3><u><span>") %>% 
#        allowedTags = "<em><b><br><u><big>") %>% 
      
      hot_cols(columnSorting = TRUE) %>%
      #  hot_col("depart_zulu_prevu", dateFormat = "hh:mm:ss"),
      hot_col("Depart (Zulu)",halign = "htCenter") %>% 
      hot_col(c("Delay Name","note"),renderer = "html") %>% 
      hot_col(c("Delay Name","note"),renderer = htmlwidgets::JS("safeHtmlRenderer")) %>%
  
      hot_col("Delay Code",halign = "htCenter") %>% 
      #  hot_col("note", colWidths = 500) %>% 
      hot_col("Diff blk time", halign = "htCenter") %>% 
      hot_col("Diff blk time",
              renderer = "
                  function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if (value > 0) {
             td.style.color = 'red';
             } else if (value < 0) {
             td.style.color = 'green';
             }
       }") %>% 

      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  )
  
  
  
  time_delay2 <- reactive({
    case_when(input$min_retard == time_delay[1]~1,
              input$min_retard == time_delay[2]~9)
  })
  
  output$cancel_tab <- renderRHandsontable(
    test26() %>% 
      filter(flightLegStatus.cancelled == TRUE) %>% 
      select(depart_zulu_prevu,flt_numb,Dep,Arr,tail) %>% 
      
      rhandsontable() %>% 
      hot_cols(colWidths = 200,
               halign = "htCenter") %>% 
      hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
  )
  
#  output$stat <- renderText(
    
#  )
  
}

shinyApp(ui, server)