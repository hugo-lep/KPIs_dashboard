library(httr)
library(rvest)
library(tidyverse)




#https://towardsdatascience.com/web-scraping-with-r-easier-than-python-c06024f6bf52
#https://riptutorial.com/r/example/23955/using-rvest-when-login-is-required


web <- "https://ameliawebreports.intelisys.ca/pascan/default.aspx"
pgsession <- session(web)
pgform <- html_form(pgsession)[[1]]
filled_form <- html_form_set(pgform,
                             "ctl00$ucLogin$txtBoxUsername"= "HUGO",
                             "ctl00$ucLogin$txtBoxPassword" = "LEPAGE")
session_submit(pgsession,filled_form)



test <- pgform$fields[[5]]
test$name




html <- read_html("http://www.google.com")
search <- html_form(html)[[1]]

search <- search %>% html_form_set(q = "My little pony", hl = "fr")

# Or if you have a list of values, use !!!
vals <- list(q = "web scraping", hl = "en")
search <- search %>% html_form_set(!!!vals)

# To submit and get result:
## Not run: 
resp <- html_form_submit(search)
read_html(resp)
