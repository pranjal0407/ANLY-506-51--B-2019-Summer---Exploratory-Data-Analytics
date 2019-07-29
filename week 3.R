getwd()
library(readr)

#working on windows
ozone <- read_csv("C:/Users/P2725076/Documents/HU/EDA/Code/US EPA.csv",
                                col_types = "ccccinnccccccncnncccccc")
ozone
#summary ozone
summary(ozone)

#rewriting the names of the columns to remove any spaces
names(ozone) <- make.names(names(ozone))

#checking number of rows and columns
nrow(ozone)
ncol(ozone)

#run str() which shows basic info like no of rows, columns, classes of each columns
str(ozone)

#Top and bottom of data using head() and tail()
head(ozone[, c(6:7, 10)])
tail(ozone[, c(6:7, 10)])

table(ozone$Longitude)

#loading dplyr
library(dplyr)

filter(ozone, Latitude == "41.175") %>% 
          select(State.Name, County.Name, Date.Local, 
                   +  Time.Local, Sample.Measurement)
#Error in .f(.x[[i]], ...) : object 'State.Name' not found

 filter(ozone, State.Code == "36" 
                & County.Code == "033" 
                & Date.Local == "2014-09-30") %>%
      select(Date.Local, Time.Local, 
                                  Sample.Measurement) %>% 
         as.data.frame
 #Error: object 'Date.Local' not found
 
select(ozone, 'State.Name') %>% unique %>% nrow

unique(ozone$State.Name)
#Unknown or uninitialised column: 'State.Name'. 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 





