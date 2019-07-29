getwd()

library(tidyverse)

#There are three interrelated rules which make a dataset tidy:
# 1. Each variable must have its own column.
# 2. Each observation must have its own row.
# 3. Each value must have its own cell.

#Compute rate per 10,000
table1 %>% 
  mutate(rate = cases / population * 10000)
 

# Compute cases per year
table1 %>% 
  count(year, wt = cases)


# Visualise changes over time
library(ggplot2)
ggplot(table1, aes(year, cases)) + 
  geom_line(aes(group = country), colour = "grey50") + 
  geom_point(aes(colour = country))

#Gather is used when the column names are not names of variables. Insted it contains the values of a variable.
#Gather() makes wide tables narrower and longer.
table4a %>%   
  gather(`1999`, `2000`, key = "year", value = "cases")


#example2
table4b %>%   
  gather(`1999`, `2000`, key = "year", value = "population")


#dplyr::left_join() - To combine the tided versions of above t2o tables.
tidy4a <- table4a %>%   
  gather(`1999`, `2000`, key = "year", value = "cases")
tidy4b <- table4b %>%   
  gather(`1999`, `2000`, key = "year", value = "population")
left_join(tidy4a, tidy4b)


#Spreading: It is opposite of gathering. Used when an observation is scattered accross different rows.
#Spread() makes long tables shorter and wider.

table2 %>%  
  spread(key = type, value = count)


# Separate() - This function is used to break one column into multiple columns. 
# This is done by splitting whereever a separator character appers. Splits non-alphanumeric character by default.
table3 %>%   
  separate(rate, into = c("cases", "population"))


#Sep argument can be used to pass the character of our choice to seperate a column
table3 %>% 
  separate(rate, into = c("cases", "population"), sep = "/")


#convert = TRUE can be used to convert the type of column. In this case converting from Character to number.
table3 %>%   
  separate(rate, into = c("cases", "population"), convert = TRUE)


#vector of integers can also be passed using Sep. Lenth of sep should be one less than the no. of names in into.
table3 %>%   
  separate(year, into = c("century", "year"), sep = 2)


#Unite: This combines multiple columns into single column.
table5 %>%   
  unite(new, century, year)


#sep argument can be used with unite. Default for this is (_) underscore.
table5 %>%   
  unite(new, century, year, sep = "")


#Creating sample dataset
stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)

stocks %>%   
  spread(year, return)

#explicit missing values can be turned to implicit by setting set na.rm = TRUE in gather():
stocks %>%   
  spread(year, return) %>%   
  gather(year, return, `2015`:`2016`, na.rm = TRUE)

#Other way of turning explicit values to implicit is complete()
stocks %>% 
  complete(year, qtr)

#Sample data to work will fill()
treatment <- tribble(
  ~ person,           ~ treatment, ~response,
  "Derrick Whitmore", 1,           7,
  NA,                 2,           10,
  NA,                 3,           9,
  "Katherine Burke",  1,           4
)

#fill() replaces the set of columns with missing values by the most recent non-missing value.
treatment %>% 
  fill(person)

#12.6 CaseStudy
who
who1 <- who %>% 
  gather(new_sp_m014:newrel_f65, key = "key", value = "cases", na.rm = TRUE)
who1
who1 %>% 
  count(key)

#replacing newrel by new_rel , str_replace
who2 <- who1 %>% 
  mutate(key = stringr::str_replace(key, "newrel", "new_rel"))
who2

#adding separate
who3 <- who2 %>% 
  separate(key, c("new", "type", "sexage"), sep = "_")
who3
who3 %>% 
  count(new)

who4 <- who3 %>% 
  select(-new, -iso2, -iso3)

#separating sexage into sex and age by splitting after the first character
who5 <- who4 %>% 
  separate(sexage, c("sex", "age"), sep = 1)
who5

#After all the steps who dataset is now tidy
who %>%
  gather(key, value, new_sp_m014:newrel_f65, na.rm = TRUE) %>% 
  mutate(key = stringr::str_replace(key, "newrel", "new_rel")) %>%
  separate(key, c("new", "var", "sexage")) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)





























