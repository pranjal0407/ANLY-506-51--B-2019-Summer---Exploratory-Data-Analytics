#loading nycflights13 and tidyverse
library(nycflights13)
library(tidyverse)

#using dataset flights which contains all 336,776 flights that departed from New York City in 2013.
flights

#using filter() to create a subset of observations
filter(flights, month == 1, day == 1)

#saving filter result in jan1
jan1 <- filter(flights, month == 1, day == 1)
jan1

#To print and save result together
(dec25 <- filter(flights, month == 12, day == 25))

#For comparisons '==' should be used instead of '='
filter(flights, month = 1) #Error: `month` (`month = 1`) must not be named, do you need `==`?
filter(flights, month == 1)

#understanding problems that can occur with '==' and how to rectify them
sqrt(2) ^ 2 == 2
1 / 49 * 49 == 1

near(sqrt(2) ^ 2,  2)
near(1 / 49 * 49, 1)

#working with logical operators
filter(flights, month == 11 | month == 12)

#storing data in nov_dec and rewriting above code using %in%
nov_dec <- filter(flights, month %in% c(11, 12))

#to find flights that weren’t delayed (on arrival or departure) by more than two hours, either of the following can be used:
filter(flights, !(arr_delay > 120 | dep_delay > 120))
filter(flights, arr_delay <= 120, dep_delay <= 120)

#missing values
NA > 5
10 == NA
NA + 10
NA / 2
NA == NA

#Let x be Mary's age. We don't know how old she is.
x <- NA
#Let y be John's age. We don't know how old he is.
y <- NA
#Are John and Mary the same age?
x == y

#Checking if x has missing values
is.na(x)

#creating a data frame and since filter only includes rows where condition = TRUE adding is.na(x) to keep missing values
df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
filter(df, is.na(x) | x > 1)

#arrange() to change order of rows
arrange(flights, year, month, day)

#using desc to order a column in descending order
arrange(flights, desc(dep_delay))

#missing values are always sorted at the end
df <- tibble(x = c(5, 2, NA))
arrange(df, x)
arrange(df, desc(x))

#select() allows to see a subset based on the name of variables.
select(flights, year, month, day)

#select all columns between year and day (inclusive)
select(flights, year:day)

#select all columns except those from year to day (inclusive)
select(flights, -(year:day))

#rename variable names using rename()
rename(flights, tail_num = tailnum)

#use everything() with select to move variables to the start of data frame
select(flights, time_hour, air_time, everything())

#Add new variables using mutate()
flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time
)
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60
)


#new column created using mutate can be referred
mutate(flights_sml,
       gain = dep_delay - arr_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours
)


#transmute() to only keep the new variables
transmute(flights,
          gain = dep_delay - arr_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours
)

transmute(flights,
          dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100
)

#working with offsets
(x <- 1:10)
lag(x)
lead(x)


#cumulative and rolling aggregates
x
cumsum(x)
cummean(x)

#ranking in R
y <- c(1, 2, 2, NA, 3, 4)
min_rank(y)
min_rank(desc(y))

#varients of rank are row_number(), dense_rank(), percent_rank(), cume_dist(), ntile()
row_number(y)
dense_rank(y)
percent_rank(y)
cume_dist(y)

#summarise() to group the data and converts data frame to a single row
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

#summarise() by day by creating by_day
by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))

#combining multiple operations with the pipe
by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE)
)
delay <- filter(delay, count > 20, dest != "HNL")

# It looks like delays increase with distance up to ~750 miles and then decrease. 
#Maybe as flights get longer there's more ability to make up delays in the air?
ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)
#plot of delay vs dist is generated

#tackling the same problem with pipe
delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")

#missing values and na.rm argument
flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

flights %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay, na.rm = TRUE))

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))

#Counts
delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay)
  )

ggplot(data = delays, mapping = aes(x = delay)) + 
  geom_freqpoly(binwidth = 10)
# plot of delay vs count is generated

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  )

ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)
#scatterplot plot showing number of flights vs. average delay is generated

delays %>% 
  filter(n > 25) %>% 
  ggplot(mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)
# scatter plot of count vs delay is generated

# Convert to a tibble to print nicely
install.packages("Lahman")
batting <- as_tibble(Lahman::Batting)

batters <- batting %>% 
  group_by(playerID) %>% 
  summarise(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE)
  )

batters %>% 
  filter(ab > 100) %>% 
  ggplot(mapping = aes(x = ab, y = ba)) +
  geom_point() + 
  geom_smooth(se = FALSE)
#`geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

batters %>% 
  arrange(desc(ba))

#useful summary functions
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0]) # the average positive delay
  )


#why is distance to some destinations more variable than to others?
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(distance_sd = sd(distance)) %>% 
  arrange(desc(distance_sd))

#when do the first and last flights leave each day?
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  )

#measure of position
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first_dep = first(dep_time), 
    last_dep = last(dep_time)
  )

not_cancelled %>% 
  group_by(year, month, day) %>% 
  mutate(r = min_rank(desc(dep_time))) %>% 
  filter(r %in% range(r))

#which destinations have the most carriers?
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(carriers = n_distinct(carrier)) %>% 
  arrange(desc(carriers))

not_cancelled %>% 
  count(dest)


not_cancelled %>% 
  count(tailnum, wt = distance)

#how many flights left before 5am? (these usually indicate delayed flights from the previous day)
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(n_early = sum(dep_time < 500))

# What proportion of flights are delayed by more than an hour?
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(hour_perc = mean(arr_delay > 60))

#grouping by multiple variables
daily <- group_by(flights, year, month, day)
(per_day   <- summarise(daily, flights = n()))
(per_month <- summarise(per_day, flights = sum(flights)))
(per_year  <- summarise(per_month, flights = sum(flights)))

#ungrouping
daily %>% 
  ungroup() %>%             # no longer grouped by date
  summarise(flights = n())  # all flights

#grouped mutates (and filters)
flights_sml %>% 
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)

popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)
popular_dests


#standardise to compute per group metrics
popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)
