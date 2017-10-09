library(dplyr)
library(nycflights13)
dim(flights)
flights
class(flights)


#filter() to select cases based on their values.
#arrange() to reorder the cases.
#select() and rename() to select variables based on their names.
#mutate() and transmute() to add new variables that are functions of existing variables.
#summarise() to condense multiple values to a single value.
#sample_n() and sample_frac() to take random samples.


filter(flights, month == 1, day == 1)

#Arrange rows with arrange()
#Use desc() to order a column in descending order
arrange(flights, desc(arr_delay))


# Select columns by name
select(flights, year, month, day)
# Select all columns between year and day (inclusive)
select(flights, year:day)
# Select all columns except those from year to day (inclusive)
select(flights, -(year:day))


#Rename variables with select() by using named arguments:
select(flights, tail_num = tailnum)


#because select() drops all the variables not explicitly mentioned, it’s not that useful. Instead, use rename():
rename(flights, tail_num = tailnum)

#Add new columns with mutate()
# Besides selecting sets of existing columns, it’s often useful to add new columns that are 
# functions of existing columns. This is the job of mutate():

mutate(flights,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60
)

#If you only want to keep the new variables, use transmute():
mutate(flights,
       gain = arr_delay - dep_delay,
       gain_per_hour = gain / (air_time / 60)
)


#If you only want to keep the new variables, use transmute():
transmute(flights,
          gain = arr_delay - dep_delay,
          gain_per_hour = gain / (air_time / 60)
)


# Summarise values with summarise()
# collapses a data frame to a single row.

summarise(flights,
          delay = mean(dep_delay, na.rm = TRUE)
)


# Randomly sample rows with sample_n() fixed number and sample_frac() for fixed fraction
sample_n(flights, 10) 
sample_frac(flights, 0.01)
#Use replace = TRUE to perform a bootstrap sample




############### Checking if Flight distance is coorelated with Delay

by_tailnum <- group_by(flights, tailnum)
#arrange(by_tailnum, desc(tailnum))
delay <- summarise(by_tailnum,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))

delay <- filter(delay, count > 20, dist < 2000)


# Interestingly, the average delay is only slightly related to the
# average distance flown by a plane.
library(ggplot2)
ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()




############### Checking if Flight destination is coorelated with Delay

by_dest <- group_by(flights, dest)
#arrange(by_tailnum, desc(tailnum))
delay <- summarise(by_dest,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))

delay <- filter(delay, count > 20, dist < 2000)


# Interestingly, the average delay is only slightly related to the
# dest of flight.
library(ggplot2)
ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()




############### Checking if 


destinations <- group_by(flights, dest)
summarise(destinations,
          planes = n_distinct(tailnum),
          flights = n()
)

daily <- group_by(flights, year, month, day)
per_day   <- summarise(daily, flights = n())
per_month <- summarise(per_day, flights = sum(flights))
per_year  <- summarise(per_month, flights = sum(flights))



























