require(dplyr)
require(ggplot2)
require(nycflights13)
geta<- mutate(flights,
                dep_delay_min_rank = min_rank(desc(dep_delay)),
                dep_delay_row_number = row_number(desc(dep_delay)),
                dep_delay_dense_rank = dense_rank(desc(dep_delay))
)
geta<-filter(geta,
              !(dep_delay_min_rank > 10 | dep_delay_row_number > 10 |
                  dep_delay_dense_rank > 10)
)
crush <- arrange(geta, dep_delay_min_rank)
print(select(
  geta, month, day, carrier, flight, dep_delay,
  dep_delay_min_rank, dep_delay_row_number, dep_delay_dense_rank
),
n = Inf
)
1:3 + 1:10 
x <- seq(-3, 7, by = 1 / 2)
sin(pi * x)
summarise(flights, delay = mean(dep_delay, na.rm = TRUE))
by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))
by_dest <- group_by(flights, dest)
del<-summarise(des,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   del = mean(arr_delay, na.rm = TRUE)
)
del <- filter(del,count > 20, dest != "HNL")
ggplot(data = del, mapping = aes(x = dist, y = del)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)
delays<- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)
  ) %>% 
  filter(count > 20, dest != "HNL")
not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay))
delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay)
  )

ggplot(data = delays, mapping = aes(x = delay)) + 
  geom_freqpoly(binwidth = 10)
delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = FALSE),
    n = n()
  )

ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0]) # the average positive delay
  )
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(distance_sd = sd(distance)) %>% 
  arrange(desc(distance_sd))
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(carriers = n_distinct(carrier)) %>% 
  arrange(desc(carriers))
daily <- group_by(flights, year, month, day)
(per_day   <- summarise(daily, flights = n()))
not_cancelled<- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))
not_cancelled %>%
  count(dest)
not_cancelled %>%
  group_by(dest) %>%
  summarise(n = length(dest))
#print(not_cancelled)
filter(flights, !is.na(dep_delay), is.na(arr_delay)) %>%
  select(dep_time, arr_time, sched_arr_time, dep_delay, arr_delay)
cancelled_per_day <-
  flights %>%
  mutate(cancelled = (is.na(arr_delay) | is.na(dep_delay))) %>%
  group_by(year, month, day) %>%
summarise(
    cancelled_num = sum(cancelled),
    flights_num = n(),
cancelled_and_delays <-
     flights %>%
    mutate(cancelled = (is.na(arr_delay) | is.na(dep_delay))) %>%
      group_by(year, month, day) %>%
summarise(
        cancelled_prop = mean(cancelled),
        avg_dep_delay = mean(dep_delay, na.rm = TRUE),
        avg_arr_delay = mean(arr_delay, na.rm = TRUE)
      ) %>%
ungroup()
ggplot(cancelled_and_delays) +
  geom_point(aes(x = avg_dep_delay, y = cancelled_prop))
flights %>%
  group_by(carrier) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(desc(arr_delay))
flights %>%
      filter(!is.na(arr_delay)) %>%
      # Total delay by carrier within each origin, dest
      group_by(origin, dest, carrier) %>%
      summarise(
        arr_delay = sum(arr_delay),
        flights = n()
      ) %>%
      # Total delay within each origin dest
group_by(origin, dest) %>%
      mutate(
        arr_delay_total = sum(arr_delay),
        flights_total = sum(flights)
      ) %>%
      # average delay of each carrier - average delay of other carriers
ungroup() %>%
    mutate(
        arr_delay_others = (arr_delay_total - arr_delay) /
          (flights_total - flights),
        arr_delay_mean = arr_delay / flights,
        arr_delay_diff = arr_delay_mean - arr_delay_others
      ) %>%
      # remove NaN values (when there is only one carrier)
filter(is.finite(arr_delay_diff)) %>%
      # average over all airports it flies to
  group_by(carrier) %>%
  summarise(arr_delay_diff = mean(arr_delay_diff)) %>%
  arrange(desc(arr_delay_diff))
sml %>% 
      group_by(year, month, day) %>%
      filter(rank(desc(arr_delay)) < 10)
popular_dests <- flights %>% 
    group_by(dest) %>% 
  filter(n() > 365)
    popular_dests 
    #upto to exercise 5.6.7
flights %>%
      filter(!is.na(tailnum)) %>%
      mutate(on_time = !is.na(arr_time) & (arr_delay <= 0)) %>%
      group_by(tailnum) %>%
      summarise(on_time = mean(on_time), n = n()) %>%
      filter(min_rank(on_time) == 1)
flights %>%
  filter(!is.na(tailnum)) %>%
  mutate(on_time = !is.na(arr_time) & (arr_delay <= 0)) %>%
  group_by(tailnum) %>%
  summarise(on_time = mean(on_time), n = n()) %>%
  filter(n >= 10) %>%
  filter((on_time) == 1)
flights %>%
  group_by(hour) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(arr_delay)
flights %>%
  filter(arr_delay > 0) %>%
  group_by(dest) %>%
  mutate(
    arr_delay_total = sum(arr_delay),
    arr_delay_prop = arr_delay / arr_delay_total
  ) %>%
  select(
    dest, month, day, dep_time, carrier, flight,
    arr_delay, arr_delay_prop
  ) %>%
  arrange(dest, desc(arr_delay_prop))
lagged_delays <- flights %>%
  arrange(origin, month, day, dep_time)%>%
  group_by(origin) %>%
  mutate(dep_delay_lag = lag(dep_delay)) %>%
  filter(!is.na(dep_delay), !is.na(dep_delay_lag))
lagged_delays %>%
  group_by(dep_delay_lag) %>%
  summarise(dep_delay_mean = mean(dep_delay)) %>%
  ggplot(aes(y = dep_delay_mean, x = dep_delay_lag)) +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 1500, by = 120)) +
  labs(y = "Departure Delay", x = "Previous Departure Delay")
standardized_flights <- flights %>%
  filter(!is.na(air_time)) %>%
  group_by(dest, origin) %>%
  mutate(
    air_time_mean = mean(air_time),
    air_time_sd = sd(air_time),
    n = n()
  ) %>%
  ungroup() %>%
  mutate(air_time_standard = (air_time - air_time_mean) / (air_time_sd + 1))


print(standardized_flights)
ggplot(standardized_flights, aes(x = air_time_standard)) +
  geom_density()
flights %>%
  # find all airports with > 1 carrier
  group_by(dest) %>%
  mutate(n_carriers = n_distinct(carrier)) %>%
  filter(n_carriers > 1) %>%
  # rank carriers by numer of destinations
  group_by(carrier) %>%
  summarize(n_dest = n_distinct(dest)) %>%
  arrange(desc(n_dest))
flights %>%
  # sort in increasing order
  select(tailnum, year, month, day, dep_delay) %>%
  filter(!is.na(dep_delay)) %>%
  arrange(tailnum, year, month, day) %>%
  group_by(tailnum) %>%
  # cumulative number of flights delayed over one hour
  mutate(cumulative_hr_delays = cumsum(dep_delay > 60)) %>%
  # count the number of flights == 0
  summarise(total_flights = sum(cumulative_hr_delays < 1)) %>%
  arrange(total_flights)
#upto exrcise 5.7.8


    