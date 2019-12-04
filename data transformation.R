require(dplyr)
require(ggplot2)
require(nycflights13)
#filter(flights, dest == "IAH"|dest=="HOU")
filter(flights,dest %in% c("IAH","HOU"))
filter(flights,carrier %in% c("AA","DL","UA"))
filter(flights,month %in% c(7:9))
filter(flights,arr_delay > 120,dep_delay<=0)
filter(flights,dep_delay >= 60,dep_delay - arr_delay>30)
filter(flights,dep_time <= 600|dep_time == 2400)
filter(flights,between(month,7,9))
filter(flights,is.na(dep_time))
NA ^ 0 == 1
NA|FALSE
arrange(flights,dep_time)%>%tail()
arrange(flights,desc(is.na(dep_time)),dep_time)
arrange(flights,desc(dep_delay))
arrange(flights,dep_delay)
arrange(flights,desc(distance))
arrange(flights,distance)
select(flights,dep_time,dep_delay,arr_time,arr_delay)
select(flights,one_of(c("dep_time","dep_delay","arr_time","arr_delay")))
select(flights,starts_with("dep_"),starts_with("arr_"))
select(flights,matches("^(dep|arr)_(time|delay)$"))
select(flights,arr_delay,everything())
select(flights,contains("TIME",ignore.case = TRUE))
dita<- mutate(flights,
       dep_time_mins=(dep_time%/%100*60 + dep_time%% 100)%%1440,
       sched_dep_time_mins = (sched_dep_time%/%100*60 + sched_dep_time%%100)%%1440)
select(dita,dep_time,dep_time_mins,sched_dep_time,sched_dep_time_mins)
#require(ggplot2)
dita<- mutate(flights,
              dep_time =(dep_time%/%100*60 + dep_time%% 100)%%1440,
              arr_time = (arr_time%/%100*60 + arr_time%%100)%%1440,
              air_time_diff = air_time - arr_time + dep_time)
transform(dita)
nrow(filter(dita,air_time_diff!=0))
ggplot(dita, aes(x = air_time_diff)) +
  geom_histogram(binwidth = 1)
fly<-mutate(flights,dep_time_min = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
         sched_dep_time_min = (sched_dep_time %/% 100 * 60 + sched_dep_time %% 100) %% 1440,
         dep_delay_diff = dep_delay - dep_time_min + sched_dep_time_min
  )
transform(fly)
nrow(filter(fly,dep_delay_diff!= 0))







