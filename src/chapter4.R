library(nycflights13)
library(tidyverse)
flights
glimpse(flights)

flights |>
  filter(dest == "IAH") |>
  group_by(year, month, day) |>
  summarize(
    arr_delay = mean(arr_delay, na.rm = TRUE)
  )

flights |>
  filter(dep_delay > 120)

flights |>
  filter(month == 1 & day == 1)
flights |>
  filter(month %in% c(1, 2))

jan1 <- flights |>
  filter(month == 1 & day == 1)
flights |>
  filter(month == 1)

flights |>
  arrange(year, month, day, dep_time)
flights |>
  arrange(desc(dep_delay))
flights |>
  distinct()

flights |>
  distinct(origin, dest)

flights |>
  distinct(origin, dest, .keep_all = TRUE)

flights |>
  count(origin, dest, sort = TRUE)

flights |>
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60
  )
flights |>
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .before = 1
  )
flights |>
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .after = day
  )
flights |>
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours,
    .keep = "used"
  )
flights |>
  select(year, month, day)
flights |>
  select(year:day)
flights |>
  select(!year:day)
flights |>
  select(where(is.character))
flights |>
  select(tail_num = tailnum)
flights |>
  relocate(time_hour, air_time)

flights |>
  relocate(year:dep_time, .after = time_hour)
flights |>
  relocate(starts_with("arr"), .before = dep_time)
glimpse(flights |>
          relocate(year:dep_time, .after = time_hour))

glimpse(flights |>
          relocate(starts_with("arr"), .before = dep_time))

variables <- c("year", "month", "day", "dep_delay", "arr_delay")
flights |> select(contains("TIME"))

flights |>
  select(tailnum, arr_delay) |>
  arrange(arr_delay)

flights |>
  filter(dest == "IAH") |>
  mutate(speed = distance / air_time * 60) |>
  select(year:day, dep_time, carrier, flight, speed) |>
  arrange(desc(speed))

arrange(
  select(
    mutate(
      filter(
        flights,
        dest == "IAH"
      ),
      speed = distance / air_time * 60
    ),
    year:day, dep_time, carrier, flight, speed
  ),
  desc(speed)
)

flights1 <- filter(flights, dest == "IAH")
flights2 <- mutate(flights1, speed = distance / air_time * 60)
flights3 <- select(flights2, year:day, dep_time, carrier, flight, speed)
arrange(flights3, desc(speed))

mtcars %>%
  group_by(cyl) %>%
  summarize(n = n())

flights |>
  group_by(month)
flights |>
  group_by(month) |>
  summarize(
    avg_delay = mean(dep_delay)
  )

flights |>
  group_by(month) |>
  summarize(
    delay = mean(dep_delay, na.rm = TRUE)
  )
flights |>
  group_by(month) |>
  summarize(
    delay = mean(dep_delay, na.rm = TRUE),
    n = n()
  )
flights |>
  group_by(dest) |>
  slice_max(arr_delay, n = 1) |>
  relocate(dest)
daily <- flights |>
  group_by(year, month, day)
daily
daily_flights <- daily |>
  summarize(n = n())

daily_flights <- daily |>
  summarize(
    n = n(),
    .groups = "drop_last"
  )
daily_flights

daily |>
  ungroup()
daily |>
  ungroup() |>
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE),
    flights = n()
  )
flights |>
  summarize(
    delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .by = month
  )
flights |>
  summarize(
    delay = mean(dep_delay, na.rm = TRUE),
    n = n(),
    .by = c(origin, dest)
  )

flights |> group_by(carrier, dest) |> summarize(n())


df <- tibble(
  x = 1:5,
  y = c("a", "b", "a", "a", "b"),
  z = c("K", "K", "L", "L", "K")
)
df |> group_by(y)

df |>
  arrange(y)
df |>
  group_by(y) |>
  summarize(mean_x = mean(x))
df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x))
df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x), .groups = "drop")
df |>
  group_by(y, z) |>
  summarize(mean_x = mean(x))

df |>
  group_by(y, z) |>
  mutate(mean_x = mean(x))

batters <- Lahman::Batting |>
  group_by(playerID) |>
  summarize(
    performance = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    n = sum(AB, na.rm = TRUE)
  )
batters
batters |>
  filter(n > 100) |>
  ggplot(aes(x = n, y = performance)) +
  geom_point(alpha = 1 / 10) +
  geom_smooth(se = FALSE)
batters |>
  arrange(desc(performance))
