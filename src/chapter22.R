library(DBI)
library(dbplyr)
library(tidyverse)
library(RMariaDB)
con <- DBI::dbConnect(
  RMariaDB::MariaDB(),
  username = "foo"
)
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = "databases.mycompany.com",
  port = 1234
)

con <- DBI::dbConnect(duckdb::duckdb())

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "duckdb")
dbWriteTable(con, "mpg", ggplot2::mpg)
dbWriteTable(con, "diamonds", ggplot2::diamonds)

dbListTables(con)

con |>
  dbReadTable("diamonds") |>
  as_tibble()

sql <- "
  SELECT carat, cut, clarity, color, price
  FROM diamonds
  WHERE price > 15000
"
as_tibble(dbGetQuery(con, sql))

diamonds_db <- tbl(con, "diamonds")
diamonds_db

big_diamonds_db <- diamonds_db |>
  filter(price > 15000) |>
  select(carat:clarity, price)

big_diamonds_db

big_diamonds_db |>
  show_query()

big_diamonds <- big_diamonds_db |>
  collect()
big_diamonds

dbplyr::copy_nycflights13(con)
flights <- tbl(con, "flights")
planes <- tbl(con, "planes")

flights |> show_query()
planes |> show_query()

flights |>
  filter(dest == "IAH") |>
  arrange(dep_delay) |>
  show_query()
flights |>
  group_by(dest) |>
  summarize(dep_delay = mean(dep_delay, na.rm = TRUE)) |>
  show_query()

planes |>
  select(tailnum, type, manufacturer, model, year) |>
  show_query()
#> <SQL>
#> SELECT tailnum, "type", manufacturer, model, "year"
#> FROM planes

planes |>
  select(tailnum, type, manufacturer, model, year) |>
  rename(year_built = year) |>
  show_query()
#> <SQL>
#> SELECT tailnum, "type", manufacturer, model, "year" AS year_built
#> FROM planes

planes |>
  select(tailnum, type, manufacturer, model, year) |>
  relocate(manufacturer, model, .before = type) |>
  show_query()
planes

flights |>
  mutate(
    speed = distance / (air_time / 60)
  ) |>
  show_query()
flights |>
  mutate(year1 = year + 1) |>
  filter(year1 == 2014) |>
  show_query()
flights |>
  left_join(planes |> rename(year_built = year), by = "tailnum") |>
  show_query()

summarize_query <- function(df, ...) {
  df |>
    summarize(...) |>
    show_query()
}
mutate_query <- function(df, ...) {
  df |>
    mutate(..., .keep = "none") |>
    show_query()
}
flights |>
  group_by(year, month, day) |>
  summarize_query(
    mean = mean(arr_delay, na.rm = TRUE),
    median = median(arr_delay, na.rm = TRUE)
  )
flights |>
  group_by(year, month, day) |>
  mutate_query(
    mean = mean(arr_delay, na.rm = TRUE),
  )
flights |>
  group_by(dest) |>
  arrange(time_hour) |>
  mutate_query(
    lead = lead(arr_delay),
    lag = lag(arr_delay)
  )
flights |>
  mutate_query(
    description = if_else(arr_delay > 0, "delayed", "on-time")
  )
#> <SQL>
#> SELECT CASE WHEN (arr_delay > 0.0) THEN 'delayed' WHEN NOT (arr_delay > 0.0) THEN 'on-time' END AS description
#> FROM flights
flights |>
  mutate_query(
    description =
      case_when(
        arr_delay < -5 ~ "early",
        arr_delay < 5 ~ "on-time",
        arr_delay >= 5 ~ "late"
      )
  )
flights |>
  mutate_query(
    description =  cut(
      arr_delay,
      breaks = c(-Inf, -5, 5, Inf),
      labels = c("early", "on-time", "late")
    )
  )
