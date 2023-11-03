library(tidyverse)
library(repurrrsive)
library(jsonlite)
x1 <- list(1:4, "a", TRUE)
x1

x2 <- list(a = 1:2, b = 1:3, c = 1:4)
x2

str(x1)

str(x2)
x3 <- list(list(1, 2), list(3, 4))
str(x3)

c(c(1, 2), c(3, 4))
#> [1] 1 2 3 4

x4 <- c(list(1, 2), list(3, 4))
str(x4)

x5 <- list(1, list(2, list(3, list(4, list(5)))))
str(x5)

df <- tibble(
  x = 1:2,
  y = c("a", "b"),
  z = list(list(1, 2), list(3, 4, 5))
)
df

data.frame(x = list(1:3, 3:5))
data.frame(
  x = I(list(1:2, 3:5)),
  y = c("1, 2", "3, 4, 5")
)

df1 <- tribble(
  ~x, ~y,
  1, list(a = 11, b = 12),
  2, list(a = 21, b = 22),
  3, list(a = 31, b = 32),
)


df2 <- tribble(
  ~x, ~y,
  1, list(11, 12, 13),
  2, list(21),
  3, list(31, 32),
)

df1 |>
  unnest_wider(y)

df1 |>
  unnest_wider(y, names_sep = "_")

df2 |>
  unnest_longer(y)
df6 <- tribble(
  ~x, ~y,
  "a", list(1, 2),
  "b", list(3),
  "c", list()
)
df6 |> unnest_longer(y)
df4 <- tribble(
  ~x, ~y,
  "a", list(1),
  "b", list("a", TRUE, 5)
)
df4 |>
  unnest_longer(y)

df4 <- tribble(
  ~x, ~y, ~z,
  "a", list("y-a-1", "y-a-2"), list("z-a-1", "z-a-2"),
  "b", list("y-b-1", "y-b-2", "y-b-3"), list("z-b-1", "z-b-2", "z-b-3")
)

repos <- tibble(json = gh_repos)
repos

repos |>
  unnest_longer(json)

repos |>
  unnest_longer(json) |>
  unnest_wider(json)

repos |>
  unnest_longer(json) |>
  unnest_wider(json) |>
  names() |>
  head(10)

repos |>
  unnest_longer(json) |>
  unnest_wider(json) |>
  select(id, full_name, owner, description)

repos |>
  unnest_longer(json) |>
  unnest_wider(json) |>
  select(id, full_name, owner, description) |>
  unnest_wider(owner)
repos |>
  unnest_longer(json) |>
  unnest_wider(json) |>
  select(id, full_name, owner, description) |>
  unnest_wider(owner, names_sep = "_")


chars <- tibble(json = got_chars)
chars
chars |>
  unnest_wider(json)
characters <- chars |>
  unnest_wider(json) |>
  select(id, name, gender, culture, born, died, alive)
characters
chars |>
  unnest_wider(json) |>
  select(id, where(is.list))
chars |>
  unnest_wider(json) |>
  select(id, titles) |>
  unnest_longer(titles)

titles <- chars |>
  unnest_wider(json) |>
  select(id, titles) |>
  unnest_longer(titles) |>
  filter(titles != "") |>
  rename(title = titles)
titles

gmaps_cities

gmaps_cities |>
  unnest_wider(json)
gmaps_cities |>
  unnest_wider(json) |>
  select(-status) |>
  unnest_longer(results)

locations <- gmaps_cities |>
  unnest_wider(json) |>
  select(-status) |>
  unnest_longer(results) |>
  unnest_wider(results)
locations
locations |>
  select(city, formatted_address, geometry) |>
  unnest_wider(geometry)

locations |>
  select(city, formatted_address, geometry) |>
  unnest_wider(geometry) |>
  unnest_wider(location)

locations |>
  select(city, formatted_address, geometry) |>
  unnest_wider(geometry) |>
  # focus on the variables of interest
  select(!location:viewport) |>
  unnest_wider(bounds)

locations |>
  select(city, formatted_address, geometry) |>
  unnest_wider(geometry) |>
  select(!location:viewport) |>
  unnest_wider(bounds) |>
  rename(ne = northeast, sw = southwest) |>
  unnest_wider(c(ne, sw), names_sep = "_")

locations |>
  select(city, formatted_address, geometry) |>
  hoist(
    geometry,
    ne_lat = c("bounds", "northeast", "lat"),
    sw_lat = c("bounds", "southwest", "lat"),
    ne_lng = c("bounds", "northeast", "lng"),
    sw_lng = c("bounds", "southwest", "lng"),
  )

tibble(json = got_chars) |>
  unnest_wider(json) |>
  select(id, where(is.list)) |>
  pivot_longer(
    where(is.list),
    names_to = "name",
    values_to = "value"
  ) |>
  unnest_longer(value)

# A path to a json file inside the package:
gh_users_json()

# Read it with read_json()
gh_users2 <- read_json(gh_users_json())

# Check it's the same as the data we were using previously
identical(gh_users, gh_users2)

str(parse_json('1'))
str(parse_json('[1, 2, 3]'))
str(parse_json('{"x": [1, 2, 3]}'))

json <- '[
  {"name": "John", "age": 34},
  {"name": "Susan", "age": 27}
]'
df <- tibble(json = parse_json(json))
df

df |>
  unnest_wider(json)

json <- '{
  "status": "OK",
  "results": [
    {"name": "John", "age": 34},
    {"name": "Susan", "age": 27}
 ]
}
'
df <- tibble(json = list(parse_json(json)))
df

df |>
  unnest_wider(json) |>
  unnest_longer(results) |>
  unnest_wider(results)

df <- tibble(results = parse_json(json)$results)
df |>
  unnest_wider(results)

json_col <- parse_json('
  {
    "x": ["a", "x", "z"],
    "y": [10, null, 3]
  }
')
json_row <- parse_json('
  [
    {"x": "a", "y": 10},
    {"x": "x", "y": null},
    {"x": "z", "y": 3}
  ]
')

df_col <- tibble(json = list(json_col))
df_row <- tibble(json = json_row)
