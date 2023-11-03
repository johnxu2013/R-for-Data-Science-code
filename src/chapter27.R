library(tidyverse)
library(nycflights13)
df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)
df |> summarize(
  n = n(),
  a = median(a),
  b = median(b),
  c = median(c),
  d = median(d),
)
df |> summarize(
  n = n(),
  across(a:d, median),
)

df <- tibble(
  grp = sample(2, 10, replace = TRUE),
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

df |>
  group_by(grp) |>
  summarize(across(everything(), median))

rnorm_na <- function(n, n_na, mean = 0, sd = 1) {
  sample(c(rnorm(n - n_na, mean = mean, sd = sd), rep(NA, n_na)))
}

df_miss <- tibble(
  a = rnorm_na(5, 1),
  b = rnorm_na(5, 1),
  c = rnorm_na(5, 2),
  d = rnorm(5)
)
df_miss |>
  summarize(
    across(a:d, median),
    n = n()
  )

df_miss |>
  summarize(
    across(a:d, function(x) median(x, na.rm = TRUE)),
    n = n()
  )

df_miss |>
  summarize(
    across(a:d, \(x) median(x, na.rm = TRUE)),
    n = n()
  )

df_miss |>
  summarize(
    a = median(a, na.rm = TRUE),
    b = median(b, na.rm = TRUE),
    c = median(c, na.rm = TRUE),
    d = median(d, na.rm = TRUE),
    n = n()
  )

df_miss |>
  summarize(
    across(a:d, list(
      median = \(x) median(x, na.rm = TRUE),
      n_miss = \(x) sum(is.na(x))
    )),
    n = n()
  )

df_miss |>
  summarize(
    across(
      a:d,
      list(
        median = \(x) median(x, na.rm = TRUE),
        n_miss = \(x) sum(is.na(x))
      ),
      .names = "{.fn}_{.col}"
    ),
    n = n(),
  )

df_miss |>
  mutate(
    across(a:d, \(x) coalesce(x, 0))
  )

df_miss |>
  mutate(
    across(a:d, \(x) abs(x), .names = "{.col}_abs")
  )

# same as df_miss |> filter(is.na(a) | is.na(b) | is.na(c) | is.na(d))
df_miss |> filter(if_any(a:d, is.na))

# same as df_miss |> filter(is.na(a) & is.na(b) & is.na(c) & is.na(d))
df_miss |> filter(if_all(a:d, is.na))

expand_dates <- function(df) {
  df |>
    mutate(
      across(where(is.Date), list(year = year, month = month, day = mday))
    )
}

df_date <- tibble(
  name = c("Amy", "Bob"),
  date = ymd(c("2009-08-03", "2010-01-16"))
)

df_date |>
  expand_dates()

summarize_means <- function(df, summary_vars = where(is.numeric)) {
  df |>
    summarize(
      across({{ summary_vars }}, \(x) mean(x, na.rm = TRUE)),
      n = n()
    )
}
diamonds |>
  group_by(cut) |>
  summarize_means()

diamonds |>
  group_by(cut) |>
  summarize_means(c(carat, x:z))

df |>
  summarize(across(a:d, list(median = median, mean = mean)))

long <- df |>
  pivot_longer(a:d) |>
  group_by(name) |>
  summarize(
    median = median(value),
    mean = mean(value)
  )
long

long |>
  pivot_wider(
    names_from = name,
    values_from = c(median, mean),
    names_vary = "slowest",
    names_glue = "{name}_{.value}"
  )

df_paired <- tibble(
  a_val = rnorm(10),
  a_wts = runif(10),
  b_val = rnorm(10),
  b_wts = runif(10),
  c_val = rnorm(10),
  c_wts = runif(10),
  d_val = rnorm(10),
  d_wts = runif(10)
)

df_long <- df_paired |>
  pivot_longer(
    everything(),
    names_to = c("group", ".value"),
    names_sep = "_"
  )
df_long


df_long |>
  group_by(group) |>
  summarize(mean = weighted.mean(val, wts))

show_missing <- function(df, group_vars, summary_vars = everything()) {
  df |>
    group_by(pick({{ group_vars }})) |>
    summarize(
      across({{ summary_vars }}, \(x) sum(is.na(x))),
      .groups = "drop"
    ) |>
    select(where(\(x) any(x > 0)))
}
flights |> show_missing(c(year, month, day))

paths <- list.files("data/gapminder", pattern = "[.]xlsx$", full.names = TRUE)
paths

files <- list(
  readxl::read_excel("data/gapminder/1952.xlsx"),
  readxl::read_excel("data/gapminder/1957.xlsx"),
  readxl::read_excel("data/gapminder/1962.xlsx"),
  readxl::read_excel("data/gapminder/2007.xlsx")
)

files[[3]]

files <- map(paths, readxl::read_excel)
length(files)

files[[1]]

list_rbind(files)

paths |>
  map(readxl::read_excel) |>
  list_rbind()

paths |>
  map(\(path) readxl::read_excel(path, n_max = 1)) |>
  list_rbind()

paths |> set_names(basename)

files <- paths |>
  set_names(basename) |>
  map(readxl::read_excel)
files[["1962.xlsx"]]

paths |>
  set_names(basename) |>
  map(readxl::read_excel) |>
  list_rbind(names_to = "year") |>
  mutate(year = parse_number(year))

paths |>
  set_names() |>
  map(readxl::read_excel) |>
  list_rbind(names_to = "year") |>
  separate_wider_delim(year, delim = "/", names = c(NA, "dir", "file")) |>
  separate_wider_delim(file, delim = ".", names = c("file", "ext"))

gapminder <- paths |>
  set_names(basename) |>
  map(readxl::read_excel) |>
  list_rbind(names_to = "year") |>
  mutate(year = parse_number(year))

write_csv(gapminder, "gapminder.csv")

process_file <- function(path) {
  df <- read_csv(path)

  df |>
    filter(!is.na(id)) |>
    mutate(id = tolower(id)) |>
    pivot_longer(jan:dec, names_to = "month")
}

paths |>
  map(process_file) |>
  list_rbind()

paths |>
  map(read_csv) |>
  map(\(df) df |> filter(!is.na(id))) |>
  map(\(df) df |> mutate(id = tolower(id))) |>
  map(\(df) df |> pivot_longer(jan:dec, names_to = "month")) |>
  list_rbind()


paths |>
  map(read_csv) |>
  list_rbind() |>
  filter(!is.na(id)) |>
  mutate(id = tolower(id)) |>
  pivot_longer(jan:dec, names_to = "month")

files <- paths |>
  map(readxl::read_excel)

df_types <- function(df) {
  tibble(
    col_name = names(df),
    col_type = map_chr(df, vctrs::vec_ptype_full),
    n_miss = map_int(df, \(x) sum(is.na(x)))
  )
}

df_types(gapminder)

files |>
  map(df_types) |>
  list_rbind(names_to = "file_name") |>
  select(-n_miss) |>
  pivot_wider(names_from = col_name, values_from = col_type)

files <- paths |>
  map(possibly(\(path) readxl::read_excel(path), NULL))

data <- files |> list_rbind()
data

failed <- map_vec(files, is.null)
paths[failed]

con <- DBI::dbConnect(duckdb::duckdb())
duckdb::duckdb_read_csv(con, "gapminder", paths)


template <- readxl::read_excel(paths[[1]])
template$year <- 1952
template

con <- DBI::dbConnect(duckdb::duckdb())
DBI::dbCreateTable(con, "gapminder", template)
con |> tbl("gapminder")

append_file <- function(path) {
  df <- readxl::read_excel(path)
  df$year <- parse_number(basename(path))

  DBI::dbAppendTable(con, "gapminder", df)
}
paths |> map(append_file)
paths |> walk(append_file)
paths

con |>
  tbl("gapminder") |>
  count(year)

by_clarity <- diamonds |>
  group_nest(clarity)

by_clarity
by_clarity$data[[1]]

by_clarity <- by_clarity |>
  mutate(path = str_glue("data/diamonds-{clarity}.csv"))

by_clarity
walk2(by_clarity$data, by_clarity$path, write_csv)

carat_histogram <- function(df) {
  ggplot(df, aes(x = carat)) + geom_histogram(binwidth = 0.1)
}

carat_histogram(by_clarity$data[[1]])


by_clarity <- by_clarity |>
  mutate(
    plot = map(data, carat_histogram),
    path = str_glue("data/by_clarify/clarity-{clarity}.png")
  )
walk2(
  by_clarity$path,
  by_clarity$plot,
  \(path, plot) ggsave(path, plot, width = 6, height = 6)
)
