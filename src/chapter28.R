library(tidyverse)
x <- c("one", "two", "three", "four", "five")
x[c(3, 2, 5)]

x <- c(10, 3, NA, 5, 8, 1, NA)

# All non-missing values of x
x[!is.na(x)]

# All even (or missing!) values of x
x[x %% 2 == 0]

x <- c(abc = 1, def = 2, xyz = 5)
x[c("xyz", "def")]

df <- tibble(
  x = 1:3,
  y = c("a", "e", "f"),
  z = runif(3)
)

# Select first row and second column
df[1, 2]

# Select all rows and columns x and y
df[, c("x" , "y")]

# Select rows where `x` is greater than 1 and all columns
df[df$x > 1, ]

df1 <- data.frame(x = 1:3)
df1[, "x"]
#> [1] 1 2 3

df2 <- tibble(x = 1:3)
df2[, "x"]

df1[, "x" , drop = FALSE]

df <- tibble(
  x = c(2, 3, 1, 1, NA),
  y = letters[1:5],
  z = runif(5)
)
df |> filter(x > 1)

# same as
df[!is.na(df$x) & df$x > 1, ]

df |> arrange(x, y)

# same as
df[order(df$x, df$y), ]

df |> select(x, z)

# same as
df[, c("x", "z")]

df |>
  filter(x > 1) |>
  select(y, z)

# same as
df |> subset(x > 1, c(y, z))

tb <- tibble(
  x = 1:4,
  y = c(10, 4, 1, 21)
)

# by position
tb[[1]]
#> [1] 1 2 3 4

# by name
tb[["x"]]
tb$x

tb$z <- tb$x + tb$y
tb

max(diamonds$carat)

levels(diamonds$cut)

diamonds |> pull(carat) |> max()

diamonds |> pull(cut) |> levels()

df <- data.frame(x1 = 1)
df$x
df$z

tb <- tibble(x1 = 1)

tb$x
tb$z

l <- list(
  a = 1:3,
  b = "a string",
  c = pi,
  d = list(-1, -5)
)

str(l[1:2])
str(l[1])
str(l[4])
str(l[[1]])
str(l[[4]])
str(l$a)

df <- tibble(a = 1, b = 2, c = "a", d = "b", e = 4)

# First find numeric columns
num_cols <- sapply(df, is.numeric)
num_cols

# Then transform each column with lapply() then replace the original values
df[, num_cols] <- lapply(df[, num_cols, drop = FALSE], \(x) x * 2)
df

vapply(df, is.numeric, logical(1))

diamonds |>
  group_by(cut) |>
  summarize(price = mean(price))

tapply(diamonds$price, diamonds$cut, mean)

paths |> walk(append_file)

for (path in paths) {
  append_file(path)
}

paths <- dir("data/gapminder", pattern = "\\.xlsx$", full.names = TRUE)
files <- map(paths, readxl::read_excel)

files <- vector("list", length(paths))

seq_along(paths)

for (i in seq_along(paths)) {
  files[[i]] <- readxl::read_excel(paths[[i]])
}

do.call(rbind, files)

out <- NULL
for (path in paths) {
  out <- rbind(out, readxl::read_excel(path))
}

# Left
hist(diamonds$carat)

# Right
plot(diamonds$carat, diamonds$price)

