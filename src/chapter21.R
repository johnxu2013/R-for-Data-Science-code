library(readxl)
library(tidyverse)
library(writexl)
students <- read_excel("data/students.xlsx")
students
read_excel(
  "data/students.xlsx",
  col_names = c("student_id", "full_name", "favourite_food", "meal_plan", "age")
)
read_excel(
  "data/students.xlsx",
  col_names = c("student_id", "full_name", "favourite_food", "meal_plan", "age"),
  skip = 1
)
read_excel(
  "data/students.xlsx",
  col_names = c("student_id", "full_name", "favourite_food", "meal_plan", "age"),
  skip = 1,
  na = c("", "N/A")
)
read_excel(
  "data/students.xlsx",
  col_names = c("student_id", "full_name", "favourite_food", "meal_plan", "age"),
  skip = 1,
  na = c("", "N/A"),
  col_types = c("numeric", "text", "text", "text", "numeric")
)
students <- read_excel(
  "data/students.xlsx",
  col_names = c("student_id", "full_name", "favourite_food", "meal_plan", "age"),
  skip = 1,
  na = c("", "N/A"),
  col_types = c("numeric", "text", "text", "text", "text")
)

students <- students |>
  mutate(
    age = if_else(age == "five", "5", age),
    age = parse_number(age)
  )
students

#21.2.4
read_excel("data/penguins.xlsx", sheet = "Torgersen Island")
penguins_torgersen <- read_excel("data/penguins.xlsx", sheet = "Torgersen Island", na = "NA")

penguins_torgersen

excel_sheets("data/penguins.xlsx")

penguins_biscoe <- read_excel("data/penguins.xlsx", sheet = "Biscoe Island", na = "NA")
penguins_dream  <- read_excel("data/penguins.xlsx", sheet = "Dream Island", na = "NA")
dim(penguins_torgersen)
dim(penguins_biscoe)
dim(penguins_dream)

penguins <- bind_rows(penguins_torgersen, penguins_biscoe, penguins_dream)
penguins

deaths_path <- readxl_example("deaths.xlsx")
deaths <- read_excel(deaths_path)
deaths

read_excel(deaths_path, range = "A5:F15")

bake_sale <- tibble(
  item     = factor(c("brownie", "cupcake", "cookie")),
  quantity = c(10, 5, 8)
)

bake_sale

write_xlsx(bake_sale, path = "data/bake-sale.xlsx")

read_excel("data/bake-sale.xlsx")
read_excel("data/survey.xlsx")

roster <- read_excel("data/roster.xlsx")
roster

sales <- read_excel("data/sales.xlsx", skip = 3)
sales

library(googlesheets4)
library(tidyverse)

#google sheet needs auth
#students_sheet_id <- "1V1nPp1tzOuutXFLb3G9Eyxi3qxeEhnOXUzL5_BcCQ0w"
#students <- read_sheet(students_sheet_id)

#students





