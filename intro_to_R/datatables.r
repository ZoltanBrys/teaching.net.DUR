# ABOUT ----
# Introduction to R: data.table
# Run line by line (Ctrl+Enter) and look at every output.


# WHAT IS IT AND WHY? ----
# data.table is an add-on package: an enhanced data frame.
# when to use it: when you have >100 000 of rows!
# based on RAM, it is good up to 10 millions, > that, use database
# It IS a data frame (same idea: rows = cases, columns = variables), but:
#
#   1. SPEED     - very fast on big data (millions of rows): grouping, joining,
#                  sorting, reading files (fread).
#   2. SHORT     - one compact syntax:   DT[ i , j , by ]
#                  i = which rows,  j = what to do with columns,  by = in which groups
#   3. EASY      - inside [ ] columns are used by bare name: no  df$  needed.
#   4. NO COPIES - changes are made "by reference" (in place): saves memory and time.
#   5. FLEXIBLE  - a cell may hold more than a single value: a vector, a list,
#                  even a whole table ("list column").
#
#  the syntax is different from base R! Always pay att if data.frame or table!

if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")
library(data.table)


# CREATING ----
dt <- data.table(
  name  = c("Anna", "Bela", "Cecil", "Dora", "Emil", "Fanni"),
  age   = c(20, 22, 21, 23, 20, 22),
  sex   = c("F", "M", "M", "F", "M", "F"),
  grade = c(5, 3, 4, 5, NA, 4)
)
dt                   # no row numbers as names; text stays text (no stringsAsFactors)
str(dt)              # class: "data.table" "data.frame"  -> it is still a data frame
class(dt)

# Converting
df <- data.frame(a = 1:3, b = c("x", "y", "z"))
as.data.table(df)    # makes a converted copy
setDT(df)            # converts df IN PLACE (by reference), no copy
class(df)


# ADDRESSING: DT[ i , j , by ] ----
# i: rows - write the condition directly, no dt$ needed
dt[2]                          # row 2
dt[1:3]                        # rows 1 to 3
dt[age > 21]                   # base R:  df[df$age > 21, ]
dt[sex == "F" & age > 20]
dt[name %in% c("Anna", "Dora")]

# j: columns - and calculations
dt[, age]                      # one column -> a VECTOR (like df$age)
dt[, .(age)]                   # .() = list(): one column -> a data.table
dt[, .(name, grade)]           # several columns by bare name
dt[, mean(age)]                # calculate right inside
dt[, .(mean_age = mean(age), max_age = max(age))]   # several results, named

# i and j together
dt[sex == "F", .(name, grade)]

# columns whose names are stored in a variable
cols <- c("name", "age")
dt[, ..cols]                   # ..  means: "look for it outside the table"
dt[, cols, with = FALSE]       # older way, same result

# by: do it for every group (this is where data.table shines)
dt[, .(mean_age = mean(age)), by = sex]              # base R: tapply / aggregate
dt[, .(mean_age = mean(age), n = .N), by = sex]      # .N = number of rows
dt[!is.na(grade), .(mean_grade = mean(grade)), by = sex]
dt[age > 20, .N, by = .(sex, grade)]                 # more than one group variable

# Special symbols: .N (row count),  .() (list),  .SD (subset of data)


# CHANGING: := (by reference) ----
dt[, passed := grade >= 3]                 # add a column
dt[, age_months := age * 12]               # add another
dt[sex == "F", grade := grade + 0]         # change only some rows
dt[, c("passed", "age_months") := NULL]    # remove columns
dt[, `:=`(passed = grade >= 3, adult = age >= 18)]   # add several at once
dt

# WARNING: by reference means "no copy". Both names point to the SAME table!
a <- dt
a[, extra := 1]
names(dt)                                  # dt got the column "extra" too!
b <- copy(dt)                              # copy() makes a real, independent copy
dt[, extra := NULL]


# SORTING ----
dt[order(age)]                 # ascending (temporary view)
dt[order(-age, name)]          # descending age, then name
setorder(dt, age)              # sorts the table itself, by reference


# JOINING (merging) TWO TABLES ----
labels <- data.table(sex = c("F", "M"), sex_label = c("Female", "Male"))
labels[dt, on = "sex"]         # for every row of dt, look up the label via "sex"


# FLEXIBLE CELLS: LIST COLUMNS ----
# A cell can hold a vector, a list, ... (in a plain data.frame this is awkward).
scores <- data.table(
  id     = 1:3,
  scores = list(c(5, 4), 3, c(4, 4, 5))   # each cell has a different length!
)
scores
scores$scores[[1]]                         # the content of the first cell
scores[, mean_score := sapply(scores, mean)]
scores[, n_scores := lengths(scores)]
scores


# SPEED ----
# Try it: the same job (mean by group) on 2 million rows.
set.seed(1)
big_df <- data.frame(g = sample(letters, 2e6, replace = TRUE), v = runif(2e6))
big_dt <- as.data.table(big_df)

system.time(aggregate(v ~ g, data = big_df, FUN = mean))   # base R
system.time(big_dt[, .(m = mean(v)), by = g])              # data.table

# Reading files is fast too (instead of read.csv / write.csv):
# dt <- fread("file.csv")
# fwrite(dt, "file.csv")


# SUMMARY: data.frame vs data.table ----
#                    data.frame                   data.table
# rows               df[df$age > 21, ]            dt[age > 21]
# columns            df[, c("name", "age")]       dt[, .(name, age)]
# new column         df$x <- df$age * 2           dt[, x := age * 2]
# mean by group      aggregate(age ~ sex, df, mean)   dt[, mean(age), by = sex]
# count rows         nrow(df)                     dt[, .N]
# sort               df[order(df$age), ]          setorder(dt, age)


# EXERCISES ----
# 1. Turn the built-in data set  mtcars  into a data.table
#    (hint: as.data.table(mtcars, keep.rownames = "car")).
# 2. Select the cars with 6 cylinders (cyl) and show only car and mpg.
# 3. Calculate the mean mpg and the number of cars for every cyl group.
# 4. Add a column "kpl" (km per litre) = mpg * 0.425.
# 5. Sort the table by mpg, descending.
