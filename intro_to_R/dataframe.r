#  ABOUT ----
# Introduction to R: Data Frames
# Run line by line (Ctrl+Enter) and look at every output.


#  DEFINITION ----
# A data frame is the standard format for datasets:
#   rows = cases (e.g. respondents),  columns = variables (e.g. age, sex).

students <- data.frame(
  name   = c("Anna", "Bela", "Cecil", "Dora", "Emil", "Fanni"),
  age    = c(20, 22, 21, 23, 20, 22),
  sex    = c("F", "M", "M", "F", "M", "F"),
  grade  = c(5, 3, 4, 5, NA, 4),         # Emil has no grade: NA
  stringsAsFactors = FALSE               # keep text as text (see below)
)
students

str(students)        # STRUCTURE: type of each column. Your best friend!
summary(students)    # quick statistics per column
head(students, 3)    # first 3 rows (default 6)
tail(students, 2)    # last 2 rows
dim(students)        # rows, columns
nrow(students)
ncol(students)
names(students)      # column names (same as colnames())
rownames(students)   # row names: "1" "2" "3" ... (they are text!)
# View(students)     # spreadsheet-like window (RStudio)

# A factor stores a categorical variable as labelled codes.
students$sex_f <- factor(students$sex, levels = c("F", "M"),
                         labels = c("Female", "Male"))
str(students)        # sex: chr   vs   sex_f: Factor
levels(students$sex_f)
table(students$sex_f)          # frequency table


# ADDRESSING ----
# General form:   df[ rows , columns ]
# Leave one part empty to mean "all".

# by position
students[2, 3]         # row 2, column 3 -> one value
students[2, ]          # whole row 2     -> a data frame
students[, 3]          # whole column 3  -> a VECTOR
students[1:3, ]        # rows 1 to 3
students[c(1, 4), ]    # rows 1 and 4
students[-1, ]         # everything EXCEPT row 1
students[1:3, 1:2]     # block: rows 1-3, columns 1-2

# by name
students[, "age"]              # column "age" -> vector
students[, c("name", "age")]   # two columns  -> data frame
students$age                   # $ = shortcut for one column -> vector
students[["age"]]              # same, double brackets
students["age"]                # single bracket, no comma -> data FRAME (!)

class(students$age)            # numeric   (a vector)
class(students["age"])         # data.frame

# columns are vectors!
students$age[2]
students$name[students$age > 21]

# by condition [WIDELY USED, important!]
students$age > 21              # TRUE/FALSE for each row
students[students$age > 21, ]  # keep rows where TRUE
students[students$sex == "F", c("name", "grade")]

# which(): gives the row NUMBERS where the condition is TRUE
which(students$age > 21)
students[which(students$age > 21), ]   # same result as above, safer with NA

# subset() and with()
subset(students, age > 21, select = c(name, age))  # no $ needed
with(students, mean(age))                          # use columns by name


# CHANGING ----
students[2, "grade"] <- 4              # change one value
students$passed <- students$grade >= 3 # new column from a calculation
students$country <- "HU"               # constant: recycled to every row
students$country <- NULL               # remove a column with NULL
students

# New rows: rbind()   and   new columns: cbind()
new_row <- data.frame(name = "Gabor", age = 24, sex = "M", grade = 3,
                      sex_f = "Male", passed = TRUE)
students <- rbind(students, new_row)
tail(students, 2)


# OPERATORS ----
# Operators work on whole columns at once (vectorized): no loop needed.

# Arithmetic:  +  -  *  /  ^  %%  %/%
students$age + 1               # every age plus 1
students$age * 12              # age in months
students$age^2
students$age %% 2              # remainder: 0 = even age
students$age %/% 10            # integer division: the "decade"

# Two vectors of the same length work element by element:
students$age - students$grade

# Different lengths: the shorter one is RECYCLED (repeated)
c(1, 2, 3, 4) + c(10, 20)      # 11 22 13 24
# (a warning appears if the longer length is not a multiple of the shorter)

# Relational:  <  >  <=  >=  ==  !=
students$age >= 22
students$sex == "F"            # == is comparing; = or <- is assigning!
students$name != "Anna"

# Logical:  &  |  !
#   &  AND   both must be TRUE
#   |  OR    at least one TRUE
#   !  NOT   flips TRUE <-> FALSE
students[students$sex == "F" & students$age > 20, ]
students[students$age == 20 | students$grade == 5, ]
students[!(students$sex == "F"), ]

# && and || compare only ONE value each (used inside if). For columns use & |.

# Membership:  %in%
students$name %in% c("Anna", "Dora")            # TRUE/FALSE per row
students[students$name %in% c("Anna", "Dora"), ]
students[!students$age %in% c(20, 21), ]

# Counting with logicals: TRUE counts as 1, FALSE as 0
sum(students$sex == "F")       # how many women?
mean(students$sex == "F")      # share of women
any(students$grade == 5)       # is there at least one 5?
all(students$age >= 18)        # is everybody an adult?


# MISSING VALUES (NA) ----
is.na(students$grade)
sum(is.na(students$grade))                  # how many NAs?
which(is.na(students$grade))                # which rows?
students[is.na(students$grade), ]           # who has no grade?
mean(students$grade)                        # NA!
mean(students$grade, na.rm = TRUE)          # ignore NAs
students[!is.na(students$grade), ]          # drop rows with NA in "grade"
na.omit(students)                           # drop rows with NA anywhere


# SUMMARIES, SORTING, GROUPS ----
mean(students$age)
sd(students$age)
min(students$age); max(students$age)
table(students$sex)
table(students$sex, students$passed, useNA = "ifany")   # cross-table

order(students$age)                          # row order that sorts age
students[order(students$age), ]              # sort ascending
students[order(-students$age), ]             # sort descending

tapply(students$age, students$sex, mean)     # mean age by sex
aggregate(age ~ sex, data = students, FUN = mean)


# EXERCISES ----
# 1. Create a data frame "cars2" with 5 rows: brand (text), year (number),
#    price (number). Look at it with str().
# 2. Select the 2nd row, then the "price" column, then the value in row 3 / "year".
# 3. Show only the cars cheaper than the average price.
# 4. Add a column "age" = 2026 - year.
# 5. How many cars are older than 5 years? (hint: sum of a logical)
# 6. Use the built-in data set  mtcars  (type ?mtcars):
#    a) how many cars have 6 cylinders?       b) mean mpg of cars with 4 cylinders?
#    c) which car has the highest hp? (hint: rownames, which.max)
