# ABOUT ----
# Introduction to R: Quick recap
# Run line by line (Ctrl+Enter) and look at every output.


# BASICS ----
# <-  assigns a value to a name.   #  starts a comment.   ?name  opens help.
x <- 5
x
(y <- 10)        # parentheses around an assignment: assign AND print
?mean


# DATA TYPES ----
x <- TRUE        # 1. logical    TRUE / FALSE
x <- 42L         # 2. integer    the L makes it an integer
x <- 42.5        # 3. numeric    (double) decimal numbers
x <- "Hello"     # 4. character  text, always in quotes
x <- 2 + 3i      # 5. complex    rarely used
x <- charToRaw("A")  # 6. raw    bytes, rarely used

# 3 ways to ask "what is it?"
x <- 42L
typeof(x)        # internal storage type  -> "integer"
class(x)         # what R treats it as    -> "integer"
mode(x)          # broad group            -> "numeric"

# typeof vs class differ sometimes:
typeof(mean)     # "closure"
class(mean)      # "function"
typeof(1:3)      # "integer"
typeof(c(1, 2))  # "double"   <- plain numbers are doubles, not integers!

# Converting: as.<type>()
as.numeric("3.14")
as.integer(3.9)      # cuts the decimals (does not round, floor or ceiling)
as.character(100)
as.logical(c(0, 1, 2))   # 0 is FALSE, everything else TRUE


# SPECIAL VALUES ----
NA       # missing value ("Not Available") - very common in survey data!
NaN      # Not a Number            e.g. 0/0
Inf      # infinity                e.g. 1/0
-Inf
NULL     # "nothing" / empty object

is.na(c(1, NA, 3))   # test for NA: never use  x == NA
mean(c(1, NA, 3))                # NA - one missing value spoils the result
mean(c(1, NA, 3), na.rm = TRUE)  # na.rm = TRUE removes NAs first

# and factors !


# DATA STRUCTURES ----
# Vector: one dimension, ONE type only
v <- c(1, 2, 3, 4)
v[2]                    # 2nd element
c(1, "a", TRUE)         # mixed -> everything becomes character

# Matrix: two dimensions, ONE type only
m <- matrix(1:6, nrow = 2)
m
m[2, 3]                 # [row, column]

# List: a container for ANYTHING (different types and lengths)
l <- list(name = "Endre", age = 17, student = TRUE)
l$name                  # by name
l[["age"]]              # by name or position, double brackets

# Data frame: a TABLE. Columns can have different types,
# but every column has the same length (it is a list of equal-length vectors).
df <- data.frame(
  age = c(30, 40, 50),
  gender = c("M", "F", "M")
)
df

# Rule of thumb:
#            same type   mixed types
# 1-D        vector      list
# 2-D        matrix      data frame
# >2D        array, special: lists


# OBJECT SYSTEMS (advanced): S3 and S4 ----
# R has "object systems": rules for how an object gets its class and behaviour.
# Functions like print() and summary() behave differently depending on the class.
class(df)              # "data.frame"
is.list(df)            # TRUE: underneath, a data frame is a list of columns
unclass(df)            # remove the class label -> a plain list is printed
methods(summary)       # summary() is generic: one method per class

# S3
s <- list(name = "Anna", age = 20)
class(s) <- "student"                      # attach the label
print.student <- function(x, ...) cat("Student:", x$name, "\n")  # method for the class
s                                          # print() finds print.student automatically
s$age                                      # still an ordinary list underneath
inherits(s, "student")                     # TRUE
unclass(s)                                 # plain list again

# S4: highly advanced, it is strict and formal, has slots, access with @


# ---- HIDDEN VARS -----

print(ls(all.name = TRUE))
?.Random.seed

x <- 6

# this deletes, but keep the memory for the object
rm(x)
print(x)
# for memory:
tomb <- NULL
rm(tomb)

# delete all, codes often start with it
rm(list = ls())
print(ls())
