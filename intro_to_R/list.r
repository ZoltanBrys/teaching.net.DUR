# ABOUT ----
# Introduction to R: Lists
# Run line by line (Ctrl+Enter) and look at every output.


# WHAT IS A LIST? ----
# A list is a container that can hold ANYTHING:
# numbers, text, vectors, data frames, functions, even other lists.
# The elements can have different types and different lengths.
# (A data frame is a list too: a list of equal-length columns.)

students <- data.frame(
  name  = c("Anna", "Bela", "Cecil"),
  age   = c(20, 22, 21),
  grade = c(5, 3, 4)
)
nums   <- 1:10
fruits <- c("apple", "pear", "plum")

L <- list(table = students, numbers = nums, fruits = fruits)
L
str(L)               # STRUCTURE: the best way to look at a list
length(L)            # number of elements: 3
names(L)             # element names


# ADDRESSING ----
# By name or by position
L$fruits             # $ by name
L[["fruits"]]        # double brackets, by name
L[[3]]               # double brackets, by position

# [ ] versus [[ ]]  - the most important point about lists !!!!
# Think of a list as a train:
#   L[3]    gives you the WAGON (still a list, with the fruits inside)
#   L[[3]]  gives you the CONTENT of the wagon (the fruits themselves)
L[3]
L[[3]]
class(L[3])          # "list"
class(L[[3]])        # "character"

L[c(1, 3)]           # several elements with [ ]: a smaller list
# L[[c(1, 3)]]       # [[ ]] takes ONE element (or a path, see below)

# Going deeper: chain the addressing
L$fruits[2]                    # 2nd fruit
L$table$name                   # a column of the data frame in the list
L$table[1:2, "name"]           # rows 1-2 of column "name"
L[[1]][1:2, "name"]            # same, by position
L[[3]][2]                      # 2nd fruit, by position
L[[c(3, 2)]]                   # path: element 3, then its 2nd element


# NESTING: LISTS IN LISTS ----
L2 <- list(course = "R intro", content = L)
str(L2)
L2$content$fruits[2]           # "pear"
L2[[2]][[3]][2]                # same, by position: 2nd element, 3rd, 2nd item
L2$content$table$name[1]       # first name in the data frame, deep inside


# CHANGING ----
L$flag <- TRUE                       # add an element with $
L[["note"]] <- "hello"               # add an element with [[ ]]
L$fruits[2] <- "cherry"              # change one value inside an element
L$flag <- NULL                       # remove an element with NULL
L[["note"]] <- NULL
names(L)
L$fruits


# APPLYING A FUNCTION TO EVERY ELEMENT ----
scores <- list(anna = c(5, 4), bela = 3, cecil = c(4, 4, 5))   # different lengths

lapply(scores, mean)     # l-apply: returns a LIST
sapply(scores, mean)     # s-apply: simplifies to a named VECTOR
sapply(scores, length)   # same as lengths(scores)
unlist(scores)           # flatten everything into one vector

# the same with a loop (longer, but the same idea)
for (n in names(scores)) {
  cat(n, ":", mean(scores[[n]]), "\n")
}

# A data frame is a list of columns, so these work on it too:
sapply(students, class)  # the class of every column
lapply(students, mean)   # mean() fails for text: try it on students$age only


# LISTS AND DATA FRAMES ----
is.list(students)                          # TRUE
as.list(students)                          # data frame -> list of columns
data.frame(a = 1:2, b = c("x", "y"))       # equal-length vectors -> data frame

# a list of data frames can be stacked into one table
parts <- list(
  data.frame(id = 1, x = "a"),
  data.frame(id = 2, x = "b"),
  data.frame(id = 3, x = "c")
)
do.call(rbind, parts)


# PRACTICAL: A LIST OF DATA.TABLES AND A FUNCTION CALLING ALL ----
# Three classes, one table each: name, gender, final grade.
library(data.table)

classes <- list(
  class_A = data.table(name        = c("Anna", "Bela", "Cecil", "Dora"),
                       gender      = c("F", "M", "M", "F"),
                       final_grade = c(5, 3, 4, 5)),
  class_B = data.table(name        = c("Emil", "Fanni", "Gabor", "Hanna", "Imre"),
                       gender      = c("M", "F", "M", "F", "M"),
                       final_grade = c(2, 4, 3, 5, 4)),
  class_C = data.table(name        = c("Jakab", "Klara", "Lajos"),
                       gender      = c("M", "F", "M"),
                       final_grade = c(4, 4, NA))      # Lajos has no grade yet
)
str(classes)
classes$class_B                      # one table of the list

# 1. Define a function for ONE table: mean final grade by gender
mean_by_gender <- function(dt) {
  dt[, .(mean_grade = mean(final_grade, na.rm = TRUE)), by = gender]
}
mean_by_gender(classes$class_A)      # test it on one table first!

# 2. Call it for ALL tables with lapply: one result per class (a list again)
result <- lapply(classes, mean_by_gender)
result
result$class_C                       # the result for one class

# 3. Stack the results into one table (idcol: the list names become a column)
rbindlist(result, idcol = "class")


# EXERCISES ----
# 1. Create a list "me" with your name (text), your age (number) and
#    three hobbies (a vector). Look at it with str().
# 2. Get the second hobby in two different ways.
# 3. What is the difference between me[1] and me[[1]]? Check with class().
# 4. Add an element "student" = TRUE, then remove "age".
# 5. Put "me" and a second list "friend" into a list "people".
#    Get the first hobby of the friend.
# 6. Put the data frame "students" into a list, then get the first name of
#    the data frame inside it (hint: list$table$name[1]).
# 7. Create a list of three numeric vectors of different lengths and use
#    sapply() to calculate the max of each.
# 8. Extend the practical: write a function that returns the number of
#    students per gender, and call it on all classes with lapply().
