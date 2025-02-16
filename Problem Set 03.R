#### Exercise 1 ####

# Consider the partially completed scatterplot() function below:

library(ggplot2)

scatterplot <- function() {
  ggplot2::ggplot(
    data = data,
    mapping = aes(x = .data[[x]], y = .data[[y]])
  ) +
    geom_point()
}

# Complete the scatterplot() function such that the user is able to draw a scatter plot based on
# any two numeric variables from a given data frame, and can freely customize the appearance of the
# points using any of the aesthetics understood by geom_point().

# Note that the names of the x and y variables must be passed to the scatterplot() function as
# character vectors.

#### Instructor Solution ####

library(ggplot2)

scatterplot <- function(data, x, y, ...) {
  ggplot2::ggplot(
    data = data,
    mapping = aes(.data[[x]], .data[[y]])
  ) +
    geom_point(...)
}

## Example usage
scatterplot(data = mtcars, x = "hp", y = "mpg", color = "red", size = 4)
scatterplot(data = mtcars, x = "hp", y = "mpg", color = "red", size = 10, stroke = 4, pch = 9)

## Exercise 2

# The Open Notify API allows you to retrieve real-time data about the astronauts currently in space.
# Import this data into your environment using the following code:

library(jsonlite)
current_space <- jsonlite::read_json("http://api.open-notify.org/astros.json")

# Write a function named which_craft(), given the list returned from the Open Notify API and a
# scalar^[A scalar vector is a vector with one element] character vector as input, searches the list
# for an astronaut whose name matches the character vector input. If a match is found in the list,
# your function should return a scalar character vector holding the name of the spacecraft this
# astronaut is aboard.  If a match is *not* found in the list, your function should NA.

# Your function may assume that the user will input the astronaut's name as a scalar character
# vector, and your function may assume the list returned from the Open Notify API will have the
# following structure:

# List of 3
#  $ people : List of *N*
#   ..$ :List of 2
#   .. ..$ craft: chr
#   .. ..$ name : chr
#  $ number : int *N*
#  $ message: chr "success"

# However,the accuracy of your function should not depend on there being exactly 12 astronauts in
# space.

#### Instructor Solution ####

find_craft <- function(space_list, astronaut) {

  if (typeof(astronaut) != "character" || length(astronaut) != 1L) {
    stop("The `astronaut` argument should be a scalar character vector")
  }

  craft <- NA

  for (person in space_list$people) {

    if (person$name == astronaut) {
      craft <- person$craft
    }
  }

  return(craft)
}

# Example usage
find_craft(space_list = current_space, astronaut = "Sunita Williams")
find_craft(space_list = current_space, astronaut = "Will Hopper")

#### Exercise 3 ####

# R's sum() function takes in a vector of numeric values, adds together all the
# numbers in that vector, and returns this grand sum to the caller.

# Write a complement to the sum() function named subtract(). The subtract() function should
# takes in a vector of numeric values, subtract all the values, and report this grand difference to
# the caller. For example, if given the vector c(2, 10, -2, 5), your function should return -11
# (i.e., the result of 2 - 10 - -2 - 5).

# Make sure that, like the sum() function, your subtract() function allows the user to remove
# missing values from the vector before computing.

#### Instructor Solution

subtract <- function(x, na_rm = TRUE) {

  if (na_rm) {
    x <- x[!is.na(x)]
  }

  y <- x[1]
  for (v in x[-1]){
    y <- y - v
  }
  return(y)
}

subtract(c(2, 10, -2, 5, NA), na.rm = TRUE)
