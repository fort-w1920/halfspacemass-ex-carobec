################################################################################
################################################################################
#---------------------- train depth -----------------------------------------
################################################################################
################################################################################

# Halfspace MASS based on Chen, B., Ting, K.M., Washio, T. et al. (2015)
# Half-space mass: a maximally robust and efficient data depth method,
# Machine Learning, 100(2):677–699 [pdf]
# Link: http://scheipl.userweb.mwn.de//downloads/fortprog/ChenEtAl-HalfspaceMass-MachLearn2015.pdf7

################################################################################
#---------------------- train depth -----------------------------------------
################################################################################

# Input:
# data: training data (zi, i = 1, ... ,n)
#--> D in the paper
# n_halfspace: count of drawn halfspaces
#--> t in the paper
# subsample: part of the data which is dran to calculate every halfspace
#--> Psi in the Paper is the number of data not the part!!
# scope
#--> lambda in paper
# seed for RNG

# task: : a random subsample Di is projected
# onto a random direction  in d , t times.
# For each projection, a split point s is randomly selected
# between a range adjusted by <U+03BB>;
# and then the number of points that fall in either sides of s are recorded.

# Output:
# halfspaces with direction, points and borders left and right
# (input for evaluate depth!)
train_depth <- function(data, 
                        n_halfspace, 
                        subsample = 1, 
                        scope = 1, 
                        seed = 1233) {
  # input check
  checkmate::assert(checkmate::test_data_frame(data), 
                    checkmate::test_matrix(data), 
                    combine = "or")
  data <- as.matrix(data)
  checkmate::assert_count(n_halfspace)
  checkmate::assert_number(subsample, lower = 0, upper = 1)
  checkmate::assert_numeric(scope)
  checkmate::assert_number(seed)

  # set seed
  set.seed(seed)

  # empty list
  halfspace <- list()

  # get halfspaces with random direction, random point, mass left and mass right
  # for i = 1, ... , number of halfspaces
  for (i in seq_len(n_halfspace)) {
    halfspace[[i]] <- get_halfspace(data, subsample, scope)
  }
  

  # return list with length n_halfspace
  halfspace
}

#---------------------------SUBFUNCTIONS train depth---------------------------

#get the halfspaces
get_halfspace <- function(data, subsample, scope) {
  # generate a random direction (l_i) in the 
  #data space with dimension ncol(data)
  random_direction <- generate_random_direction(ncol(data))

  # generate a subsample (D_i) by randomly 
  #selecting "subsample" x point from the "data"
  sample_data <- get_sample_data(data, subsample)

  # project subset sample (D_i) onto random direction(l_i) denoted by D_i^(l_i)
  projection <- get_projection(sample_data, random_direction)

  # get random mid s
  random_point <- get_random_point(projection, scope)

  # m_i^l: select the points in the subset which is smaller 
  #than the randomly selcted point
  mass_left <- (sum(projection < random_point)) / nrow(sample_data)

  # m_i^r:select the points in the subset which are greater 
  #or equal than the randomly selcted point
  mass_right <- 1 - mass_left

  # halfmass with values
  list(
    "random_direction" = random_direction,
    "random_point" = random_point,
    "mass_left" = mass_left,
    "mass_right" = mass_right
  )
}

# generate a  sample direction (vector) with in the dimensions
generate_random_direction <- function(dimension) {
  # generate a vector with normal distributed dimensions
  as.vector(rnorm(dimension))
}

# get subsample without replacement
get_sample_data <- function(data, subsample) {
  # if there is no subsample
  if (subsample == 1) {
    return(data) #--> early exit!!
  }

  # get a sample of the data with subsample
  no_data <- nrow(data)
  size <- subsample * no_data
  sample <- sample(x = no_data, size = size, replace = FALSE)

  data[sample, ]
}

# project data points on direction
get_projection <- function(data, direction) {
  as.matrix(data) %*% direction
}

# getrandom point in a certain scope (split)
get_random_point <- function(projection, scope) {
  # maximum of projection
  projection_max <- max(projection)

  # minimum of projection
  projection_min <- min(projection)

  # mid of projection
  projection_mid <- (projection_min + projection_max) / 2

  # randomly select points (s_i) in 
  #a certain intervall (mid_i +- (max_i - min_i))
  intervall_length <- scope * (projection_max - projection_min)
  intervall_min <- projection_mid - 0.5 * intervall_length
  intervall_max <- projection_mid + 0.5 * intervall_length

  # get one of a uniformed distribution; randomly select a point
  # uniform distribution: one observation, with minimum and maximum
  runif(n = 1, min = intervall_min, max = intervall_max)
}


#----------------------End SUBFUNCTIONS train depth ----------------------------

################################################################################
#---------------------- evaluate depth -----------------------------------------
################################################################################


# Input:
# data: training data (zi, i = 1, ... ,n)
# halfspaces: result from train_depth
# metric "mass"

# Output:
# Halfspaces for the data

# Task:
# calculates for every point x in the data the halfspace mass on the 
#basis of train_depth (halfspaces)
evaluate_depth <- function(data, halfspaces, metric = c("mass", "depth")) {
  # check arguments
  checkmate::assert_list(halfspaces)
  checkmate::assert(checkmate::test_data_frame(data), 
                    checkmate::test_matrix(data), 
                    combine = "or")
  data <- as.matrix(data)
  checkmate::assert_character(metric)
  metric <- match.arg(tolower(metric), c("mass", "depth"))

  # halfspacemasses
  halfspaces <- sapply(halfspaces, evaluate_one_halfspace, data)

  # data mass: expected value
  if (metric == "mass") {
    return(rowMeans(halfspaces))
  } #--> early exit!

  # data depth: minimum of the masses
  # MARGIN = 1 indicates minimum per row
  apply(halfspaces, MARGIN = 1, FUN = min)
}


#-------------------------------------------------------------------------------
# input: one halfspace (one element of the result of train_depth)
# output: vector of halfspaces
# evaluate one halfspace
evaluate_one_halfspace <- function(halfspace, data) {

  # get the values of the halfspace (train_depth)
  random_direction <- halfspace$random_direction
  random_point <- halfspace$random_point
  mass_left <- halfspace$mass_left
  mass_right <- halfspace$mass_right

  # project  x onto random direction
  projection <- get_projection(data, random_direction)

  # if the prjection is smaller, return mass left, else: mass right.
  halfmass <- ifelse(
    projection < random_point,
    mass_left,
    mass_right
  )

  halfmass
}
