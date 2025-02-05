################################################################################
# Week 2 HW R CODE
# CIPOLLI
# MATH 240 - SPRING 2025
################################################################################

################################################################################
# NOTES
################################################################################
#################################
# Function to integrate
#################################
integrand <- function(x){
  f <- (7 - x^2)
  return(f)
}
#################################
# Testing Values
#################################
a <- 0
b <- 2
n.rect <- 100
(delta.x <- (b-a)/n.rect)

#################################
# Left Rule
#################################
left.points <- a + 0:99*(delta.x)
(left.area <- sum(delta.x*(integrand(left.points))))
#################################
# Right Rule
#################################
right.points <- a + 1:100*(delta.x)
(right.area <- sum(delta.x*(integrand(right.points))))
#################################
# Midpoint Rule
#################################
mid.points <- (left.points+right.points)/2
(mid.area <- sum(delta.x*(integrand(mid.points))))


##################################
# Trapezoid Rule
##################################
left.trap = left.points
right.trap = right.points
trap.area = sum((integrand(left.trap) + integrand(right.trap))*(delta.x)/2)
print(trap.area)


################################################################################
# Question 1(b) Skeleton
################################################################################
riemann.sums <- function(fnct,                        # function to integrate
                         a,                           # lower bound of integral
                         b,                           # upper bound of integral
                         n.rect,                      # number of  bound of integral
                         method = "Trapezoidial"){    # method to use (trap by default)
  ######################################
  # Check Input
  ######################################
  if(!is.numeric(a)){ # if a is not numeric
    stop("The lower bound of the integral (a) must be numeric.")
  }
  if(!is.numeric(b)){ # if b is not numeric
    stop("The lower bound of the integral (a) must be numeric.")
  }
  if(!(is.numeric(n.rect)) | (n.rect%%1!=0)){ # if n.rect is not a whole number
    stop("The number of rectangles must be a positive whole number.")
  }
  ######################################
  # Compute Area
  ######################################
  delta.x <- (b-a)/n.rect
  left.points <- a + 0:(n.rect-1)*(delta.x)
  right.points <- a + (a+1):n.rect*(delta.x)
  if(method == "Left"){
    area <- sum(delta.x*(integrand(left.points)))
  }else if(method == "Right"){
    area <- sum(delta.x*(integrand(right.points)))
  }else if(method == "Midpoint"){
    mid.points <- (left.points+right.points)/2
    area <- sum(delta.x*(integrand(mid.points)))
  }else if(method == "Trapezoidial"){
    left.trap = left.points
    right.trap = right.points
    area = sum((integrand(left.trap) + integrand(right.trap))*(delta.x)/2)
  }else{
    stop("Please select a valid method (e.g., 'Left', 'Right', 'Midpoint', 'Trapezoidial')")
  }
  ######################################
  # Return the area
  ######################################
  return(area)
}
######################################
# Test the function
######################################
riemann.sums(fnct = integrand,
             a = 0,
             b = 2,
             n.rect = 100)
######################################
# Compare to numerical integral
######################################
integrate(f = integrand, # integrate() is an R function
          lower = 0,     # that completes numerical
          upper = 2)     # integration
