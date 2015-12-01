# Two-point central fintite difference method to approximate derivative
#
#SYNOPSYS
# p = CFD(Fn, x)
#
# Used to approximate the pdf at a given point from the cdf using finite difference method (central difference with two points)
# [Karen A. Kopecky (2007). Lecture Notes. http://www.karenkopecky.net/Teaching/eco613614/Notes_NumericalDifferentiation.pdf]
# ~10e-11 accuracy (O(h^2))
#
#INPUT
# Fn - handler of the function
# x  - point(s) where we are interested in the value of the ferivative of Fn, /can be vector/
#
#OUTPUT
# p  - numerically estimated derivative at point x /same size and type as x/
#
#See also


cfd = function(Fn, x) {
  
  eps =  .Machine$double.eps 
  h   = (eps)^(1/3)*vapply(x, function(x) max(x, 1), vector(mode = "double",length = 1))
  xph = x + h
  xmh = x - h
  dx  = xph - xmh # to account for rounding error
  
  p = (Fn(xph) - Fn(xmh))/dx
  
  return(p)
  
}