## @knitr example

library(MASS) # provides `cats` data

gamma_est <- function(data) {
  # this fits a gamma distribution to a collection of numbers
  m <- mean(data)
  v <- var(data)
  s <- v/m
  a <- m/s
  return(list(a=a,s=s))
}

calc_var <- function(estimates){
  var_of_ests <- apply(estimates, 2, var)
  return(((n-1)^2/n)*var_of_ests)
}

gamma_jackknife <- function(data) {
  ## jackknife the estimation
  
  n <- length(data)
  jack_estimates = gamma_est(data[-1])
  for (omitted_point in 2:n) {
    jack_estimates = rbind(jack_estimates, gamma_est(data[-omitted_point]))
  }
  jack_var = calc_var(jack_estimates)
  return(sqrt(jack_var))
}

# jackknife gamma dist. estimates of cat heart weights
gamma_jackknife(cats$Hwt) 

## @knitr num-prec

1/3 == 4*(4/12-3/12)

## @knitr stop

mysqrt <- function(x) {
	if (is.list(x)) {
		warning("x is a list; converting to a vector")
		x <- unlist(x)
	}
	if (!is.numeric(x)) {
		stop("What is the square root of 'bob'?")
        ## alternatively: stopifnot(is.numeric(x))
	} else {
		if (any(x < 0)) {
			warning("mysqrt: found negative values; proceeding anyway")
			x[x >= 0] <- (x[x >= 0])^(1/2)
			x[x < 0] <- NaN
			return(x)
		} else return(x^(1/2))
	}
}
mysqrt(c(1, 2, 3))
mysqrt(c(5, -7))
mysqrt(c("asdf", "sdf"))
mysqrt(list(5, 3, "ab"))


## @knitr try

library(methods)
set.seed(0)
nCats <- 30
n <- 100
y <- rnorm(n)
x <- rnorm(n)
cats <- sample(1:nCats, n, replace = TRUE)
data <- data.frame(y, x, cats)

params <- matrix(NA, nr = nCats, nc = 2)
for(i in 1:nCats){
    sub <- data[data$cats == i, ] 
    fit <- try(
        lm(y ~ x, data = sub) )
    if(!is(fit, "try-error")) 
        params[i, ] = fit$coef
}
params

## @knitr drop

mat <- matrix(1:4, 2, 2)
dim(mat); print(mat)
colSums(mat)
rowSubset <- 1
mat2 <- mat[rowSubset, ]
colSums(mat2)

mat2 <- mat[rowSubset, , drop = FALSE]
colSums(mat)

## @knitr globals

library(codetools)
f <- function(z) {y <- 3; print(x + y + z)}
findGlobals(f)

# let's see the globals used in lm
findGlobals(lm)[1:25]
