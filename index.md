---
title: Debugging in R
layout: default
author: Christopher Paciorek
---

# How to use R’s debugging tools, handle errors, and avoid bugs

## 1 This tutorial

This tutorial covers R’s debugging tools, as well as strategies and
tools for catching and avoiding errors.

A [screencast](https://youtu.be/-yy_3htRHdU) that demonstrates the use
of R’s interactive debugging tools on a specific example accompanies
this document. The screencast uses a virtual machine developed a number
of years ago. You should be able to follow it using R on your own
computer.

This tutorial assumes you have a working knowledge of R.

Materials for this tutorial, including the R markdown file and
associated code files that were used to create this document are
available on
[GitHub](https://github.com/berkeley-scf/tutorial-R-debugging).

## 2 Basic debugging strategies

Here we’ll discuss some basic strategies for finding and fixing bugs.
Other useful locations for tips on debugging include:

  - [Efficient Debugging by
    Goldspink](https://www.codementor.io/mattgoldspink/how-to-debug-code-efficiently-and-effectively-du107u9jh)
  - [Debugging for Beginners by
    Brody](https://blog.hartleybrody.com/debugging-code-beginner/)
  - [Advanced R by Wickham](https://adv-r.hadley.nz/debugging.html)

Read and think about the error message. Sometimes it’s inscrutable, but
often it just needs a bit of deciphering. Looking up a given error
message by simply doing a web search with the exact message in double
quotes can be a good strategy, or you could look specifically on Stack
Overflow.

Fix errors from the top down - fix the first error that is reported,
because later errors are often caused by the initial error. It’s common
to have a string of many errors, which looks daunting, caused by a
single initial error.

Below we’ll see how one can view the stack trace. Usually when an error
occurs, it occurs in a function call that is nested in a series of
function calls. This series of calls is the *call stack* and the *stack
trace* shows that series of calls that led to the error. To debug,
you’ll often need to focus on the function being executed at the time
the error occurred (which will be at the top of the call stack) and the
arguments passed into that function. However, if the error occurs in a
function you didn’t write, the problem will often be with the arguments
that your code provided at the last point in the call stack at which
code that you wrote was run. Check the arguments that your code passed
into that first function that is not a function of yours.

Is the bug reproducible - does it always happen in the same way at at
the same point? It can help to restart R and see if the bug persists -
this can sometimes help in figuring out if there is a scoping issue and
we are using a global variable that we did not mean to.

If you can’t figure out where the error occurs based on the error
messages, a basic strategy is to build up code in pieces (or tear it
back in pieces to a simpler version). This allows you to isolate where
the error is occurring. You might use a binary search strategy. Figure
out which half of the code the error occurs in. Then split the ‘bad’
half in half and figure out which half the error occurs in. Repeat until
you’ve isolated the problem.

If you’ve written your code modularly with lots of functions, you can
test individual functions. Often the error will be in what gets passed
into and out of each function.

You can have warnings printed as they occurred, rather than saved, using
`options(warn = 1)`. This can help figure out where in a loop a warning
is being generated. You can also have R convert warnings to error using
`options(warn = 2)`.

At the beginning of time (the 1970s?), the standard debugging strategy
was to insert print statements in one’s code to see the value of a
variable and thereby decipher what could be going wrong. We have better
tools nowadays. But sometimes we still need to fall back to inserting
print statements.

R is a scripting language, so you can usually run your code line by line
to figure out what is happening. This can be a decent approach,
particularly for simple code. However, when you are trying to find
errors that occur within a series of many nested function calls or when
the errors involve variable scoping (how R looks for variables that are
not local to a function), or in other complicated situations, using
formal debugging tools can be much more effective. Finally, if the error
occurs inside of functions provided by R, rather than ones you write, it
can be hard to run the code in those functions line by line.

## 3 R’s interactive debugging tools

This section gives an overview of the various debugging tools. In the
screencast, you’ll see a live demonstration of using the tools in the
context of a real (albeit rather simple) example.

Note that RStudio wraps all of functionality of these tools in its
graphical interface, so you can use all the tools there, but the tools
will be provided with some additional graphical functionality from
RStudio.

Hadley Wickham’s book on Advanced R has a [very good chapter on
debugging](https://adv-r.hadley.nz/debugging.html), covering much of the
same material as in this section, but with a focus on how the R debugger
works within RStudio. I highly recomend working through his material in
conjunction with this tutorial.

### 3.1 Interactive debugging via the browser

The core strategy for interactive debugging is to use the *browser*
function, which pauses the current execution, and provides an
interpreter, allowing you to view the current state of R. You can invoke
*browser* in four ways

  - by inserting a call to `browser()` in your code if you suspect where
    things are going wrong

  - by invoking the browser after every step of a function using *debug*

  - by using `options(error = recover)` to invoke the browser when an
    error occurs

  - by temporarily modifying a function to allow browsing using *trace*

Once in the browser, you can execute any R commands you want. In
particular, using *ls* to look at the objects residing in the current
function environment, looking at the values of objects, and examining
the classes of objects is often helpful.

### 3.2 Using *debug* to step through code

To step through a function, use `debug(nameOfFunction)`. Then run your
code. When the function is executed, R will pause execution just before
the first line of the function. You are now using the browser and can
examine the state of R and execute R statements.

Once in the browser context, you can use the following special debugger
commands:

  - ‘n’ or <return> to step to the next line,
  - ‘f’ to finish executing the entire current function or current loop,
  - ‘c’ to continue to any subsequent browser calls, or
  - ‘Q’ to stop debugging.

To unflag the function so that calling it doesn’t invoke debug, use
`undebug(nameOfFunction)`. In addition to working with functions you
write you can use *debug* with standard R functions and functions from
packages. For example you could do `debug(glm)`.

If you know you only want to run the function once in debugging mode (to
avoid having to use *undebug*), use `debugonce(nameOfFunction)`.

### 3.3 Tracing errors in the call stack

*traceback* and *recover* allow you to see the call stack at the time of
the error - i.e., they will show you all the functions that have been
called, in the order called. This helps pinpoint where in a series of
function calls the error may be occurring.

If you’ve run the code and gotten an error, you can invoke `traceback()`
after things have gone awry. R will show you the call stack, which can
help pinpoint where an error is occurring. Here’s the traceback from the
example discussed later on.

    > traceback()
    6: stop(simpleError(msg, call = if (p <- sys.parent(1L)) sys.call(p)))
    5: stopifnot(is.atomic(x))
    4: FUN(newX[, i], ...)
    3: apply(estimates, 2, var) at #2
    2: calc_var(jack_estimates) at #9
    1: gamma_jackknife(cats$Hwt)

`gamma_jackknife(cats$Hwt)` while `stop()` was (not surprisingly) the
function that was called most recently (the function at the top of the
call stack).

More helpful is to be able to browse within the call stack. To do this
invoke `options(error = recover)` (potentially in your *.Rprofile* if
you do a lot of programming). Then when an error occurs, *recover* gets
called, usually from the function in which the error occurred. The call
to *recover* allows you to navigate the stack of active function calls
at the time of the error and browse within the desired call. You just
enter the number of the call you’d like to enter (or 0 to exit). You can
then look around in the frame of a given function, entering <return>
when you want to return to the list of calls again.

Here’s what happens in the example discussed later on. Notice the call
stack is shown in reverse order relative to what we saw just above with
`traceback()`.

``` r
> gamma_jackknife(cats$Hwt)
Error in FUN(newX[, i], ...) : is.atomic(x) is not TRUE

Enter a frame number, or 0 to exit   

1: gamma_jackknife(cats$Hwt)
2: #9: calc_var(jack_estimates)
3: #2: apply(estimates, 2, var)
4: FUN(newX[, i], ...)
5: stopifnot(is.atomic(x))

Selection: 
```

In this case we would probably want to select ‘2’ to get into the frame
of the `calc_var` function, which is the last function that we wrote
before calling existing R functions. We could then examine the
information in the `estimates` data structure, as the problem likely
occurs there, since that is the argument passed into the first R
function we did not write, namely `apply`.

You can also combine this with `options(warn = 2)`, which turns warnings
into errors to get to the point where a warning was issued.

### 3.4 Using *trace* to temporarily insert code

*trace* lets you temporarily insert code into a function (including
standard R functions and functions in packages\!) that can then be
easily removed. You can use trace in a variety of ways.

The most flexible way to use *trace* is to use the argument `edit =
TRUE` and then insert whatever code you want wherever you want in the
function given as the first argument to *trace*. If I want to ensure I
use a particular editor, such as emacs, I can use the argument `edit =
“emacs”`. A standard approach would be to add a line with `browser()`
at some point in the function to be able to step through the code from
that point.

You can also use *trace* without directly editing the function. Here are
a couple examples:

  - `trace(lm, recover)` \# invoke *recover* when the function (*lm* in
    this case) starts
  - `trace(lm, exit = browser)` \# invoke *browser* when the function
    ends

You call *untrace*, e.g., `untrace(lm)`, to remove the temporarily
inserted code; otherwise it’s removed when the session ends.

To figure out why warnings are being issued, you can do `trace(warning,
recover)` which will insert a call to *recover* whenever *warning* is
called.

Of course you can manually change the code in a function without using
*trace*, but it’s very easy to forget to change things back (and a pain
to remember exactly what you changed) and hard to do this with functions
in packages, so *trace* is a nice way to do things.

### 3.5 Live demonstration

The live demonstration concerns the following code, which uses the
jackknife (a method related to the bootstrap) to try to estimate
standard errors for parameter estimates for some data on cat heart
weights. The details of what the code is trying to do are not critical
for our purposes here.

Here is the code, consisting of three functions, with *gamma\_jackknife*
being the top-level function called by the user. When we try to run this
code it gives a not-very-helpful error message. There are two reasons it
is not helpful. First, the message itself is indirect in that it says
something is *not atomic*, which is hard to interpret unless you have a
bit of detailed knowledge about R’s variable types. Second, the error
message does not indicate in which of the user-defined functions the
error occurs.

``` r
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
try(gamma_jackknife(cats$Hwt))
```

    ## Error in FUN(newX[, i], ...) : is.atomic(x) is not TRUE

See the screencast for how to use *traceback*, `options(error =
recover)`, *debug*, and *trace* to figure out the problem.

### 3.6 Debugging R6 and ReferenceClass methods

To debug an R6 method, you can call the `debug` method of the class on
the method you want to debug:

``` r
myClass$debug("method_name")
## now run the method
myClass$method_name()
## to unset the debugger
myClass$undebug("method_name")
```

To debug a ReferenceClass method, you can call the `trace` method of the
class on the method you want to debug:

``` r
myClass$trace("method_name", browser)
## now run the method
myClass$method_name()
## to unset the debugger
myClass$untrace("method_name")
```

## 4 Some common causes of bugs

Some of these are R-specific, while others are common to a variety of
languages.

  - Parenthesis mis-matches

  - `[[...]]` vs. `[...]`

  - `==` vs. `=`

  - Comparing real numbers exactly using `==` is dangerous because
    numbers on a computer are only represented to limited numerical
    precision. For example,
    
    ``` r
    1/3 == 4*(4/12-3/12)
    ```
    
        ## [1] FALSE

  - Vectors vs. single values:
    
      - `||` vs. `|` and `&&` vs `&`
      - You expect a single value but your code gives you a vector
      - You want to compare an entire vector but your code just compares
        the first value (e.g., in an `if` statement) – consider using
        `identical()` or `all.equal()`

  - You expect a single value but execution of the code gives a vector

  - You want to compare an entire vector but your code just compares the
    first value (e.g., in an if statement) – consider using *identical*
    or *all.equal*

  - Silent type conversion when you don’t want it, or lack of coercion
    where you’re expecting it

  - Using the wrong function or variable name

  - Giving unnamed arguments to a function in the wrong order

  - A common error is that the condition of an if statement is `NA`. You
    can handle NAs using `if(isTRUE(condition))` or
    `if(isFALSE(condition))`.

  - In an if-else statement, the `else` cannot be on its own line
    (unless all the code is enclosed in `{}`) because R will see the
    `if` part of the statement, which is a valid R statement, will
    execute that, and then will encounter the `else` and return an
    error.

  - Forgetting to define a variable in the environment of a function and
    having R, via lexical scoping, get that variable as a global
    variable from one of the enclosing environments. At best the types
    are not compatible and you get an error; at worst, you use a garbage
    value and the bug is hard to trace. In some cases your code may work
    fine when you develop the code (if the variable exists in the
    enclosing environment), but then may not work when you restart R if
    the variable no longer exists or is different.

  - R (usually helpfully) drops matrix and array dimensions that are
    extraneous. This can sometimes confuse later code that expects an
    object of a certain dimension. More on this in Section 5.3.

## 5 Tips for avoiding bugs and catching errors

### 5.1 Practice defensive programming

When writing functions, and software more generally, you’ll want to warn
the user or stop execution when there is an error and exit gracefully,
giving the user some idea of what happened. Here are some things to
consider:

  - check function inputs and warn users if the code will do something
    they might not expect or makes particular choices;
  - check inputs to `if` and the ranges in `for` loops;
  - provide reasonable default arguments;
  - document the range of valid inputs;
  - check that the output produced is valid; and
  - stop execution based on checks and give an informative error
    message.

The *warning* and *stop* functions allow you to do stop execution and
issue warnings or errors in the same way that base R code does; in
general they would be called based on an `if` statement. More
succinctly, to stop code if a condition is not satisfied, you can use
*stopifnot*. This allow you to catch errors that can be anticipated. I
also recommend using some of R’s packages for doing such checks, such as
*assertthat*, *assertr*, and *checkmate*.

Here’s an example of building a robust square root function using *stop*
and *warning*. Note you could use `stopifnot(is.numeric(x))` or
`assert_that(is.numeric(x))` in place of one of the checks here.

``` r
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
```

    ## [1] 1.000000 1.414214 1.732051

``` r
mysqrt(c(5, -7))
```

    ## Warning in mysqrt(c(5, -7)): mysqrt: found negative values; proceeding anyway

    ## [1] 2.236068      NaN

``` r
try(mysqrt(c("asdf", "sdf")))
```

    ## Error in mysqrt(c("asdf", "sdf")) : What is the square root of 'bob'?

``` r
try(mysqrt(list(5, 3, "ab")))
```

    ## Warning in mysqrt(list(5, 3, "ab")): x is a list; converting to a vector

    ## Error in mysqrt(list(5, 3, "ab")) : What is the square root of 'bob'?

### 5.2 Catch run-time errors with `try` statements

Also, sometimes a function you call will fail, but you want to continue
execution. For example, suppose you are doing a stratified analysis in
which you take subsets of your data based on some categorical variable
and fit a statistical model for each value of the categorical variable.
If some of the subsets have no or very few observations, the statistical
model fitting might fail. To do this, you might be using a for loop or
*lapply*. You want your code to continue and fit the model for the rest
of the cases even if one (or more) of the cases cannot be fit. You can
wrap the function call that may fail within the `try` function (or
`tryCatch`) and then your code won’t stop, even when an error occurs.
Here’s a toy example.

``` r
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
```

    ## Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) : 
    ##   0 (non-NA) cases

``` r
params
```

    ##                [,1]        [,2]
    ##  [1,] -0.4358199953  0.27748127
    ##  [2,]  1.1581183671  0.80912437
    ##  [3,] -0.3554216408  0.47883210
    ##  [4,]  0.1557150115  0.48844740
    ##  [5,]  1.3686786299 -0.93914607
    ##  [6,] -0.4253189229  0.83353130
    ##  [7,]  1.4554922258  1.70845976
    ##  [8,] -0.3658129573 -0.06813899
    ##  [9,]  0.0301244577 -0.08146689
    ## [10,] -0.4686109164 -2.05689759
    ## [11,]  2.2806852741 -1.66464111
    ## [12,]  0.2221364736  0.35794243
    ## [13,]  0.2072138141  0.83705334
    ## [14,] -0.2273286914          NA
    ## [15,]            NA          NA
    ## [16,]  0.5851340251 -0.67609092
    ## [17,] -0.4604747167 -0.12513299
    ## [18,]  1.7525542597  2.46613921
    ## [19,]  0.5986682621 -0.10533300
    ## [20,]  0.0341767117  0.16544258
    ## [21,]  0.4234918160 -0.25409147
    ## [22,] -0.6337088930 -2.37384473
    ## [23,] -0.3153278059  0.26485741
    ## [24,] -0.0008991281  1.39578005
    ## [25,]  0.0091840186  0.04655760
    ## [26,]  0.8239709421  0.41607748
    ## [27,] 14.0124163710 -6.91639295
    ## [28,]  0.2046168245 -0.42460241
    ## [29,] -0.5619773029 -0.82265332
    ## [30,] -0.9483964596  0.83635071

The seventh stratum had no observations, so that call to *lm* failed,
but the loop continued because we ‘caught’ the error with *try*. In this
example, we could have checked the sample size for the subset before
doing the regression, but in other contexts, we may not have an easy way
to check in advance whether the function call will fail.

### 5.3 Maintain dimensionality

R (usually helpfully) drops matrix and array dimensions that are
extraneous. This can sometimes confuse later code that expects an object
of a certain dimension. The `[` operator takes an additional optional
argument that can avoid dropping dimensions.

``` r
mat <- matrix(1:4, 2, 2)
dim(mat); print(mat)
```

    ## [1] 2 2

    ##      [,1] [,2]
    ## [1,]    1    3
    ## [2,]    2    4

``` r
colSums(mat)
```

    ## [1] 3 7

``` r
rowSubset <- 1
mat2 <- mat[rowSubset, ]
try(colSums(mat2))
```

    ## Error in colSums(mat2) : 'x' must be an array of at least two dimensions

``` r
mat2 <- mat[rowSubset, , drop = FALSE]
colSums(mat)
```

    ## [1] 3 7

In this simple case it’s obvious that a dimension will be dropped, but
in more complicated settings, this can easily occur for some inputs
without the coder realizing that it may happen. Not dropping dimensions
is much easier than putting checks in to see if dimensions have been
dropped and having the code behave differently depending on the
dimensionality.

### 5.4 Find and avoid global variables

In general, using global variables (variables that are not created or
passed into a function) results in code that is not robust. Results will
change if you or a user modifies that global variable, usually without
realizing/remembering that a function depends on it.

One ad hoc strategy is to remove objects you don’t need from R’s global
environment, to avoid accidentally using values from an old object via
R’s scoping rules.

To be more systematic, the *codetools* library has some useful tools for
checking code, including a function, *findGlobals*, that let’s you look
for the use of global variables

``` r
library(codetools)
f <- function(z) {y <- 3; print(x + y + z)}
findGlobals(f)
```

    ## [1] "{"     "+"     "<-"    "print" "x"

``` r
# let's see the globals used in lm
findGlobals(lm)[1:25]
```

    ##  [1] "-"              "!"              "!="             ".getXlevels"   
    ##  [5] "["              "[[<-"           "{"              "*"             
    ##  [9] "&&"             "<-"             "=="             "$<-"           
    ## [13] "as.vector"      "attr"           "c"              "class<-"       
    ## [17] "eval"           "gettextf"       "if"             "is.empty.model"
    ## [21] "is.matrix"      "is.null"        "is.numeric"     "length"        
    ## [25] "list"

Note that some use of globals to find base R functions is unavoidable.
But we can easily see that *f* makes use of *x* as a global variable.
This is of course particularly helpful for more complicated functions
that use many variables.

### 5.5 Miscellaneous tips

  - Use core R functionality and algorithms already coded. Figure out if
    a functionality already exists in (or can be adapted from) an R
    package (or potentially in a C/Fortran library/package): code that
    is part of standard mathematical/numerical packages will probably be
    more efficient and bug-free than anything you would write.
  - Code in a modular fashion, making good use of functions, so that you
    don’t need to debug the same code multiple times. Smaller functions
    are easier to debug, easier to understand, and can be combined in a
    modular fashion (like the UNIX utilities).
  - Write code for clarity and accuracy first; then worry about
    efficiency. Write an initial version of the code in the simplest
    way, without trying to be efficient (e.g., you might use for loops
    even if you’re coding in R); then make a second version that employs
    efficiency tricks and check that both produce the same output.
  - Plan out your code in advance, including all special
    cases/possibilities.
  - Write tests for your code early in the process.
  - Build up code in pieces, testing along the way. Make big changes in
    small steps, sequentially checking to see if the code has broken on
    test case(s).
  - Be careful that the conditions of `if` statements and the sequences
    of `for` loops are robust when they involve evaluating R code.
  - Don’t hard code numbers - use variables (e.g., number of iterations,
    parameter values in simulations), even if you don’t expect to change
    the value, as this makes the code more readable and reduces bugs
    when you use the same number multiple times; e.g. `speedOfLight
    <- 3e8` or `nIts <- 1000`.

## 6 How to get help online

### 6.1 Mailing lists and online forums

There are several mailing lists that have lots of useful postings. In
general if you have an error, others have already posted about it.

  - [Stack Overflow](https://stackoverflow.com):
      - R stuff will be tagged with ‘R’:
        <https://stackoverflow.com/questions/tagged/r>
  - R help: [R mailing lists archive](https://search.r-project.org)
  - R help special interest groups (SIG) such as r-sig-hpc (high
    performance computing), r-sig-mac (R on Macs), etc. Unfortunately
    these are not easily searchable, but can often be found by simple
    web searchs, potentially including the name of the SIG in the
    search.
  - Simple web searches: You may want to include “R” or “in R” (possibly
    with the quotes in latter case) in the search. To search a SIG you
    might include the name of the SIG in the search string
  - [Rseek.org](https://Rseek.org) for web searches restricted to sites
    that have information on R

Note: of course the various mailing lists are also helpful for figuring
out how to do things, not just for fixing bugs. For example, this [blog
post](http://www.r-bloggers.com/the-guerilla-guide-to-r/?utm_source=feedburner&utm_medium=email&utm_campaign=Feed%3A+RBloggers+%28R+bloggers%29)
has a guide to R based simply on Stack Overflow posts.

### 6.2 Asking questions online

If you’ve searched the archive and haven’t found an answer to your
problem, you can often get help by posting to the R-help mailing list or
one of the other lists mentioned above. A few guidelines (generally
relevant when posting to mailing lists beyond just the R lists):

  - Search the archives and look through relevant R books or manuals
    first.
  - Boil your problem down to the essence of the problem, giving an
    example, including the output and error message. The [reprex
    package](https://reprex.tidyverse.org) can help you prepare a
    reproducible example to post on a mailing list.
  - Say what version of R, the versions of all R packages involved, what
    operating system and what operating system version you’re using.
    Both *sessionInfo* and *Sys.info* can be helpful for getting this
    information.
  - Read the [R mailing list posting
    guide](https://www.r-project.org/posting-guide.html).

The R mailing lists are a way to get free advice from the experts, who
include some of the world’s most knowledgeable R experts - seriously -
members of the R core development team contribute frequently. The cost
is that you should do your homework and that sometimes the responses you
get might be blunt, along the lines of “read the manual”. I think it’s a
pretty good tradeoff - where else do you get the foremost experts in a
domain actually helping you?
