https://rlang.r-lib.org/reference/topic-defuse.html

library( rlang )
library( tidyverse )


# When defused R doesn't return the value but the expression (tree form to describe how to compute the value) ----

1 + 1 
expr( 1 + 1 )
e <- expr( 1 + 1 )
typeof( e )

eval( e ) # Evaluation of defused expression 

# Defuse expressions to resume its evaluation in a data mask -> refers to columns of df as regular objects
e <- expr(mean(cyl))
typeof( e )
eval(e, mtcars)

# We call data-masking function which take care of defusing the arguments and resuming it in the context of a data-mask
mtcars %>% dplyr::summarise(
  mean(cyl)  # This is defused and data-masked
)

# When working with functions, "{{ }}" need to be used to defuse the correct expression
my_mean <- function(data, var) {
  dplyr::summarise(data, mean = mean({{ var }}))
}


# Types of defused expressions ----

## Calls ----
# like f(1, 2, 3) or 1 + 1 represent the action of calling a function to compute a new value, such as a vector

## Symbols ----
# like x or df = named objects. Env-var if object in function or in global environment / Data-var if object = col of data frame

## Constants ----
# like 1 or NULL 


## Creating new call or symbol with expr() ----

### Symbol ----
expr( foo )
typeof( expr( foo ) )

### Call ----
expr(mean(foo, na.rm = TRUE))
typeof( expr(mean(foo, na.rm = TRUE)) )

### Constant ----
expr( 1 )
typeof( expr( 1 ) )
expr( NULL )
typeof( expr( NULL ) )


## Defusing from data ----

# Defusing is not the only way to create defused expressions. You can also assemble them from data:

# Assemble a symbol from a string
var <- "foo"
sym( var )
typeof( sym( var ) )

call( "mean", sym( var ), na.rm = TRUE ) 


# Local expressions versus function arguments ----

# There are two main ways to defuse expressions, to which correspond two functions in rlang, expr() and enquo():

## expr( ) ----
# To defuse our own expression 
expr( 1 + 1 )

## enquo( ) / enquos( ) ----
# defuse function arguments / expressions supplied by the user the function -> defuse function arguments
abc <- function( arg ) enquo( arg )
abc( 1 + 1 )
abc( 1 ) 
abc( "ccsa" )
abc( foo )


# Defuse and inject ---- 

# Defusing evaluation of expression = interface with data-masking function by injecting the expression back into another function with !!. 
# defuse and inject pattern -> https://rlang.r-lib.org/reference/topic-metaprogramming.html

## Example ----
my_summarise <- function(data, arg) {
  # Defuse the user expression in `arg`
  arg <- enquo(arg)
  
  # Inject the expression contained in `arg`
  # inside a `summarise()` argument
  data |> dplyr::summarise(mean = mean(!!arg, na.rm = TRUE))
}

# But this step is usually done with {{ }} 
my_summarise <- function(data, arg) {
  # Defuse and inject in a single step with the embracing operator
  data |> dplyr::summarise(mean = mean({{ arg }}, na.rm = TRUE))
}

# Using enquo() and !! separately is useful in more complex cases where you need access to the defused expression instead of just passing it on.


# Defused arguments and quosures ----

# If inspect values of expr() and enquo(), -> enquo( ) = quosure -> wrapper with expression and environment
expr( 1 + 1 )
my_function <- function( arg ) enquo( arg )
my_function( 1 + 1 )

# R needs information about env to properly evaluate arg expr bcs they come from different context than the current function. 
# If a function calls dplyr::mutate(), the quosure env tells where all the private functions of the package are defined
# https://rlang.r-lib.org/reference/topic-quosure.html



# Comparison with base R ----

# Defusing is known as quoting in other frameworks

# The equivalent of expr() is base::bquote()
# The equivalent of enquo() is base::substitute(). The latter returns a naked expression instead of a quosure
# There is no equivalent for enquos(...) but you can defuse dots as a list of naked expressions with eval(substitute(alist(...)))
