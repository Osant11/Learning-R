## Programming with Dplyr ## 

library( dplyr )

https://dplyr.tidyverse.org/articles/programming.html
https://adv-r.hadley.nz/functions.html#lazy-evaluation
https://rlang.r-lib.org/reference/dyn-dots.html
https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html
https://mastering-shiny.org/action-tidy.html

# Introduction ----

## Data Masking ----
# Makes easy to work with variables within a dataset ( rows )
# Use data variables as if they were variables in the environment ( my_var and not df$my_var )
# arrange - count - filter - group_by - mutate - summarise

## Tidy selection ---- 
# Complementary tool : Easy to work with columns of a dataset ( columns )
# Choose variables based on their position, name, type ( starts_with( "x" ), is.numeric... )
# across - relocate - rename - select - pull 

# Look at the doc and arg = <data-masking> or <tidy-select>
?mutate



# Data Masking ---- 
# faster computation ( avoiding $ every time ) & and also env-var and data-var
# A variable are defined either as env-var or data-var -> need to be differentiated

## Env-variable ----
# "programming" var -> inside an environment ( created with <- )

## Data-variables ---- 
# "statistical" var -> inside dataset 

df <- data.frame(x = runif(3), y = runif(3))
df$x
# Creates env-var df with two data-var ( x and y ) then extracts data-var x from env-var df using $ 

## Indirection ----
# Challenge when transforming env-data into data-var : 

### Data-var in function arg ----
# function argument lazily evaluated -> they don't hold a value, only a promise until evaluated
# Need to embrace the arg : filter(df, {{ var }})

var_summary <- function( data, var ) {
  data %>%
    summarise( n = n(), min = min( {{ var }} ), max = max( {{ var }} ) )
}
mtcars %>% 
  group_by( cyl ) %>% 
  var_summary( mpg )


### Env-var as character vector ----
# Index into the .data pronoun with [[ : summarise( df, mean = mean( .data[[ var ]] ) )
# .data is not a DF, only a special construct to access the data-var directly .data$x or indirectly .data[[ var ]] 

for ( var in names( mtcars ) ) {
  mtcars %>% count( .data[[ var ]]) %>% print( )
}



## Name injection ----
# Dynamic dots : Generating names programmatically with ":="

### Name in env-var ----
# Glue syntax to interpolate it 
name <- "susan"
tibble("{name}" := 2)

### Name as data-var in fct arg ----
# Embracing syntax 
my_df <- function(x) {
  tibble("{{x}}_2" := x * 2)
}
my_var <- 10
my_df(my_var)




# Tidy Selection ----
# Easy to manipulate columns 

## Tidyselect DSL ----
# tidyselect package : specific language to select columns by name, position, type...
https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html

## Indirection ----
# Tidy select with column stored in an intermediate variable 

### Data-var in Env-var ----
# Need to "embrace" the function argument 
summarise_mean <- function( data, vars ) {
  data %>% summarise( n = n(), across( {{ vars }}, mean ) )
}
mtcars %>% 
  group_by( cyl ) %>% 
  summarise_mean( where( is.numeric ) )


### Env-var as characted vector ----
# all_of( ) or any_of( ) -> Depending if an error should be displayed
vars <- c( "mpg", "vs" )
mtcars %>% select( all_of( vars ) )
mtcars %>% select( !all_of( vars ) )

vars <- c( "mpg", "vsac" )
mtcars %>% select( all_of( vars ) )
mtcars %>% select( any_of( vars ) )



# How-tos ----

## User-supplied data ----
# .data never uses data masking or tidy select -> Anything special in the function

mutate_y <- function(data) {
  mutate(data, y = a + x)
}


## One or more user-supplied expressions ----

### 1 Expression ----
# 1 Expression that's passed onto an argument for data-masking or tidy-select -> "embrace" 
my_summarise2 <- function(data, expr) {
  data %>% summarise(
    mean = mean({{ expr }}),
    sum = sum({{ expr }}),
    n = n()
  )
}

### Multiple expression ----
# "embrace" all of them 
my_summarise3 <- function(data, mean_var, sd_var) {
  data %>% 
    summarise(mean = mean({{ mean_var }}), sd = sd({{ sd_var }}))
}


### Argument as name in the output ----
# "embrace" and ":=" 
my_summarise4 <- function(data, expr) {
  data %>% summarise(
    "mean_{{expr}}" := mean({{ expr }}),
    "sum_{{expr}}" := sum({{ expr }}),
    "n_{{expr}}" := n()
  )
}
my_summarise5 <- function(data, mean_var, sd_var) {
  data %>% 
    summarise(
      "mean_{{mean_var}}" := mean({{ mean_var }}), 
      "sd_{{sd_var}}" := sd({{ sd_var }})
    )
}


### Any number of user-supplied expressions ----
# use "..." -> Full control to the user over a single part of the pipeline ( like group_by(), mutate() )

my_summarise <- function(data, ...) {
  data %>%
    group_by(...) %>%
    summarise(mass = mean(mass, na.rm = TRUE), height = mean(height, na.rm = TRUE))
}

starwars %>% my_summarise(homeworld)
starwars %>% my_summarise(sex, gender)


### Transforming user-supplied variables ----
# Set of data-var and then transformed -> across() and pick()

my_summarise <- function( data, summary_vars ) {
  data %>%
    summarise( across( {{ summary_vars }}, ~ mean( ., na.rm = TRUE ) ) )
}
starwars %>% 
  group_by( species ) %>% 
  my_summarise( c( mass, height ) )


# pick -> select a subset of columns using select() semantics while inside a "data-masking" function 
my_summarise <- function( data, group_var, summarise_var ) {
  data %>%
    group_by( pick( {{ group_var }} ) ) %>% 
    summarise( across( {{ summarise_var }}, ~ mean( ., na.rm = T ) ) )
}

starwars %>% my_summarise( c( sex, gender), c( mass, height ) )

# Use the .names argument to across() to control the names of the output
my_summarise <- function( data, group_var, summarise_var ) {
  data %>%
    group_by( pick( {{ group_var }} ) ) %>% 
    summarise( across( {{ summarise_var }}, mean, .names = "mean_{.col}" ) )
}
starwars %>% my_summarise( c( sex, gender), c( mass, height ) )


## Loop over multiple variables ----
# Character vector -> .data pronoun
for ( var in names( mtcars ) ) {
  mtcars %>% count( .data[[ var ]] ) %>% print()
}

mtcars %>% 
  names( ) %>% 
  purrr::map( ~ count( mtcars, .data[[ .x ]] ) )
# -> Note that the x in .data[[x]] is always treated as an env-variable; it will never come from the data


## Variable from an Shiny input ----
# Shiny input -> return character vectors -> .data[[ input$var ]]
https://mastering-shiny.org/action-tidy.html

library( shiny )

ui <- fluidPage(
  selectInput( "var", "Variable", choices = names( diamonds ) ),
  tableOutput( "output" )
)
server <- function( input, output, session ) {
  data <- reactive( filter( diamonds, .data[[ input$var ]] > 0 ) )
  output$output <- renderTable( head( data( ) ) ) 
}
