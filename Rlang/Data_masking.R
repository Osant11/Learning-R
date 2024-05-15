## Data Masking ## 

disp <- 10
mtcars %>% mutate( disp_1 = .data$disp * .env$disp, 
                   disp_2 = disp * disp, .keep = "used" )



## Argument of function is a data-variable but is referenced as env-variable -> need to EMBRACE in function -> {{ var }} 
var_summary <- function( data, var ) {
  data %>%
    summarise( n = n(), min = min( {{ var }} ), max = max( {{ var }} ) )
}

mtcars %>% 
  group_by(cyl) %>% 
  var_summary(mpg)



select_fun <- function( data, var ){  
  data %>% 
    select( all_of( var ) )
}

select_fun( mtcars, c( "mpg", "disp" ) )



## Variables names are given by a Vector -> .data[[ x ]]

for ( var in names( adsl_res %>% select( is.numeric  ) ) ) {
  adsl_res %>% 
    summarise( mean = mean( .data[[ var ]], na.rm = T ) ) %>% 
    print( )
}


## Tidy Selection and data-var as Env-var 
sum_fun <- function( data, vars ){
  data %>% 
    summarise( n = n(), across( {{ vars }}, mean ) )
}

adsl_res %>% 
  group_by( SEX ) %>% 
  sum_fun( where( is.numeric ) ) 



## Tidy Selection and column name as Env-var
vars <- c( "USUBJID", "SEXN" )
adsl_res %>% select( all_of( vars ) )


## Multiple data-var as function argument 
my_summarise3 <- function( data, mean_var, sd_var ) {
  data %>% 
    summarise(n = n(), 
              mean = mean({{ mean_var }} ), 
              sum = sum( {{ mean_var }} ), 
              sd = sd( {{ sd_var }} ) )
}
adsl_res %>% 
  group_by( SEXN ) %>% 
  my_summarise3( AGE, AGE )



## Name of the variable as name 
my_summarise4 <- function( data, mean_var, sd_var ) {
  data %>% 
    summarise(n = n(), 
              "mean_{{mean_var}}" := mean({{ mean_var }} ), 
              "sum_{{mean_var}}" := sum( {{ mean_var }} ), 
              "sd_{{sd_var}}" := sd( {{ sd_var }} ) )
}
adsl_res %>% 
  group_by( SEXN ) %>% 
  my_summarise4( AGE, AGE )


## Name in an env-var : 
name <- "susan"
tibble( "{name}" := 2 )


## User full control of part 
my_summarise5 <- function( .data, ... ) {
  .data %>% 
    group_by( ... ) %>% 
    summarise(n = n(), 
              AGE = mean( AGE, na.rm = T ) )
}

adsl_res %>% my_summarise5( SEXN )
adsl_res %>% my_summarise5( SEXN, WHOFCBL )



## Multiple Data-var 
my_summaris6 <- function( data, group_var, summarise_var ) {
  data %>%
    group_by( across( {{ group_var }} ) ) %>% 
    summarise( across( {{ summarise_var }}, ~ mean( ., na.rm = T ) ) )
}
adsl_res %>% my_summaris6( c( SEXN, WHOFCBL ), c( AGE, PULSEBL ) )


my_summaris7 <- function( data, group_var, summarise_var ) {
  data %>%
    group_by( across( {{ group_var }} ) ) %>% 
    summarise( across( {{ summarise_var }}, ~ mean( ., na.rm = T ), .names = "mean_{.col}" ) )
}
adsl_res %>% my_summaris7( c( SEXN, WHOFCBL ), c( AGE, PULSEBL ) )


