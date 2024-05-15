library( tidyverse )
library( haven )
library( admiral )

countries <- c( "USA", "FRA", "GER", "UKR", "ARG" )

adsl <- admiral_adsl

adsl_res <- adsl %>% 
  filter( SAFFL == "Y" )%>% 
  mutate( RND_NBR = ceiling( runif( n(), min = 0, max = 4 ) ), 
          COUNTRY = sample( countries, n(), replace = TRUE ), 
          SEXN = if_else( SEX == "M", 1, 0 ), 
          BMIBL = rnorm( n(), 25, 5 ) ) 

## Training with DPLYR ## 

# Data masking -> Easier to work with values within a dataset can use data.var as data.env : my_variable not df$my_variable ( arrange, count, filter, group_by, mutate, summarise )
# Tidy selection -> Easier to work with columns of dataset (data.var) with position, name, type.. : starts_with("x") / is.numeric.. ( across, relocate, rename, select, and pull )

##### ROWS #####
#### arrange
#### distinct ####
distinct( adsl_res, AGE )
distinct( adsl_res, AGE ) %>% arrange( AGE )
distinct( adsl_res, add = SEXN + RND_NBR )
distinct( adsl_res, across( contains( "FL" ) ) )
distinct( adsl_res, across( where( is.numeric ) & contains( "FL" ) ) )
distinct( adsl_res, across( c( "AGE", "SEX" ) ) )
distinct( adsl_res, c( "AGE", "SEX" ) )

adsl_res %>% arrange( RND_NBR, AGE ) %>% 
  group_by( RND_NBR ) %>% 
  distinct( AGE )


#### filter ####
adsl_res %>% select( USUBJID, AGE ) %>% 
  filter( AGE > mean( AGE, na.rm = T ) )

adsl_res %>% select( USUBJID, AGE, RND_NBR ) %>% 
  group_by( RND_NBR ) %>% 
  filter( AGE > mean( AGE, na.rm = T ) ) %>% 
  mutate( MEAN = mean( AGE, na.rm = T ) ) %>% 
  arrange( RND_NBR, AGE )


vars <- c( "BMIBL" )
vars_1 <- c( "AGE", "RND_NBR", "BMIBL" )
cond <- c( 30 )

adsl_res %>% 
  select( USUBJID, BMIBL ) %>% 
  filter( .data[[ vars_1[[ 3 ]] ]] > cond ) 

adsl_res %>% 
  group_by( SEX ) %>% 
  select( USUBJID, RND_NBR ) %>% 
  filter( !duplicated( RND_NBR ), .preserve = F )


#### slice #### 

adsl_res %>% 
  slice( 1L ) %>% 
  select( 2 )

adsl_res %>% 
  slice_head( ) %>% 
  select( 2 )

adsl_res %>% 
  slice( n() ) %>% 
  select( 2 )

adsl_res %>% 
  slice_tail( ) %>% 
  select( 2 )

adsl_res %>% 
  slice( 5:15 ) %>% 
  select( 2 )

# To drop rows
adsl_res %>% 
  slice( -c( 1:10 ) ) %>% 
  select( 2 )

# First and last 10 
adsl_res %>% 
  slice_head( n = 10 ) %>% 
  select( 2 )

adsl_res %>% 
  slice_tail( n = 10 ) %>% 
  select( 2 )

# Min and Max 
adsl_res %>% 
  slice_min( BMIBL, n = 10 ) %>% 
  select( 2, BMIBL, SEX )

adsl_res %>% 
  slice_max( BMIBL, n = 10 ) %>% 
  select( 2, BMIBL, SEX )

# Sampling
adsl_res %>% 
  slice_sample( n = 10 ) %>% 
  select( 2, BMIBL, SEX )

adsl_res %>% 
  slice_sample( n = 10, replace = T ) %>% 
  select( 2, BMIBL, SEX )

# Sampling, weighted by a var -> More likely to be picked
adsl_res %>% 
  slice_sample( weight_by = AGE, n = 10 ) %>% 
  select( 2, AGE, SEX )

# Group_by 
adsl_res %>% 
  group_by( COUNTRY ) %>% 
  mutate( n_country = n() ) %>% 
  slice_head( prop = 0.5 ) %>% 
  select( USUBJID, COUNTRY, n_country )

adsl_res %>% 
  select( USUBJID ) %>% 
  slice( rep( 1 : n(), each = 4 ) )



##### COLUMNS #####
#### Glimpse
#### mutate : create, modify, delete columns #### 
adsl_res %>% 
  mutate( AGE_BMI = AGE / BMIBL, .keep = "used" )

adsl_res %>% 
  mutate( AGE_BMI = AGE / BMIBL, .keep = "unused" )


adsl_res %>% 
  mutate( AGE_BMI = AGE / BMIBL, .keep = "none" )

adsl_res %>% 
  transmute( AGE_BMI = AGE / BMIBL )


#### pull : extract single column / relocate : change column order ####
adsl_res %>% 
  mutate( ID = BMIBL ) %>% pull()

adsl_res %>% 
  relocate( RND_NBR, BMIBL )

adsl_res %>% 
  relocate( RND_NBR, .after = "USUBJID" )

adsl_res %>% 
  relocate( RND_NBR, .before = "USUBJID" )

adsl_res %>% 
  relocate( WHOFC = RND_NBR, .before = "USUBJID" )

adsl_res %>% 
  relocate( where(is.numeric), .before = "USUBJID" )



#### rename / rename_with : rename columns #### 
rename( adsl_res, SUBJECT = USUBJID )

rename_with( adsl_res, tolower, any_of( c( "USUBJID", "ARM" ) ) )



#### select : keep or drop columns by names / type




##### GROUPS #####
#### count() / tally() / add_count() / add_tally() : Count observations #### 
count( adsl_res, RND_NBR ) 
count( adsl_res, SEX, RND_NBR )
count( adsl_res, SEX, RND_NBR, sort = T )

adsl_res %>% group_by( RND_NBR ) %>% tally( )

adsl_res %>% select( RND_NBR ) %>% add_count( RND_NBR )

#### group_by() / ungroup() : ####

fk <- mtcars %>% 
  group_by( cyl )

# Max disp by cyl 
fk %>% filter( disp == max( disp ) )

# By default, group_by() overrides existing grouping
fk %>% 
  group_by( vs, am ) %>%
  group_vars( )

# Use add = TRUE to instead append
fk %>%
  group_by( vs, am, .add = TRUE ) %>%
  group_vars( )

# Grouped by cutting
mtcars %>%
  group_by(hp_cut = cut(hp, 3))

mtcars %>%
  group_by( vs ) %>%
  mutate( hp_cut = cut( hp, 3 ) ) %>%
  group_by( hp_cut )

# when factors are involved and .drop = FALSE, groups can be empty
tbl <- tibble(
  x = 1:10,
  y = factor( rep( c( "a", "c" ), each  = 5 ), 
              levels = c( "a", "b", "c" ) )
  )

tbl %>%
  group_by( y ) %>%
  summarise( n = n() )

tbl %>%
  group_by( y, .drop = FALSE ) %>%
  summarise( n = n() )

#### summarise #### 
## Useful Function : 
# Center : mean() / median() - Spread : sd() / IQR() / mad()
# Range : min() / max() - Position : first() / last() / nth()
# Count : n() / n_distinct() - Logical : any() / all() 

adsl_res %>% 
  summarise( mean_bmi = mean( BMIBL, na.rm = T ),
             n = n(), 
             n_na = sum( !is.na( BMIBL ) ) )

adsl_res %>% 
  group_by( SEX ) %>% 
  summarise( mean_bmi = mean( BMIBL, na.rm = T ) )

adsl_res %>% 
  group_by( SEX ) %>%
  summarise( qs = quantile( BMIBL, c( 0.25, 0.5, 0.75 ), na.rm = T ), prob = c( 0.25, 0.5, 0.75 ), 
             n = n() )

adsl_res %>% 
  summarise( mean = mean( AGE ), n = n(), .by = RND_NBR )


my_quantile <- function( x, probs ){ 
  tibble( x = quantile( x, probs, na.rm = T ), probs = probs )
  }

adsl_res %>% 
  group_by( SEX ) %>%
  summarise( my_quantile( BMIBL, c( 0.25, 0.5, 0.75 ) ),  
             n = n() )

var <- "BMIBL"
summarise( adsl_res, avg = mean( .data[[var]], na.rm = TRUE ) )

vars <- c( "BMIBL", "AGE" )
summarise( adsl_res, across( all_of( vars ), ~ mean( .x, na.rm = TRUE ) ) )

adsl_res %>% select( where( is.numeric ) ) %>%  map( ~{ 
  tibble( mean = mean( .x, na.rm = T ), 
          sd = sd( .x, na.rm = T ) )
  } )

adsl_res %>% select( where( is.numeric ) ) %>%  map( ~{ 
  mean( .x, na.rm = T )
} )

car <- function( x, funs ){  funs( rnorm( x ) ) }
car( 1000, mean )


#### .by -> Only on single Verb ####
## mutate( .by = ) / summarise(.by = ) / reframe(.by = ) / filter(.by = ) / slice(.by = ) / slice_(by = )

adsl_res %>% group_by( SEXN ) %>% summarise( mean = mean( AGE ) )
adsl_res %>% summarise( mean = mean( AGE ), .by = SEXN )
adsl_res %>% summarise( mean = mean( AGE ), .by = c( SEXN,  RND_NBR ) )

cols <- c( "SEXN",  "RND_NBR" )
adsl_res %>% summarise( mean = mean( AGE ), .by = all_of( cols ) )

adsl_res %>% mutate( MWD_by_WHO = mean( AGE ), .by = RND_NBR, .keep = "used" )

adsl_res %>% slice_max( AGE, n = 2, by = RND_NBR ) %>% select( AGE, RND_NBR )
adsl_res %>% slice_max( AGE, n = 2, with_ties = FALSE, by = RND_NBR ) %>% select( AGE, RND_NBR )

#### rowwise() : One Row at a time ####
df <- tibble( x = runif( 6 ), y = runif( 6 ), z = runif( 6 ) )

# Compute the mean of x, y, z in each row
df %>% rowwise() %>% mutate( m = mean( c( x, y, z ) ) )

# use c_across() to more easily select many variables
df %>% rowwise() %>% mutate( m = mean( c_across( x:z ) ) )

# Compute the minimum of x and y in each row
df %>% rowwise( ) %>% mutate( m = min( c( x, y, z ) ) )
df %>% mutate( m = pmin( x, y, z ) ) #Much Faster

# rowwise() is also useful when doing simulations
params <- tribble(
  ~sim, ~n, ~mean, ~sd,
  1,  10,     1,   1,
  2,  10,     2,   4,
  3,  10,    -1,   2
)


#### reframe() : transform group to rows -> ungroup output ####

# enframe() : vector -> data.frame
# deframe() : data.frame -> vector 
# reframe() : data.frame -> data.frame

table <- c( "a", "b", "d", "f" )

df <- tibble(
  g = c( 1, 1, 1, 2, 2, 2, 2 ),
  x = c( "e", "a", "b", "c", "f", "d", "a" ) )

df %>%
  reframe( x = intersect( x, table ) )

df %>%
  reframe( x = intersect( x, table ), .by = g )

quantile_df <- function( x, probs = c( 0.25, 0.5, 0.75 ) ) {
  tibble(
    val = quantile( x, probs, na.rm = TRUE ),
    quant = probs
  )
}

adsl_res %>%
  reframe( quantile_df( AGE ) )

adsl_res %>%
  reframe( quantile_df( AGE ), .by = RND_NBR )

adsl_res %>%
  reframe( across( c( AGE, BMIBL ), quantile_df ), .by = RND_NBR )

adsl_res %>%
  reframe( across( c( AGE, BMIBL ), quantile_df, .unpack = T ), .by = RND_NBR )


#### Current group information ####

adsl_grp <- adsl_res %>% 
  group_by( SEX )

## n() -> Current group size
adsl_grp %>% summarise( n = n() )

## cur_group() -> group keys 
adsl_grp %>% summarise( n = cur_group( ) )

## cur_group_id() -> unique num id
adsl_grp %>% mutate( id = cur_group_id( ), .keep = "used" )

## cur_group_rows() -> row indices 
adsl_grp %>% reframe( ind = cur_group_rows() )

## cur_column() -> name of column (across())








##### DATA FRAMES #####
#### bind_cols -> bind df by column ####
df1 <- tibble( x = 1 : 3 )
df2 <- tibble( y = 3 : 1 )
bind_cols( df1, df2 )

#### bind_rows -> bind df by row ####
df1 <- tibble( x = 1 : 2, y = letters[ 1 : 2 ] )
df2 <- tibble( x = 4 : 5, z = 1 : 2 )
bind_rows( df1, df2 )
bind_rows( list( df1, df2 ) )

# .id option create a new column to reference the original dataset
bind_rows( df1, df2, .id = "id" )
bind_rows( list( a = df1, b = df2 ), .id = "id" )

#### Set Operations ####
df1 <- tibble( x = c( 1 : 3, 3, 3 ) )
df2 <- tibble( x = c( 3 : 5, 5 ) )

## intersect( x, y ) -> finds all rows in both x and y 
intersect( df1, df2 )

## union( x, y ) -> finds all rows in either x or y excluding duplicates
union( df1, df2 )

## union_all( x, y ) -> same but including duplicates
union_all( df1, df2 )

## setdiff( x, y ) -> finds all rows in x that aren't in y
setdiff( df1, df2 )

## symdiff( x, y ) -> all rows in x but not in y and all rows in y but not in x 
symdiff( df1, df2 )

## setequal( x, y ) -> TRUE if x and y contain the same rows
setequal( df1, df2 )
setequal( df1, df1[ 3:1, ] )



#### Joining dataset ####
xxx_join( x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ..., keep = NULL, na_matches = c("na", "never"), multiple = NULL, unmatched = "drop" )

df1 <- tibble( x = 1:3 )
df2 <- tibble( x = c( 1, 1, 2 ), y = c( "first", "second", "third" ) )

## inner_join() -> x and y have matching keys
band_members %>% inner_join( band_instruments )
band_members %>% inner_join( band_instruments, 
                             by = join_by( name ) )

df1 %>% inner_join( df2, join_by( x > x ) )
df1 %>% right_join( df2, join_by( x > x ), unmatched = "drop" )
df1 %>% right_join( df2, join_by( x > x ), unmatched = "error" )

#### left_join() -> keeps all observations in x ####
band_members %>% left_join( band_instruments )

df1 %>% left_join( df2 )
df1 %>% left_join( df2, multiple = "all" )
df1 %>% left_join( df2, multiple = "any" ) # To evalulate if a match exist, faster than first and last
df1 %>% left_join( df2, multiple = "first" )
df1 %>% left_join( df2, multiple = "last" )
df1 %>% left_join( df2, multiple = "error" )
df1 %>% left_join( df2, multiple = "warning" )

df2 %>% left_join( df1, by = join_by( x ), unmatched = "error", multiple = "all" )
df2 %>% left_join( df1, by = join_by( x ), unmatched = "drop", multiple = "all" )



#### right_join() -> keeps all observation in y #### 
band_members %>% right_join( band_instruments )

#### full_join() -> keeps all observations in x and y #### 
band_members %>% full_join( band_instruments )
band_members %>% full_join( band_instruments2, 
                            by = join_by( name == artist ) )
# To keep both key in x and y 
band_members %>% full_join( band_instruments2, 
                            by = join_by( name == artist ), 
                            keep = TRUE )


#### nest_join() -> adds a new list column for all row where y = x #### 
nest_join( x, y, by = NULL, copy = FALSE, keep = NULL, name = NULL, ..., na_matches = c("na", "never"), unmatched = "drop" )

df1 <- tibble( x = 1 : 3 )
df2 <- tibble( x = c( 2, 3, 3 ), y = c( "a", "b", "c" ) )

out <- nest_join( df1, df2 )
out$df2


#### semi_join() -> all rows from x with a match in y #### 
semi_join( x, y, by = NULL, copy = FALSE, ..., na_matches = c( "na", "never" ) )

band_members %>% semi_join( band_instruments )
band_members %>% semi_join( band_instruments, by = join_by( name ) )
band_members %>% inner_join( band_instruments )


#### anti_join() -> all rows from x without a match in y #### 
anti_join( x, y, by = NULL, copy = FALSE, ..., na_matches = c( "na", "never" ) )
band_members %>% anti_join( band_instruments )


#### cross_join() -> match each row in x to every row in y ( rows = nrow( x ) * nrow( y ) ) #### 
cross_join( x, y, ..., copy = FALSE, suffix = c( ".x", ".y" ) )
cross_join( band_instruments, band_members )
cross_join( band_instruments, band_members, suffix = c( "", "_y" ) )


#### join_by() -> Specification how to join two tables #### 

sales <- tibble(
  id = c(1L, 1L, 1L, 2L, 2L),
  sale_date = as.Date(c("2018-12-31", "2019-01-02", "2019-01-05", "2019-01-04", "2019-01-01"))
)

promos <- tibble(
  id = c(1L, 1L, 2L),
  promo_date = as.Date(c("2019-01-01", "2019-01-05", "2019-01-02")), 
  flag = c( "Y", "N", "Y" )
)

# Equality condition : "==" / Inequality : ">=, >, <=, <" / Rolling helper : "closest()" / Overlap helpers : "between(), within(), overlaps()" 

# Equality : keys to be equal between one or more pairs of columns join_by( x ) = join_by( x == x )
by <- join_by(id, sale_date == promo_date )
left_join(sales, promos, by )

# Inequality : single row in x to a potentially large number of rows in y 
by <- join_by(id, sale_date >= promo_date)
left_join(sales, promos, by)

# Rolling : closest match forward/backward when no exact match => closest( x >= y ) = for each value in x, find the closest y that is less or equal to x (x is always the primary table)
by <- join_by(id, closest(sale_date >= promo_date))
left_join(sales, promos, by)

by <- join_by(id, closest(sale_date > promo_date))
left_join(sales, promos, by)

sales <- mutate(sales, sale_date_lower = sale_date - 1)
by <- join_by(id, closest(sale_date >= promo_date), sale_date_lower <= promo_date)
full_join(sales, promos, by)

# Overlap : one or two columns from x overlapping a range defined by two columns in y
between(x, y_lower, y_upper, ..., bounds = "[]") bounds = "[]", "[)", "(]", or "()"
within(x_lower, x_upper, y_lower, y_upper)
overlaps(x_lower, x_upper, y_lower, y_upper, ..., bounds = "[]") bounds = "[]", "[)", "(]", or "()"


segments <- tibble(
  segment_id = 1:4,
  chromosome = c("chr1", "chr2", "chr2", "chr1"),
  start = c(140, 210, 380, 230),
  end = c(150, 240, 415, 280)
)

reference <- tibble(
  reference_id = 1:4,
  chromosome = c("chr1", "chr1", "chr2", "chr2"),
  start = c(100, 200, 300, 415),
  end = c(150, 250, 399, 450)
)

by <- join_by(chromosome, between(start, start, end))
full_join(segments, reference, by)

by <- join_by(chromosome, between(y$start, x$start, x$end))
full_join(reference, segments, by)

by <- join_by(chromosome, within(x$start, x$end, y$start, y$end))
inner_join(segments, reference, by)

by <- join_by(chromosome, overlaps(x$start, x$end, y$start, y$end))
full_join(segments, reference, by)

by <- join_by(chromosome, overlaps(x$start, x$end, y$start, y$end, bounds = "[)"))
full_join(segments, reference, by)



##### MANIPULATE INDIVIDUAL ROWS #####
data <- tibble( a = 1:3, b = letters[ c( 1:2, NA ) ], c = 0.5 + 0 : 2 )


#### rows_insert() -> adds new rows with keys in x ####
rows_insert( x, y, by = NULL, ..., conflict = c("error", "ignore"), copy = FALSE, in_place = FALSE )
rows_insert( data, tibble( a = 4, b = "z" ) )
rows_insert( data, tibble( a = 4, b = "z" ), by = "a" )

rows_insert( data, tibble( a = 3, b = "z" ) ) 
rows_insert( data, tibble( a = 3, b = "z" ), conflict = "ignore" )

#### rows_append() -> adds new rows without keys ####
rows_append( x, y, ..., copy = FALSE, in_place = FALSE )
rows_append( data, tibble( a = 3, b = "z" ) )

#### rows_update() -> modifies existing rows, key values in y must be unique and exist in x ####
rows_update( x, y, by = NULL, ..., unmatched = c("error", "ignore"), copy = FALSE, in_place = FALSE )
rows_update( data, tibble( a = 2:3, b = "z" ) )
rows_update( data, tibble( b = "z", a = 2:3 ), by = "a" )

rows_update( data, tibble( a = 3:4, b = "z" ), by = "a" )
rows_update( data, tibble( a = 3:4, b = "z" ), by = "a", unmatched = "ignore" )

#### rows_patch() -> overwrites NA values ####
rows_patch( x, y, by = NULL, ..., unmatched = c("error", "ignore"), copy = FALSE, in_place = FALSE )
rows_patch( data, tibble( a = 2:3, b = "z" ) )

rows_patch( data, tibble( a = 3:4, b = "z" ), by = "a" )
rows_patch( data, tibble( a = 3:4, b = "z" ), by = "a", unmatched = "ignore" )


#### rows_upsert() -> inserts or updates if unique key value in y already exists in x
rows_upsert( x, y, by = NULL, ..., copy = FALSE, in_place = FALSE )
rows_upsert( data, tibble( a = 2:4, b = "z" ) )

#### rows_delete() -> deletes rows, key values in y must exist in x
rows_delete( x, y, by = NULL, ..., unmatched = c("error", "ignore"), copy = FALSE, in_place = FALSE )
rows_delete( data, tibble( a = 2 : 3 ) )
rows_delete( data, tibble( a = 2:3, b = "b" ) )

rows_delete( data, tibble( a = 3:4, b = "z" ), by = "a" )
rows_delete( data, tibble( a = 3:4, b = "z" ), by = "a", unmatched = "ignore" )



##### MULTIPLE COLUMNS #####
#### across() -> same transformation to multiple columns ####
iris <- as_tibble( iris )

across( .cols, .fns, ..., .names = NULL, .unpack = FALSE )
# .fns -> function or purrr-style lambda, e.g. ~ mean(.x, na.rm = TRUE) 
# .names -> {.col} = selected column name / {.fn} = name of the function applied /  
# Default = "{.col}" and if list then "{.col}_{.fn}"

gdf <- tibble( g = c( 1, 1, 2, 3 ), v1 = 10 : 13, v2 = 20 : 23 ) %>%
  group_by( g )
set.seed( 1 )

# Outside: 1 normal variate
n <- rnorm( 1 )
gdf %>% mutate( across( v1:v2, ~ .x + n ) )

# Inside a verb: 3 normal variates (ngroup)
gdf %>% mutate( n = rnorm( 1 ), across( v1 : v2, ~ .x + n ) )

# Inside `across()`: 6 normal variates (ncol * ngroup)
gdf %>% mutate( across( v1 : v2, ~ .x + rnorm( 1 ) ) ) 

iris %>%
  mutate( across( c( Sepal.Length, Sepal.Width ), round ) )
iris %>%
  mutate( across( c( 1, 2 ), round ) )
iris %>%
  mutate( across( 1: Sepal.Width, round ) )
iris %>%
  mutate( across( where( is.double ) & !c( Petal.Length, Petal.Width ), round ) )
iris %>%
  group_by( Species ) %>%
  summarise( across( starts_with( "Sepal" ), ~ mean( .x, na.rm = TRUE ) ) )
iris %>%
  group_by( Species ) %>%
  summarise( across( starts_with( "Sepal" ), list( mean = mean, sd = sd ) ) )
iris %>%
  group_by( Species ) %>%
  summarise( across( starts_with( "Sepal" ), ~ list( mean = mean( .x, na.rm = T ), 
                                                     sd = sd( .x, na.rm =  T ) ) ) )
iris %>%
  group_by( Species ) %>%
  summarise( across( starts_with( "Sepal" ), mean, .names = "mean_{.col}" ) )
iris %>%
  group_by( Species ) %>%
  summarise( across( starts_with( "Sepal" ), list( mean = mean, sd = sd ), .names = "{.col}.{.fn}" ) )
# list not named then .fn is replaced with function's position
iris %>%
  group_by( Species ) %>%
  summarise( across( starts_with( "Sepal" ), list( mean, sd ), .names = "{.col}.fn{.fn}" ) )


#### if_any() if_all() -> same predicate function to a selection of columns and then return a logical vector ####
iris %>% 
  filter( if_any( ends_with( "Width" ), ~ . > 2 ) )

iris %>% 
  filter( if_all( ends_with( "Width" ), ~ . > 2 ) )

iris %>%
  filter( if_any( where( is.numeric ), ~ . > 2.4 ) )

iris %>%
  filter( if_all( where( is.numeric ), ~ . > 2.4 ) )

big <- function(x) {
  x > mean( x, na.rm = TRUE )
}


iris %>% 
  filter( if_all( where( is.numeric ), big ) )

iris %>% 
  mutate(
    category = case_when(
      if_all( where( is.numeric ), big ) ~ "all big", 
      if_any( where( is.numeric ), big ) ~ "one big", 
      TRUE                          ~ "small"
    ) )

#### c_across() -> row_wise aggregations #### 
iris %>% 
  rowwise( ) %>% 
  mutate( sum = sum( c_across( where( is.numeric ) ) ), 
          sd = sd( c_across( where( is.numeric ) ) ) )

#### pick() -> select a subset of columns inside data.masking #### 
df <- tibble(
  x = c(3, 2, 2, 2, 1),
  y = c(0, 2, 1, 1, 4),
  z1 = c("a", "a", "a", "b", "a"),
  z2 = c("c", "d", "d", "a", "c")
)
df

df %>% group_by( z1, z2 ) %>% count( )
df %>% count( pick( starts_with( "z" ) ) )
               

##### VECTOR FUNCTIONS -> Only on individual vector and not on DF #####
#### between() -> shorcut for x >= left & x <= right ####
between( 1 : 12, 7, 9 )
starwars %>% filter( between( height, 100, 150 ) )

#### case_match() -> vectorise switch() statements (column name doesn't need to be supplied at each statement -> better for character variable ) #### 
case_match( .x, ..., .default = NULL, .ptype = NULL )

x <- c( "a", "b", "a", "d", "b", NA, "c", "e" )

case_match( x, 
            "a" ~ 1, 
            "b" ~ 2, 
            "c" ~ 3, 
            "d" ~ 4 )

case_match( x, 
            "a" ~ 1, 
            "b" ~ 2, 
            "c" ~ 3, 
            "d" ~ 4, 
            NA ~ 0, 
            .default = 100 )

case_match( x,
            c("a", "b") ~ "low",
            c("c", "d", "e") ~ "high"
)

y <- c(1, 2, 1, 3, 1, NA, 2, 4)

case_match( y,
            between(y, 1, 3) ~ "odd",
            c( 4 ) ~ "even",
            .default = "missing" )

case_match( y, NA ~ 0, .default = y )

starwars %>%
  mutate(
    # Replace missings, but leave everything else alone
    hair_color = case_match(hair_color, NA ~ "unknown", .default = hair_color),
    # Replace some, but not all, of the species
    species = case_match(
      species,
      "Human" ~ "Humanoid",
      "Droid" ~ "Robot",
      c("Wookiee", "Ewok") ~ "Hairy",
      .default = species
    ),
    .keep = "used"
  )

#### case_when
#### case_when() -> vectorise if_else() statements ( order is really important )####
x <- 1 : 70
case_when(
  x %% 35 == 0 ~ "fizz buzz",
  x %% 5 == 0 ~ "fizz",
  x %% 7 == 0 ~ "buzz",
  .default = as.character( x )
)

# Missing cases and no .default then NA
case_when(
  x %% 35 == 0 ~ "fizz buzz",
  x %% 5 == 0 ~ "fizz",
  x %% 7 == 0 ~ "buzz",
)

x[ 2 : 4 ] <- NA_real_
case_when(
  x %% 35 == 0 ~ "fizz buzz",
  x %% 5 == 0 ~ "fizz",
  x %% 7 == 0 ~ "buzz",
  is.na(x) ~ "nope",
  .default = as.character(x)
)

# case_when with conbination of variables 
starwars %>%
  select(name:mass, gender, species) %>%
  mutate(
    type = case_when(
      height > 200 | mass > 200 ~ "large",
      species == "Droid" ~ "robot",
      .default = "other"
    )
  )

# Inside a function 
case_character_type <- function( height, mass, species ) {
  case_when(
    height > 200 | mass > 200 ~ "large",
    species == "Droid" ~ "robot",
    .default = "other"
  )
}

case_character_type( 150, 250, "Droid" )
case_character_type( 150, 150, "Droid" )

starwars %>%
  mutate( type = case_character_type( height, mass, species ) ) %>%
  pull( type )

# Condition can be added for a statement to be executed
case_character_type <- function( height, mass, species, robots = TRUE ) {
  case_when(
    height > 200 | mass > 200 ~ "large",
    if ( robots ) species == "Droid" ~ "robot",
    .default = "other"
  )
}

starwars %>%
  mutate( type = case_character_type( height, mass, species, robots = FALSE ), .keep = "used" )
starwars %>%
  mutate( type = case_character_type( height, mass, species, robots = TRUE ), .keep = "used" )

#### coalesce() -> replace missing values with specified value #### 
# Single value to replace missing value 
x <- sample( c( 1 : 5, NA, NA, NA ) )
coalesce( x, 99 )

# Generate a complte vector from partially missing
y <- c( 1, 2, NA, NA, 5 )
z <- c( NA, NA, 3, 4, 5 )
coalesce( y, z )

#### consecutive_id() -> generarates unique id that increment every time a variable changes ####
consecutive_id( c( TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, NA, NA ) )
consecutive_id( c( 1, 1, 1, 2, 1, 1, 2, 2 ) )
iris %>% mutate( id = consecutive_id( Species ), .keep = "used" ) %>% print()


#### cumall() - cumany() - cummean() -> cumulative functions (cumall / cumany = logicals )####
x <- c( 1, 3, 5, 2, 2 )
cummean( x )
cumsum( x ) / seq_along( x )
cumall( x <= 3 )
cumany( x == 3 )

df <- data.frame(
  date = as.Date("2020-01-01") + 0:6,
  balance = c(100, 50, 25, -25, -50, 30, 120)
)

df %>% filter( cumany( balance < 0 ) )
df %>% filter( cumall( balance > 0 ) )
df %>% filter( cumall( !( balance < 0 ) ) )

#### desc() -> sort in descending order #### 
desc( 1:10 )


#### if_else() -> vectorized if-else #### 
if_else( condition, true, false, missing = NULL, ..., ptype = NULL, size = NULL )

x <- c( -5 : 5, NA )
if_else( x < 0, NA, x )

x <- factor( sample( letters[ 1 : 5 ], 10, replace = TRUE ) )
ifelse( x %in% c( "a", "b", "c" ), x, NA )
if_else( x %in% c( "a", "b", "c" ), x, NA )

starwars %>%
  mutate( category = if_else( height < 100, "short", "tall" ), .keep = "used" )


#### lag() / lead() -> previous or next values within a vector ####
lag( x, n = 1L, default = NULL, order_by = NULL, ... )
lead( x, n = 1L, default = NULL, order_by = NULL, ... )

lag( 1:5 )
lag( 1:5, n = 2 )
lag( 1:5, n = 1, default = 0 )
lead( 1:5 )
lead( 1:5, n = 2 )
lead( 1:5, n = 1, default = 6 )

x <- 1:5
tibble( behind = lag( x ), x, ahead = lead( x ) )

scrambled <- slice_sample(
  tibble( year = 2000 : 2005, value = ( 0:5 ) ^ 2 ),
  prop = 1
)
scrambled %>% mutate( prev = lag( value ) )
scrambled %>% mutate( prev = lag( value, order_by = year ) )


#### n_distinct() -> counts the number of unique combinations ####
n_distinct( ..., na.rm = FALSE )

x <- c( 1, 1, 2, 2, 2 )
n_distinct( x )

y <- c( 3, 3, NA, 3, 3 )
n_distinct( y )
n_distinct( y, na.rm = T )

n_distinct( x, y )
n_distinct( x, y, na.rm = T )

n_distinct( data.frame( x, y ) )

#### na_if() -> convert value to NA ####
na_if( x, y )

na_if( 1:5, 5 )
na_if( 1:5, 5:1 )
na_if( 1:5, 1:5 )

x <- c( 1, -1, 0, 10 ) 
100 / x
100 / na_if( x, 0 )

y <- c( "abc", "def", "", "ghi" )
na_if( y, "" )

z <- c( 1, NaN, NA, 2, NaN )
na_if( z, NaN )

starwars %>%
  select( name, eye_color ) %>%
  mutate( eye_color = na_if( eye_color, "unknown" ) )

starwars %>%
  mutate( across( where( is.character ), ~ na_if( ., "unknown" ) ) )

#### near() -> comparing two vectors #### 
near( x, y, tol = .Machine$double.eps^0.5 )

sqrt( 2 ) ^ 2 == 2
near( sqrt( 2 ) ^ 2, 2 ) 



#### nth() / first() / last() -> extracting single values from vector ####
nth( x, n, order_by = NULL, default = NULL, na_rm = FALSE )
first( x, order_by = NULL, default = NULL, na_rm = FALSE )
last( x, order_by = NULL, default = NULL, na_rm = FALSE )

x <- 1 : 10
y <- 10 : 1
z <- c( NA, NA, 1, 3, NA, 5, NA )

first( x )
first( z )
first( z, na_rm = TRUE )
last( y )
nth( x, 1 )
nth( x, 5 )
nth( x, -2 )
nth( x, 11 )
nth( x, 11, default = 1L )
last( x, order_by = y )

df <- tibble( x = x, y = y )

first( df )
nth( df, 4 )

df %>%
  summarise(
    across( x:y, first, .names = "{col}_first" ),
    y_last = last( y ) )

#### ntile() -> breaks the input vector into n buckets #### 
ntile( x = row_number(), n )
x <- c( 5, 1, 3, 2, 2, 3 )
ntile( x, 2 )
ntile( x, 6 )
ntile( desc( x ), 2 )

ntile( c( 1:9, NA ) , 3 )
ntile( c( 1:9, NA ) , 4 )
ntile( c( 1:9, NA ) , 5 )

ntile( rep( 1, 8 ), 3 )

#### order_by() -> ordering the window function #### 
order_by( order_by, call )

order_by( 10 : 1, cumsum( 1 : 10 ) )

df <- data.frame( year = 2000 : 2005, value = ( 0:5 ) ^ 2 ) 
scrambled <- df[ sample( nrow( df ) ), ]

scrambled %>% mutate( running = cumsum( value ) ) %>% 
  arrange( year )

scrambled %>% mutate( running = order_by( year, cumsum( value ) ) ) %>% 
  arrange( year )

#### cume_dist() / percent_rank() -> compute percentile ####
# cume_dist() -> total number of values less than or equal than x_i then divide by the total of obs
# percent_rank() -> total number of values less than than x_i then divide by the total of obs -1 

percent_rank( x )
cume_dist( x )

x <- c( 5, 1, 3, 2, 2 )
cume_dist( x )
percent_rank( x )

sapply( x, function( xi ) sum( x <= xi ) / length( x ) )
sapply( x, function( xi ) sum( x < xi ) / ( length( x ) - 1 ) )

#### row_number() / min_rank() / dense_rank() -> Ranking function #### 
# row_number() -> every input a unique rank 
# min_rank() -> every tie same ranking value with gaps
# dense_rank() -> every tie same ranking value without gaps

row_number( x )
min_rank( x )
dense_rank( x )

x <- c( 5, 1, 3, 2, 2, NA )
row_number( x )
min_rank( x )
dense_rank( x )

df <- data.frame(
  grp = c( 1, 1, 1, 2, 2, 2, 3, 3, 3 ),
  x = c( 3, 2, 1, 1, 2, 2, 1, 1, 1 ),
  y = c( 1, 3, 2, 3, 2, 2, 4, 1, 2 ),
  id = 1 : 9 )

df %>% group_by( grp ) %>% filter( row_number( x ) == 1 )
df %>% group_by( grp ) %>% filter( row_number( ) == 1 )
df %>% group_by( grp ) %>% mutate( grp_id = row_number( ) )
df %>% group_by( grp ) %>% filter( min_rank( x ) == 1 )
df %>% group_by( grp ) %>% filter( min_rank( pick( x, y ) ) == 1 )
