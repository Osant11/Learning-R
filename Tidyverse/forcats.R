library( tidyverse ) 
library( forcats )
library( ggplot2 )
library( admiral )

adsl <- admiral_adsl

colnames( adsl )

countries <- c( "USA", "FRA", "GER", "UKR", "ARG" )

adsl_res <- adsl %>% 
  filter( SAFFL == "Y" ) %>% 
  select( USUBJID, SITEID, ACTARM, AGE, AGEGR1, SEX, REGION1, RACE, RACEGR1, EOSSTT ) %>% 
  mutate( RND_NBR = ceiling( runif( n(), min = 0, max = 4 ) ), 
          COUNTRY = sample( countries, n(), replace = TRUE ) )

adsl_res$RND_NBR <- factor( adsl_res$RND_NBR )

adsl_res_small <- adsl_res %>% 
  slice_sample( n = 15 )

adsl_res_small$COUNTRY <- factor( adsl_res_small$COUNTRY )
adsl_res_small$RND_NBR <- factor( adsl_res_small$RND_NBR, levels = c( "4", "3", "2", "1", "0" ) )

adsl_res_small$COUNTRY
adsl_res_small$RND_NBR 



##### CHANGE ORDER OF LEVELS ##### 
##### CHANGE ORDER OF LEVELS #####

#### fct_relevel() : move any number of levels to any location #### 
fct_relevel( .f, ..., after = 0L )

distinct( adsl_res_small, COUNTRY )
count( adsl_res_small, COUNTRY )

fct_relevel( adsl_res_small$COUNTRY )
fct_relevel( adsl_res_small$COUNTRY, "USA" )
fct_relevel( adsl_res_small$COUNTRY, c( "USA", "UKR" ) )
fct_relevel( adsl_res_small$COUNTRY, "USA", after = 2 )
fct_relevel( adsl_res_small$COUNTRY, "USA", after = Inf )
fct_relevel( adsl_res_small$COUNTRY, sort )
fct_relevel( adsl_res_small$COUNTRY, sample )
fct_relevel( adsl_res_small$COUNTRY, rev )
fct_relevel( adsl_res_small$COUNTRY, "FRA" )



#### fct_inorder() / fct_infreq() / fct_inseq() : Reorder factor ####

## fct_inorder() -> Order in which they first appear
fct_inorder( f, ordered = NA )
fct_inorder( adsl_res_small$COUNTRY )

## fct_infreq() -> By number of observations with each level (largest first)
fct_infreq( f, w = NULL, ordered = NA )
fct_infreq( adsl_res_small$COUNTRY )

## fct_inseq() -> By numeric value of level
fct_inseq( f, ordered = NA )
fct_inseq( adsl_res_small$RND_NBR )


#### fct_reorder() / fct_reorder2() / last2() / first2() : Reorder factor levels by sorting along another variable ####

## fct_reorder() -> 1d display where factor is mapped to position
fct_reorder( .f, .x, .fun = median, ..., .na_rm = NULL, .default = Inf, .desc = FALSE )

boxplot( AGE ~ RND_NBR, data =  adsl_res )
boxplot( AGE ~ fct_reorder( RND_NBR, AGE ), data =  adsl_res )
ggplot( adsl_res, aes( fct_reorder( RND_NBR, AGE ), AGE ) ) +
  geom_boxplot( )

## fct_reorder2() -> 2d display where factor mapped to non-position aesthetic
fct_reorder2( .f, .x, .y, .fun = last2, ..., .na_rm = NULL, .default = -Inf, .desc = TRUE )

ggplot( adsl_res, aes( RND_NBR, AGE, col = COUNTRY ) ) +
  geom_point( )
ggplot( adsl_res, aes( fct_reorder( RND_NBR, AGE ), AGE, col = fct_reorder2( COUNTRY, AGE, RND_NBR ) ) ) +
  geom_point( )

## last2() -> helper for fct_reorder2() : Finds the last value of y when sorted by x 
last2( .x, .y )

## first2() -> helper for fct_reorder2() : Finds the first value of y when sorted by x 
first2( .x, .y )
#### fct_shuffle() : Randomly permuate factor levels ####
fct_shuffle( f )

fct_shuffle( adsl_res_small$COUNTRY )
fct_shuffle( adsl_res_small$COUNTRY )

#### fct_rev() : Reverse ordering of factor ####
fct_rev( f )

fct_rev( adsl_res_small$COUNTRY )

#### fct_shift() : Mostly for cyclical factors and conventions on starting point ####
fct_shift( f, n = 1L )

x <- factor(
  c("Mon", "Tue", "Wed"),
  levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
  ordered = TRUE
)

fct_shift( x )
fct_shift( x, 2 )
fct_shift( x, -1 )


##### CHANGE VALUE OF LEVELS #####
##### CHANGE VALUE OF LEVELS #####

#### fct_anon() : Anonymise factor levels ####
fct_anon( f, prefix = "" )

gss_cat$relig %>% fct_count( )

gss_cat$relig %>%
  fct_anon( ) %>%
  fct_count( )

gss_cat$relig %>%
  fct_anon( "X" ) %>%
  fct_count( ) 

#### fct_collapse() : Collapse factor levels into manually defined groups ####
fct_collapse( .f, ..., other_level = NULL, group_other = "DEPRECATED" )

fct_count( gss_cat$partyid )
partyid2 <- fct_collapse( gss_cat$partyid,
                          missing = c( "No answer", "Don't know" ),
                          other = "Other party",
                          rep = c( "Strong republican", "Not str republican" ),
                          ind = c( "Ind,near rep", "Independent", "Ind,near dem" ),
                          dem = c( "Not str democrat", "Strong democrat" ) )

fct_count( partyid2 )
partyid2[[ c( 1, 2, 3) ]] 
