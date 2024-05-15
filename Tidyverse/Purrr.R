## PURRR ## 

library( tidyverse )
library( questionr )


discours <- c(
  "nous privilégierons une intergouvernementalisation sans agir anticonstitutionnellement",
  "le souffle de la nation est le vent qui agite les drapeaux de nos libertés",
  "nous devons faire preuve de plus de pédagogie pour cette réforme",
  "mon compte twitter a été piraté"
)

mots <- str_split( discours, " " )

discours %>% map_lgl( ~ { str_detect( .x,  "le" ) } )
discours %>% map_lgl( ~ str_detect( .x,  "le" ) )
discours %>% map_lgl( function( x ) { str_detect( x,  "le" ) } )

resultat <- list( )

for( item in mots ){ 
  resultat <- c( resultat, length( item ) )
  }

map( mots, length )
mots %>% map( length )


resultat_num <- numeric( length( mots ) )

for( i in seq_along( mots ) ){ resultat_num[ i ] <- length( mots[[ i ]] ) }
resultat_num

map_dbl( mots, length )
map_int( mots, length )


## Recuperer le dernier mot de chaque vecteur : 

dernier_mot <- function( x ) {
  tail( x, 1 ) 
}

rs_char <- character( length( mots ) )
for( i in seq_along( mots ) ) {  rs_char[ i ] <- dernier_mot( mots[[ i ]] ) }
rs_char

mots %>% map_chr( dernier_mot )

mots %>% map_chr( function( x ) {  tail( x , 1 ) } )

mots %>% map_chr( ~ tail( .x, 1 ) )

mots %>% map_chr( tail, 1 )


## Variantes of Map ## 

# map_int -> Vecteur atomique entier 
mots %>% map_int( length )

# map_dlb -> Vecteur atomique flottant
mots %>% map( str_count )
mots %>% map_dbl( ~ mean( str_count( . ) ) ) 

# map_char -> Vecteur atomique character 
mots %>% map_chr( tail, 1 )

# map_lgl -> Vecteur atomique TRUE/FALSE
mots %>% map_lgl( ~ length( . ) > 10  )

# VARIANTES Only gives results with 1 dimension for each element 
mots %>% map_chr( ~ str_subset( .x, "f" ) )
mots %>% map( ~ str_subset( .x, "f" ) ) %>% 
  map_chr( ~ paste( .x, collapse = " - " ) )
mots %>% map( ~ str_subset( .x, "f" ) ) %>% 
  map_int( ~ length( .x ) )


#### map_dfr() - bind_row et map_dfc() - bind_col ####

## Importation of multiple dataset 
genere_url <- function(codes) {
  paste0(
    "https://raw.githubusercontent.com/juba/tidyverse/main/resources/data/rp2018/rp2018_",
    codes,
    ".csv"
  )
}

genere_url( c( "42", "69" ) )

departements <- c( "38", "42", "69" )
urls <- genere_url( departements )

dfs <- urls %>% map( read_csv )

## Computation of mean
dfs %>% map_dbl( ~ { mean( .x$dipl_aucun ) } ) 

dfs %>% map_dbl( ~ { 
  reg <- lm( cadres ~ dipl_sup,  data = .x ) 
  reg$coefficients[ "dipl_sup" ]
  } )


## Binding importing data into one dataset 
df <- urls %>% map_dfr( read.csv )


## All Data from a file, importing and concatenate 
fichier <- list.files( "C:/Users/astos/OneDrive - JNJ/Macitentan/MACiTEPH/Data", 
                        "*.sas7bdat", full.names = T )



#### Itérer sur les colonnes d'un tableau de données ####

## DF ou Tibble = named list + vector atomique of same length -> If not list map will execute on column 

starwars %>% map_int( n_distinct )

starwars %>% summarise( across( everything(), n_distinct ) )

starwars %>% map_int( ~ { sum( is.na( .x ) ) } )
starwars %>% map_int( ~ { sum( is.na( .x ) ) } ) %>% discard( ~ .x == 0 )
starwars %>% keep( is.numeric ) %>% map_dbl( mean, na.rm = T )


#### Modify #### 

## Variant of map and result is same type as input
## Purpose: To modify without changing type

v <- c( "brouette", "moto", "igloo" )
v %>% modify( nchar )

v <- list( "brouette", "moto", "igloo" )
v %>% modify( nchar )

starwars %>% modify_if( is.character, as.factor )

starwars %>% mutate( across( where( is.character ), as.factor ) ) 

starwars %>% modify_at( c( "name", "eye_color" ), as.factor )
starwars %>% modify_at( vars( "hair_color" : "eye_color" ), as.factor )

starwars %>% 
  mutate( 
    across( "hair_color" : "eye_color" , as.factor ) )



#### imap #### 

## To retain the name of the element 

## Variant : imap_dbl() / imap_chr() / imap_dfc()

l <- list( nom1 = 1, nom2 = 3 )
l2 <- l %>% imap( function( valeur, nom ) {
  message( "La valeur de ", nom, " est ", valeur )
 } )

l2 <- l %>% imap( ~{ 
  message( "La valeur de ", .y, " est ", .x  )
  })


restos <- list(
  "La bonne fourchette"     = c(3, 3, 2, 5, 2, 3, 2, 4, 1, 3),
  "La choucroute de l'amer" = c(4, 1, 2, 4, 2, 5, 2),
  "L'Hair de rien"          = c(1, 5, 5, 1, 5, 3, 1, 5, 2),
  "La blanquette de Vaulx"  = c(4, 1, 3, 1, 3, 3, 1, 4, 2, 5)
)

restos %>% map_dfr( ~ {  
  tibble( mean = mean( .x ), 
          sd = sd( .x ) )
  })

restos %>% imap_dfr( ~ {  
  tibble( name = .y,
          mean = mean( .x ), 
          sd = sd( .x ) )
} )


restos %>% imap_dfr( function( notes, nom ){   
  tibble( name = nom,
          mean = mean( notes ), 
          sd = sd( notes ) )
} )



#### Walk ##### 

## Do not give result and only output messages, graphics, save a file.. 

walk( restos, ~ barplot( table( .x ) ) )

iwalk( restos, ~ barplot( table( .x ), main = .y ) )




#### map2 and pmap : iteration on parallele vector #### 

## Variant: map2_int() - map2_chr() - map2_dlb()


correlations <- tribble(
  ~var1,       ~var2,
  "dipl_sup", "dipl_aucun",
  "dipl_sup", "cadres",
  "hlm",      "cadres",
  "hlm",      "ouvr",
  "proprio",  "hlm"
)

correlations

## map2 / walk2 -> Iteration on 2 vectors 

map2( correlations$var1, 
      correlations$var2, 
      ~{ cor( rp2018[[.x]], rp2018[[.y]] ) } )

correlations$corr <- map2_dbl( correlations$var1, 
                               correlations$var2, 
                               ~{ cor( rp2018[[.x]], rp2018[[.y]] ) } )
correlations



walk2( correlations$var1, 
       correlations$var2, 
       ~ { p <- ggplot( data = rp2018, aes( x = .data[[ .x ]], 
                                            y = .data[[ .y ]] ) ) + 
           geom_point( ) 
           print( p ) } )


## pmap / pwalk -> Iteration over p vectors
nuages <- tribble(
  ~var1,       ~var2,       ~titre,
  "dipl_sup", "dipl_aucun", "Diplômés du supérieur x sans diplôme",
  "dipl_sup", "cadres",     "Pourcentage de cadres x diplômés du supérieur",
  "hlm",      "cadres",     "Pas facile de trouver un titre",
  "proprio",  "cadres",     "Oui non vraiment c'est pas simple"
)

# with function -> Arguments are named
pwalk(
  list( nuages$var1, nuages$var2, nuages$titre ),
  function( var1, var2, titre ) {
    plot(
      rp2018[[ var1 ]], rp2018[[ var2 ]],
      xlab = var1, ylab = var2, main = titre
    )
  }
)

# With formule -> Argument = ..x
pwalk( list( nuages$var1, nuages$var2, nuages$titre ),
       ~ { plot( rp2018[[ ..1 ]], rp2018[[ ..2 ]],
                 xlab = ..1, ylab = ..2, main = ..3 )
         } )

## If list is named 
pwalk(
  list(v1 = nuages$var1, v2 = nuages$var2, titre = nuages$titre),
  function(v1, v2, titre) {
    plot(
      rp2018[[v1]], rp2018[[v2]],
      xlab = v1, ylab = v2, main = titre
    )
  }
)



#### Répéter une opération #### 

## 10 vectors of 100 random number 

res <- list( )
for ( i in 1:10 ) {
  res[[ i ]] <- rnorm( 100 )
}

res <- map( 1:10, ~ { rnorm( 100 ) } )


l <- list(1:3, c(2, 5))
l %>% map_int(length)



#### Exercice with Purrr #####

notes <- list(
  maths = c(12, 15, 8, 10),
  anglais = c(18, 11, 9),
  sport = c(5, 13),
  musique = 14,
  techno = c(12, NA)
)

notes %>% map( mean, na.rm = T )
notes %>% map_dbl( ~ mean( .x, na.rm = T ) )
notes %>% map_dfr( ~ tibble( min = min( .x ), 
                             max = max( .x ) ) )
notes %>% imap_dfr( ~ tibble( name = .y,
                              mean = mean( .x, na.rm = T ), 
                              min = min( .x ), 
                              max = max( .x ) ) )

notes %>% imap_dfr(function( matiere, note ){
  tibble( name = note,
                              mean = mean( matiere, na.rm = T ), 
                              min = min( matiere ), 
                              max = max( matiere ) ) } )


parcours <- list(
  c("Lyon", "F1ixevi11e", "Saint-Dié-en-Poui11y"),
  c("Sainte-Gabelle-sur-Sarthe"),
  c("Décines", "Meyzieu", "Demptezieu"),
  c("Meyzieu", "Lyon", "Paris", "F1ixevi11e", "Lyon"),
  c("La Bâtie-Divisin", "Versai11es")
)

parcours %>% map_int( length )
parcours %>% map_int( ~ sum( str_detect( .x, "Lyon" ) ) )
parcours %>% map_int( ~ sum( .x == "Lyon" ) )


urls <- c(
  "https://raw.githubusercontent.com/juba/tidyverse/main/resources/data/rp2018/rp2018_01.csv",
  "https://raw.githubusercontent.com/juba/tidyverse/main/resources/data/rp2018/rp2018_69.csv"
)

dfs <- urls %>% map_dfr( read.csv )
