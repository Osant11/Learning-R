# TIDYR ----
library( tidyverse )

sessionInfo()

## pivot_longer() : increasing the number of rows and decreasing the number of columns ----
relig_income
relig_income %>%
  pivot_longer( !religion, names_to = "income", values_to = "count" )


billboard
billboard %>%
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    names_prefix = "wk", #to drop prefix 
    values_to = "rank",
    values_drop_na = TRUE #to drop missing
  )


who
who %>% pivot_longer(
  cols = new_sp_m014:newrel_f65,
  names_to = c( "diagnosis", "gender", "age" ), #multiple values
  names_pattern = "new_?(.*)_(.)(.*)", #pattern to define how to broken-up the columns into the names_to
  values_to = "count", 
  values_drop_na = TRUE
)


anscombe
anscombe %>%
  pivot_longer(
    everything(),
    cols_vary = "slowest",
    names_to = c(".value", "set"),
    names_pattern = "(.)(.)"
  )

### Syntax ----
pivot_longer(
  data,
  cols,
  ...,
  cols_vary = "fastest",
  names_to = "name",
  names_prefix = NULL,
  names_sep = NULL,
  names_pattern = NULL,
  names_ptypes = NULL,
  names_transform = NULL,
  names_repair = "check_unique",
  values_to = "value",
  values_drop_na = FALSE,
  values_ptypes = NULL,
  values_transform = NULL
)



## pivot_wider() : increasing the number of columns and decreasing the number of rows ----
fish_encounters 
fish_encounters %>%
  pivot_wider( names_from = station, values_from = seen )

fish_encounters %>%
  pivot_wider( names_from = station, values_from = seen, values_fill = 0 )

us_rent_income 
us_rent_income %>%
  pivot_wider(
    names_from = variable,
    values_from = c( estimate, moe )
  )

us_rent_income %>%
  pivot_wider(
    names_from = variable,
    names_sep = ".",
    values_from = c( estimate, moe )
  )

us_rent_income %>%
  pivot_wider(
    names_from = variable,
    names_glue = "{variable}_{.value}",
    values_from = c(estimate, moe)
)

us_rent_income %>%
  pivot_wider(
    id_cols = !c( "moe", "GEOID", "NAME" ), 
    names_from = variable,
    values_from = estimate,
    #values_fn = mean 
    values_fn = ~ mean( .x, na.rm = TRUE )
  )

us_rent_income %>%
  pivot_wider(
    names_from = variable,
    values_from = c( estimate, moe),
    names_vary = "slowest"
  )

### Syntax ----
pivot_wider(
  data,
  ...,
  id_cols = NULL,
  id_expand = FALSE,
  names_from = name,
  names_prefix = "",
  names_sep = "_",
  names_glue = NULL,
  names_sort = FALSE,
  names_vary = "fastest",
  names_expand = FALSE,
  names_repair = "check_unique",
  values_from = value,
  values_fill = NULL,
  values_fn = NULL,
  unused_fn = NULL
)

