# github: gabrielrdw20

#install.packages('data.table')
#install.packages('tidyverse')
library('dplyr')
library('tidyverse')
library('data.table')

test <- fread(sep ='%', decimal_point = '.', ("~/ADPS/R/ugly_diamonds.csv"))
as_tibble(test) -> test
glimpse(test)
