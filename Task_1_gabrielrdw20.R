# github: gabrielrdw20

# Z1 ---------------------------------------------

data1 <- readRDS('./crypto.rds')
str(data1)
any(data1$Currency == 'bitcoin')
data <- as_tibble(data1)
bitcoin <- data %>% filter(Currency == 'bitcoin')
bitcoin <- bitcoin %>% select(Date, Close)
Sys.getlocale()
bitcoin$Date <- as.Date(bitcoin$Date, format = '%B %d, %Y')
bitcoin$Rate <- with(bitcoin, c(NA, diff(Close)/Close[-length(Close)]))
bitcoin <- bitcoin[order(bitcoin$Rate, decreasing = TRUE),] 
bitcoin

# Z2 ---------------------------------------------

library(dplyr)
album <- read.csv('~/ADPS/R2/albums.csv')
ablum <- as_tibble(album)
str(album)    

tmp <- filter(album, genre == "Metal"); tmp #poszukiwanie s³owa w zbiorze, w danej kolumniestmp1 <- select(album,id,album_title,genre,num_of_tracks) # selekcjonowanie kolumn
tmp1 <- select(album,id,album_title,genre,num_of_tracks) # selekcjonowanie kolumn
tmp2 <- summary(tmp); tmp2 #podsumowanie zbioru
tmp3 <- album %>% filter(genre == "Rock") %>% select(album_title) #u¿ycie pipe lines
tmp4 <- album %>% mutate(nowaKolumna = artist_id + 5); tmp4 #tworzenie nowych kolumn na bazie istniej¹cych
tmp5 <- album %>% group_by(genre); tmp5 # grupowanie danych po wybranych typach 
tmp6 <- album %>% group_by(genre, rolling_stone_critic) %>% summarize(sr_ocena = mean(rolling_stone_critic)); tmp6 #pipe line do grupowania i wyci¹gania 
tmp7 <- album %>% group_by(artist_id) %>% filter(rolling_stone_critic == max(rolling_stone_critic) & genre == 'Rock') #poka¿ artystów,którzy otrzymali maksymalne noty na liœcie Rolling Stones i graj¹ utwory z gatunku rocka
tmp8 <- album %>% group_by(year_of_pub) %>%  arrange(desc(num_of_tracks)); tmp8 # pogrupuj nutwory latami i wypisz malej¹co wzglêdem liczby utworów
tmp9 <- album %>% filter(genre == 'Folk' & music_maniac_critic == 4.0) %>% count(); tmp9


# Z3 ---------------------------------------------

library(sqldf)
library(dplyr)
library(plyr)

# AD. 1
plik <- readRDS('~/ADPS/R2/suicides.rds')
top5 <- plik[with(plik, order(- plik$suicides.100k.pop)), ]
finalTOP5_desc <- head(top5$country, 5) # tylko nazwy krajów
finalTOP5_desc <- as.matrix(finalTOP5_desc,ncol=1); finalTOP5_desc
finalTOP5_desc2 <- head(top5, 5); finalTOP5_desc2  # ³¹czone statystyki

# AD. 2
colnames(plik)[7] <- "suicides100kpop"
byYear <- sqldf (" SELECT year, sum(suicides100kpop) as suicides
                   FROM plik
                   GROUP BY year
                   ORDER BY year DESC
                 "); byYear


# AD. 3
bySex <- sqldf ("SELECT sex, age, sum(suicides100kpop) as suicides 
                 FROM plik 
                 GROUP BY age,sex
                 ORDER BY sex DESC"); bySex

# AD. 4
library(data.table)
require(data.table)
aaa <- sqldf(" SELECT *
             FROM (SELECT *
             FROM (SELECT year,country, sum(suicides_no) as suicides_sum 
             FROM plik GROUP BY country, year ORDER BY year, suicides_sum DESC))
             GROUP BY country, year
             ORDER BY year, suicides_sum DESC
             ");aaa

d <- by(aaa, aaa["year"], head, n=3); d

# AD. 5
tttMAX <- sqldf("SELECT country, MAX(suicides100kpop) as suicidesmax FROM plik GROUP BY country ORDER BY country ASC");tttMAX
tttMIN <- sqldf("SELECT country, MIN(suicides100kpop) as suicidesmin FROM plik GROUP BY country ORDER BY country ASC");tttMIN
ttt2 <- sqldf("SELECT tttMAX.country, suicidesmax, suicidesmin FROM tttMAX JOIN tttMIN ON tttMAX.country = tttMIN.country GROUP BY tttMAX.country ORDER BY tttMAX.country ASC")
ttt2$substract <- (ttt2$suicidesmax - ttt2$suicidesmin )
finMAX <- sqldf("SELECT country, MAX(substract) FROM ttt2"); finMAX #maksymalna ró¿nica 
finMIN <- sqldf("SELECT country, MIN(substract) FROM ttt2"); finMIN #mininalna ró¿nica ! Nie mo¿na zastosowaæ dense_rank i odró¿niæ pozosta³ych krajów z wartoœci¹ 0

# Z4 ---------------------------------------------

library(dplyr)
library(plyr)
df <- readRDS('./norat_apps.rds')
df1 <- readRDS('./free_apps.rds')
df2 <- readRDS('./paid_apps.rds')
dd1 <- rbind.fill(df,df1,df2)
final <- as.matrix(dd1, ncol=5)
write.csv(final, file='~/ADPS/R2/merged.csv', row.names = F)
read.csv("merged.csv", header = T, sep = "," )

# Z5 ---------------------------------------------

library(dplyr)
tags <- readRDS('~/ADPS/R2/tags.rds')
movies <- readRDS('~/ADPS/R2/movies.rds')
ratings <- readRDS('~/ADPS/R2//ratings.rds')

# AD. 1
sr_r <- ratings %>% group_by(movieId) %>% summarise(sr_r = mean(rating)) 
uzyt <- ratings %>% group_by(movieId) %>% summarise(uzyt = n_distinct(userId))
sr_r_uzyt <- sr_r %>% inner_join(uzyt, by = c('movieId' = 'movieId'))
movies <-movies %>% left_join(sr_r_uzyt, by = c('movieId' = 'movieId'))

# AD.2
tags %>% group_by(movieId) %>% summarise(last_timestamp = max(timestamp)) -> last_timestamp
movies %>% left_join(last_timestamp, by = c('movieId' = 'movieId')) -> movies