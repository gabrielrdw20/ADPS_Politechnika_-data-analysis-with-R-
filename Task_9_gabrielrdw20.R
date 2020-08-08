# github: gabrielrdw20

library('dplyr')
library('entropy')
library('tidyverse')
library('ggthemes')
library('plyr')
library('scales')
library('formattable')

dataX <- read.csv('~/ADPS/R/albums.csv')
data <- data.frame(dataX); data

RScritics <- data$rolling_stone_critic
MTVcritics <- data$mtv_critic
ManiacCritics <- data$music_maniac_critic
tabMARK <-c(0,0,0)

sumRS <- summary(RScritics)
sumMTV <- summary(MTVcritics)
sumManiacCritics <- summary (ManiacCritics)

markEntropy <- round(entropy(RScritics, MTVcritics, unit='log2'), digits = 2)

meanRS <-mean(RScritics)
meanMTV <- mean(MTVcritics)
meanManiacCritics <- mean(ManiacCritics)

sdRS <- sd(RScritics)
sdMTV <- sd(MTVcritics)
sdManiacCritics <- sd(ManiacCritics)

rs_q1 <- quantile(RScritics, probs=0.25)
rs_q3 <- quantile(RScritics, probs=0.75)

mtv_q1 <- quantile(MTVcritics, probs=0.25)
mtv_q3 <- quantile(MTVcritics, probs=0.75)

ManiacQ1 <- quantile(ManiacCritics, probs=0.25)
ManiacQ3 <- quantile(ManiacCritics, probs=0.75)

tmp <- data$genre
tmp2 <- data$year_of_pub
incompGenre <- sort(unique(tmp, incomparables = F))
incompYear <- sort(unique(tmp2, incomparables = F))

genrePopularity <- count(tmp)
summedNo <- as.numeric(sum(genrePopularity$freq))
dividedPopularity <- summedNo/as.numeric(id_sum)

allOccurences<- sum(genrePopularity$freq)

for(i in 1:38)
{
  counted[i] <- sum(genrePopularity$freq[i]/allOccurences)
}


genreT <- data.frame("Genre" = genrePopularity$x,"Total_occurrences"= genrePopularity$freq, "Occutences by percentage" = percent(counted));
marksT<-  data.frame("Name" = c("Rolling Stones", "MTV", "Music Maniac Critics"), "Mean" = c(meanRS, meanMTV, meanManiacCritics), "standard deviation" = c(sdRS, sdMTV, sdManiacCritics), "Q1"=c(rs_q1, mtv_q1,ManiacQ1), "Q3"=c(rs_q3,mtv_q3, ManiacQ3)); 


print(marksT, print.gap = 8)
print(genreT, print.gap = 8)





