# github: gabrielrdw20

library('dplyr')
library('tidyverse')
library('tidyr')
library('lubridate')
library('stringr')
library('strsplit')
library('base')


bankData <- readRDS('~/ADPS/R/bank_register.rds')
bankData2 <- data.frame(bankData)
bankData2 %>% separate(id, c("client_id", "agreement_id"), "_") -> newBankData
date1<- str(newBankData$date)
newBankData$date <- noquote(as.Date(parse_date_time(newBankData$date, c('mdy', 'Y-m-d'))))
newBankData$income <- str_replace(newBankData$income, "[.]", "")
newBankData$income <- str_replace(newBankData$income, "[,]", ".")
newBankData$income <- str_replace(newBankData$income, "[$,]", "")
newBankData$income <- as.numeric(newBankData$income)
newBankData$products[newBankData$products == ""] <- NA

newBankData %>% separate(demographic, c('sex','age','child'),",") -> newBankData
newBankData %>% separate(products, c('CRE','DEP','MOR'),",") -> newBankData


strA <- matrix(newBankData$products)
len <- as.numeric(length(newBankData$CRE))

for(i in 1:len)
{
  
  c1<- newBankData$CRE[i]
  ctmp <- toString(c1)
  
  d1<- newBankData$DEP[i]
  dtmp <- toString(d1)
  
  m1<- newBankData$MOR[i]
  mtmp <- toString(m1)
  
  
  if(ctmp == "CRE" || dtmp == "CRE" || mtmp == "CRE" )
  {
    newBankData$CRE[i] <- 'TRUE'
  } else 
  {
    newBankData$CRE[i] <- 'FALSE'
  }
  
  
  if(ctmp == "DEP" || dtmp == "DEP" || mtmp == "DEP" )
  {
    newBankData$DEP[i] <- 'TRUE'
  } else 
  {
    newBankData$DEP[i] <- 'FALSE'
  }
  
  
  if(ctmp == "MOR" || dtmp == "MOR" || mtmp == "MOR" )
  {
    newBankData$MOR[i] <- 'TRUE'
  } else 
  {
    newBankData$MOR[i] <- 'FALSE'
  }
  
}  


print(newBankData, print.gap = 8)


