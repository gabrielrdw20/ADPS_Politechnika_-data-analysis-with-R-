# github: gabrielrdw20

library('dplyr')
library('tidyverse')
library('data.table')
library('psych')
library('formattable')

rm(list= ls())

# 1 ----------------------------------------
rdsFile <- readRDS('~/ADPS/R/age.rds')
print(rdsFile)

minV <- min(rdsFile, na.rm=TRUE)
maxV <- max(rdsFile, na.rm=TRUE)

print(paste0('Najmniejsza wartoœæ: ',minV))
print(paste0('Najwiêksza wartoœæ: ',maxV))

# lub po prostu describe(rdsFile) <--w tym wartoœci min i max

# 2 ---------------------------------------
meanV <- round(mean(rdsFile, na.rm=TRUE))
print(paste0('Przeciêtny (zaokr¹glony) wiek klienta banku: ',meanV, ' lat'))

# 3 ---------------------------------------

describe(rdsFile)

# 4 ---------------------------------------
withNA <- length(rdsFile); withNA
withoutNA <- length(noNA); withoutNA
noNA <- rdsFile[!is.na(rdsFile)]
lV <- length(rdsFile[!is.na(rdsFile)])
lVS <- sum(lV); lVS #checking if the sum is correct
Teen <- sum(noNA <18)
Adult <- sum(noNA >= 18)
TeenP <-  Teen/lVS
AdultP <- Adult/lVS

print(paste0('% nieletnich w ca³ej próbie [z wy³¹czeniem wartoœci NA]: ', percent(TeenP)))
print(paste0('% doros³ych w ca³ej próbie [z wy³¹czeniem wartoœci NA]: ', percent(AdultP)))

# 5 ---------------------------------------
Adult3_5 <- sum(noNA>=30 & noNA<=50)
Adult3_5P <- Adult3_5 / lVS
print(paste0('% Osób doros³ych pomiêdzy 30 a 50 rokiem ¿ycia [z wy³¹czeniem wartoœci NA]: ', percent(Adult3_5P)))


# 6 ---------------------------------------
total <- sum(withNA - withoutNA); total
print(paste0('Wieku nie poda³y: ', total, ' osoby.'))

totalP <- (total*100)/lVS
print(paste0('Osoby te stanowi¹ ', percent(totalP), ' ca³oœci próby.'))

# 7 ---------------------------------------

K16_17 <- sum(noNA >= 16 & noNA <= 17); K