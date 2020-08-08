# github: gabrielrdw20

library('roperators')
library('dplyr')

K <- 20000
r <- 0.25
n <- 360

x <- "Wysokoœæ czêœci kapita³owej raty: "
R0 <- K/n
R0F <- round(R0, digits = 2)

i <- readline(prompt='Podaj numer i-tej raty: ') # --- PODAJ WARTOŒC I ZANIM PRZEJDZIESZ DALEJ -----------------------
a <- i
x1 <- paste('Wysokoœæ czêœci odsetkowej raty', a, ': ')
i <- as.numeric(as.character(i))
Ri <- ((K- (i-1) * R0)*r)/12
RiF <- round(Ri, digits = 2)

y <- paste('Wysokoœæ raty ', a)
R <- Ri + R0
RF <- round(R, digits = 2)

z <- 'Ca³kowita kwota do sp³aty: '
start = 0;
for (s in 1:n) 
{
  
  start <-start+(
    (Ri <- ((K- (s-1) * R0)*r)/12) + R0)
}
Rx <- start
RxF <- round(Rx, digits = 2)



# Wyniki -----------------------
print(paste0(x,R0F,' z³')) # 1
print(paste0(x1,RiF,' z³')) # 2
print(paste0(y,RF,' z³')) # 3
print(paste0(z,RxF,' z³')) # 4



