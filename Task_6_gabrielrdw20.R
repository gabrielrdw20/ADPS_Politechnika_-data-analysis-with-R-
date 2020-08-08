# github: gabrielrdw20

#install.packages('mgcv')
#install.packages("e1071")
#install.packages('ehaGoF')
library(e1071)
library(ehaGoF)

c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14) -> ctl 
c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69) -> trt

gl(2, 10, 20, labels = c("Ctl","Trt")) -> group 
c(ctl, trt) -> weight

summary(lm(weight ~ group)) -> model  # Builded the model on training data



#2 coefficients - wpó³czynniki modelu
print(coef(model2))

#3 residuals - wartoœci resztowe modelu
print(residuals(model2))

#4 ------------------------------------
summary(lm(weight ~ group))$adj.r.squared -> rqmodel
print(rqmodel)
