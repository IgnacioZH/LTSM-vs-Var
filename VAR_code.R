library(tseries)
library(forcats)
library(vars)

#GDP

gdp<-read.table("SPAIN/GDP.csv",sep=",",header=TRUE)
gdp$DATE<-as.Date(gdp$DATE,"%Y-%m-%d")
names(gdp)<-c("DATE","GDP")
gdp$GDP=gdp$GDP/1000000

#Unemployment
Unem<-read.table("SPAIN/UNEM.csv",sep=",",header=TRUE)
Unem$DATE<-as.Date(Unem$DATE,"%Y-%m-%d")
names(Unem)<-c("DATE","Unem")

#Irate
Ecb<-read.table("SPAIN/ECB_dep.csv",sep=",",header=TRUE)
Ecb$DATE<-as.Date(Ecb$DATE,"%Y-%m-%d")
names(Ecb)<-c("DATE","i_rate")


#Debt
Debt<-read.table("SPAIN/DEBT.csv",sep=",",header=TRUE)
Debt$DATE<-as.Date(Debt$DATE,"%Y-%m-%d")
Debt$DEBT=Debt$DEBT/1000000



#We make them have the same date 
initDate<-as.Date("1999-01-01",format="%Y-%m-%d")
gdp<-gdp[gdp$DATE>=initDate,]

Debt<-Debt[Debt$DATE %in% gdp$DATE,]
Unem<-Unem[Unem$DATE %in% Debt$DATE,]
Ecb<-Ecb[Ecb$DATE %in% Debt$DATE,]
gdp<-gdp[gdp$DATE %in% Debt$DATE,]

row.names(Debt) <- NULL
row.names(Unem) <- NULL
row.names(Ecb) <- NULL
row.names(gdp) <- NULL





# This is the Df that we are going to use in Python
Df <- data.frame(gdp$DATE,gdp$GDP,Debt$DEBT,Unem$Unem,Ecb$i_rate)
colnames(Df) <- cbind("DATE","GDP","Debt","Unem", "i_rate")

write.csv(Df,"SPAIN/Data_for_Python.csv", row.names = FALSE)




#2 diff in GPD in order to obtain a stationary serie

y1<-diff(gdp$GDP[1:72])
plot(y1,type="l")
adf.test(y1)
y2<-diff(y1)
plot(y2,type="l")
adf.test(y2)



#2 diff in DEBT in order to obtain a stationary serie
plot(Debt$DEBT,type="l")
e1<-diff(Debt$DEBT[1:72])
plot(e1,type="l")
adf.test(e1)
e2<-diff(e1)
plot(e2,type="l")
adf.test(e2)



#2 diff in UNEM in order to obtain a stationary serie
plot(Unem$Unem,type="l")

u1<-diff(Unem$Unem[1:72])
plot(u1,type="l")
adf.test(u1)
u2<-diff(u1)
plot(u2,type="l")
adf.test(u2)



#2 diff in ECB_int_rate in order to obtain a stationary serie
plot(Ecb$i_rate,type="l")

in1<-diff(Ecb$i_rate[1:72])
plot(in1,type="l")
adf.test(in1)
in2<-diff(in1)
plot(in2,type="l")
adf.test(in2)








#Model creation
y<-matrix(c(y2,e2,u2,in2),c(length(y2),4))
colnames(y) <- cbind("GDP","Debt","Unem", "i_rate")

lagselect <- VARselect(y, lag.max = 16, type = "const")
lagselect$selection
Model1 <- VAR(y, p = 10, type = "const", season = NULL, exog = NULL) 
summary(Model1)
#Granger causality test
GrangerGDP<- causality(Model1, cause = "GDP")
GrangerGDP

Grangeri_rate<- causality(Model1, cause = "i_rate")
Grangeri_rate

GrangerUnem<- causality(Model1, cause = "Unem")
GrangerUnem

GrangerDebt<- causality(Model1, cause = "Debt")
GrangerDebt


#                WE MAKE ALL THE PERIODS PREDICTIONS

j=0
U=matrix(0, nrow = 8, ncol = 17)
for(i in seq(72, 88, 1)){
  
  j=j+1
  y1<-diff(gdp$GDP[1:i])
  y2<-diff(y1)
  
  e1<-diff(Debt$DEBT[1:i])
  e2<-diff(e1)
  
  u1<-diff(Unem$Unem[1:i])
  u2<-diff(u1)
  
  in1<-diff(Ecb$i_rate[1:i])
  in2<-diff(in1)
  
  #Model creation
  
  y<-matrix(c(y2,e2,u2,in2),c(length(y2),4))
  colnames(y) <- cbind("GDP","Debt","Unem", "i_rate")
  
  lagselect <- VARselect(y, lag.max = 16, type = "const")
  lagselect$selection
  Model1 <- VAR(y, p = 10, type = "const", season = NULL, exog = NULL) 
  
  
  steps=8
  forecast <- predict(Model1, n.ahead = steps, ci = 0.95)
  
  gdp_forecast=forecast$fcst$GDP[,1]
  debt_forecast=forecast$fcst$Debt[,1]
  unem_forecast=forecast$fcst$Unem[,1]
  i_rate_forecast=forecast$fcst$i_rate[,1]
  
  #We focus on unemployment
  unem_plus_forecast<-as.vector(c(u2,unem_forecast))
  unem_plus_forecast_ad1<-diffinv(unem_plus_forecast, xi=u1[1])
  unem_plus_forecast_ad2<-diffinv(unem_plus_forecast_ad1, xi=Unem$Unem[1])
  
  unem_plus_forecast_ad2[(i+1):(i+steps)]#predictions
  Unem$Unem[(i+1):(i+steps)]#real values
  
  U[,j]= Unem$Unem[(i+1):(i+steps)]-unem_plus_forecast_ad2[(i+1):(i+steps)]
  #In U we are going to store the residuals
  
  
}
U=abs(U)
rowMeans(U,na.rm=TRUE)
