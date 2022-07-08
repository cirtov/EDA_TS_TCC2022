library(tidyverse)
library(zoo)
library(ggfortify)
library(summarytools)
library(plotly)
library(psych)
library(forecast)
library(tseries)



#rain <- read.csv("C:/Users/Victor/Desktop/chuva.csv")
#unique(rain$nome_estacao_original)
#
#t <- rain %>% select(nome_estacao_original, tempo, vintequatrohoras ) %>% filter(nome_estacao_original == "MORRO DO BUMBA", vintequatrohoras != "NULL")
#sapply(t, class)
#
#bumba <-  data.frame(chuva = as.double(t$vintequatrohoras), tempo = as.POSIXct(t$tempo, tz="UTC" ))
#bumba <- bumba %>% filter(tempo <= as.Date("2021-01-01 00:00:00"))
#
#p <- ggplot(bumba, aes(x=tempo, y=chuva)) +geom_line() + xlab("")
#p
#
#eco <- economics
#head(eco)
#
#date_base <- ggplot(eco, aes(date, psavert)) + 
#  geom_line(na.rm = TRUE) +
#  labs(x = NULL, y = NULL)
#
#date_base 

birth <- read.csv("C:/Users/Victor/Desktop/births.csv")
head(birth)

mb <- birth %>% filter(gender == "M", day != "null", day != "99",births > 1000)
temp_df_birth <- mb %>% mutate(ndate = as.Date(paste(mb$year,mb$month,mb$day, sep="-")))
temp_df_birth <- temp_df_birth %>% filter(year >= 1970 & year <= 1988)
df_Birth <- data.frame(dia = as.Date(temp_df_birth$ndate), births = temp_df_birth$births)

temp_z_df <- zoo(df_Birth$births,df_Birth$dia )

zb_rm <- rollmean(temp_z_df, 90, fill = list(NA, NULL, NA))
b_cm <- cummean(df_Birth$births)

#Janela Deslisante
df_Birth$birthsRm <- coredata(zb_rm)

#Janela Expansiva
df_Birth$birthsCm <- b_cm


bf <- ggplot(df_Birth, aes(x=dia, y=births)) +geom_line() + xlab("") +
  geom_hline( yintercept= mean(df_Birth$births), color="red", size=1)+
  geom_line(aes(y = birthsRm), color = "blue",size=1)+
  geom_line(aes(y = birthsCm), color = "orange",size=1.5)+
  theme_classic()

bf


b0 <- ggplot(df_Birth, aes(x=dia, y=births)) +geom_line() + xlab("") +
  geom_hline( yintercept= mean(df_Birth$births), color="red", size=1)+
  geom_smooth(se = FALSE) +
  theme_classic()

b0

b1 <- ggplot(df_Birth, aes(x=dia, y=births)) +geom_line() + xlab("") +
  geom_line(aes(y = birthsRm), color = "blue",size=1)+
  theme_classic()

b1

# janela exp
b2 <- ggplot(df_Birth, aes(x=dia, y=births)) +geom_line() + xlab("") +
  geom_line(aes(y = birthsCm), color = "orange",size=1)+
  theme_classic()

b2


#z_airP <- zoo(AirPassengers)
#AirP <- data.frame(passageiros = coredata(zoo(AirPassengers)), passageiros_cm = cummean(AirPassengers))

#q <ggplot(AirP, aes(y = AirP$passageiros)) +geom_line() + xlab("")
          

#q


#Normalizado
hd <- ggplot(df_Birth, aes(x=scale(births))) + 
        geom_histogram(aes(y=..density..),      
        colour="black", fill="white") +
        geom_density(alpha=.2, fill="#FF6666")

hd

acf(df_Birth$births)
pacf(df_Birth$births)

s_df_birth <- data.frame(dia = df_Birth$dia,births = df_Birth$births)
q_temp <- summarytools::descr( s_df_birth)
view(summarytools::descr( s_df_birth))
####



data("EuStockMarkets")
stocks <- as.data.frame(EuStockMarkets) %>%
  gather(index, price) %>%
  mutate(time = rep(time(EuStockMarkets), 4))
plot_ly(stocks, x = ~time, y = ~price, color = ~index, mode = "lines")

###
psych::describeBy(stocks, stocks$index)

##

plot(stl(AirPassengers,"periodic"))

#mostrando a distribuição  com tendência > Dax STOCKS
shd <- ggplot(stocks, aes(x=scale(DAX))) + 
  geom_histogram(aes(y=..density..),      
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

shd

ggseasonplot(AirPassengers)

adf.test(SMI_Y$price)
SMI_Y$price
plot(SMI_Y$price)

plot( EuStockMarkets[,"SMI"], EuStockMarkets[,"DAX"] )



plot( diff(EuStockMarkets[,"SMI"]), diff(EuStockMarkets[,"DAX"]) )

plot(lag( as.vector( diff(EuStockMarkets[,"SMI"])),1), diff(EuStockMarkets[,"DAX"]))

pacf (AirPassengers)
acf (AirPassengers)

 acf(df_Birth$births)
 pacf(df_Birth$births)


 plot( diff(EuStockMarkets[,"SMI"]))
 