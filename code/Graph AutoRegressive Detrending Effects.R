# This code is for generating graphs to show the lab group the effects of autocorrelating
pdf("Effect of Auto Regressive Detrending on long term climate.pdf")
# Detrend Auto Regressive

DR32<-detrend(Rings3200m,method="Ar")
DR35<-detrend(Rings3500m,method="Ar")
DR38<-detrend(Rings3800m,method="Ar")

# Detrend  Modified Negative Exponential
NER32<-detrend(Rings3200m,method="ModNegExp")
NER35<-detrend(Rings3500m,method="ModNegExp")
NER38<-detrend(Rings3800m,method="ModNegExp")
# show effect on indivual chronologies with and without autoregressive and Negative exponential detrending
detrend.series(Rings3200m$`09b`,method = c("Ar","ModNegEx"))
detrend.series(Rings3800m$`30c`,method = c("Ar","ModNegEx"))

#show effect on master chronology with and without Autoregressive and Negative Exponential = detrending
#3200m
# plot(chron(Rings3200m,prefix = ""),main="No Detrend Average Chronology")
# plot(chron(DR32,prefix = ""), main="Auto Regressive Detrend")
# plot(chron(NER32,prefix = ""), main="Negative Exponential Detrend")
#3500m
# plot(chron(Rings3500m,prefix = ""),main="No Detrend")
# plot(chron(DR35,prefix = ""), main="Auto Regressive Detrend")
# plot(chron(NER35,prefix = ""), main="Negative Exponential Detrend")
#3800m
plot(chron(Rings3800m,prefix = ""),main="No Detrend")
plot(chron(DR38,prefix = ""), main="Auto Regressive Detrend")
plot(chron(NER38,prefix = ""), main="Negative Exponential Detrend")

#plot(rings3800m$`21a`, main=" 3800m Individual Raw Series", type= "o")


dev.off()