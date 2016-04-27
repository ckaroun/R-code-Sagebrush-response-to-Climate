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

#show effect on chronology with and without AR detrending
#3200m
plot(chron(Rings3200m,prefix = ""),plot.type="spag",main="No Detrend")
plot(chron(DR32,prefix = ""),plot.type="spag", main="Auto Regressive Detrend")
plot(chron(NER32,prefix = ""),plot.type="spag", main="Negative Exponential Detrend")
#3500m
plot(chron(Rings3500m,prefix = ""),plot.type="spag",main="No Detrend")
plot(chron(DR35,prefix = ""),plot.type="spag", main="Auto Regressive Detrend")
plot(chron(NER35,prefix = ""),plot.type="spag", main="Negative Exponential Detrend")
#3800m
plot(chron(Rings3800m,prefix = ""),plot.type="spag",main="No Detrend")
plot(chron(DR38,prefix = ""),plot.type="spag", main="Auto Regressive Detrend")
plot(chron(NER38,prefix = ""),plot.type="spag", main="Negative Exponential Detrend")

dev.off()