
require("dplR")

# #test
# test<-read.rwl("data/3200m.rwl")

# #NO PITH ALTERNATIVE TO NORMAL RINGS LOOKS LIKE THEY CROSS DATE A LITTLE WORSE BUT SOME OF THE RINGS WERE RIDICULOUSLY PLACED ON Star pith formation and middle of pith
rings3200m<-read.rwl("data/3200m_w_o_pith_rings.rwl",format = "tucson")
rings3500m<-read.rwl("data/3500m_w_o_pith_rings.rwl",format = "tucson")
rings3800m<-read.rwl("data/3800m_w_o_pith_rings.rwl",format = "tucson")

# ## original rings with pith
# 
# rings3200m<-read.rwl("data/3200m.rwl")
# rings3500m<-read.rwl("data/3500m.rwl")
# rings3800m<-read.rwl("data/3800m.rwl")

# write.csv(rings3800m,"White Mountains Artemisia Rings 3800m.csv")
# write.csv(rings3500m,"White Mountains Artemisia Rings 3500m.csv")
# write.csv(rings3200m,"White Mountains Artemisia Rings 3200m.csv")
#plyr::ddply(chrons,.(Year,Ring.Width.Index),summarise,average=               chrons["Species"]<-c("")           

# do a ring curve standarsization which is for preserving climate trends that are longer than the length of one chronology by not dividing by a series curve but the mean size of a ring width given its age.
rings3200mrcs<-rcs(rwl=rings3200m,po=data.frame(series=names(rings3200m), offset=c(4,4,1,1,6,6,4,4,5,5,4,4,18,18,1,1,1,1,1,1)))# Took 8a outlier out so that it is 20 instead of 21 samples po data taken from Jeff's plant and soil folder on google drive > data for ms> White Mountains Artemisia 3200m,3500m and 3800m
rings3500mrcs<-rcs(rwl=rings3500m,po=data.frame(series=names(rings3500m),offset=1)) # pith on all of 3500m samples
rings3800mrcs<-rcs(rwl= rings3800m, po=data.frame(series=names(rings3800m),offset=1)) #pith on all of 3200m samples

# Visually check rings
plot(rings3200m,plot.type="spag")
plot(rings3200mrcs,plot.type="spag")
plot(rings3500m,plot.type="spag") 
plot(rings3500mrcs,plot.type="spag")
plot(rings3800m,plot.type="spag")
plot(rings3800mrcs,plot.type="spag")


ChronMaker<-function(rings3200m,elevation,detrend=FALSE, method="ModNegExp", rcs=FALSE){
  if(detrend){
    # detrend become the input value for the rest of the function
    rings3200m<-detrend(rings3200m,method=method, verbose=FALSE,make.plot = FALSE)
  }
  # rings3200m chron32 is just a vesitigual name to make debuging easier. they can contain data from any elevation
  chron32<-chron(rings3200m,prefix="")
  # Rename data column
  names(chron32)[1]<-"Ring.Width.Index"
  # Create Source column
  chron32$Source<-rep(elevation,length(chron32[,1]))
  # create Year column
  chron32$Year<-as.numeric(rownames(chron32))
  # rearrange columns so samp.depth is on the end
  chron32<-chron32[c("Ring.Width.Index","Source","Year","samp.depth")]
  
  return(chron32)
}
#none detrened
chron32<-ChronMaker(rings3200m,"3200m radial growth")#This many: "                                                                            "spaces to align the white mountain Precip and growth graphs
chron35<-ChronMaker(rings3500m,"3500m radial growth")
chron38<-ChronMaker(rings3800m,"3800m radial growth")
chrons<-Reduce(function(x,y) rbind(x,y),list(chron32,chron35,chron38))

#neg exponential detrend
chron32<-ChronMaker(rings3200m,"3200m radial growth",detrend=TRUE,method = "ModNegExp")#This many: "                                                                            "spaces to align the white mountain Precip and growth graphs
chron35<-ChronMaker(rings3500m,"3500m radial growth",detrend=TRUE,method = "ModNegExp")
chron38<-ChronMaker(rings3800m,"3800m radial growth",detrend=TRUE,method = "ModNegExp")
chronsNeg<-Reduce(function(x,y) rbind(x,y),list(chron32,chron35,chron38))

# autoregressive detrend
chron32<-ChronMaker(rings3200m,"3200m radial growth",detrend=TRUE,method = "Ar")#This many: "                                                                            "spaces to align the white mountain Precip and growth graphs
chron35<-ChronMaker(rings3500m,"3500m radial growth",detrend=TRUE,method = "Ar")
chron38<-ChronMaker(rings3800m,"3800m radial growth",detrend=TRUE,method = "Ar")
chronsAr<-Reduce(function(x,y) rbind(x,y),list(chron32,chron35,chron38))
 
# regional curve standardization (cambial age dependent stadardization)
chron32rcs<-ChronMaker(rings3200mrcs,"3200m radial growth")#This many: "
chron35rcs<-ChronMaker(rings3500mrcs,"3500m radial growth")#This many: "
chron38rcs<-ChronMaker(rings3800mrcs,"3800m radial growth")#This many: " 
chronsrcs<-Reduce(function(x,y) rbind(x,y),list(chron32,chron35,chron38))
##############################################################
# #MAKE THE PLOTS only once
# # writes fiel to pdf
# pdf(file = "3200m detrending.pdf")
# detrend(rings3200m,verbose=TRUE,make.plot = TRUE)
# # turn device off to stop writing to pdf
# dev.off()
# 
# pdf(file = "3500m detrending.pdf")
# detrend(rings3500m,verbose=TRUE,make.plot = TRUE)
# dev.off()
# 
# pdf(file = "3800m detrending.pdf")
# detrend(rings3800m,verbose=TRUE,make.plot = TRUE)
# dev.off()