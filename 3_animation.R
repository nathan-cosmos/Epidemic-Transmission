library("networkDynamic")
library("ndtv")
I2$f<-max(c(I2$dateg,c(I2$date.infected,I2$date.spreader)))

I2$col<-""
I2$col[I2$Type=="MED"]<-"red"
I2$col[I2$Type=="ADM"]<-"blue"
I2$col[I2$Type=="NUR"]<-"green"
I2$col[I2$Type=="PAT"]<-"yellow"


raw3<-cbind(floor(I2$date.infected/3600),floor(I2$f/3600),I2$contaminateur+1,I2$ID+1)
nd2<-networkDynamic(edge.spells=raw3)
set.vertex.attribute(nd2,"col",I2$col)

compute.animation(nd2, animation.mode = "kamadakawai",
                  slice.par=list(start=0, end=95, interval=1, 
                         aggregate.dur=1, rule='any'))



render.animation(nd2,displaylabels=FALSE, vertex.col = nd2 %v% "col", vertex.cex = function(slice){ sign(degree(slice)) },) 
saveVideo(render.animation(nd2,displaylabels=FALSE, vertex.col = nd2 %v% "col", vertex.cex = function(slice){ sign(degree(slice)) },) 
, video.name = "animation1.mp4", img.name = "Rplot",ffmpeg="/Users/nathan/Downloads/SnowLeopard_Lion_Mountain_Lion_Mavericks_Yosemite_El-Captain_28.12.2015/ffmpeg")

plot(nd2)