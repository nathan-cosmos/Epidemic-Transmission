### Import library/packages/installing....
library(igraph)
library(plyr)
setwd("C:/Users/Marie/Desktop/Analyse des réseaux")
library(rgexf)
library("networkDynamic")
##read data
data <- read.delim("Hp.dat", header = FALSE, sep="\t",stringsAsFactors=FALSE) 


##Nomme les colonnes
names(data)<-c("date","N1","N2","T1","T2")

length(data$N1)


# fonction qui prend une vecteur de dates et qui retourne un vecteur
# avec le numero de l'interaction 
f<-function(a)
{
  d<-c()
  l<-1
  d[1]<-l
  if (length(a)>1)
  {for (i in 2:(length(a)))
    
    if (a[i-1]+20==a[i])
    {d[i]<-l}
    else
    {l<-l+1
    d[i]<-l}
  }
  return (d)
}

dd<-ddply(.data=data, .variable=.(N1,N2), function(x){data.frame(x,N=f(x$date))})
dd<-ddply(.data=dd, .variable=.(N1,N2,N), function(x){data.frame(x,l=length(x$date)*20)})
dd<-ddply(.data=dd, .variable=.(N1,N2,N), function(x){data.frame(x,m=min(x$date))})

## effa?age de variables inutiles, de doublons et changement d'ordre

dd$date<-NULL
dd$N<-NULL
dd<-unique(dd)
dd <- dd[order(dd$m),]
head(dd)

##Creation base d'individus

I<-unique(rbind(data.frame(ID=dd$N1,Type=dd$T1),data.frame(ID=dd$N2,Type=dd$T2)))
I$etat<-0
I$dateg<-0
I$datem<-999999999
I$Type<-as.character(I$Type)
I$datei<-0
I$contaminateur<-0

## Statistiques descriptives 

# la durée totale de l'ensemble des échanges en minutes
sum(dd$l)/60

# la durée totale selon la relation considérée (ex pour MED-ADM)
sum(dd[(dd$T1=="MED" & dd$T2=="ADM")|(dd$T2=="MED" & dd$T1=="ADM"),]$l)

# le nombre d'échanges selon la relation 
length(dd[(dd$T1=="MED" & dd$T2=="ADM")|(dd$T2=="MED" & dd$T1=="ADM"),]$l)

# graphique durée des contacts en fonction du temps - plot
plot(dd$m/3600, dd$l/60, xlab = "temps en heures", ylab = "durée des contacts en minutes", type = "p", pch=1)
plot(13+dd[dd$l<=1200,]$m/3600, dd[dd$l<=1200,]$l/60, xlab = "temps en heures", ylab = "durée des contacts en minutes",
     type = "p", pch=1)

# graphique durée des contacts en fonction du temps - ggplot
install.packages("ggplot2")
library(ggplot2)
p <- ggplot(data=dd[dd$l<=1800,], aes(x=13+m/3600, y =l/60))
p <- p  + geom_point(size=2)
r <- ggplot(data=dd[dd$l<=1800,], aes(x=13+m/3600, y =l/60))
r <- r + geom_line()
#p <- p + ggtitle("Durée des contacts en fonction du temps")
p <- p + xlab("Temps en heures")
p <- p + ylab("Durée en minutes")
p <- p + scale_x_continuous(breaks=c(13,24,36,48,60,72,84,96))
print(p)


# on attribue à chaque relation les caractéristiques des interlocuteurs : variable v
dd$v<-""
dd[(dd$T1 == "MED" & dd$T2 =="PAT")|(dd$T2 == "MED" & dd$T1 =="PAT"),]$v <- "MED-PAT"
dd[(dd$T1 == "MED" & dd$T2 =="ADM")|(dd$T2 == "MED" & dd$T1 =="ADM"),]$v <- "MED-ADM"
dd[(dd$T1 == "MED" & dd$T2 =="NUR")|(dd$T2 == "MED" & dd$T1 =="NUR"),]$v <- "MED-INF"
dd[(dd$T1 == "NUR" & dd$T2 =="ADM")|(dd$T2 == "NUR" & dd$T1 =="ADM"),]$v <- "INF-ADM"
dd[dd$T1 == "MED" & dd$T2 =="MED",]$v <- "MED-MED"
dd[dd$T1 == "PAT" & dd$T2 =="PAT",]$v <- "PAT-PAT"
dd[dd$T1 == "NUR" & dd$T2 =="NUR",]$v <- "INF-INF"
dd[dd$T1 == "ADM" & dd$T2 =="ADM",]$v <- "ADM-ADM"


# on crée une nouvelle base dd2 pour déclarer un réseau
# on crée une nouvelle base dd3 où on a supprimé les couples doublons
dd2<- dd
dd2$T1 <- NULL
dd2$T2 <- NULL
dd3 <- unique(dd2[,1:2])

# conversion en graph
library(igraph)
G <- graph.data.frame(dd2, directed=FALSE, vertices=NULL)
F <- graph.data.frame(dd3, directed=FALSE, vertices=NULL)


# STATISTIQUES DESCRIPTIVES
#degree distribution
hist(degree(F), col="pink", xlab=("Degré"), xlim=c(0,70), ylab=("Effectif"), main=("") )
hist(degree(G), col="pink", xlim=c(0,2000), xlab=("Degré"), ylab=("Effectif"), main=("") )

#plus court chemin 
average.path.length(F, directed=FALSE, unconnected=TRUE)
#betweenness
between <- betweenness(G)
#degré
deg <- degree(G)
mean(deg)

#on crée un nouveau data frame qui à chaque ind associe son type, son degré et la betweenness centrality
fr <- data.frame(Num = I$ID, type = I$Type, degré = deg, betw = between)

# degré moyen selon type
mean(fr[fr$type=="NUR",]$degré)
mean(fr[fr$type=="PAT",]$degré)
mean(fr[fr$type=="MED",]$degré)
mean(fr[fr$type=="ADM",]$degré)
mean(fr$degré)


library(ggplot2)
#graphique distribution degré en fonction du statut de l'individu
w <- ggplot(fr, aes(x=degré, fill=type, color=type)) + geom_histogram(position="stack", alpha=0.5)
#w <- w + ggtitle("Distribution du degré en fonction du statut de l'individu")
w <- w + xlab("Degré") + scale_x_continuous(breaks=c(0,10,20,30,40,50,60))
w <- w + ylab("Effectif")
print(w)
# http://www.statmethods.net/advgraphs/ggplot2.html


# graphique betweenness centrality
v <- ggplot(fr, aes(x=reorder(Num, -betw), y=betw)) + geom_point(aes(color=type))
v <- v + theme(axis.text.x = element_text(angle = 90, hjust = 1))
v <- v + xlab("Numéro d'identification") + ylab("Betweenness Centrality")
print(v)
#on identifie les 7 principaux selon def betweenness centrality
print(fr[fr$betw>90,]$Num) #1157 1164 1193 1115 1207 1295 1210



# durée totale superspreaders
a<- sum(dd[(dd$N1 =="1157" |dd$N2=="1157"),]$l/60) 
b <- sum(dd[(dd$N1 =="1164" |dd$N2=="1164"),]$l/60)
ab <- sum(dd[(dd$N1 =="1193" |dd$N2=="1193"),]$l/60)
ba <- sum(dd[(dd$N1 =="1115" |dd$N2=="1115"),]$l/60)
c <- sum(dd[(dd$N1 =="1207" |dd$N2=="1207"),]$l/60)
ac <- sum(dd[(dd$N1 =="1295" |dd$N2=="1295"),]$l/60)
ca <- sum(dd[(dd$N1 =="1210" |dd$N2=="1210"),]$l/60)
(a+b+ab +ba +c+ac+ca)/sum(dd$l/60)
# pourcentage de la durée dû aux super spreaders : 67%

ka<- length(dd[(dd$N1 =="1157" |dd$N2=="1157"),]$l) 
kb <- length(dd[(dd$N1 =="1164" |dd$N2=="1164"),]$l)
kab <- length(dd[(dd$N1 =="1193" |dd$N2=="1193"),]$l)
kba <- length(dd[(dd$N1 =="1115" |dd$N2=="1115"),]$l)
kc <- length(dd[(dd$N1 =="1207" |dd$N2=="1207"),]$l)
kac <- length(dd[(dd$N1 =="1295" |dd$N2=="1295"),]$l)
kca <- length(dd[(dd$N1 =="1210" |dd$N2=="1210"),]$l)
(ka+kb+kab +kba +kc+kac+kca)/length(dd$l) 
# pourcentage du nombre de contacts dû aux super spreaders : 63%

bka<-   length(dd[((dd$N1 =="1157" |dd$N2=="1157")&(dd$T1=="PAT" | dd$T2 == "PAT")),]$l) 
bkb <-  length(dd[((dd$N1 =="1164" |dd$N2=="1164")&(dd$T1=="PAT" | dd$T2 == "PAT")),]$l)
bkab <- length(dd[((dd$N1 =="1193" |dd$N2=="1193")&(dd$T1=="PAT" | dd$T2 == "PAT")),]$l)
bkba <- length(dd[((dd$N1 =="1115" |dd$N2=="1115")&(dd$T1=="PAT" | dd$T2 == "PAT")),]$l)
bkc <-  length(dd[((dd$N1 =="1207" |dd$N2=="1207")&(dd$T1=="PAT" | dd$T2 == "PAT")),]$l)
bkac <- length(dd[((dd$N1 =="1295" |dd$N2=="1295")&(dd$T1=="PAT" | dd$T2 == "PAT")),]$l)
bkca <- length(dd[((dd$N1 =="1210" |dd$N2=="1210")&(dd$T1=="PAT" | dd$T2 == "PAT")),]$l)
(bka+bkb+bkab +bkba +bkc+bkac+bkca)/length(dd[(dd$T1=="PAT" | dd$T2 == "PAT"),]$l) 
# pourcentage du nombre de contacts avec patients dû aux super spreaders : 35%

abka<-   sum(dd[((dd$N1 =="1157" |dd$N2=="1157")&(dd$T1=="PAT" | dd$T2 == "PAT")),]$l/60) 
abkb <-  sum(dd[((dd$N1 =="1164" |dd$N2=="1164")&(dd$T1=="PAT" | dd$T2 == "PAT")),]$l/60)
abkab <- sum(dd[((dd$N1 =="1193" |dd$N2=="1193")&(dd$T1=="PAT" | dd$T2 == "PAT")),]$l/60)
abkba <- sum(dd[((dd$N1 =="1115" |dd$N2=="1115")&(dd$T1=="PAT" | dd$T2 == "PAT")),]$l/60)
abkc <-  sum(dd[((dd$N1 =="1207" |dd$N2=="1207")&(dd$T1=="PAT" | dd$T2 == "PAT")),]$l/60)
abkac <- sum(dd[((dd$N1 =="1295" |dd$N2=="1295")&(dd$T1=="PAT" | dd$T2 == "PAT")),]$l/60)
abkca <- sum(dd[((dd$N1 =="1210" |dd$N2=="1210")&(dd$T1=="PAT" | dd$T2 == "PAT")),]$l/60)
(abka+abkb+abkab +abkba +abkc+abkac+abkca)/sum(dd[(dd$T1=="PAT" | dd$T2 == "PAT"),]$l/60) 
# pourcentage du nombre de contacts avec patients dû aux super spreaders : 38%

v <- ggplot(fr, aes(x=reorder(Num, -degré), y=degré)) + geom_point(aes(color=type))
v <- v + theme(axis.text.x = element_text(angle = 90, hjust = 1))
v <- v + xlab("Numéro d'identification") + ylab("Degré")
print(v)
# on identifie les 7 principaux selon le degré
print(fr[fr$degré>885,]$Num) #1157 1144 1164 1193 1115 1207 1295 1210




##fonction qui change les ID en 1,2....75

or<-unique(I$ID)
ren<-function(x)
{	
  u<-1	
  for (i in or)
  {
    x[x==i]<-u
    u<-u+1
  }
  return(x)	
}
dd$N1<-ren(dd$N1)
dd$N2<-ren(dd$N2)
I$ID<-ren(I$ID)