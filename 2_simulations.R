####SIMULATION
library("ggplot2")

##fonction qui dirige la proba d'etre infect??
pm<-rep(1, 16)*0.0003
f<-function(k,j,debut,proba=pm)
{	
	a<-0
	mul<-dd[dd$N1==k & dd$N2==j & dd$m==debut,]$l
	if(is.null(mul)==TRUE){mul<-dd[dd$N1==j & dd$N2==k & dd$m==debut,]$l}
	if (I[I$ID==k,]$Type=="MED" & I[I$ID==j,]$Type=="MED"){a<-proba[1]}
	if (I[I$ID==k,]$Type=="MED" & I[I$ID==j,]$Type=="NUR"){a<-proba[2]}
	if (I[I$ID==k,]$Type=="MED" & I[I$ID==j,]$Type=="PAT"){a<-proba[3]}
	if (I[I$ID==k,]$Type=="MED" & I[I$ID==j,]$Type=="ADM"){a<-proba[4]}
	if (I[I$ID==k,]$Type=="NUR" & I[I$ID==j,]$Type=="MED"){a<-proba[5]}
	if (I[I$ID==k,]$Type=="PAT" & I[I$ID==j,]$Type=="MED"){a<-proba[6]}
	if (I[I$ID==k,]$Type=="ADM" & I[I$ID==j,]$Type=="MED"){a<-proba[7]}
	if (I[I$ID==k,]$Type=="NUR" & I[I$ID==j,]$Type=="NUR"){a<-proba[8]}
	if (I[I$ID==k,]$Type=="NUR" & I[I$ID==j,]$Type=="ADM"){a<-proba[9]}
	if (I[I$ID==k,]$Type=="NUR" & I[I$ID==j,]$Type=="PAT"){a<-proba[10]}
	if (I[I$ID==k,]$Type=="PAT" & I[I$ID==j,]$Type=="NUR"){a<-proba[11]}
	if (I[I$ID==k,]$Type=="ADM" & I[I$ID==j,]$Type=="NUR"){a<-proba[12]}
	if (I[I$ID==k,]$Type=="ADM" & I[I$ID==j,]$Type=="ADM"){a<-proba[13]}
	if (I[I$ID==k,]$Type=="PAT" & I[I$ID==j,]$Type=="ADM"){a<-proba[14]}
	if (I[I$ID==k,]$Type=="ADM" & I[I$ID==j,]$Type=="PAT"){a<-proba[15]}
	if (I[I$ID==k,]$Type=="PAT" & I[I$ID==j,]$Type=="PAT"){a<-proba[16]}
return(1-(1-a)^mul)}

## Fonction determinant la proba de guerison

pg<-c(0.00001,0.00001,0.00001,0.00001)
g<-function(k,p=pg,t)
{
  tt<-t-max(unique(append(0,dd$m)[append(0,dd$m)<t]))
	a<-0
	for (i in 1:length(unique(I$Type)))
	{
		if (I[I$ID==k,]$Type==unique(I$Type)[i])
		{
			a<-p[i]
		} 	
	}
	return(1-(1-a)^tt)
}


###FONCTION qui simule la transmission de la maldie
#n= individu initial malade
#I base des individus
#dd base des contacts
#f fonction de transmission
#g fonction de guerison
transmission<-function(n,I,dd,f,g)
{	
  #Initialisation du premier malade
	I$etat[I$ID==n]<-2
	I$datem[I$ID==n]<-1
	I$contaminateur[I$ID==n]<-n
	FIN<-I
	
	#a chaque instant
	for (i in (unique(dd$m)))
	{	#pour toutes les personnes infect??es mais non infectieuses
		for(j in subset(I,etat==1)$ID)
		{ #si le delai d'incubation est depas??,
			if (i>I$datei[I$ID==j])
				{ #Je suis infectieux
					I$etat[I$ID==j]<-2
				}
		}
    # Pour tous les infectieux
		for(j in subset(I,etat==2)$ID)	
		{ #Simulation d'une uniforme
			q<-runif(1,0,1)
			# Si la proba de guerison est > a l'uniforme
			if(q<g(j,pg,i) & I$etat[I$ID==j]==2)
			{# Je gueris et j'enregistre la date de guerison
				I$etat[I$ID==j]<-3
				I$dateg[I$ID==j]<-i
		}# Si je ne gueris pas
		else
		{# pour tous les indicidus qui ont un contact avec moi 
		  for (k in  subset(dd,N1==j & m==i)$N2)
		  {#Si ils sont sains
					
					if (I$etat[I$ID==k]==0) 
					{	#Probabilit?? d'infection
					  l<-runif(1,0,1)
						if (l<f(j,k,i))
						{# Si infection, l'individu est infect?? mais non
						  #infectueux, j'enregistre la date et je genere un delai 
						  #d'incubation et j'enregistre l'infecteur
							I$etat[I$ID==k]<-1
							I$datem[I$ID==k]<-i
							I$datei[I$ID==k]<-i+rweibull(1,2.21,1.10)*3600*5
							I$contaminateur[I$ID==k]<-j
						}
					}
				}
      # Idem que precedament sauf que c'est "l'autre cot?? des contacts"
		  for (k in  subset(dd,N2==j & m==i)$N1)
				{
					l<-runif(1,0,1)
					if (l<f(k,j,i))
					{	
						if (I$etat[I$ID==k]==0)
						{
							I$etat[I$ID==k]<-1
							I$datem[I$ID==k]<-i
							I$datei[I$ID==k]<-i+rweibull(1,2.21,1.10)*3600*5
							I$contaminateur[I$ID==k]<-j
						}
					}
		
				}

			}
		}
	
	}
	# Renomme les variables, et je retourne une table d'individus 
	#avec l'infecteur, la date d'infection, le temps d'incubation
	#leur type et la date de guerison
  names(I)<-c("ID","Type","etat","dateg", "date.infected","date.spreader","contaminateur")
  return(I)
}
# ESSAIS
I2<-transmission(1,I,dd,f,g)
I2<-I2[I2$contaminateur!=0,]
length(I2[I2$contaminateur!=0,]$contaminateur)
gra<-graph.data.frame(I2[c(2:length(I2$ID)),c(7,1)])
average.path.length(gra,directed=TRUE)
betweenness(gra,directed=TRUE)
plot(gra)
graph.density(gra)
#SIMULATIONS SANS VACCIN
taille<-c()
pathl<-c()
dens<-c()
for (i in 1:100)
{
  t<-transmission(1,I,dd,f,g)
  t<-t[t$contaminateur!=0,]
  taille[i]<-length(t$ID)
  gra<-graph.data.frame(t[c(2:length(t$ID)),c(7,1)])
  pathl[i]<-average.path.length(gra,directed=TRUE)
  dens[i]<- graph.density(gra)
}

#Creation table individus vaccin??s (infirmi??res)
Iv<-I
Iv$etat[Iv$Type=="NUR"]<-3


#SIMULATIONS AVEC VACCIN

taillev<-c()
pathlv<-c()
densv<-c()
for (i in 1:100)
{
  t<-transmission(1,Iv,dd,f,g)
  t<-t[t$contaminateur!=0,]
  taillev[i]<-length(t$ID)
  gra<-graph.data.frame(t[c(2:length(t$ID)),c(7,1)])
  pathlv[i]<-average.path.length(gra,directed=TRUE)
  densv[i]<- graph.density(gra)
}


#Simulations avec vaccin spreders
Is<-I
Is$etat[Iv$ID==9]<-3
Is$etat[Iv$ID==12]<-3
Is$etat[Iv$ID==28]<-3
Is$etat[Iv$ID==30]<-3
Is$etat[Iv$ID==31]<-3


tailles<-c()
pathls<-c()
denss<-c()
for (i in 1:100)
{
  t<-transmission(1,Is,dd,f,g)
  t<-t[t$contaminateur!=0,]
  tailles[i]<-length(t$ID)
  gra<-graph.data.frame(t[c(2:length(t$ID)),c(7,1)])
  pathls[i]<-average.path.length(gra,directed=TRUE)
  denss[i]<- graph.density(gra)
}


tail<-data.frame(t=c(taille/75*100,taillev/48*100,tailles/70*100),c=c(rep("NON VACCINE",length(taille),),rep("VACCINE",length(taillev)), rep("SREADERS VACCIONES",length(tailles))))
tail2<-data.frame(t=c(taille,taillev,tailles),c=c(rep("NON VACCINE",length(taille),),rep("VACCINE",length(taillev)), rep("SREADERS VACCIONES",length(tailles))))

qplot(taillev,geom="")+geom_density(aes(taille),colour="red")

ggplot(data =tail,aes(x=t))+geom_density(color=c,group=c )
qplot(t, ..count.., data=tail2, geom="density",fill=c,alpha=.3)+xlab("Nombre d'affectes")+ylab("Nombre d'ocurrences")
