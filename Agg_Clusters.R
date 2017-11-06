library(readr)
library(ggplot2)
bigdata <- read_csv("C:/Users/jburr/Downloads/Accidental_Drug_Related_Deaths__2012-June_2017 (1).csv")
attach(bigdata)
bigdata$Combo = rep('',nrow(bigdata))
attach(bigdata)

# cleaning up drug categories
bigdata[is.na(bigdata)] <- 0

bigdata$Heroin[bigdata$Heroin=='Y']<-1
bigdata$Cocaine[bigdata$Cocaine=='Y']<-1
bigdata$Fentanyl[bigdata$Fentanyl=='Y']<-1
bigdata$Oxycodone[bigdata$Oxycodone=='Y']<-1
bigdata$Oxymorphone[bigdata$Oxymorphone=='Y']<-1
bigdata$EtOH[bigdata$EtOH=='Y']<-1
bigdata$Hydrocodone[bigdata$Hydrocodone=='Y']<-1
bigdata$Benzodiazepine[bigdata$Benzodiazepine=='Y']<-1
bigdata$Methadone[bigdata$Methadone=='Y']<-1
bigdata$Amphet[bigdata$Amphet=='Y']<-1
bigdata$Tramad[bigdata$Tramad=='Y']<-1
bigdata$`Morphine (not heroin)`[bigdata$`Morphine (not heroin)`=='Y']<-1

# convert to numeric for calculations
attach(bigdata)
bigdata$Heroin = as.numeric(as.character(bigdata$Heroin))
bigdata$Cocaine = as.numeric(as.character(bigdata$Cocaine))
bigdata$Oxycodone = as.numeric(as.character(bigdata$Oxycodone))
bigdata$Oxymorphone = as.numeric(as.character(bigdata$Oxymorphone))
bigdata$EtOH = as.numeric(as.character(bigdata$EtOH))
bigdata$Hydrocodone = as.numeric(as.character(bigdata$Hydrocodone))
bigdata$Benzodiazepine = as.numeric(as.character(bigdata$Benzodiazepine))
bigdata$Methadone = as.numeric(as.character(bigdata$Methadone))
bigdata$Amphet = as.numeric(as.character(bigdata$Amphet))
bigdata$Tramad = as.numeric(as.character(bigdata$Tramad))
bigdata$`Morphine (not heroin)` = as.numeric(as.character(bigdata$`Morphine (not heroin)`))

# aggregate based on age, sex, race, and sum the drugs found in person
agg = aggregate(cbind(Heroin, Cocaine, Oxycodone, Oxymorphone, EtOH, Hydrocodone, Benzodiazepine, Methadone, Amphet, Tramad, `Morphine (not heroin)`) ~ Age+Sex+Race,FUN=sum,bigdata)

# pca
pcadef = prcomp(agg[,4:14])
summary(pcadef)


pc = data.frame(pcadef$x)

# determine best k value
kcvar = rep(0,15)
for (i in 1:15){
  kc = kmeans(pc[,1:2],i)
  kcvar[i] = kc$tot.withinss
}
plot(kcvar,type = 'l')
kcfinal = kmeans(pc[,1:2],3)

# plot clusters
kcfinal$cluster <- as.factor(kcfinal$cluster)
ggplot(data.frame(pcadef$x), aes(PC1, PC2, color = kcfinal$cluster)) + geom_point()




# normalize sums of drugs used

scalar1 <- function(x) {x / sqrt(sum(x^2))}
attach(agg)
agg$Heroin = scalar1(Heroin)
agg$Cocaine = scalar1(Cocaine)
agg$Oxycodone = scalar1(Oxycodone)
agg$Oxymorphone = scalar1(Oxymorphone)
agg$EtOH = scalar1(EtOH)
agg$Hydrocodone = scalar1(Hydrocodone)
agg$Benzodiazepine = scalar1(Benzodiazepine)
agg$Methadone = scalar1(Methadone)
agg$Amphet = scalar1(Amphet)
agg$Tramad = scalar1(Tramad)
agg$`Morphine (not heroin)` = scalar1(`Morphine (not heroin)`)

# pca
dfpca = agg[,4:14]
pcanorm = prcomp(dfpca)
summary(pcanorm)

pc = data.frame(pcanorm$x)

# determine best k value
kcvar = rep(0,15)
for (i in 1:15){
  kc = kmeans(pc[,1:2],i)
  kcvar[i] = kc$tot.withinss
}
plot(kcvar,type = 'l',main='Inertia(SSE) per Kmeans fit with given K', xlab = 'Number of Clusters',ylab='SSE',lwd = 2)
kcfinal = kmeans(pc[,1:2],4)

# plot clusters
kcfinal$cluster <- as.factor(kcfinal$cluster)
ggplot(data.frame(pcanorm$x), aes(PC1, PC2, color = kcfinal$cluster,size=1)) + geom_point()+  theme(plot.title = element_text(hjust = 0.5)) +theme(axis.text=element_text(size=23),axis.title=element_text(size=25,face="bold"))
kcvar = data.frame(kcvar)
colnames(kcvar)[1] = 'SSE'
kcvar$Clusters = seq(1:nrow(kcvar))
ggplot(kcvar, aes(x = Clusters,y = SSE,size=1)) + geom_line()+theme(plot.title = element_text(hjust = 0.5)) +theme(axis.text=element_text(size=30),axis.title=element_text(size=33,face="bold"))

# tie back cluster to original data
dfMapBack = cbind(agg,kcfinal$cluster)


# --------------------------------------------------------------------------
#14-25, 26-35, 46-56, 57-87 age groups

agegroup <- rep(0, nrow(bigdata))
bigdata <- cbind(bigdata, agegroup)
groupd <- subset(bigdata, bigdata$Age > 1 & bigdata$Race != 0 & bigdata$Race != 'Unknown')
groupd$agegroup[groupd$Age <= 25]<-1
groupd$agegroup[groupd$Age >= 26 & groupd$Age <= 35]<-2
groupd$agegroup[groupd$Age >= 36 & groupd$Age <= 45]<-3
groupd$agegroup[groupd$Age >= 46 & groupd$Age <= 56]<-4
groupd$agegroup[groupd$Age >= 57 & groupd$Age <= 87]<-5
bigdata = groupd

# aggregate by agegroup, sex, race
agg = aggregate(cbind(Heroin, Cocaine, Oxycodone, Oxymorphone, EtOH, Hydrocodone, Benzodiazepine, Methadone, Amphet, Tramad, `Morphine (not heroin)`) ~ agegroup+Sex+Race,FUN=sum,bigdata)

# pca
pcadef = prcomp(agg[,4:14])
summary(pcadef)


pc = data.frame(pcadef$x)

# determine best k value
kcvar = rep(0,15)
for (i in 1:15){
  kc = kmeans(pc[,1:2],i)
  kcvar[i] = kc$tot.withinss
}
plot(kcvar,type = 'l')
kcfinal = kmeans(pc[,1:2],3)

# plot clusters
kcfinal$cluster <- as.factor(kcfinal$cluster)
ggplot(data.frame(pcadef$x), aes(PC1, PC2, color = kcfinal$cluster)) + geom_point()




# normalize

# scalar1 <- function(x) {x / sqrt(sum(x^2))}
# attach(agg)
# agg$Heroin = scalar1(Heroin)
# agg$Cocaine = scalar1(Cocaine)
# agg$Oxycodone = scalar1(Oxycodone)
# agg$Oxymorphone = scalar1(Oxymorphone)
# agg$EtOH = scalar1(EtOH)
# agg$Hydrocodone = scalar1(Hydrocodone)
# agg$Benzodiazepine = scalar1(Benzodiazepine)
# agg$Methadone = scalar1(Methadone)
# agg$Amphet = scalar1(Amphet)
# agg$Tramad = scalar1(Tramad)
# agg$`Morphine (not heroin)` = scalar1(`Morphine (not heroin)`)

# pca

dfpca = agg[,4:14]
pcanorm = prcomp(dfpca)
summary(pcanorm)

pc = data.frame(pcanorm$x)

# find best k value
kcvar = rep(0,15)
for (i in 1:15){
  kc = kmeans(pc[,1:2],i)
  kcvar[i] = kc$tot.withinss
}
# plot clusters
plot(kcvar,type = 'l',main='Inertia(SSE) per Kmeans fit with given K', xlab = 'Number of Clusters',ylab='SSE',lwd = 2)
kcfinal = kmeans(pc[,1:2],4)

kcfinal$cluster <- as.factor(kcfinal$cluster)
ggplot(data.frame(pcanorm$x), aes(PC1, PC2, color = kcfinal$cluster,size=1)) + geom_point()+  theme(plot.title = element_text(hjust = 0.5)) +theme(axis.text=element_text(size=23),axis.title=element_text(size=25,face="bold"))
kcvar = data.frame(kcvar)
colnames(kcvar)[1] = 'SSE'
kcvar$Clusters = seq(1:nrow(kcvar))
ggplot(kcvar, aes(x = Clusters,y = SSE,size=1)) + geom_line()+theme(plot.title = element_text(hjust = 0.5)) +theme(axis.text=element_text(size=30),axis.title=element_text(size=33,face="bold"))

# map clusters back to original data
dfMapBack = cbind(agg,kcfinal$cluster)

# ---------------------------------------------------------------------------------
# account for differences in race proportions - most meaningful model

for (i in 1:nrow(agg)){
  if(agg$Race[i] == "White"){agg[i,4:14] = agg[i,4:14]/.677}
  else if(agg$Race[i] == "Black"){agg[i,4:14] = agg[i,4:14]/.118}
  else if(agg$Race[i] == "Native American, Other"){agg[i,4:14] = agg[i,4:14]/.005}
  else if(agg$Race[i] == "Chinese"){agg[i,4:14] = agg[i,4:14]/.047}
  else if(agg$Race[i] == "Hawaiian"){agg[i,4:14] = agg[i,4:14]/.001}
  else if(agg$Race[i] == "Hispanic, White"){agg[i,4:14] = agg[i,4:14]/.157}
  else {agg[i,4:14] = agg[i,4:14]/.023}
}

# remove Hawaiian data point - used benzos and was an outlier
agg = subset(agg,agg$Race != "Hawaiian")

# pca
pcadef = prcomp(agg[,4:14])
summary(pcadef)


pc = data.frame(pcadef$x)

# find best k value
kcvar = rep(0,15)
for (i in 1:15){
  kc = kmeans(pc[,1:2],i)
  kcvar[i] = kc$tot.withinss
}

# plot clusters
plot(kcvar,type = 'l')
kcfinal = kmeans(pc[,1:2],3)

kcfinal$cluster <- as.factor(kcfinal$cluster)
ggplot(data.frame(pcadef$x), aes(PC1, PC2, color = kcfinal$cluster)) + geom_point()




# normalize

# scalar1 <- function(x) {x / sqrt(sum(x^2))}
# attach(agg)
# agg$Heroin = scalar1(Heroin)
# agg$Cocaine = scalar1(Cocaine)
# agg$Oxycodone = scalar1(Oxycodone)
# agg$Oxymorphone = scalar1(Oxymorphone)
# agg$EtOH = scalar1(EtOH)
# agg$Hydrocodone = scalar1(Hydrocodone)
# agg$Benzodiazepine = scalar1(Benzodiazepine)
# agg$Methadone = scalar1(Methadone)
# agg$Amphet = scalar1(Amphet)
# agg$Tramad = scalar1(Tramad)
# agg$`Morphine (not heroin)` = scalar1(`Morphine (not heroin)`)

# pca
dfpca = agg[,4:14]
pcanorm = prcomp(dfpca)
summary(pcanorm)

pc = data.frame(pcanorm$x)

# find best k value
kcvar = rep(0,15)
for (i in 1:15){
  kc = kmeans(pc[,1:2],i)
  kcvar[i] = kc$tot.withinss
}

# plout clusters
plot(kcvar,type = 'l',main='Inertia(SSE) per Kmeans fit with given K', xlab = 'Number of Clusters',ylab='SSE',lwd = 2)
kcfinal = kmeans(pc[,1:2],3)

kcfinal$cluster <- as.factor(kcfinal$cluster)
ggplot(data.frame(pcanorm$x), aes(PC1, PC2, color = kcfinal$cluster,size=1)) + geom_point()+  theme(plot.title = element_text(hjust = 0.5)) +theme(axis.text=element_text(size=23),axis.title=element_text(size=25,face="bold"))
kcvar = data.frame(kcvar)
colnames(kcvar)[1] = 'SSE'
kcvar$Clusters = seq(1:nrow(kcvar))
ggplot(kcvar, aes(x = Clusters,y = SSE,size=1)) + geom_line()+theme(plot.title = element_text(hjust = 0.5)) +theme(axis.text=element_text(size=30),axis.title=element_text(size=33,face="bold"))

# tie back clusters to original data
dfMapBack = cbind(agg,kcfinal$cluster)

# ---------------------------------------------------------------------------------
# account for differences in race proportions - weird clusters

for (i in 1:nrow(agg)){
  if(agg$Race[i] == "White"){agg[i,4:14] = agg[i,4:14]/length(which(bigdata$Race=="White"))/nrow(bigdata)}
  else if(agg$Race[i] == "Black"){agg[i,4:14] = agg[i,4:14]/length(which(bigdata$Race=="Black"))/nrow(bigdata)}
  else if(agg$Race[i] == "Native American, Other"){agg[i,4:14] = agg[i,4:14]/length(which(bigdata$Race=="Native American, Other"))/nrow(bigdata)}
  else if(agg$Race[i] == "Chinese"){agg[i,4:14] = agg[i,4:14]/length(which(bigdata$Race=="Chinese"))/nrow(bigdata)}
  else if(agg$Race[i] == "Hawaiian"){agg[i,4:14] = agg[i,4:14]/length(which(bigdata$Race=="Hawaiian"))/nrow(bigdata)}
  else if(agg$Race[i] == "Hispanic, White"){agg[i,4:14] = agg[i,4:14]/length(which(bigdata$Race=="Hispanic, White"))/nrow(bigdata)}
  else {agg[i,4:14] = agg[i,4:14]/.023}
}

# remove Hawaiian data point - used benzos and was an outlier
#agg = subset(agg,agg$Race != "Hawaiian")

# pca
pcadef = prcomp(agg[,4:14])
summary(pcadef)


pc = data.frame(pcadef$x)

# find best k value
kcvar = rep(0,15)
for (i in 1:15){
  kc = kmeans(pc[,1:2],i)
  kcvar[i] = kc$tot.withinss
}

# plot clusters
plot(kcvar,type = 'l')
kcfinal = kmeans(pc[,1:2],3)

kcfinal$cluster <- as.factor(kcfinal$cluster)
ggplot(data.frame(pcadef$x), aes(PC1, PC2, color = kcfinal$cluster)) + geom_point()




# normalize

# scalar1 <- function(x) {x / sqrt(sum(x^2))}
# attach(agg)
# agg$Heroin = scalar1(Heroin)
# agg$Cocaine = scalar1(Cocaine)
# agg$Oxycodone = scalar1(Oxycodone)
# agg$Oxymorphone = scalar1(Oxymorphone)
# agg$EtOH = scalar1(EtOH)
# agg$Hydrocodone = scalar1(Hydrocodone)
# agg$Benzodiazepine = scalar1(Benzodiazepine)
# agg$Methadone = scalar1(Methadone)
# agg$Amphet = scalar1(Amphet)
# agg$Tramad = scalar1(Tramad)
# agg$`Morphine (not heroin)` = scalar1(`Morphine (not heroin)`)

# pca
dfpca = agg[,4:14]
pcanorm = prcomp(dfpca)
summary(pcanorm)

pc = data.frame(pcanorm$x)

# find best k value
kcvar = rep(0,15)
for (i in 1:15){
  kc = kmeans(pc[,1:2],i)
  kcvar[i] = kc$tot.withinss
}

# plout clusters
plot(kcvar,type = 'l',main='Inertia(SSE) per Kmeans fit with given K', xlab = 'Number of Clusters',ylab='SSE',lwd = 2)
kcfinal = kmeans(pc[,1:2],3)

kcfinal$cluster <- as.factor(kcfinal$cluster)
ggplot(data.frame(pcanorm$x), aes(PC1, PC2, color = kcfinal$cluster,size=1)) + geom_point()+  theme(plot.title = element_text(hjust = 0.5)) +theme(axis.text=element_text(size=23),axis.title=element_text(size=25,face="bold"))
kcvar = data.frame(kcvar)
colnames(kcvar)[1] = 'SSE'
kcvar$Clusters = seq(1:nrow(kcvar))
ggplot(kcvar, aes(x = Clusters,y = SSE,size=1)) + geom_line()+theme(plot.title = element_text(hjust = 0.5)) +theme(axis.text=element_text(size=30),axis.title=element_text(size=33,face="bold"))

# tie back clusters to original data
dfMapBack = cbind(agg,kcfinal$cluster)

