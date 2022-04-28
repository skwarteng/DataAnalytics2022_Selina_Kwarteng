#Selina Kwarteng
#Lab2 pt.1 and pt.2

EPI_data <- read.csv(file.choose(), header = TRUE)
EPI_data
View(EPI_data)


names(EPI_data) <- as.matrix(EPI_data[1, ])
EPI_data <- EPI_data[-1, ]
EPI_data[] <- lapply(EPI_data, function(x) type.convert(as.character((x))))
EPI_data
View(EPI_data)

attach(EPI_data)

tf <- is.na(EPI_data)
E <- EPI_data[!tf]

summary(EPI_data$EPI)
fivenum(EPI_data$EPI,na.rm=TRUE)

par(mar=c(1,1,1,1))

#histogram for EPI
hist(EPI_data$EPI, seq(30,95,1),probability = TRUE)
lines(density(EPI_data$EPI),na.rm = TRUE, bw=1)
rug(EPI_data$EPI)

#histogram for DALY
hist(EPI_data$DALY, seq(30,95,1),probability = TRUE)
lines(density(EPI_data$DALY),na.rm = TRUE, bw=1)
rug(EPI_data$DALY)

#box plot
boxplot(ENVHEALTH, ECOSYSTEM, names = c(1,2))

# Q-Q plot
qqplot(ENVHEALTH,ECOSYSTEM)

#linear and least-squares
boxplot(ENVHEALTH,DALY,AIR_H,WATER_H)
lmENVH = lm(ENVHEALTH~DALY+AIR_H+WATER_H)
lmENVH
summary(lmENVH)
cENVH<-coef(lmENVH)
cENVH

#predictions
DALYNEW<-c(seq(5,95,5))
AIR_HNEW<-c(seq(5,95,5))
WATER_HNEW<-c(seq(5,95,5))
NEW<-data.frame(DALYNEW,AIR_HNEW,WATER_HNEW)
pENV<-predict(lmENVH,newdata = NEW,interval='prediction')
pENV
cENV<-predict(lmENVH,NEW,interval= 'confidence')
cENV

AIR_ENEW<-c(seq(5,95,5))
CLIMATE_NEW<-c(seq(5,95,5))
NEW_2<-data.frame(AIR_ENEW,CLIMATE_NEW)
pENV_2<-predict(lmENVH,newdata = NEW_2,interval='prediction')
pENV_2
cENV_2<-predict(lmENVH,NEW_2,interval= 'confidence')
cENV_2
