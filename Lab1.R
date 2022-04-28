#Selina Kwarteng
#Lab1 pt.1 and pt.2

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

stem(EPI_data$EPI)
hist(EPI_data$EPI, seq(30,95,1),probability = TRUE)
lines(density(EPI_data$EPI),na.rm = TRUE, bw=1)
rug(EPI_data$EPI)

plot(ecdf(EPI_data$EPI),do.points=FALSE,verticals = TRUE) 
plot(ecdf(EPI_data$EPI),do.points=TRUE,verticals = TRUE) # points are visible on the plot.
par(pty="s")

qqnorm(EPI_data$EPI)
qqline(EPI_data$EPI)

x <- seq(30,95,1)
x
x2 <- seq(30,95,2)
x2


qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)
