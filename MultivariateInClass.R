multivariate <- read.csv(file.choose(), header = TRUE)
attach(multivariate)
mm <- lm(multivariate$Homeowners ~ multivariate$Immigrant)
mm
summary(mm$coefficients)
plot(multivariate$Homeowners ~ multivariate$Immigrant)
abline(mm)
abline(mm,col=2,lwd=3)

#figure out how ton include multiple independent variables

#Homeowners = multivariate$Homeowners
#Population = multivariate$Population
#Immigrant = multivariate$Immigrant
#area = multivariate$area
#Income = multivariate$Income
#City = multivariate$City

HP <- Homeowners/Population
PD <- Population/area

mm = lm(Immigrant~Income+Population+HP+PD)
summary(mm)

lm(formula = Immigrant~Income+Population+HP+PD)

cm = coef(mm)
cm
