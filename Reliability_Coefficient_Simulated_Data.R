###########################################################################
#                                                                         #
#   This code demonstrates the mathematical relationships underlying      #
#   single-administration reliability coefficients and accompanies        #
#   the article by Komperda, Pentecost and Barbera (2018).                #
#                                                                         #
###########################################################################

## First, check if necessary packages are installed, and if not, install.

## psych contains functions for examining descriptive statistics
if("psych" %in% rownames(installed.packages()) == FALSE) {install.packages("psych")}
library(psych)

## usefriendlyscience includes functions for computing  
## single-administration reliability coefficients 
if("userfriendlyscience" %in% rownames(installed.packages()) == FALSE) {install.packages("userfriendlyscience")}
library(userfriendlyscience)

## lavaan is used for confirmatory factor analysis
if("lavaan" %in% rownames(installed.packages()) == FALSE) {install.packages("lavaan")}
library(lavaan)
## Next, generate simulated data 
## Setting the seed allows the same numbers to be generated each time
set.seed(1234)

## This code generates normally distributed categorical data
## representing a common construct value ranging from 1 to 5 
cc<-sample(1:5,10000,rep=TRUE,prob=c(0.2,0.3,0.5,0.3,0.2))

## View descriptive statistics and visualize distribution
describe(cc)
hist(cc)

################# Parallel model ################# 

## Generate the error values for a parallel model where
## all error values are essentially identical
errorA.parallel<-sample(1:5,10000,rep=TRUE,prob=c(0.2,0.3,0.5,0.3,0.2))
errorB.parallel<-sample(1:5,10000,rep=TRUE,prob=c(0.2,0.3,0.5,0.3,0.2))
errorC.parallel<-sample(1:5,10000,rep=TRUE,prob=c(0.2,0.3,0.5,0.3,0.2))
errorD.parallel<-sample(1:5,10000,rep=TRUE,prob=c(0.2,0.3,0.5,0.3,0.2))

## All items have the same relationship to the common construct value (2).
## The value of 2 was chosen arbitrarily and can be changed.
obsA.parallel<-2*cc + errorA.parallel
obsB.parallel<-2*cc + errorB.parallel
obsC.parallel<-2*cc + errorC.parallel
obsD.parallel<-2*cc + errorD.parallel

## Calculate composite score
total.parallel<-obsA.parallel + obsB.parallel + obsC.parallel + obsD.parallel

## Combine all variables into single data frame
parallel<-data.frame(cbind(obsA.parallel, obsB.parallel, obsC.parallel, obsD.parallel, total.parallel, cc))
## Examine descriptive statistics 
describe(parallel)
## Calculate alpha for Observed A-D using a function from the psych package 
alpha(parallel[,1:4])
## Confirm that the value for alpha is equal to the squared correlation 
## between the total score and the common construct value
cor(total.parallel, cc)^2
## Use the function from the userfriendlyscience package to confirm that
## under parallel model conditions, alpha, omega (total), and coefficient H      
## are equivalent
scaleStructure(parallel[,1:4])

################# Tau equivalent model ################# 

## Each error term has a different distribution and therefore different 
## variance
errorA.tau<-sample(1:5,10000,rep=TRUE,prob=c(0.2,0.3,0.5,0.3,0.2))
errorB.tau<-sample(1:5,10000,rep=TRUE,prob=c(0.1,0.2,0.4,0.1,0.3))
errorC.tau<-sample(1:5,10000,rep=TRUE,prob=c(0.1,0.3,0.3,0.6,0.5))
errorD.tau<-sample(1:5,10000,rep=TRUE,prob=c(0.7,0.5,0.2,0.1,0.1))

## All error terms have unique variances (standard deviations)
describe(cbind(errorA.tau, errorB.tau, errorC.tau, errorD.tau))

## All items have the same relationship to the common construct value (2).
## The value of 2 was chosen arbitrarily and can be changed.
## Each item has a unique error variance.
obsA.tau<-2*cc + errorA.tau
obsB.tau<-2*cc + errorB.tau
obsC.tau<-2*cc + errorC.tau
obsD.tau<-2*cc + errorD.tau

## Calculate composite score
total.tau<-obsA.tau + obsB.tau + obsC.tau + obsD.tau

## Combine all variables into single data frame
tau<-data.frame(cbind(obsA.tau, obsB.tau, obsC.tau, obsD.tau, total.tau, cc))

## Examine descriptive statistics
describe(tau)
## Calculate alpha for Observed A-D using a function from the psych package 
alpha(tau[,1:4])
## Confirm that the value for alpha is equal to the squared correlation 
## between the total score and the common construct value
cor(total.tau, cc)^2
## Use the function from the userfriendlyscience package to confirm that
## under tau equivalent model conditions, alpha, omega (total),                  
## and coefficient H are equivalent
scaleStructure(tau[,1:4])

################# Essentially tau equivalent model ################# 

## An additive term has been added, but all items have the same 
## relationship to the common construct value (2).
## The value of 2 was chosen arbitrarily and can be changed.
## Each item has a unique error variance.
obsA.ess.tau<-1 + 2*cc + errorA.tau
obsB.ess.tau<-5 + 2*cc + errorB.tau
obsC.ess.tau<-10 + 2*cc + errorC.tau
obsD.ess.tau<-50 + 2*cc + errorD.tau

## Calculate composite score
total.ess.tau<-obsA.ess.tau + obsB.ess.tau + obsC.ess.tau + obsD.ess.tau

## Combine all variables into single data frame
ess.tau<-data.frame(cbind(obsA.ess.tau, obsB.ess.tau, obsC.ess.tau, obsD.ess.tau, total.ess.tau, cc))

## Examine descriptive statistics
describe(ess.tau)
## Calculate alpha for Observed A-D using a function from the psych package 
alpha(ess.tau[,1:4])
## Confirm that the value for alpha is equal to the squared correlation 
## between the total score and the common construct value
cor(total.ess.tau, cc)^2
## Use the function from the userfriendlyscience package to confirm that
## under essentially tau equivalent model conditions, alpha, omega (total)       
## and coefficient H are equivalent
scaleStructure(ess.tau[,1:4])

################# Congeneric model ################# 

## Now item is associated with the common construct to a different degree
## and each item has unique error variance.
obsA.congeneric<-1 + 2*cc + errorA.tau
obsB.congeneric<-5 + 1*cc + errorB.tau
obsC.congeneric<-10 + 0.5*cc + errorC.tau
obsD.congeneric<-50 + 1*cc + errorD.tau
## Calculate composite score
total.congeneric<-obsA.congeneric + obsB.congeneric + obsC.congeneric + obsD.congeneric

## Combine all variables into single data frame
congeneric<-data.frame(cbind(obsA.congeneric, obsB.congeneric, obsC.congeneric, obsD.congeneric, total.congeneric, cc))

## Examine descriptive statistics
describe(congeneric)
## Calculate alpha for Observed A-D using a function from the psych package 
alpha(congeneric[,1:4])
## Now the value for alpha is not equal to the squared correlation 
## between the total score and the common construct value
cor(total.congeneric, cc)^2
## Use the function from the userfriendlyscience package to see that
## under congeneric model conditions, alpha, omega (total), and coefficient H    
## are not equivalent. Omega (total) is equal to the squared correlation         
## between the total score and the common construct value (within rounding)
scaleStructure(congeneric[,1:4])

################# Confirmatory factor analysis ################# 

## Testing single-factor models for each set of data using functions from the
## lavaan package

## The =~ operator means 'is measured by'
## Starting each observed variable with l* forces loading values to be equal

parallel.model<-'
cc=~ l*obsA.parallel + l*obsB.parallel + l*obsC.parallel + l*obsD.parallel
'

fit.parallel<-cfa(model = parallel.model, data = parallel)
summary(fit.parallel, standardized=TRUE, fit.measures=TRUE, rsquare=TRUE) 
## CFI/TLI = 1.00; RMSEA = 0.00; SRMR = 0.00 indicate excellent data-model fit

tau.model<-'
cc=~ l*obsA.tau + l*obsB.tau + l*obsC.tau + l*obsD.tau
'

fit.tau<-cfa(model = tau.model, data = tau)
summary(fit.tau, standardized=TRUE, fit.measures=TRUE, rsquare=TRUE) 
## CFI/TLI = 1.00; RMSEA = 0.01; SRMR = 0.01 indicate excellent data-model fit

ess.tau.model<-'
cc=~ l*obsA.ess.tau + l*obsB.ess.tau + l*obsC.ess.tau + l*obsD.ess.tau
'

fit.ess.tau<-cfa(model = ess.tau.model, data = ess.tau)
summary(fit.ess.tau, standardized=TRUE, fit.measures=TRUE, rsquare=TRUE) 
## CFI/TLI = 1.00; RMSEA = 0.01; SRMR = 0.01 indicate excellent data-model fit

## Notice what happens when the congeneric data are fit to a model with
## loadings constrained to be equal

congeneric.restrict.model<-'
cc=~ l*obsA.congeneric + l*obsB.congeneric + l*obsC.congeneric + l*obsD.congeneric
'

fit.congeneric.restrict<-cfa(model = congeneric.restrict.model, data = congeneric)
summary(fit.congeneric.restrict, standardized=TRUE, fit.measures=TRUE, rsquare=TRUE)
## CFI = 0.63/TLI = 0.56; RMSEA = 0.30; SRMR = 0.26 indicate poor data-model fit
## These results provide evidence that alpha would not be an appropriate 
## single-administration reliability coefficient to report

## Allowing each congeneric item to have its own loading value
## improves data-model fit
congeneric.model<-'
cc=~ obsA.congeneric + obsB.congeneric + obsC.congeneric + obsD.congeneric
'

fit.congeneric<-cfa(model = congeneric.model, data = congeneric)
summary(fit.congeneric, standardized=TRUE, fit.measures=TRUE, rsquare=TRUE)
## CFI/TLI = 1.00; RMSEA = 0.01; SRMR = 0.00 indicate excellent data-model fit
## Since the congeneric model with unequal loadings showed good fit to the data
## and forcing the loadings to be equal showed poor fit, omega (total) would be
## a more reasonable single-administration reliability value to report

## Having good data-model fit does not guarantee a high value for
## alpha or omega, as evidenced by these items with weak relationships to        
## the common construct
obsA.new<-1 + 0.1*cc + errorA.tau
obsB.new<-5 + 0.7*cc + errorB.tau
obsC.new<-10 + 0.5*cc + errorC.tau
obsD.new<-50 + 0.3*cc + errorD.tau

## Calculate composite score
total.new<-obsA.new + obsB.new + obsC.new + obsD.new

## Combine all variables into single data frame
new.data<-data.frame(cbind(obsA.new, obsB.new, obsC.new, obsD.new, total.new, cc))

congeneric.new<-'
cc=~ obsA.new + obsB.new + obsC.new + obsD.new
'

fit.congeneric.new<-cfa(model = congeneric.new, data = new.data)
summary(fit.congeneric.new, standardized=TRUE, fit.measures=TRUE, rsquare=TRUE)
## CFI/TLI = 1.00; RMSEA = 0.01; SRMR = 0.00 indicate excellent data-model fit

scaleStructure(new.data[,1:4])
## However, the value for omega (total) is very low

## Even though omega (total) is low, it is still equal to the squared correlation
## between the summed items and the common construct for this congeneric model
cor(total.new, cc)^2
