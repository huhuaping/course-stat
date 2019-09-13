### Name: florida
### Title: County-by-County results of year 2000 US presidential election
###   in Florida
### Aliases: florida
### Keywords: datasets

### ** Examples

data(florida)
attach(florida)
result.lm <- lm(BUCHANAN ~ BUSH)
plot(BUSH,BUCHANAN)
abline(result.lm) ## can you find Miami-Dade coutny?



