### Name: WeightData
### Title: Data on height, weight and age of children
### Aliases: WeightData
### Keywords: datasets

### ** Examples

data(WeightData)
age.yrs = floor(WeightData$age/12)
boxplot(WeightData$weight ~ age.yrs)



