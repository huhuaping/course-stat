### Name: failrate
### Title: Failrates for 7 different adjuncts
### Aliases: failrate
### Keywords: datasets

### ** Examples

data(failrate)
fr = stack(failrate)
fr = fr[!is.na(fr$values),]
oneway.test(values ~ ind,data=fr)



