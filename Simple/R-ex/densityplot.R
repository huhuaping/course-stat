### Name: densityplot
### Title: Plots densities of data
### Aliases: densityplot densityplot.default densityplot.formula
### Keywords: multivariate

### ** Examples

## taken from boxplot
## using a formula
data(InsectSprays)
densityplot(count ~ spray, data = InsectSprays)
## on a matrix (data frame)
mat <- cbind(Uni05 = (1:100)/21, Norm = rnorm(100),
             T5 = rt(100, df = 5), Gam2 = rgamma(100, shape = 2))
densityplot(data.frame(mat))




