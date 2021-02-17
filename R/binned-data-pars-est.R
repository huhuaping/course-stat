### Code for Estimating Parameters from a Binned Distribution
### by Jack Davis, jackd@sfu.ca , Simon Fraser University
### www.stats-et-al.com
### Last Updated 2018-11-01

### Section 2 Fitting a distribution: Finding point estimates of parameters
	### Minimize the Kullback Leibler Divergence between a distribution binned and actual binned
group_freq = c(114,76,58,51,140,107,77,124,42) # observed frequencies
cutoffs = c(0,25,50,75,100,250,500,1000,5000,20000) # bin boundaries

group_prob = group_freq / sum(group_freq)

### Getting initial estimates for exponential distribution
midpoints = 1/2 * (cutoffs[-1] + cutoffs[-length(cutoffs)])
init_par_exp = sum(midpoints * group_prob)
init_par_exp
	

### 1b Measuring the KL Divergence

get.KL.div.exp = function(x, cutoffs, group_prob)
{
	exp_freq = diff( pexp(cutoffs, rate = 1/x))
	KL = sum(group_prob * log(group_prob / exp_freq))
	return(KL)
}	

### Negative log-likelihood, if you like
get.nllik.exp = function(x, cutoffs, yi)
{
	pi = diff( pexp(cutoffs, rate = 1/x))
	nll = -sum(yi * log(pi))
	return(nll)
}

result_exp = optim(par = init_par_exp, fn = get.KL.div.exp, method = "Brent", lower=1, upper=10000,
				cutoffs=cutoffs, group_prob=group_prob)
				
result$par ## This is the parameters for the distribution
result$value ## This is the KL Divergence

				
				
				
### Section 3 Choosing a distribution

	ex = sum(midpoints * group_prob)
	ex2 = sum(midpoints^2 * group_prob)

init_mean_lognorm = ex
init_sd_lognorm = sqrt(ex2 - ex^2)
init_mean_lognorm 
init_sd_lognorm 


get.KL.div.lognorm = function(x, cutoffs, group_prob)
{
	if(x[2] < 0){x[2] = 0.0001}
	exp_freq = diff( plnorm(cutoffs, meanlog=x[1], sdlog=x[2]))
	KL = sum(group_prob * log(group_prob / exp_freq))
		return(KL)
}	


result_lnorm = optim(par = c(init_mean_lognorm, init_sd_lognorm), fn = get.KL.div.lognorm, 
				method = "Nelder-Mead",
				cutoffs=cutoffs, group_prob=group_prob)
			
par_lnorm = result_lnorm$par
par_lnorm


#shape * scale = mean
#shape * scale^2 = variance
init_scale_gamma = (ex2 - ex^2) / ex
init_shape_gamma = ex / init_scale_gamma

get.KL.div.gamma = function(x, cutoffs, group_prob)
{
	#if(x[2] < 0){x[2] = 0.0001}
	exp_freq = diff( pgamma(cutoffs, shape=x[1], scale=x[2]))
	KL = sum(group_prob * log(group_prob / exp_freq))
		return(KL)
}	


result_gamma = optim(par = c(init_shape_gamma, init_scale_gamma), fn = get.KL.div.gamma, 
				method = "Nelder-Mead",
				cutoffs=cutoffs, group_prob=group_prob)



dlnorm_times_x = function(x, meanlog, sdlog)
{
	output = x * dlnorm(x, meanlog=meanlog, sdlog=sdlog)
	return(output)
}


dgamma_times_x = function(x, shape, scale)
{
	output = x * dgamma(x, shape=shape, scale=scale)
	return(output)
}



## The mean from the log-norm distribution			
integrate(dlnorm_times_x, meanlog=result_lnorm$par[1], sdlog=result_lnorm$par[2], lower=0, upper=10^6)

## And the gamma
integrate(dgamma_times_x, shape=result_gamma$par[1], scale=result_gamma$par[2], lower=0, upper=10^6)

### Section 4 Uncertainty of distribution parameters - a monte carlo approach


Niter = 3000
random_results = numeric(Niter)
random_results_lnorm = matrix(NA, nrow=Niter, ncol=2)
E_lnorm =numeric(Niter)

for(k in 1:Niter)
{
	random_freqs = table(sample(1:length(group_freq), size=sum(group_freq), 
									prob=group_prob, replace=TRUE))
	
	random_prob = random_freqs / sum(random_freqs)	
	#init_mean = sum(midpoints * random_prob)
	random_results[k] = optim(par = mean_rev1, fn = get.KL.div.exp, method = "Brent", lower=1, upper=10000,
				cutoffs=cutoffs, group_prob=random_prob)$par
				
	random_results_lnorm[k,] = optim(par = c(init_mean_lognorm, init_sd_lognorm), fn = get.KL.div.lognorm, 
				method = "Nelder-Mead",
				cutoffs=cutoffs, group_prob=random_prob)$par
	E_lnorm[k] = integrate(dlnorm_times_x, meanlog=random_results_lnorm[k,1], sdlog=random_results_lnorm[k,2], lower=0, upper=10^6)$value
}


summary(random_results)
summary(E_lnorm)
quantile(random_results_lnorm[,1], prob=c(0.005,0.025,0.05,0.50,.95,0.975,0.995))
quantile(random_results_lnorm[,2], prob=c(0.005,0.025,0.05,0.50,.95,0.975,0.995))
quantile(E_lnorm, prob=c(0.005,0.025,0.05,0.95,0.05,0.975,0.995))
sd(random_results_lnorm[,1])
sd(random_results_lnorm[,2])
sd(E_lnorm)

### Section 4a Uncertainty of hyper-parameters, measured with the dirichlet
### (Not shown in blog post)

Niter = 3000
random_results2 = numeric(Niter)
random_results2_lnorm = matrix(NA, nrow=Niter, ncol=2)
E_lnorm2 =numeric(Niter)
hyper_para = rdirichlet(Niter, group_freq)

for(k in 1:Niter)
{
	random_freqs = table(sample(1:length(group_freq), size=sum(group_freq), 
									prob=hyper_para[k,], replace=TRUE))
	
	random_prob = random_freqs / sum(random_freqs)	
	init_mean = sum(midpoints * random_prob)
	random_results2[k] = optim(par = mean_rev1, fn = get.KL.div.exp, method = "Brent", lower=1, upper=10000,
				cutoffs=cutoffs, group_prob=random_prob)$par
				
				
	random_results2_lnorm[k,] = optim(par = c(init_mean_lognorm, init_sd_lognorm), fn = get.KL.div.lognorm, 
				method = "Nelder-Mead", cutoffs=cutoffs, group_prob=random_prob)$par
	E_lnorm2[k] = integrate(dlnorm_times_x, meanlog=random_results_lnorm[k,1], sdlog=random_results_lnorm[k,2], lower=0, upper=10^6)$value
				
}


summary(random_results)
#hist(random_results)
quantile(random_results2, prob=c(0.005,0.025,0.05,0.95,0.975,0.995))
var(random_results2)


quantile(E_lnorm2, prob=c(0.005,0.025,0.05,0.95,0.975,0.995))
quantile(random_results2_lnorm[,1], prob=c(0.005,0.025,0.05,0.95,0.975,0.995))
quantile(random_results2_lnorm[,2], prob=c(0.005,0.025,0.05,0.95,0.975,0.995))


### Plots! Plots!


hist(E_lnorm, n=20, col="Lightblue", xlab="Mean of Fitted Distribution", las=1, main="Histogram of Mean \n Derived from MC Resampling")


smoothScatter(cbind(x1,x2), xlab="MeanLog", ylab="SDLog", las=1, main="Density Plot of MeanLog and SDLog \nin Log-Normal Distribution Fitted to Resamples")

plot(c(1,max(cutoffs)), c(0,max(1.25*group_freq/diff(cutoffs)/sum(group_freq))), type = "n", xlab = "", ylab = "", las=1,
		main = "Density Plot of Observed Relative Frequency vs Log-Normal Distribution,\n log-scaled in x", log="x")
rect( 	xleft = 1+cutoffs[-length(cutoffs)], xright=1+cutoffs[-1], col="Lightblue", 
		ybottom = rep(0,length(cutoffs)-1), ytop=group_freq/diff(cutoffs)/sum(group_freq))
		
curve( dlnorm(x, meanlog=par_lnorm[1], sdlog=par_lnorm[2]), from=1, to=20000, add=T, lwd=5,col="Red")


#### Section 6 Discussion
	#### Distributions with higher numbers of parameters
	#### Overfitting to a small number of categories
	#### Estimation of values within categories: The conditional mean
	#### Better estimation of values using covariates and the ordinal logit