library(ggplot2)
library(grid)
library(gridExtra)

###### create data, use example similar to that from George:
###### (http://dan.iel.fm/george/current/user/model/#a-simple-mean-model)

# model parameters
N <- 50
m <- 0.05
b <- 0.3
alpha <- -1
l <- 0.1
sigma <- 0.5
sigma_noise <- 0.05

# simulate data
yerr <- sigma_noise + 0.05 * runif(n=N, min=0, max=1)
n <- yerr * rnorm(n=N, mean=0, sd=1)
t <- runif(n=N, min=-5, max=5)
s <- b + m*t + alpha * exp(-(t-l)**2/2/sigma**2)
data <- s + n

# plot data
#qplot(t, data) + geom_errorbar(aes(ymin = data - yerr, ymax = data + yerr))

# likelihood function
log.likelihood <- function(params){
  m <- params[1]
  b <- params[2]
  alpha <- params[3]
  l <- params[4]
  sigma <- params[5]

  mean <- b + m*t + alpha * exp(-(t-l)**2/2/sigma**2)
  lnlike <- dnorm(data, mean=mean, sd=yerr, log=TRUE)
  return(sum(lnlike))

dnor
}

# prior function
log.prior <- function(params){

  pmin <- c(-10, -10, -10, -5, 0.1)
  pmax <- c(10, 10, 10, 5, 3)

  lpr = dunif(params, min=pmin, max=pmax, log=TRUE)
  return(sum(lpr))
}

# posterior function
log.posterior <- function(params){
  return(log.likelihood(params)+log.prior(params))
}

# simple proposal function
proposal <- function(params){

  # probability of jump scales sizes
  p <- c(0.05, 0.7, 0.25)
  sizes <- c(0.1, 1.0, 10.0)

  scale <- sample(sizes, size=1, prob=p, replace=TRUE)
  q <- rnorm(length(params), mean=params, sd=jsig*scale)
  return(q)
}

# simple MCMC
run.mcmc <- function(p0, N){
  chain <- array(dim=c(N+1,length(p0)))
  chain[1,] <- p0
  for (ii in 1:N){

    # proposal
    q <- proposal(chain[ii,])

    # posterior ratio
    ldiff <- log.posterior(q) - log.posterior(chain[ii,])
    if (ldiff >= log(runif(1))){
      chain[ii+1,] <- q
    } else{
      chain[ii+1,] = chain[ii,]
    }
  }
  return(chain)
}

params <- c(0.01, 0.2, -0.5, 0.2, 0.2)
jsig = c(0.1, 0.1, 0.1, 0.1, 0.1)
print(log.likelihood(params))
print(log.prior(params))
proposal(params)

# run sampler for 100000 samples
N <- 100000
chain <- run.mcmc(params, N)

# make plots
nparams <- length(params)
burn <- 5000
pars <- c('m', 'b', 'alpha', 'l', 'sigma')
truths <- c(m, b, alpha, l, sigma)
par(mfrow=c(nparams, nparams))
for (ii in 1:nparams){
  for (jj in 1:nparams){
    if (ii==jj){
      hist(chain[-(1:burn),ii], nclass=30)
      abline(v = truths[ii], col="red")
    } else if (ii<jj){
      plot(0,type='n',axes=FALSE,ann=FALSE)
    } else{
      plot(x=chain[-(1:burn),ii], y=chain[-(1:burn),jj])
      points(x=truths[ii], y=truths[jj], col='red', pch=4, lwd=2)
    }
  }
}
