#Problem 6.1
#There is an urn containing 9 balls which are either red or green.  The number of red balls in the urn 
#is not known.  One ball is drawn at random from the urn and it's color observed.

#a) What is the Bayesian universe of the experiment.
merge(0:9,0:1)

#b) Let X be the number of red balls in the urn.  Assume that all possible values of X
#from 0 to 9 are equally likely.  Let Y1=1 if the first ball drawn is red and Y1=0 otherwise.  Fill in
#the joint probability table for X and Y1.

X = 0:9
prior = rep(.10,10)
Y1_eq_0 = prior * (9:0 / 9)
Y1_eq_1 = prior * (0:9 / 9)
df = data.frame(X, prior, Y1_eq_0, Y1_eq_1)
df

#c) Find the marginal distribution of Y1 and put it in the table:
marg_Y1_eq_0 = sum(Y1_eq_0)
marg_Y1_eq_1 = sum(Y1_eq_1)

#d) Suppose a red ball is drawn.  What is the reduced Bayesian universe?
merge(0:9,1)

#e) Calculate the posterior probability distribution of X.
posterior = (prior * Y1_eq_1) / marg_Y1_eq_1
df = data.frame(X, prior, Y1_eq_0, Y1_eq_1, posterior)
df

#f) Find the posterior distribution of X by filling in the simplified table:
likelihood = 0:9 / 9
pl = prior * likelihood
marginal_prob = sum(pl)
posterior = pl / marginal_prob
df = data.frame(X, prior, likelihood, pl, posterior)
df

#Problem 6.2
#Suppose that a second ball is drawn from the urn (w/o replacement).  Use the posterior distribution of X from the previous question as the prior distribution
#Suppose the second ball is green (ie Y2=0).  Find the posterior distribution of X by filling in the simplified table.
prior = posterior
likelihood = c(8,8:0) / 8   #The min fn() accepts but doesn't return a vector
pl = prior * likelihood
marginal_prob = sum(pl)
posterior = pl / marginal_prob
df = data.frame(X, prior, likelihood, pl, posterior)
df

#Problem 6.3
#Suppose we look at the two draws from the urn (w/o replacement) as a single experiment.  The results were first
#draw red, second draw green.  Find the posterior distribution of X by filling the the simplified table.
X = 0:9
prior = rep(.10,10)
likelihood = (0:9 / 9) * c(8,8:0 / 8)
pl = prior * likelihood
marginal_prob = sum(pl)
posterior = pl / marginal_prob
df = data.frame(X, prior, likelihood, pl, posterior)
df

#Problem 6.4
#The code below uses the binomial distribution functions in stats::Binomial
#The following are coin toss examples:
#> dbinom(0:4, 4, .5)  
#[1] 0.0625 0.2500 0.3750 0.2500 0.0625
#> pbinom(0:4, 4, .5)
#[1] 0.0625 0.3125 0.6875 0.9375 1.0000

pd = seq(.2,.8,.2)
prior = rep(.25,4)
likelihood = dbinom(7,10,pd)
lp = likelihood * prior
marginal_prob = sum(lp)
posterior = lp / marginal_prob
posterior
df = data.frame(pd, prior, likelihood, lp, marginal_prob, posterior)
df

#Problem 6.5
n = 5
prior = posterior
likelihood = dbinom(2,5,pd)
pl = likelihood * prior
marginal_prob = sum(lp)
posterior = pl / marginal_prob
posterior
df = data.frame(pd, prior, likelihood, pl, marginal_prob, posterior)
df

#Problem 6.6
#Suppose we combine all the n=15 trials and think of them as a single experiement where we observed a total
#of 9 successes.  Start with the initial equally weighted prior from exercise 6.4 and find the posterior 
#after the single combined experiement.  What do the results of 6.4 - 6.6 show? 

pd = seq(.2,.8,.2)
prior = rep(.25,4)
likelihood = dbinom(9,15,pd)
lp = likelihood * prior
marginal_prob = sum(lp)
posterior = lp / marginal_prob
posterior
df = data.frame(pd, prior, likelihood, lp, marginal_prob, posterior)
df

#Problem 6.7
#This problem makes use of the dpois function in the stats::Poisson package.

mu = 1:5
prior = rep(1/5,5)
likelihood = dpois(2,mu)
pl = prior * likelihood
marginal_prob = sum(pl)
posterior = pl / marginal_prob
data.frame(mu, prior, likelihood, pl, posterior)


