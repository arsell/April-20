library(rethinking)
library(rstan)

data(rugged)

d <- rugged[complete.cases(rugged$rgdppc_2000),]
head(d)

hist(d$rgdppc_2000)

# make log version of outcome
d$log_gdp <- log( d$rgdppc_2000 )

hist(d$log_gdp)

# extract countries with GDP data
dd <- d[ complete.cases(d$rgdppc_2000) , ]

# split countries into Africa and not-Africa
d.A1 <- dd[ dd$cont_africa==1 , ] # Africa
d.A0 <- dd[ dd$cont_africa==0 , ] # not Africa

# Fitting separate models
A1_dat <- list(N = dim(d.A1)[1],
               l_gdp = d.A1$log_gdp,
               rugged = d.A1$rugged)
fit.A1 <- stan(file='africa.stan',
               data=A1_dat,
               iter=1000,
               chains=2)
print(fit.A1)

plot(fit.A1)
plot(As.mcmc.list(fit.A1, pars = "beta"))
pairs(fit.A1, pars = 'beta')



A0_dat <- list(N = dim(d.A0)[1],
               l_gdp = d.A0$log_gdp,
               rugged = d.A0$rugged)
fit.A0 <- stan(file='africa.stan',
               data=A0_dat,
               iter=1000,
               chains=2)
print(fit.A0)

plot(fit.A0)
plot(As.mcmc.list(fit.A0, pars = "beta"))
pairs(fit.A0, pars = "beta")





#############################################
# African nations
m7.1 <- map(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged ,
    a ~ dnorm( 8 , 100 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=d.A1 )
# non-African nations
m7.2 <- map(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged ,
    a ~ dnorm( 8 , 100 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ) ,
  data=d.A0 )
##################################################

# Interaction term
inter_dat <- list(N = dim(d)[1],
                  l_gdp = d$log_gdp,
                  rugged = d$rugged,
                  africa = d$cont_africa)
fit.inter <- stan(file = 'africa2.stan',
                  data = inter_dat,
                  iter = 1000, chains = 2)
print(fit.inter)
plot(fit.inter, show_density = TRUE, ci_level = 0.95, fill_color = "purple")
stan_hist(fit.inter)
preds <- extract(fit.inter, 'l_gdp_pred')$l_gdp_pred # extract predictions
hist(apply(preds, 2, mean)) # Average value at each observation
hist(preds[,1]) # Histogram of samples for 1st observation
plot(d$log_gdp, apply(preds, 2, mean)) # How do the predictions match up with the real values
cor(d$log_gdp, apply(preds, 2, mean))

