#===============================================================================
#                              Resampling Methods
#===============================================================================

# Parameters --> sigma=2.5, beta_0=2.3 , beta_1=6.4 , beta_2=15.8, beta_3=7.5

set.seed(7)
e = rnorm(100, mean = 0, sd = 2.5)
x1 = rbinom(100, 10, 0.45)
x2 = rexp(100, 2)
x3 = rchisq(100, 15)
y = 2.3 + 6.4*x1 + 15.8*x2 + 7.5*x3 + e

# Now we divide the total observations in 5 parts, each consists of 20 obs.
LM1=lm(y[1:80]~(x1[1:80]+x2[1:80]+x3[1:80]))
y_new=fitted.values(LM1)
y_new[81:100]=3.021+ 6.378*x1[81:100]+15.348*x2[81:100]+7.507*x3[81:100]

mse1=sum((y[1:20]-y_new[1:20])^2)/(20-1)
mse2=sum((y[21:40]-y_new[21:40])^2)/(20-1)
mse3=sum((y[41:60]-y_new[41:40])^2)/(20-1)
mse4=sum((y[61:80]-y_new[61:80])^2)/(20-1)
mse5=sum((y[81:100]-y_new[81:100])^2)/(20-1)

CV=mean(c(mse1, mse2, mse3, mse4, mse5))
CV