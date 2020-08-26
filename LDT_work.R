


flip_prob <-  function(lower, upper, n){
  vec <- numeric(n)
  prob <- NULL
  for (i in 1:n){
    if (i/n > lower & i/n < upper){
      vec[i] <- choose(n, i) * 1/(2^n)
    } else {vec[i] <- 0}
  }
  prob <<- sum(vec)
}

flip_prob(.475, .525, 16)
prob


results <- numeric(19)
for (i in 1:19){
  results[i] <- flip_prob(.05*i-.05, .05*i+.05, 128)
}

barplot(results, names.arg = seq(from = 0.05, to = .95, by = .05), 
        xlab = "average number of heads per toss",
        ylab = "probability of occurrence",
        main = "histogram for 128 tosses",
        ylim = c(0,1))

result.vec <- NULL
for (i in 1:250){
  result.vec[i] <- flip_prob(.6, 1, i+2)
}
result.df <- data.frame(n = seq(from = 1, to = 250, by = 1), prob = result.vec)
length(seq(from = 1, to = 250, by = 1))
length(result.vec)
result.df


coin.flip <- function(x){
  rate <<- log(2) + x*log(x) + (1-x)*log(1-x)
}
curve(coin.flip(x), from = 0, to = 1,
      xlab = "a",
      ylab = "I(a)",
      ylim = c(0,1))




install.packages('latex2exp')
library(latex2exp)

result.df %>%
  ggplot(aes(x = n, y = log(prob))) + 
  geom_line() +
  xlab("number of tosses (n)") + 
  ylab(TeX('log $P (S_n /n \\,  > \\, 0.6)$')) 


result.vec <- NULL
for (i in 1:250){
  result.vec[i] <- flip_prob(.9, 1, i+11)
}
result.df <- data.frame(n = seq(from = 1, to = 250, by = 1), prob = result.vec)

result.df %>%
  ggplot(aes(x = n, y = log(prob))) + 
  geom_line() +
  xlab("number of tosses (n)") + 
  ylab(TeX('$\\log P(S_n/n > .9)$')) +
  ylim(-50,0)



rateFn <- function(x,p){
  rate <<- x*log(x/p) + (1-x)*log((1-x)/(1-p))
}

rateFn(.5,.5)
rate
x<-0.9
p<-.5
x*log(x/p) + (1-x)*log((1-x)/(1-p))

plot(x, rateFn(x, .25),
     xlim = c(0.01,.9))

curve(rateFn(x, .25), from = 0, to = 1,
      xlab = "z",
      ylab = TeX('$I(z)$'))

curve(x^2/2, from = -2, to = 2,
      xlab = "z",
      ylab = "I(z)")

lambda <- NULL

expoI <- function(x, lambda){
  rate <<- x*lambda - 1 - log(lambda) - log(x)
}

curve(expoI(x, 1), from = 0.001, to = 10,
      xlab = "z",
      ylab = "I(z)",
      main = TeX('$\\lambda = 1$'))



curve(expoI(x, .1), from = 0.0, to = 100,
      xlab = "z",
      ylab = "I(z)",
      main = TeX('$\\lambda = 0.1$'))


m<-NULL
s <- NULL
z <- NULL

normalI <- function(m, s, x){
  rate <<- (s^2 + 2)*(x - m)^2/(2*s^2)
}

curve(normalI(5, 10, x), from = 0, to = 10,
      xlab = "z",
      ylab = "I(z)",
      main = TeX('$N(5,10^2)$'))
curve(normalI(5, 1, x), from = 0, to = 10,
      xlab = "z",
      ylab = "I(z)",
      main = TeX('$N(5,1^2)$'))


normalI(1,1,1)


