
# (a)
normal = function(mu, sigma) {
  d = length(mu)
  L = t(chol(sigma))
  res = rnorm(d)
  return(L %*% res + mu)
}

mu <- rep(0, 4)
sigma <- cbind(c(1,0,0,0), c(0,1,0,0), c(0,0,1,0), c(0,0,0,1))
sigma
hola <- normal(mu, sigma)
hola


# (b)
calc_discr = function(x, p, mu, sigma) {
  xnorm = x - mu
  inv = solve(sigma)
  d = length(mu)
  dt = det(sigma)
  return(-0.5*t(xnorm)%*%inv%*%xnorm-d/2*log(2*pi)-0.5*log(dt)+log(p))
}

p = 0.3
a = calc_discr(x, p, mu, sigma)
a

# (c)
# pre: length(x)=length(y)
euc = function(x, y, p = 2) {
  n = length(x)
  sum = 0
  for (i in 1:n) {
    sum = sum + abs(x[i]-y[i])^p
  }
  return(sum^(1/p))
}

euc2 = function(x,y) {
  dd = rbind(x,y)
  return(dist(dd))
}
  

x = c(0,0,0,0)
y = c(1,1,0,0)
euc2(x,y)
euc(x,y)
# (d)
mah = function(x, mu, sigma) {
  xnorm = x - mu
  return(sqrt(t(xnorm)%*%solve(sigma)%*%xnorm))
}

mah(x,y,sigma)