
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
  return(-0.5*t(xnorm)%*%solve(sigma)%*%xnorm-length(mu)/2*log(2*pi)-0.5*log(det(sigma))+log(p))
}

p = 0.3
a = calc_discr(x, p, mu, sigma)
a

# (c)
# pre: length(x)=length(y)
euc = function(x,y) {
  n = length(x)
  sum = 0
  for (i in 1:n) {
    sum = sum + (x[i]-y[i])^2
  }
  return(sqrt(sum))
}
  

x = c(0,0,0,0)
y = c(1,0,0,0)
euc(x,y)

# (d)
mah = function(x, mu, sigma) {
  xnorm = x - mu
  return(sqrt(t(xnorm)%*%solve(sigma)%*%xnorm))
}

mah(x,y,sigma)