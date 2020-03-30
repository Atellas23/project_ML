
# (a)
normal = function(mu, sigma) {
  d = length(mu)
  L = t(chol(sigma))
  res = rnorm(d)
  return(L %*% res + mu)
}

mu <- rep(0, 4)
sigma <- cbind(c(2,1,0,0), c(1,3,0,0), c(0,0,3,0), c(0,0,0,1))
sigma
example <- normal(mu, sigma)
example


# (b)
calc_discr = function(x, p, mu, sigma) {
  xnorm = x - mu
  inv = solve(sigma)
  d = length(mu)
  dt = det(sigma)
  return(-0.5*t(xnorm)%*%inv%*%xnorm-d/2*log(2*pi)-0.5*log(dt)+log(p))
}
x = as.vector(c(1, 0, -1, 1))
p = 0.3
sigma = as.matrix(sigma)
a = calc_discr(x, p, mu, sigma)
a

# (c)
# pre: length(x)=length(y)
euc = function(x, y, p = 2) {
  sum <- sum(abs(x - y)^p)
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

