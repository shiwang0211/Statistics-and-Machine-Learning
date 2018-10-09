
data1 = rgamma(1000, shape = 2, scale = 3)

# 2(a)
options(repr.plot.width=5, repr.plot.height=5)
plot.ecdf(data1, col="red")

# 2(b)
X2 = seq(0,50,1)
data2 = sapply(X2, function(x) pgamma(q = x, shape = 2, scale = 3))

X3 = seq(0,0.99,0.01)
data3 = sapply(X3, function(x) qgamma(p = x, shape = 2, scale = 3))

plot.ecdf(data1, col="red")
lines(X2, data2, col="green")
lines(data3, X3, col="blue")
legend(20,0.5, legend=c("rgamma", "pgamma", "qgamma"), col=c("red", "green","blue"), lty=1, cex=0.8)

#3
r <- function() runif(1,0,1) > 0.5

sim <- function(K){
    result = array(0, dim = 1000)
    for (i in c(1:1000)){
        for (k in c(1:K)){
            result[i] = result[i] + r() * 2 / (3^k)
        }
    }
    return(result)
}

plot.ecdf(sim(1))

plot.ecdf(sim(2))

plot.ecdf(sim(5))

# 5
r = function(p) rbinom(1,1,0.1)

sim <- function(){
    r1=0
    r2=0
    p=0.9
    n=0
    while (r1==r2){
        r1 = r(p)
        r2 = r(p)
        n = n+1
    }
    return(n)
}

n_s= array(0, dim = 1000)
for(i in c(1:1000)){
    n_s[i] = sim()
}

mean(n_s)
