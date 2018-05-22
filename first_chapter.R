xx = faithful$eruptions
fit = density(xx)
plot(fit)

## Dibujo de una función de densidad e intervalos de confianza bootstrap
xx <- faithful$eruptions
fit1 <- density(xx)
fit2 <- replicate(10000, {
  x <- sample(xx,replace=TRUE);
  density(x, from=min(fit1$x), to=max(fit1$x))$y
})
fit3 <- apply(fit2, 1, quantile,c(0.025,0.975))
plot(fit1, ylim=range(fit3))
polygon(c(fit1$x,rev(fit1$x)),
        c(fit3[1,], rev(fit3[2,])),
        col='grey', border=F)
lines(fit1)

## Secuencia de fibonacci 

fibR = function(n){
  if (n==0) return(0)
  if (n==1) return(1)
  return(fibR(n-1)+fibR(n-2))
}

## Secuencia de fibonacci en c++
# La secuencia de fibonacci está escrita en c++ en el archivo fibonacci.cpp
sourceCpp("fibonacci.cpp")

# también puedo utilizar el paquete inline 

if (!require(inline)) install.packages("inline"); require(inline)
if (!require(rbenchmark)) install.packages("rbenchmark"); require(rbenchmark)

## Este es el cuerpo de la expresión que se utiliza a continuación 
# y está escrito en c++
incltxt <- '
int fibonacci(const int x) { 
  if (x == 0) return(0);
  if (x == 1) return(1);
  return fibonacci(x - 1) + fibonacci(x - 2);
}'

fibRcpp <- cxxfunction(signature(xs="int"),
                       plugin="Rcpp",
                       incl=incltxt,
                       body='
                       int x = Rcpp::as<int>(xs);
                       return Rcpp::wrap( fibonacci(x) );
                       ')

# La función hecha con c++ es significativamente más rápida
within(benchmark(sapply(1:10, fibR),
                 sapply(1:10, fibRcpp),
                 sapply(1:10, fibonacci),
                 replications = 100, 
                 columns = c('test', 'replications', 'elapsed'),
                 order = c('test','replications')), 
       {average = elapsed/replications})


## Solución con memoria 

# El uso de la función local me permite evaluar una expresión sin guardarla y pasarla a una función
mfibR <- local({
  memo <- c(1, 1, rep(NA, 1000))
  f <- function(x) {
    if (x == 0) return(0)
    if (x < 0) return(NA)
    if (x > length(memo))
      stop("x too big for implementation")
    if (!is.na(memo[x])) return(memo[x])
    ans <- f(x-2) + f(x-1)
    memo[x] <<- ans
    ans
  }
})

