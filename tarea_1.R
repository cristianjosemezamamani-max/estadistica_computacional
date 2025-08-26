
set.seed(123)  
datos <- data.frame(
  Var1 = rnorm(100, mean = 50, sd = 10),     
  Var2 = rpois(100, lambda = 30),            
  Var3 = runif(100, min = 10, max = 100),   
  Var4 = rbinom(100, size = 100, prob = 0.4),
  Var5 = rexp(100, rate = 0.1),              
  Var6 = rchisq(100, df = 5)                
)

cv <- sapply(datos, function(x) sd(x) / mean(x) * 100)

print(cv)

barplot(cv,
        main = "Coeficiente de Variación por Variable",
        ylab = "Coeficiente de Variación (%)",
        xlab = "Variables",
        col = "steelblue",
        border = "black",
        ylim = c(0, max(cv) + 10))



