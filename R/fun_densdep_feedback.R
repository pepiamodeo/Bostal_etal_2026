
# funcion que aplica densodependencia

# compensatory density feedback in survival
# La curva define cómo se reduce (compensación) el parámetro que se va a afectar, en distintos niveles de N respecto a K
 
# generacion de la funcion feedback function

# potrillos
vec_K_f <- c(1,0.75*K, 0.85*K, K, 1.5*K) 
vec_reduction_f <- c(1,0.99, 0.95,0.9, 0.6)
df_K_f <- data.frame(vec_K_f, vec_reduction_f)

# adultos
vec_K_a <- c(1,0.75*K, 0.95*K, K, 1.5*K) 
vec_reduction_a <- c(1,0.99, 0.95,0.9, 0.6)
df_K_a <- data.frame(vec_K_a, vec_reduction_a)

# fecundidad adultos
vec_K_fa <- c(1, 0.20*K, 0.30*K, K, 1.5*K)
vec_reduction_fa <- c(1.30, 1.20, 1.15, 1.00, 0.9)
df_K_fa <- data.frame(vec_K_fa, vec_reduction_fa)

# logistic power function a/(1+(x/b)^c)
# feedback function foals

fit.f <- nls(vec_reduction_f ~ a/(1+(vec_K_f/b)^c), 
                  data = df_K_f,
                  algorithm = "port",
                  start = c(a = 1, b = 2*K, c = 2), # paramentros iniciales
                  nls.control(maxiter = 1000, tol = 1e-05, minFactor = 1/1024))

pred_N_f <- data.frame(vec_K_f=seq(1,4*K,1))
pred_reduction_f<-predict(fit.f,newdata = pred_N_f)

# feedback function adults
fit.a <- nls(vec_reduction_a ~ a/(1+(vec_K_a/b)^c), 
             data = df_K_a,
             algorithm = "port",
             start = c(a = 1, b = 2*K, c = 2), # paramentros iniciales
             nls.control(maxiter = 1000, tol = 1e-05, minFactor = 1/1024))

pred_N_a <- data.frame(vec_K_a=seq(1,4*K,1))
pred_reduction_a<-predict(fit.a,newdata = pred_N_a)


# feedback function fecundity adults
fit.fa <- nls(vec_reduction_fa ~ a/(1+(vec_K_fa/b)^c), 
             data = df_K_fa,
             algorithm = "port",
             start = c(a = 1, b = 2*K, c = 2), # paramentros iniciales
             nls.control(maxiter = 1000, tol = 1e-05, minFactor = 1/1024))

pred_N_fa <- data.frame(vec_K_fa=seq(1,4*K,1))
pred_reduction_fa<-predict(fit.fa,newdata = pred_N_fa)


plot(vec_K_a, vec_reduction_a, pch=19,xlab="N",ylab="reduction factor",
      xlim = c(0, 1000), ylim = c(0,1.5))
lines(pred_N_a$vec_K, pred_reduction_a, lty=3,lwd=3,col="blue")
lines(pred_N_f$vec_K, pred_reduction_f, lty=3,lwd=3,col="green")
lines(pred_N_fa$vec_K, pred_reduction_fa, lty=3,lwd=3,col="red")
legend("topright",
       legend = c("Sup Adults", "Sup Foals", "Fecundity"),
       col = c("blue", "green", "red"),
       lty = 3,
       lwd = 3)

# función que crea el modificador de la matriz
fun_densdep_feedback<- function(Nad=Nad){
  mat_modifier<-matrix(1,4,4)
  mat_modifier[2,1]<-predict(fit.f,newdata = data.frame(vec_K_f=Nad)) # afecto supervivencia de potrillos
  mat_modifier[4,4]<-predict(fit.a,newdata = data.frame(vec_K_a=Nad)) # afecto upervivencia de adultos
  mat_modifier[1,4]<-predict(fit.fa,newdata = data.frame(vec_K_fa=Nad)) #NEW NEW NEW
  return(mat_modifier)
}
