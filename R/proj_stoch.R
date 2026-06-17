# library
library(popdemo)
source("./R/fun_removal.R")

# function to create stochastic projection and obtain the outcomes

# la funcion esta estructurada en el siguiente orden
## 1) densodependencia
## 2) proyeccion de la siguiente generacion
## 3) aplicacion de la remoción de individuos

proj.stoch <- function(data, ini.vec,               # insumos basicos
                       dur_proj = 50,               # años de proyección, por defecto 50
                       iterations = 1000,           # numero de iteraciones
                       K = Inf,                     # capacidad de carga
                       n_removal = 0,               # cantidad total removida
                       vec_t_removal = -9999,
                       vec_t_fertility = -9999,
                       n_fertility = 0,
                       vec_fertility = vec_fertility) {
  
  #Check primitivity, irreducibility and ergodicity
  
  #isPrimitive(mat)})=="FALSE"))
  #print(which(sapply(X=list.mat, FUN=function(x){isIrreducible(A=x)})=="FALSE"))
  #print(which(sapply(X=list.mat, FUN=function(x){isErgodic(A=x)})=="FALSE"))
  
  # Stochastic projection
  
  #stochastic growth rate
  #growth.rate<-stoch(list.mat, c("lambda", "var"), vector = ini.vec,
  #                     iterations = iterations, discard = 100, Aseq = Aseq)
  
  # Output vectors
  
  list.iterations <- list()
  list.loghistory <- list()
  list.rates <- list()
  list.vec_fertility <- list()
  
  for (i in 1:iterations) {
    proj <- matrix(0, nrow = dur_proj, ncol = length(ini.vec))
    proj[1, ] <- ini.vec
    
    rates_iter <- vector("list", dur_proj) #NUEVONUEVO
    
    for (t in 2:dur_proj) {
      mat <- matrix_build_stoch(parameters=data) #
      
      # Modificaciones ADHOC (densodependencia, control)

      # 1) densodependencia (modificacion de la matriz previo a proyección)
      Nad <- proj[t-1, 4] # adults del tiempo anterior
   
      # funcion que aplica densodependencia
      mat<-mat*fun_densdep_feedback(Nad=Nad)
      
      #correccion de fecundidad segun datos historicos Ransom et al., 2016
      mat[1,4] <- min(0.451, mat[1,4]) #NEW NEW
      mat[1,4] <- max(0.115, mat[1,4]) #NEW NEW
      
      # 1.1) control de fertilidad

      if (t %in% vec_t_fertility) {
        # evitar reproducción negativa
        treated <- min(Nad, n_fertility) #NUEVO cuantas puedo tratar realmente
        repcontrol <- max(0, Nad - (treated * 0.9))
        
        vec_fertility[[t]] <<- treated #NUEVONUEVO para obtener los valores efectivos de hembras tratadas
        
        proj[t,1] <- mat[1,4] * repcontrol # reduce el numero de hembras que se reproducen (90% de efectividad de la vacuna)
        
        # solo ajustar supervivencia si hay adultos
        if (!is.na(Nad) && Nad > 0) {
          mat[4,4] <- mat[4,4] * ((Nad - n_fertility)/Nad) +
            mat[4,4] * (n_fertility/Nad) * 1.02 # FOLT ET AL 2023 cuando no estan preñadas, su supervivencia aumente 1.02 veces
          mat[4,4] <- min(1, mat[4,4]) # correccion para que la sup no supere 1
        }
        proj[t,2:4] <- mat[2:4,1:4] %*% proj[t-1,] 
        } else {
        proj[t,] <- mat %*% proj[t-1,] # 2) proyección de siguiente generación sin control de Fecundidad
        vec_fertility[[t]] <<- 0 #NUEVONUEVO
        }
      
      rates_iter[[t]] <- mat #NUEVONUEVO para chequear como funciona el ajuste de densodep
      
      
      # 3) Remoción (Modificacion del vector proyectado post-proyección)
      # Apica remocion en t
      
      
      proj <- fun_removal(type=removal_type,
                          t=t,
                          proj=proj,
                          n_removal=n_removal, 
                          vec_t_removal=vec_t_removal)
      
      #NUEVO redondear los valores para evitar valores con coma
      proj[t, ] <- floor(proj[t, ])
    }

      list.iterations[[i]]<-proj
      list.loghistory[[i]]<-list(removal=vec_removal)
      list.rates[[i]] <- rates_iter #NUEVONUEVO
      list.vec_fertility[[i]] <- vec_fertility #NUEVONUEVO
}
  
  assign(x = paste("proj", name, sep = ""), 
         value = list.iterations, envir = .GlobalEnv)
  assign(x = paste("loghistory", name, sep = ""), 
         value = list.loghistory, envir = .GlobalEnv)
  assign(x = paste("logrates", name, sep = ""), 
         value = list.rates, envir = .GlobalEnv)
  assign(x = paste("logvec_fertility", name, sep = ""), 
         value = list.vec_fertility, envir = .GlobalEnv)
}

