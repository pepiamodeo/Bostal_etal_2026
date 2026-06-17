# library
library(popdemo)
source("./R/fun_calculations.R") 

# DETERMINISTIC MATRIX

# matrix loading

matrix_build_det <- function(parameters){
  
  stg.ID<- unique(parameters[,1])
  
  # parameters
  
  parameters[,3] 
  
  # annual survival 
  mu.f<-parameters[1,3] 
  mu.y<-parameters[2,3]
  mu.j<-parameters[3,3]
  
  mu.a<-parameters[4,3]
  
  #correcion adultos (es necesario truncarlos, ponerle un limite)
  
  d.a<-29 #how long live a feral horse in nature in Ransom et al., ‘Wild and Feral Equid Population Dynamics’.
  
  mu.ac<-sup_correction(sup=mu.a,d=d.a)[[1]]
  
  # fertility
  
  mu.fa<-parameters[5,3]
  
  # DETERMINISTIC MATRIX ####
  
  mat.m<-matrix(0,4,4,dimnames=list(stg.ID,stg.ID)) 
  
  mat.m[2,1]<-mu.f
  mat.m[3,2]<-mu.y
  mat.m[4,3]<-mu.j
  mat.m[4,4]<-mu.ac
  
  mat.m[1,4]<-mu.fa
  
  assign(x="mat.m",value=mat.m,envir = .GlobalEnv)
  return(mat.m)
}

# STOCHASTIC MATRIX

matrix_build_stoch <- function(parameters){

  stg.ID<- unique(parameters[,1])
  
  # parameters
  
  parameters[,3] 
  
  # annual survival 
  mu.f<-parameters[1,3] 
  mu.y<-parameters[2,3]
  mu.j<-parameters[3,3]
  
  mu.a<-parameters[4,3]
  
  #correcion adultos (es necesario truncarlos, ponerle un limite)
  
  d.a<-25 #cuanto vive un caballo en la naturaleza
  
  mu.ac<-sup_correction(sup=mu.a,d=d.a)[[1]]
  
    # fertility
  
  mu.fa<-parameters[5,3]
  
  #standard deviation for each parameter
  
  parameters[,4] 
  
  s.f<-parameters[1,4]
  s.y<-parameters[2,4]
  s.j<-parameters[3,4]
  s.a<-parameters[4,4]

  s.fa<-parameters[5,4]
  
  # STOCHASTIC MATRIX ####
  
  # function to build many probable matrices based on distributions  
  
  matrix.build<-function(){
    sf<-rnormt(1,mean=mu.f,sd=s.f)
    sy<-rnormt(1,mean=mu.y,sd=s.y)
    sj<-rnormt(1,mean=mu.j,sd=s.j)
    sa<-rnormt(1,mean=mu.a,sd=s.a)
    
    sac <- sup_correction(sup=sa,d=d.a)[[1]]
    
    
    fa<-rnorm(1,mean=mu.fa,sd=s.fa)
    
    mat<-matrix(0,4,4,dimnames=list(stg.ID,stg.ID)) # creo una matriz en blanco
    
    mat[2,1]<-sf
    mat[3,2]<-sy
    mat[4,3]<-sj
    mat[4,4]<-sac
    
    mat[1,4]<-fa
    return(mat)
    }
  mat<-matrix.build()
  
  # proceso de control. Si la matriz no es reproducible, la reemplaza
  # imprime un cartel con el numero de intento para detectar si se quedó ciclando
  # if not irreducible, repeat
  intento<-1
  while(!isIrreducible(mat)){
    cat(paste0("Matriz no Irreducible, recalculando... intento ", intento),"\n")
    mat<-matrix.build()
    intento<-intento+1
    }
  
  assign(x="mat.s",value=mat,envir = .GlobalEnv)    
  return(mat)
  
}
