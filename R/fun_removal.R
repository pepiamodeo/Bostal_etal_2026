# funcion para aplicar remocion de individuos

fun_removal <- function(type=removal_type,
                        t=t,
                        proj=proj,
                        n_removal=n_removal,
                        vec_t_removal=vec_t_removal,
                        vec_removal=vec_removal){
  
  if (length(vec_t_removal) == 0) return(proj) #NUEVONUEVO
  
  # fixed
  if(type=="fixed"){
    if (t %in% vec_t_removal) {
      tmp <- proj[t, 4] - n_removal
      proj[t, 4] <- tmp
      
      if (proj[t, 4] < 0) {
        proj[t, 4] <- 0
        vec_removal[t] <<- tmp + n_removal
      }else{
        vec_removal[t] <<- n_removal
      }
    }
    
  }else{ # proportional
    
    if (t %in% vec_t_removal) {
      Nvec <- proj[t, ]
      #Nvec <- c(0, 0, 0, 0)
      Ntot <- sum(Nvec)
      #n_removal <- 100
      
      if (Ntot > 0) {
        prop <- Nvec / Ntot # proporción por clase
        removal_vec <- n_removal * prop # remoción proporcional
        
        real_removal <- pmin(removal_vec, Nvec) #NUEVO 
        excess <- sum(removal_vec - real_removal) #NUEVO
        
        available_adults <- max(0, Nvec[4] - real_removal[4]) #NUEVO
        extra_adults <- min(excess, available_adults) #NUEVO
        
        real_removal[4] <- real_removal[4] + extra_adults #NUEVO
        
        proj[t, ] <- Nvec - real_removal # aplicar remoción
        vec_removal[t] <<- sum(real_removal)
        
      }else{
        proj[t, ] <- rep(0, length(Nvec))
        vec_removal[t] <<- 0
      }
    }
  }
  return(proj)
}

