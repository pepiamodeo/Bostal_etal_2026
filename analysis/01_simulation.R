rm(list=ls())

#Library

library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)

# load custom functions 
source("./R/fun_matrix_build.R") # data loading and matrix construction
source("./R/proj_stoch.R") # projection and outcomes

v2026 <- c(53, 8, 4, 253)
vlowdensity <- c(13, 12, 10, 60)
nlowdensity <- sum(vlowdensity)

ini.vec <- vlowdensity 
name.vec <- "vlowdensity"

durproj <- 50 #project duration
y_star <- 2 #year in which the interventions would begin to be applied
K <- 240 #carrying capacity (k) value
source("./R/fun_densdep_feedback.R") # configuro la densodependencia en funcion de K

n_iterations <- 10 #nro of iterations

# removal
removal_type <- "fixed"  #"fixed" or "proportional"
by_removal <- 0 #cada cuanto se hacen las remociones
n_removal <- 0 #cuatos individuos son removidos

if (by_removal == 0) {
  vec_t_removal <- numeric(0)  # no hay intervenciones
  } else {
    vec_t_removal <- seq(y_star, durproj, by = by_removal) # defino años de intervención
  }

vec_removal <- rep(0,durproj)

# fertility
by_fertility <- 2
n_fertility <- 75

if (by_fertility == 0) {
  vec_t_fertility <- numeric(0)  # no hay intervenciones
  } else {
    vec_t_fertility<-seq(y_star,durproj,by_fertility) # defino años de intervención
    }

vec_fertility <- rep(0, durproj)


mt <- c()
if (n_removal != 0) { mt <- c(mt, ifelse(removal_type == "fixed", "rf", "rp")) }
if (n_fertility != 0) { mt <- c(mt, "f") }
alternative <- if (length(mt) == 0) { "n" } else { paste(mt, collapse = " + ") }

me <- paste0(pmax(by_removal, by_fertility),"/",pmax(n_removal, n_fertility))

suffix <- paste(name.vec, alternative, me, sep = "_") 


#puma####
name="Puma"
data_puma<-read.csv("./data/data_predation_stochastic.csv",sep=";") 
matrix_build_det(parameters=data_puma) # generacion de matriz deterministica
project(A=mat.m,vector=ini.vec) # proyección deterministica
matrix_build_stoch(parameters=data_puma) # generacion de matriz estocastica (una de muestra)
proj.stoch(data=data_puma,ini.vec,
           iterations=n_iterations,K=k_value,
           vec_t_removal=vec_t_removal,
           vec_t_fertility=vec_t_fertility,
           n_removal=n_removal,
           n_fertility=n_fertility,
           dur_proj=durproj,
           vec_fertility = vec_fertility)


name="Puma_max"
data_puma_max<-read.csv("./data/data_predation_deterministic_max.csv",sep=";") 
matrix_build_det(parameters=data_puma_max) # generacion de matriz deterministica
project(A=mat.m,vector=ini.vec) # proyección deterministica
matrix_build_stoch(parameters=data_puma_max) # generacion de matriz estocastica (una de muestra)
proj.stoch(data=data_puma_max,ini.vec,
           iterations=n_iterations,K=k_value,
           vec_t_removal=vec_t_removal,
           vec_t_fertility=vec_t_fertility,
           n_removal=n_removal,
           n_fertility=n_fertility,
           dur_proj=durproj,
           vec_fertility = vec_fertility)

#No puma####
name="NoPuma"
data_nopuma<-read.csv("./data/data_nopredation_stochastic.csv",sep=";") 
matrix_build_det(parameters=data_nopuma) 
project(A=mat.m,vector=ini.vec) # proyección deterministica
matrix_build_stoch(parameters=data_nopuma) # generacion de matriz estocastica (una de muestra)
proj.stoch(data=data_nopuma,ini.vec,
           iterations=n_iterations,K=k_value,
           vec_t_removal=vec_t_removal,
           vec_t_fertility=vec_t_fertility,
           n_removal=n_removal,
           n_fertility=n_fertility,
           dur_proj=durproj,
           vec_fertility = vec_fertility)


name="NoPuma_max"
data_nopuma_max<-read.csv("./data/data_nopredation_deterministic_max.csv",sep=";") 
matrix_build_det(parameters=data_nopuma_max) 
project(A=mat.m,vector=ini.vec) # proyección deterministica
matrix_build_stoch(parameters=data_nopuma_max) # generacion de matriz estocastica (una de muestra)
proj.stoch(data=data_nopuma_max,ini.vec,
           iterations=n_iterations,K=k_value,
           vec_t_removal=vec_t_removal,
           vec_t_fertility=vec_t_fertility,
           n_removal=n_removal,
           n_fertility=n_fertility,
           dur_proj=durproj,
           vec_fertility = vec_fertility) 


#Figure####
make_df <- function(proj, proj_max, label) {
  df <- bind_rows(melt(proj), melt(proj_max))
  names(df) <- c("time", "stage", "proj", "iteration")
  df$stage <- factor(df$stage, labels = c("f", "y", "j", "a"))
  df$iteration <- as.factor(df$iteration)
  df$predation <- c(
    rep(label, nrow(melt(proj))),
    rep(paste(label, "max"), nrow(melt(proj_max))))
  df <- dcast(df, time + stage + iteration ~ predation, value.var = "proj")
  names(df)[4:5] <- c("proj", "proj_max")
  df$predation <- label
  df
}

df_NoPuma_all <- make_df(projNoPuma, projNoPuma_max, "Without predation")
df_Puma_all   <- make_df(projPuma, projPuma_max, "With predation")
df_all <- rbind(df_NoPuma_all, df_Puma_all)

#Figure 1: stage population size
ggplot(df_all, aes(x = time, y = proj, colour = predation)) +
  geom_line(aes(group = interaction(iteration, predation)),
    alpha = 2 / n_iterations) +
  stat_summary(aes(group = predation),
    geom = "line", fun = mean, linewidth = 1) +
  stat_summary(aes(y = proj_max, group = predation),
    geom = "line", fun = mean, linetype = "dashed", alpha = 0.5) +
  facet_wrap(~stage, scales = "free_y") +
  labs(x = "Time (years)", y = "n", title = suffix, colour = NULL) +
  theme_minimal()
#ggsave(paste0("./fig/fig_stage_puma_all", suffix, ".png"), width=180,height=140,units="mm", dpi = 600)

#Figure 2: adult size####
df_ad<-df_all[df_all$stage=="a",]

ggplot(data=df_ad,aes(x=time,y=proj, colour=predation))+
  geom_line(
    aes(group=interaction(iteration, predation)),
    alpha=1/n_iterations) +
  stat_summary(geom = "line",fun.data = mean_se)+
  stat_summary(aes(y=proj_max),geom = "line", fun = mean,
               linetype="dashed",alpha=0.3)+
  labs(x = "Time (years)" , y = "Adults")+
  theme_minimal()+
  theme(legend.position = "bottom", legend.title = element_blank())

#ggsave(paste0("./fig/adults_all", suffix, ".png"), width=180,height=140,units="mm",dpi = 600)

#Figure 3: total size####
df_T <- df_all %>%
  group_by(iteration, time, predation) %>%
  summarise(
    proj = sum(proj, na.rm = TRUE),
    proj_max = sum(proj_max, na.rm = TRUE),
    .groups = "drop")

ggplot(data=df_T,aes(x=time,y=proj, colour=predation)) +
  geom_line(aes(group=interaction(iteration, predation)), alpha=1/n_iterations) +
  stat_summary(geom = "line",fun.data = mean_se) +
  stat_summary(aes(y=proj_max),geom = "line", fun = mean, linetype="dashed",alpha=0.3) +
  labs(x = "Time (years)" , y = "Population Size", title = suffix) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())
#ggsave(paste0("./fig/total_all", suffix, ".png"), width = 180, height = 140, units = "mm", dpi = 600)

#indice####
ntotal <- df_all %>%
  group_by(iteration, time, predation) %>%
  summarise(
    Mean = sum(proj, na.rm = TRUE),
    Max = sum(proj_max, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(Mean, Max),
    names_to = "rate",
    values_to = "n")

  #remocion efectiva/tiempo
  Er_puma_max <- sum(round(rowMeans(data.frame(loghistoryPuma_max))))/durproj
  Er_puma_mean <- sum(round(rowMeans(data.frame(loghistoryPuma))))/durproj
  Er_nopuma_mean <- sum(round(rowMeans(data.frame(loghistoryNoPuma))))/durproj
  Er_nopuma_max <- sum(round(rowMeans(data.frame(loghistoryNoPuma_max))))/durproj
  
  Ef_puma_max <- sum(round(rowMeans(data.frame(logvec_fertilityPuma_max))))/durproj
  Ef_puma_mean <- sum(round(rowMeans(data.frame(logvec_fertilityPuma))))/durproj
  Ef_nopuma_mean <- sum(round(rowMeans(data.frame(logvec_fertilityNoPuma))))/durproj
  Ef_nopuma_max <- sum(round(rowMeans(data.frame(logvec_fertilityNoPuma_max))))/durproj
  
  #Rm y Re
  Rm_iter <- ntotal %>%
    group_by(iteration, predation, rate) %>%
    summarise(
      dev_mean = mean(abs(n - nlowdensity), na.rm = TRUE),
      Rm = 1 / dev_mean,
      .groups = "drop")
  
  Rm <- Rm_iter %>%
    group_by(predation, rate) %>%
    summarise(
      Rm = mean(Rm, na.rm = TRUE),
      .groups = "drop")
  
  Re <- ntotal %>%
    group_by(predation, rate, time) %>%
    summarise(
      prop_ext = mean(n == 0),
      .groups = "drop"
    ) %>%
    group_by(predation, rate) %>%
    summarise(
      Re = mean(prop_ext)*100,
      .groups = "drop"
    )
  
  ext50 <- ntotal %>%
    filter(time == 50) %>%
    group_by(predation, rate) %>%
    summarise(
      ext50 = mean(n == 0, na.rm = TRUE),
      .groups = "drop"
    )
  
  #dataframe
  ind <- data.frame(
    name.vec = rep(name.vec, 4),
    alternative = rep(alternative, 4),
    by_removal = rep(by_removal, 4),
    n_removal = rep(n_removal, 4),
    by_fertility = rep(by_fertility, 4),
    n_fertility = rep(n_fertility, 4),
    predation = c("With predation", "With predation", "Without predation", "Without predation"),
    rate = c("Mean", "Max", "Mean", "Max"),
    Er = c(Er_puma_mean, Er_puma_max,
           Er_nopuma_mean, Er_nopuma_max),
    Ef = c(Ef_puma_mean, Ef_puma_max,
           Ef_nopuma_mean, Ef_nopuma_max)) %>%
    left_join(Rm, by = c("predation", "rate")) %>%
    left_join(ext50, by = c("predation", "rate")) %>%
    left_join(Re, by = c("predation", "rate"))
  
  #IER = Re/(Er+Ef)  IEM = Rm/(Er + Ef)
  EMTI <- ind %>%
    group_by(name.vec, alternative, by_removal,n_removal,by_fertility,n_fertility,predation,rate) %>%
    summarise(
      Erf = Er+Ef,
      ETI = Re * exp(-0.1*Erf), 
      MTI = Rm * exp(-0.01*Erf),
      .groups = "drop")
  
  
  ind <- ind %>%
    left_join(EMTI, by = c("name.vec", "alternative","by_removal","n_removal","by_fertility","n_fertility","predation","rate"))

write.csv2(ind,paste0("./table/index_", suffix, ".csv"), row.names = FALSE, quote = FALSE)


#recoery time if by_X = 0
rec_it <- ntotal %>%
  group_by(iteration, predation, rate) %>%
  summarise(
    N_ref = n[time == (y_star-1)][1],
    recovery_time = {
      t_rec <- time[time > y_star & n >= N_ref] - y_star
      if (length(t_rec) == 0) NA else min(t_rec) },
    N_recovery = n[time == recovery_time] [1],
    .groups = "drop")

rec_t <- rec_it %>%
  group_by(predation, rate) %>%
  summarise(
    recovery = mean(recovery_time, na.rm = TRUE),
    sd = sd(recovery_time, na.rm = TRUE),
    .groups = "drop")

recovery_t <- data.frame(
  name.vec = rep(name.vec, 4),
  alternative = rep(alternative, 4),
  by_removal = rep(by_removal, 4),
  n_removal = rep(n_removal, 4),
  by_fertility = rep(by_fertility, 4),
  n_fertility = rep(n_fertility, 4),
  predation = c("With predation", "With predation", "Without predation", "Without predation"),
  rate = c("Mean", "Max", "Mean", "Max"))

recovery_t <- recovery_t %>%
  left_join(rec_t, by = c("predation","rate"))

write.csv2(recovery_t,paste0("./table/recovery_", suffix, ".csv"), row.names = FALSE, quote = FALSE)

