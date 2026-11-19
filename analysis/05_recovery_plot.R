library(dplyr)
library(ggplot2)
library(ggbreak)
library(extrafont)
loadfonts()

recovery <- read.csv2("./table/recovery.csv", sep = ";", header = TRUE)

recovery$name.vec <- as.factor(recovery$name.vec)
recovery$alternative <- as.factor(recovery$alternative)
recovery$predation <- as.factor(recovery$predation)
recovery$rate <- as.factor(recovery$rate)

df_plot <- recovery %>%
  filter(predation=="Without puma") %>%
  mutate(x = case_when(
    alternative == "f" ~ n_fertility,
    alternative %in% c("rf", "rp") ~ n_removal,
    TRUE ~ n_fertility))

recovery_plot <- ggplot(df_plot, aes(x = factor(x), y = recovery, fill = rate)) +
  geom_col(position = position_dodge(width =0.9)) +
  facet_wrap(~ alternative,ncol=5, scales = "free_x") +
  scale_fill_manual(
    values = c("#1b9e77", "#d95f02", "#7570b3"),
    labels = c(
      "Max" = "Deterministic max",
      "Mean" = "Stochastic mean")) +
  labs(x = "Intervention magnitude", y = "Recovery time ", fill = "") +
  theme_minimal() +
  theme(
    text = element_text(color = "black", family = "Calibri", size = 9),
    axis.title = element_text(family = "Calibri", size = 9),
    axis.title.x = element_text(vjust = 6),
    axis.text = element_text(color = "black", family = "Calibri", size = 9),
    axis.text.x = element_text(vjust = 7),
    strip.text = element_text(family = "Calibri", size = 9),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.justification = "center",
    legend.direction = "horizontal")


ggsave(plot=recovery_plot,"./fig/recovery_plot_18_11.tiff",width=145,height=80,units="mm",
       dpi = 600,compression="lzw")


#SPANISH
recovery_plot_es <- ggplot(df_plot, aes(x = factor(x), y = recovery, fill = rate)) +
  geom_col(position = position_dodge(width =0.9)) +
  facet_wrap(~ alternative,ncol=5, scales = "free_x") +
  scale_fill_manual(
    values = c("#1b9e77", "#d95f02", "#7570b3"),
    labels = c(
      "Max" = "Máxima determinística",
      "Mean" = "Media determinística",
      "Mean_sto" = "Media estocástica")) +
  labs(x = "Magnitud de la intervención", y = "Tiempo de recuperación", fill = "") +
  theme_minimal() +
  theme(
    text = element_text(color = "black", family = "Calibri", size = 9),
    axis.title = element_text(family = "Calibri", size = 9),
    axis.title.x = element_text(vjust = 6),
    axis.text = element_text(color = "black", family = "Calibri", size = 9),
    axis.text.x = element_text(vjust = 7),
    strip.text = element_text(family = "Calibri", size = 9),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.justification = "center",
    legend.direction = "horizontal")


ggsave(plot=recovery_plot_es,"./fig/recovery_plot_es.tiff",width=145,height=80,units="mm",
       dpi = 600,compression="lzw")
