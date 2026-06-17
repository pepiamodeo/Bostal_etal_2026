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
  filter(predation=="Without predation") %>%
  mutate(x = case_when(
    alternative == "f" ~ n_fertility,
    alternative %in% c("rf", "rp") ~ n_removal,
    TRUE ~ n_fertility))

recovery_plot <- ggplot(df_plot, aes(x = factor(x), y = recovery, fill = rate)) +
  geom_col(position = position_dodge(width =0.9)) +
  facet_wrap(~ alternative,ncol=5, scales = "free_x") +
  scale_fill_manual(
    values = c("#E69F00", "#0072B2"),
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
    legend.position = c(.9,.9))


ggsave(plot=recovery_plot,"./fig3/recovery_plot.tiff",width=145,height=70,units="mm",
       dpi = 600,compression="lzw")
