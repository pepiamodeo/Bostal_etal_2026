library(ggplot2)
library(dplyr)
library(ggrepel)
library(patchwork)
library(extrafont)
loadfonts()

index <- read.csv2("./table/index.csv", sep = ";", header = TRUE)

index$name.vec <- as.factor(index$name.vec)
index$alternative <- as.factor(index$alternative)
index$predation <- as.factor(index$predation)
index$rate <- as.factor(index$rate)

index <- index %>%
  group_by(name.vec, by_removal, n_removal, by_fertility, n_fertility,
           predation, rate) %>%
  mutate(
    management_effort = n_removal * by_removal + n_fertility * by_fertility,
    scenario = case_when(
      predation == "With puma" & rate == "Mean" ~ "With puma\nMean",
      predation == "With puma" & rate == "Max"  ~ "With puma\nMax",
      predation == "Without puma" & rate == "Mean" ~ "Without puma\nMean",
      predation == "Without puma" & rate == "Max"  ~ "Without puma\nMax"),
    scenario = factor(scenario, levels = c("Without puma\nMax", "Without puma\nMean", 
                                           "With puma\nMax", "With puma\nMean")),
    alt_me = paste0(alternative, " ", pmax(by_removal, by_fertility),"/",pmax(n_removal, n_fertility))) %>%
  ungroup()


#Rm#### 
plot_Rm <- index %>% filter(name.vec == "vlowdensity") 

label_Rm <- plot_Rm %>%
  filter(alternative == "n") %>%
  select(alternative, Erf, Rm, predation)

bubble_Rm <- ggplot(plot_Rm,
                    aes(x = Erf, y = Rm, size = management_effort, fill = alternative)) +
  geom_point(shape = 21, alpha = 0.5) +
  scale_size(range = c(1, 8)) +
  scale_fill_manual(values = c(
    "#1b9e77", "black", "#e7298a", "#7570b3", "#66a61e", "#d95f02")) +
  guides(fill = guide_legend(title.position = "top", 
                             override.aes = list(size = 4, alpha = 0.5)),
         size = guide_legend(title.position = "top")) +
  facet_wrap(predation ~.) +
  labs(x = "Erf", y = "log Rm") +
  geom_text_repel(data = label_Rm, aes(x = Erf, y = Rm, label = "n"),  color = "black", 
                  size = 7/.pt, box.padding = 0.1, min.segment.length = 1, 
                  segment.size = 0.2, show.legend = FALSE) + 
  scale_y_log10() +
  geom_hline(yintercept = 1/30, 
             linetype = "dashed", 
             color = "black", 
             linewidth = 0.4) +
  theme_minimal() +
  theme(
    text = element_text(family = "Calibri", size = 9),
    axis.title = element_text(family = "Calibri", size = 9),
    axis.text = element_text(family = "Calibri", size = 9),
    strip.text = element_text(family = "Calibri", size = 9),
    legend.position = "none")
  
#Re####
plot_Re <- index %>% filter(name.vec == "v2026")

label_Re <- plot_Re %>%
  filter(alternative == "n") %>%
  group_by(alternative, predation) %>%
  summarise(
    Erf = median(Erf),
    Re = median(Re),
    .groups = "drop"
  )

bubble_Re <- ggplot(plot_Re, 
                    aes(x = Erf, y = Re, size = management_effort, fill = alternative)) +
  geom_point(shape = 21, alpha = 0.5) +
  scale_size(range = c(1, 8), name = "Interval x\nMagnitude") +
  scale_fill_manual(values = c(
    "#1b9e77", "black", "#e7298a", "#7570b3", "#66a61e", "#d95f02")) +
  facet_wrap(predation ~.) +
  labs(x = "log Erf", y = "Re", fill = "Management type") +
  geom_text_repel(data = label_Re, aes(x = Erf, y = Re, label = "n"),  color = "black", 
                  size = 7/.pt, box.padding = 0.1, min.segment.length = 1, 
                  segment.size = 0.2, show.legend = FALSE) +  
  scale_x_log10() +
  theme_minimal() + 
  theme(
    text = element_text(family = "Calibri", size = 9),
    axis.title = element_text(family = "Calibri", size = 9),
    axis.text = element_text(family = "Calibri", size = 9),
    legend.title = element_text(family = "Calibri", size = 8),
    legend.text = element_text(family = "Calibri", size = 8),
    legend.position = "bottom",
    strip.text = element_blank(),
    strip.background = element_blank()) 



#combined and save####
combined_plot <- (bubble_Rm/bubble_Re)+
  plot_annotation(tag_levels = list(c("a)", "b)"))) &
  theme(
    plot.tag = element_text(family = "Calibri", size = 9),
    plot.tag.position = c(0, 1)
  )

ggsave(plot=combined_plot,"./fig/Re_Rm-plot_18_11.tiff",width=150,height=190,units="mm",
       dpi = 600,compression="lzw")



#SPANISH

bubble_Rm_es <- ggplot(plot_Rm,
                    aes(x = Erf, y = Rm, size = management_effort, fill = alternative)) +
  geom_point(shape = 21, alpha = 0.5) +
  scale_size(range = c(1, 8)) +
  scale_fill_manual(values = c(
    "#1b9e77", "black", "#e7298a", "#7570b3", "#66a61e", "#d95f02")) +
  guides(fill = guide_legend(title.position = "top", 
                             override.aes = list(size = 4, alpha = 0.5)),
         size = guide_legend(title.position = "top")) +
  facet_wrap(predation ~ ., 
             labeller = as_labeller(c(
               "With puma" = "Con puma",
               "Without puma" = "Sin puma"
             ))) +
  labs(x = "Erf", y = "log Rm") +
  geom_text_repel(data = label_Rm, aes(x = Erf, y = Rm, label = "n"),  color = "black", 
                  size = 7/.pt, box.padding = 0.1, min.segment.length = 1, 
                  segment.size = 0.2, show.legend = FALSE) + 
  scale_y_log10(labels = scales::label_number(decimal.mark = ",", 
                                  big.mark = ".")) +
  geom_hline(yintercept = 1/30, 
             linetype = "dashed", 
             color = "black", 
             linewidth = 0.4) +
  theme_minimal() +
  theme(
    text = element_text(family = "Calibri", size = 9),
    axis.title = element_text(family = "Calibri", size = 9),
    axis.text = element_text(family = "Calibri", size = 9),
    strip.text = element_text(family = "Calibri", size = 9),
    legend.position = "none")


bubble_Re_es <- ggplot(plot_Re, 
                    aes(x = Erf, y = Re, size = management_effort, fill = alternative)) +
  geom_point(shape = 21, alpha = 0.5) +
  scale_size(range = c(1, 8), name = "Intervalo x\nMagnitud") +
  scale_fill_manual(values = c(
    "#1b9e77", "black", "#e7298a", "#7570b3", "#66a61e", "#d95f02")) +
  facet_wrap(predation ~.) +
  labs(x = "log Erf", y = "Re", fill = "Tipo de manejo") +
  geom_text_repel(data = label_Re, aes(x = Erf, y = Re, label = "n"),  color = "black", 
                  size = 7/.pt, box.padding = 0.1, min.segment.length = 1, 
                  segment.size = 0.2, show.legend = FALSE) +  
  scale_x_log10() +
  theme_minimal() + 
  theme(
    text = element_text(family = "Calibri", size = 9),
    axis.title = element_text(family = "Calibri", size = 9),
    axis.text = element_text(family = "Calibri", size = 9),
    legend.title = element_text(family = "Calibri", size = 8),
    legend.text = element_text(family = "Calibri", size = 8),
    legend.position = "bottom",
    strip.text = element_blank(),
    strip.background = element_blank()) 

combined_plot_es <- (bubble_Rm_es / bubble_Re_es) +
  plot_annotation(tag_levels = list(c("a)", "b)"))) &
  theme(
    plot.tag = element_text(family = "Calibri", size = 9),
    plot.tag.position = c(0, 1)
  )

ggsave(plot=combined_plot_es,"./fig/Re_Rm-plot_es_12_11.tiff",width=150,height=190,units="mm",
       dpi = 600,compression="lzw")
