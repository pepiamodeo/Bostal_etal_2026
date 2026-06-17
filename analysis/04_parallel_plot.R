library(dplyr)
library(tidyr)
library(ggplot2)
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
      predation == "With predation" & rate == "Mean" ~ "With predation\nStochastic mean",
      predation == "With predation" & rate == "Max"  ~ "With predation\nDeterministic max",
      predation == "Without predation" & rate == "Mean" ~ "Without predation\nStochastic mean",
      predation == "Without predation" & rate == "Max"  ~ "Without predation\nDeterministic max"),
    scenario = factor(scenario, levels = c("Without predation\nDeterministic max", "Without predation\nStochastic mean", 
                                           "With predation\nDeterministic max", "With predation\nStochastic mean")),
    alt_me = paste0(alternative, " ", pmax(by_removal, by_fertility),"/",pmax(n_removal, n_fertility))) %>%
  ungroup()

eti_10 <- index %>%
  filter(name.vec == "v2026") %>%
  group_by(scenario) %>%
  arrange(desc(ETI)) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  select(rank, scenario, alt_me) %>%
  pivot_wider(
    names_from = scenario,
    values_from = alt_me
  ) %>%
  arrange(rank)

# plot ####
eti_table <- index %>%
  filter(name.vec == "v2026") %>%
  group_by(scenario) %>%
  arrange(desc(ETI), .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  ungroup()

top_eti <- eti_table %>%
  filter(rank <= 10) %>%
  pull(alt_me) %>%
  unique()

eti_plot <- eti_table %>%
  filter(alt_me %in% top_eti)

eti_plot <- eti_plot %>%
  mutate(rank_plot = ifelse(rank > 10, 11, rank))

letters_custom <- LETTERS[9:26]  # i hasta z

eti_plot <- eti_plot %>%
  mutate(
    rank_plot = ifelse(rank > 10, 11, rank),
    letter = letters_custom[as.numeric(as.factor(alt_me))],
    alt_letter = paste(letter, alt_me)
  )

leg <- distinct(eti_plot, alt_me, letter)
leg <- leg %>% arrange(letter)

eti_plot <- eti_plot %>% arrange(letter)

eti_rank_plot <- ggplot(eti_plot, aes(x = scenario, y = rank, 
                     group = alt_me, label = letter)) +
  geom_line(aes(color = alt_me), alpha = 1, size = 2, show.legend = FALSE) +
  geom_point(aes(color = alt_me), alpha = 1, size = 7,
             key_glyph = draw_key_text) +
  geom_text(size = 3) +
  scale_y_reverse(breaks = 1:10) +
  scale_x_discrete(expand = c(0.05, 0.05)) + 
  coord_cartesian(ylim = c(1,10)) +
  scale_color_manual(
    values = c("#a6cee3","#1f78b4","#b2df8a","#33a02c",
               "#fb9a99","#e31a1c","#fdbf6f","#ff7f00",
               "#cab2d6","#6a3d9a","#b15928","#8c510a"),
    breaks = leg$alt_me, labels = leg$alt_me) +
  labs(x = "Scenario", y = "Rank") + 
  guides(color = guide_legend(override.aes = list(label = unique(eti_plot$letter),
                                                  size = 5))) +
  theme_minimal() +
  theme(panel.grid.minor.y = element_blank()) + 
  theme(legend.title = element_blank(),
        axis.text = element_text(color = "black", family = "Calibri", size = 9),
        axis.title = element_text(family = "Calibri", size = 9),
        text = element_text(color = "black", family = "Calibri", size = 9)) +
  theme(
    legend.key.width = unit(0.9, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.margin = margin(0, 0, 0, 0),
    legend.text = element_text(size = 9),
    legend.position = "right",
    legend.justification = "top")

ggsave(plot=eti_rank_plot,"./fig/eti_rank_plot.tiff",width=150,height=100,units="mm",
       dpi = 600,compression="lzw")

#IEM####

mti_table <- index %>%
  filter(name.vec == "vlowdensity" & Rm > 0.03) %>%
  group_by(scenario) %>%
  arrange(desc(MTI), .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  ungroup()

top_mti <- mti_table %>%
  filter(rank <= 5) %>%
  pull(alt_me) %>%
  unique()

mti_plot <- mti_table %>%
  filter(alt_me %in% top_mti)

mti_plot <- mti_plot %>%
  mutate(rank_plot = ifelse(rank > 5, 6, rank))

mti_plot <- mti_plot %>%
  mutate(rank_plot = ifelse(rank > 6, 6, rank),
         letter = LETTERS[as.numeric(as.factor(alt_me))],
         alt_letter = paste(letter, alt_me))

leg_mti <- distinct(mti_plot, alt_me, letter)
leg_mti <- leg_mti %>% arrange(letter)

mti_plot <- mti_plot %>% arrange(letter)

mti_rank_plot <- ggplot(mti_plot, aes(x = scenario, y = rank, 
                                      group = alt_me, label = letter)) +
  geom_line(aes(color = alt_me), alpha = 1, size = 2, show.legend = FALSE) +
  geom_point(aes(color = alt_me), alpha = 1, size = 7,
             key_glyph = draw_key_text) +
  geom_text(size = 3) +
  scale_y_reverse(breaks = 1:5) +
  scale_x_discrete(expand = c(0.05, 0.05), drop = FALSE) + 
  coord_cartesian(ylim = c(1,5)) +
  scale_color_manual(
    values = c("#a6cee3","#1f78b4","#b2df8a","#33a02c",
               "#fb9a99","#e31a1c","#fdbf6f","#ff7f00"),
    breaks = leg_mti$alt_me, labels = leg_mti$alt_me) +
  labs(x = "Scenario", y = "Rank") + 
  guides(color = guide_legend(override.aes = list(label = unique(mti_plot$letter),
                                                  size = 5))) +
  theme_minimal() +
  theme(panel.grid.minor.y = element_blank()) + 
  theme(legend.title = element_blank(),
        axis.text = element_text(color = "black", family = "Calibri", size = 9),
        axis.title = element_text(color = "black", family = "Calibri", size = 9),
        text = element_text(color = "black", family = "Calibri", size = 9)) +
  theme(
    legend.key.width = unit(0.9, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.margin = margin(0, 0, 0, 0),
    legend.text = element_text(size = 9),
    legend.position = "right",
    legend.justification = "top")
mti_rank_plot        


ggsave(plot=mti_rank_plot,"./fig/mti_rank_plot.tiff",width=150,height=80,units="mm",
       dpi = 600,compression="lzw")
