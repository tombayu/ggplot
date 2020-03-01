rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(ggsci)
library(ggpubr)
library(showtext)

fams <- c("Aman Alfon Saruruk", "Aman Aturan Sarorougot", "Aman Santo Sakukuret")

# Function?
# familyGraph <- function(data, title, legendTitle, limit) {
#   graphs <- list()
#   for (i in seq_along(data)) {
#     ggplot(data = data[[i]], aes(x = Meals, y = Occurence)) +
#       geom_bar(stat = "identity", aes(fill = Meals)) +
#       facet_wrap(. ~Month, ncol = 12, strip.position = "bottom") +
#       labs(subtitle = fams[[i]], fill = "Meal with") +
#       scale_y_continuous(expand = c(0,0)) + 
#       ylim(0, 65) +
#       scale_fill_grey() +
#       theme_minimal() +
#       theme(
#         axis.text.x = element_blank(), panel.grid.major.x = element_blank(),
#         axis.title.x = element_blank(), axis.title.y = element_blank(),
#         panel.grid.minor = element_line(size = 0.2), panel.grid.major = element_line(size = 0.3),
#         plot.subtitle = element_text(size = 11))
#   }
# }

# 1. STAPLES

dir <- list.files("data/", pattern = glob2rx("*staple.csv"), full.names = T)
staple <- list()
stapleFigList <- list()

for (i in seq_along(dir)) {
  staple[[i]] <- read.csv(dir[[i]], sep = ";") %>%
    select(-starts_with("X")) %>%
    gather(Month, Occurence, Jan:Dec)
  staple[[i]]$Month <- factor(staple[[i]]$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
}

for (i in seq_along(dir)) {
  stapleFigList[[i]] <- ggplot(data = staple[[i]], aes(x = Meals, y = Occurence)) +
    geom_bar(stat = "identity", aes(fill = Meals)) +
    facet_wrap(. ~Month, ncol = 12, strip.position = "bottom") +
    labs(subtitle = fams[[i]], fill = "") +
    scale_y_continuous(expand = c(0,0)) + 
    ylim(0, 65) +
    scale_fill_grey() +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(), panel.grid.major.x = element_blank(),
      axis.title.x = element_blank(), axis.title.y = element_blank(),
      panel.grid.minor = element_line(size = 0.2), panel.grid.major = element_line(size = 0.3),
      plot.subtitle = element_text(size = 10))
}

stapleFigs <- ggarrange(stapleFigList[[1]], stapleFigList[[2]], stapleFigList[[3]],
          common.legend = T, legend = "bottom",
          nrow = 3)
# annotate_figure(stapleFigs, top = text_grob("Recorded Staple Meals", size = 14))
annotate_figure(stapleFigs, left = text_grob("Number of meals", size = 10, rot = 90))
ggsave("01_staples.png", path = "export", height = 15, width = 20, units = "cm", dpi = 500)

###

# 2. MEAL COMBINATION

dir <- list.files("data/", pattern = glob2rx("*combi.csv"), full.names = T)
combi <- list()
combiFigList <- list()

for (i in seq_along(dir)) {
  combi[[i]] <- read.csv(dir[[i]], sep = ";") %>%
    select(-starts_with("X")) %>%
    gather(Month, Occurence, Jan:Dec)
  combi[[i]]$Month <- factor(combi[[i]]$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  combi[[i]]$Meals <- factor(combi[[i]]$Meals, levels = c("1 staple", "2 staples", "3 staples", "4 staples"))
}

for (i in seq_along(dir)) {
  combiFigList[[i]] <- ggplot(data = combi[[i]], aes(x = Meals, y = Occurence)) +
    geom_bar(stat = "identity", aes(fill = Meals)) +
    facet_wrap(. ~Month, ncol = 12, strip.position = "bottom") +
    labs(subtitle = fams[[i]], fill = "") +
    scale_y_continuous(expand = c(0,0)) + 
    ylim(0, 60) +
    scale_fill_grey() +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(), panel.grid.major.x = element_blank(),
      axis.title.x = element_blank(), axis.title.y = element_blank(),
      panel.grid.minor = element_line(size = 0.2), panel.grid.major = element_line(size = 0.3),
      plot.subtitle = element_text(size = 10))
  }

combiFigs <- ggarrange(combiFigList[[1]], combiFigList[[2]], combiFigList[[3]],
                       common.legend = T, legend = "bottom",
                       nrow = 3)
# annotate_figure(combiFigs, top = text_grob("Combination of Staple Meals", size = 14))
annotate_figure(combiFigs, left = text_grob("Number of meals", size = 10, rot = 90))
ggsave("02_combi.png", path = "export", height = 15, width = 20, units = "cm", dpi = 500)

###

# 3. MEALS WITH MEAT

dir <- list.files("data/", pattern = glob2rx("*meat.csv"), full.names = T)
meat <- list()
meatFigList <- list()

for (i in seq_along(dir)) {
  meat[[i]] <- read.csv(dir[[i]], sep = ";") %>%
    select(-starts_with("X")) %>%
    gather(Month, Occurence, Jan:Dec)
  meat[[i]]$Month <- factor(meat[[i]]$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  meat[[i]]$Family <- factor(meat[[i]]$Family, levels = c("Aman Alfon Saruruk", "Aman Aturan Sarorougot", "Aman Santo Sakukuret"))
}

# Legend on the bottom
ggplot(data = meat[[1]], aes(x = Month, y = Occurence, group = Family)) + #adjust data and x-axis
  geom_line(alpha = 0.7) + #adjust x-axis
  geom_point(size = 2, aes(shape = Family)) +
  labs(color = "Family") + #legend title
  scale_y_continuous(expand = c(0,0)) + 
  ylim(0, 70) + #ylimit
  # scale_fill_grey() +
  # scale_color_npg() +
  scale_shape_manual(values = c(0, 1, 2)) +
  guides(shape = guide_legend(nrow = 1)) +
  ylab("Number of meals") +
  # ggtitle("Meals with Meat") +
  theme_minimal() +
  theme(
    legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 8),
    legend.margin = margin(0, 0, 0, 0), legend.box.margin = margin(-10, -10, 0, -10),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(vjust = 7),
    axis.title.x = element_blank(), axis.title.y = element_text(size = 10),
    panel.grid.minor = element_line(size = 0.1), panel.grid.major = element_line(size = 0.3))

ggsave("03_meat_bottom.png", path = "export", height = 12, width = 15, units = "cm", dpi = 500)

# Legend on the top
ggplot(data = meat[[1]], aes(x = Month, y = Occurence, group = Family)) + #adjust data and x-axis
  geom_line(alpha = 0.7) + #adjust x-axis
  geom_point(size = 2, aes(shape = Family)) +
  labs(color = "Family") + #legend title
  scale_y_continuous(expand = c(0,0)) + 
  ylim(0, 70) + #ylimit
  # scale_fill_grey() +
  # scale_color_npg() +
  scale_shape_manual(values = c(0, 1, 2)) +
  guides(shape = guide_legend(nrow = 1)) +
  ylab("Number of meals") +
  # ggtitle("Meals with Meat") +
  theme_minimal() +
  theme(
    legend.position = "top", legend.title = element_blank(), legend.text = element_text(size = 8),
    legend.margin = margin(0, 0, 0, 0), legend.box.margin = margin(0, -10, -10, -10),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(vjust = 7),
    axis.title.x = element_blank(), axis.title.y = element_text(size = 10),
    panel.grid.minor = element_line(size = 0.1), panel.grid.major = element_line(size = 0.3))

ggsave("03_meat_top.png", path = "export", height = 12, width = 15, units = "cm", dpi = 500)

###

# 4. TYPE OF MEATS

dir <- list.files("data/", pattern = glob2rx("*meats.csv"), full.names = T)
types <- list()
typesFigList <- list()

for (i in seq_along(dir)) {
  types[[i]] <- read.csv(dir[[i]], sep = ";") %>%
    select(-starts_with("X")) %>%
    gather(Month, Occurence, Jan:Dec)
  types[[i]]$Month <- factor(types[[i]]$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  }

for (i in seq_along(dir)) {
  typesFigList[[i]] <- ggplot(data = types[[i]], aes(x = Type, y = Occurence)) + #adjust data and x-axis
    geom_bar(stat = "identity", aes(fill = Type)) + #adjust x-axis
    facet_wrap(. ~Month, ncol = 12, strip.position = "bottom") +
    labs(subtitle = fams[[i]], fill = "") + #legend title
    scale_y_continuous(expand = c(0,0)) + 
    ylim(0, 46) + #ylimit
    # scale_fill_grey() +
    scale_fill_npg() +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(), panel.grid.major.x = element_blank(),
      axis.title.x = element_blank(), axis.title.y = element_blank(),
      panel.grid.minor = element_blank(), panel.grid.major = element_line(size = 0.3),
      plot.subtitle = element_text(size = 10))
}

typesFigs <- ggarrange(typesFigList[[1]], typesFigList[[2]], typesFigList[[3]],
                        common.legend = T, legend = "bottom",
                        nrow = 3)
annotate_figure(typesFigs, left = text_grob("Number of meals", size = 10, rot = 90))
# typesFigs
ggsave("04_typesofmeatcol.png", path = "export", height = 15, width = 27, units = "cm", dpi = 500)
###

# 5. SOURCES OF MEAT

dir <- list.files("data/", pattern = glob2rx("*source.csv"), full.names = T)
source <- list()
sourceFigList <- list()

for (i in seq_along(dir)) {
  source[[i]] <- read.csv(dir[[i]], sep = ";") %>%
    select(-starts_with("X")) %>%
    gather(Month, Occurence, Jan:Dec)
  source[[i]]$Month <- factor(source[[i]]$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  source[[i]]$Source <- factor(source[[i]]$Source, levels = c("Market", "Gathering/Fishing", "Domestic Animal", "Hunting", "Others"))
  }

for (i in seq_along(dir)) {
  sourceFigList[[i]] <- ggplot(data = source[[i]], aes(x = Source, y = Occurence)) + #adjust data and x-axis
    geom_bar(stat = "identity", aes(fill = Source)) + #adjust x-axis
    facet_wrap(. ~Month, ncol = 12, strip.position = "bottom") +
    labs(subtitle = fams[[i]], fill = "") + #legend title
    scale_y_continuous(expand = c(0,0)) + 
    ylim(0, 47) + #ylimit
    scale_fill_npg() +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(), panel.grid.major.x = element_blank(),
      axis.title.x = element_blank(), axis.title.y = element_blank(),
      panel.grid.minor = element_blank(), panel.grid.major = element_line(size = 0.3),
      plot.subtitle = element_text(size = 10))
}

sourceFigs <- ggarrange(sourceFigList[[1]], sourceFigList[[2]], sourceFigList[[3]],
                        common.legend = T, legend = "bottom",
                        nrow = 3)
annotate_figure(sourceFigs, left = text_grob("Number of meals", size = 10, rot = 90))
# sourceFigs
ggsave("05_sources.png", path = "export", height = 15, width = 20, units = "cm", dpi = 500)

###

# 6. FRUITS AND VEGS
# FRUITS

dir <- list.files("data/", pattern = glob2rx("*fruits.csv"), full.names = T)
fruit <- list()
fruitFigList <- list()

for (i in seq_along(dir)) {
  fruit[[i]] <- read.csv(dir[[i]], sep = ";") %>%
    select(-starts_with("X")) %>%
    gather(Month, Occurence, Jan:Dec)
  fruit[[i]]$Month <- factor(fruit[[i]]$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  fruit[[i]]$Family <- factor(fruit[[i]]$Family, levels = c("Aman Alfon Saruruk", "Aman Aturan Sarorougot", "Aman Santo Sakukuret"))
}

ggplot(data = fruit[[1]], aes(x = Month, y = Occurence, group = Family)) + #adjust data and x-axis
  geom_line(alpha = 0.7) + #adjust x-axis
  geom_point(size = 2, aes(shape = Family)) +
  labs(color = "Family") + #legend title
  scale_y_continuous(expand = c(0,0)) + 
  ylim(-1, 43) + #ylimit
  # scale_fill_grey() +
  # scale_color_npg() +
  scale_shape_manual(values = c(0, 1, 2)) +
  guides(shape = guide_legend(nrow = 1)) +
  ylab("Number of meals") +
  # ggtitle("Fruit Consumption") +
  theme_minimal() +
  theme(
    legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 8),
    legend.margin = margin(0, 0, 0, 0), legend.box.margin = margin(-10, -10, 0, -10),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(vjust = 7),
    axis.title.x = element_blank(), axis.title.y = element_text(size = 10),
    panel.grid.minor = element_line(size = 0.1), panel.grid.major = element_line(size = 0.3))

ggsave("06_fruits_bottom.png", path = "export", height = 12, width = 15, units = "cm", dpi = 500)

ggplot(data = fruit[[1]], aes(x = Month, y = Occurence, group = Family)) + #adjust data and x-axis
  geom_line(alpha = 0.7) + #adjust x-axis
  geom_point(size = 2, aes(shape = Family)) +
  labs(color = "Family") + #legend title
  scale_y_continuous(expand = c(0,0)) + 
  ylim(-1, 43) + #ylimit
  # scale_fill_grey() +
  # scale_color_npg() +
  scale_shape_manual(values = c(0, 1, 2)) +
  guides(shape = guide_legend(nrow = 1)) +
  ylab("Number of meals") +
  # ggtitle("Fruit Consumption") +
  theme_minimal() +
  theme(
    legend.position = "top", legend.title = element_blank(), legend.text = element_text(size = 8),
    legend.margin = margin(0, 0, 0, 0), legend.box.margin = margin(0, -10, -10, -10),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(vjust = 7),
    axis.title.x = element_blank(), axis.title.y = element_text(size = 10),
    panel.grid.minor = element_line(size = 0.1), panel.grid.major = element_line(size = 0.3))

ggsave("06_fruits_top.png", path = "export", height = 12, width = 15, units = "cm", dpi = 500)

# VEGS

dir <- list.files("data/", pattern = glob2rx("*vegs.csv"), full.names = T)
vegs <- list()
vegsFigList <- list()

for (i in seq_along(dir)) {
  vegs[[i]] <- read.csv(dir[[i]], sep = ";") %>%
    select(-starts_with("X")) %>%
    gather(Month, Occurence, Jan:Dec)
  vegs[[i]]$Month <- factor(vegs[[i]]$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  vegs[[i]]$Family <- factor(vegs[[i]]$Family, levels = c("Aman Alfon Saruruk", "Aman Aturan Sarorougot", "Aman Santo Sakukuret"))
}

ggplot(data = vegs[[1]], aes(x = Month, y = Occurence, group = Family)) + #adjust data and x-axis
  geom_line(alpha = 0.7) + #adjust x-axis
  geom_point(size = 2, aes(shape = Family)) +
  labs(color = "Family") + #legend title
  scale_y_continuous(expand = c(0,0)) + 
  ylim(-1, 43) + #ylimit
  # scale_fill_grey() +
  # scale_color_npg() +
  scale_shape_manual(values = c(0, 1, 2)) +
  guides(shape = guide_legend(nrow = 1)) +
  ylab("Number of meals") +
  # ggtitle("Fruit Consumption") +
  theme_minimal() +
  theme(
    legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 8),
    legend.margin = margin(0, 0, 0, 0), legend.box.margin = margin(-10, -10, 0, -10),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(vjust = 7),
    axis.title.x = element_blank(), axis.title.y = element_text(size = 10),
    panel.grid.minor = element_line(size = 0.1), panel.grid.major = element_line(size = 0.3))

ggsave("06_vegs_bottom.png", path = "export", height = 12, width = 15, units = "cm", dpi = 500)

ggplot(data = vegs[[1]], aes(x = Month, y = Occurence, group = Family)) + #adjust data and x-axis
  geom_line(alpha = 0.7) + #adjust x-axis
  geom_point(size = 2, aes(shape = Family)) +
  labs(color = "Family") + #legend title
  scale_y_continuous(expand = c(0,0)) + 
  ylim(-1, 43) + #ylimit
  # scale_fill_grey() +
  # scale_color_npg() +
  scale_shape_manual(values = c(0, 1, 2)) +
  guides(shape = guide_legend(nrow = 1)) +
  ylab("Number of meals") +
  # ggtitle("Fruit Consumption") +
  theme_minimal() +
  theme(
    legend.position = "top", legend.title = element_blank(), legend.text = element_text(size = 8),
    legend.margin = margin(0, 0, 0, 0), legend.box.margin = margin(0, -10, -10, -10),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(vjust = 7),
    axis.title.x = element_blank(), axis.title.y = element_text(size = 10),
    panel.grid.minor = element_line(size = 0.1), panel.grid.major = element_line(size = 0.3))

ggsave("06_vegs_top.png", path = "export", height = 12, width = 15, units = "cm", dpi = 500)

###

# 7. MODES OF EATING

dir <- list.files("data/", pattern = glob2rx("*modes.csv"), full.names = T)
modes <- list()
modesFigList <- list()

for (i in seq_along(dir)) {
  modes[[i]] <- read.csv(dir[[i]], sep = ";") %>%
    select(-starts_with("X")) %>%
    gather(Month, Occurence, Jan:Dec)
  modes[[i]]$Month <- factor(modes[[i]]$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  modes[[i]]$Modes <- factor(modes[[i]]$Modes, levels = c("Eating in the lalep", "Eating in the punen", "Eating in non-ritual feast", "Eating in restaurants"))
}

# With numbers
for (i in seq_along(dir)) {
  modesFigList[[i]] <- ggplot(data = modes[[i]], aes(x = Modes, y = Occurence)) + #adjust data and x-axis
    geom_bar(stat = "identity", aes(fill = Modes)) + #adjust x-axis
    geom_text(aes(label = Occurence), position = position_dodge(width = 0.9), vjust = -0.25, size = 2) +
    facet_wrap(. ~Month, ncol = 12, strip.position = "bottom") +
    labs(subtitle = fams[[i]], fill = "") + #legend title
    scale_y_continuous(expand = c(0,0)) + 
    ylim(0, 95) + #ylimit
    scale_fill_grey() +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(), panel.grid.major.x = element_blank(),
      axis.title.x = element_blank(), axis.title.y = element_blank(),
      panel.grid.minor = element_blank(), panel.grid.major = element_line(size = 0.3),
      plot.subtitle = element_text(size = 10))
}

modesFigs <- ggarrange(modesFigList[[1]], modesFigList[[2]], modesFigList[[3]],
                        common.legend = T, legend = "bottom",
                        nrow = 3)
annotate_figure(modesFigs, left = text_grob("Number of meals", size = 10, rot = 90))
# modesFigs
ggsave("07_eatingmodes_wnumbers.png", path = "export", height = 15, width = 20, units = "cm", dpi = 500)

# No numbers
for (i in seq_along(dir)) {
  modesFigList[[i]] <- ggplot(data = modes[[i]], aes(x = Modes, y = Occurence)) + #adjust data and x-axis
    geom_bar(stat = "identity", aes(fill = Modes)) + #adjust x-axis
    # geom_text(aes(label = Occurence), position = position_dodge(width = 0.9), vjust = -0.25, size = 2) +
    facet_wrap(. ~Month, ncol = 12, strip.position = "bottom") +
    labs(subtitle = fams[[i]], fill = "") + #legend title
    scale_y_continuous(expand = c(0,0)) + 
    ylim(0, 95) + #ylimit
    scale_fill_grey() +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(), panel.grid.major.x = element_blank(),
      axis.title.x = element_blank(), axis.title.y = element_blank(),
      panel.grid.minor = element_blank(), panel.grid.major = element_line(size = 0.3),
      plot.subtitle = element_text(size = 10))
}

modesFigs <- ggarrange(modesFigList[[1]], modesFigList[[2]], modesFigList[[3]],
                       common.legend = T, legend = "bottom",
                       nrow = 3)
annotate_figure(modesFigs, left = text_grob("Number of meals", size = 10, rot = 90))
# modesFigs
ggsave("07_eatingmodes_nonumbers.png", path = "export", height = 15, width = 20, units = "cm", dpi = 500)

###
