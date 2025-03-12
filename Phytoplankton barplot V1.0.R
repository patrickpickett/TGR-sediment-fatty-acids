###############################################################################
#Script for creating phytoplankton barplots for TGR 2021
#Pat P 23/01/2025
###############################################################################

#load packages------------------------------------------------------------------

library(devtools)
library(tidyverse)
library(ggplot2)
library(janitor)
library(lubridate)
library(ggpubr)
library(ggh4x)
library(randomcoloR)
library(grid)
library(here)
library(cowplot)
library(gridtext)
library(ggtext)
library(rwantshue)


#extra functions----------------------------------------------------------------

#Suppresses unwanted labels on x-axis of plots
delete_no_display <- function(v) {
  if_else(str_detect(v, '-1'), '', v)
}

#work around for issues with get_plot from cowplot package
get_legend_fix <- function(plot, legend = NULL) {
  gt <- ggplotGrob(plot)
  
  pattern <- "guide-box"
  if (!is.null(legend)) {
    pattern <- paste0(pattern, "-", legend)
  }
  
  indices <- grep(pattern, gt$layout$name)
  
  not_empty <- !vapply(gt$grobs[indices],
                       inherits,
                       what = "zeroGrob",
                       FUN.VALUE = logical(1))
  indices <- indices[not_empty]
  
  if (length(indices) > 0) {
    return(gt$grobs[[indices[1]]])
  }
  return(NULL)
}

#takes text and converts it into an expression
toexpr <- function(x) {
  getfun <- function(x) {
    ifelse(x == "Grouped (<0.05)", "plain", "italic")
  }
  as.expression(unname(Map(
    function(f, v)
      substitute(f(v), list(f = as.name(f), v = as.character(v))), getfun(x), x
  )))
}


#set input directory------------------------------------------------------------
inputDir <- here::here('dataOrg', 'outputs', 'data')
OutputDir <- here::here('outputs', 'R')


#load data----------------------------------------------------------------------

algae_phylum_df <- read.csv(
  file.path(inputDir, 'algal_phylum_2021.csv'),
  check.names = FALSE,
  skip = 1
)
algae_biomass_df <- read.csv(
  file.path(inputDir, 'algal_biomass_2021.csv'),
  check.names = FALSE,
  skip = 1
)

#set plot theme-----------------------------------------------------------------

#Customize plot
theme_biomarker <- theme(
  axis.text.x = element_text(
    size = 7,
    angle = 45,
    hjust = 1
  ),
  axis.text.y = element_text(size = 7),
  strip.text = element_text(size = 7),
  legend.title = element_text(size = 12),
  legend.text = element_text(lineheight = .8, size = 7),
  legend.key.height = unit(.6, "cm"),
  strip.background = element_blank(),
  #axis.title.y = element_text(vjust = 4, size = 12),
  plot.margin = margin(
    t = 0,
    # Top margin
    r = 0,
    # Right margin
    b = 2,
    # Bottom margin
    l = 10
  )
) # Left margin


#Use classic theme as a base & join custom theme
theme_classic_biomarker <- theme_classic() + theme_biomarker

#data tidy----------------------------------------------------------------------

#state labels
state.labs <- c("Winter", "Spring", "Summer")
#state.labs <- c("Pre-bloom", "Algal bloom", "Monsoon")
names(state.labs) <- c("Pre-bloom", "Algal bloom", "Monsoon")

#day labels
day.labs <- c('', '', '', '', '', '')
names(day.labs) <- c('P1', 'PX4', 'PX5', 'PX6', 'MD', 'M1')

#phylum data
algae_phylum_cut <- algae_phylum_df %>%
  select(1:7, 10:15) %>%
  mutate(date = dmy(date)) %>%
  mutate(day = yday(date)) %>%
  mutate(state = month) %>%
  mutate_at("state", str_replace, "March", "Pre-bloom") %>%
  mutate_at("state", str_replace, "May", "Algal bloom") %>%
  mutate_at("state", str_replace, "Sept", "Monsoon") %>%
  mutate_at("day", str_replace, "83", "P1") %>%
  mutate_at("day", str_replace, "85", "P1") %>%
  mutate_at("day", str_replace, "246", "M1") %>%
  mutate_at("day", str_replace, "247", "M1") %>%
  mutate_at("day", str_replace, "140", "MD") %>%
  mutate(state = factor(state, levels = c("Pre-bloom", "Algal bloom", "Monsoon"))) %>%
  mutate(site = factor(site, levels = c("PX4", "PX5", "PX6", "MD2", "MD3", "MD4"))) %>%
  unite(site_day, site, day, sep = "-", remove = FALSE) %>%
  mutate_at("site_day", str_replace, "-P1", "") %>%
  mutate_at("site_day", str_replace, "-M1", "") %>%
  mutate_at("site_day", str_replace, "-MD", "") %>%
  mutate(
    new = case_when(
      grepl("PX4-", site_day) ~ "PX4",
      grepl("PX5-", site_day) ~ "PX5",
      grepl("PX6-", site_day) ~ "PX6",
      grepl("P1", day) ~ "P1",
      grepl("M1", day) ~ "M1",
      grepl("MD", day) ~ "MD"
    )
  ) %>%
  mutate(new = factor(new, level = c('P1', 'PX4', 'PX5', 'PX6', 'MD', 'M1'))) %>%
  mutate(site_day = factor(
    site_day,
    level = c(
      "PX4",
      "PX5",
      "PX6",
      "MD2",
      "MD3",
      "MD4",
      "PX4-129",
      "PX5-129",
      "PX6-129",
      "PX4-130",
      "PX5-130",
      "PX6-130",
      "PX4-131",
      "PX5-131",
      "PX6-131",
      "PX4-132",
      "PX5-132",
      "PX6-132",
      "PX4-133",
      "PX5-133",
      "PX6-133",
      "PX4-134",
      "PX5-134" ,
      "PX6-134",
      "PX4-136",
      "PX5-136",
      "PX6-136"
    )
  )) %>%
  pivot_longer(cols = 2:7,
               names_to = "phylum",
               values_to = "biomass") %>%
  mutate_at("phylum", str_replace, "Euglena", "Euglenozoa")


#biomass data
algae_biomass_cut <- algae_biomass_df %>%
  select(1:91, 94:99) %>%
  mutate(date = dmy(date)) %>%
  mutate(day = yday(date)) %>%
  mutate(state = month) %>%
  mutate_at("state", str_replace, "March", "Pre-bloom") %>%
  mutate_at("state", str_replace, "May", "Algal bloom") %>%
  mutate_at("state", str_replace, "Sept", "Monsoon") %>%
  mutate_at("day", str_replace, "83", "P1") %>%
  mutate_at("day", str_replace, "85", "P1") %>%
  mutate_at("day", str_replace, "246", "M1") %>%
  mutate_at("day", str_replace, "247", "M1") %>%
  mutate_at("day", str_replace, "140", "MD") %>%
  mutate(state = factor(state, levels = c("Pre-bloom", "Algal bloom", "Monsoon"))) %>%
  mutate(site = factor(site, levels = c("PX4", "PX5", "PX6", "MD2", "MD3", "MD4"))) %>%
  unite(site_day, site, day, sep = "-", remove = FALSE) %>%
  mutate_at("site_day", str_replace, "-P1", "") %>%
  mutate_at("site_day", str_replace, "-M1", "") %>%
  mutate_at("site_day", str_replace, "-MD", "") %>%
  mutate(
    new = case_when(
      grepl("PX4-", site_day) ~ "PX4",
      grepl("PX5-", site_day) ~ "PX5",
      grepl("PX6-", site_day) ~ "PX6",
      grepl("P1", day) ~ "P1",
      grepl("M1", day) ~ "M1",
      grepl("MD", day) ~ "MD"
    )
  ) %>%
  mutate(new = factor(new, level = c('P1', 'PX4', 'PX5', 'PX6', 'MD', 'M1'))) %>%
  mutate(site_day = factor(
    site_day,
    level = c(
      "PX4",
      "PX5",
      "PX6",
      "MD2",
      "MD3",
      "MD4",
      "PX4-129",
      "PX5-129",
      "PX6-129",
      "PX4-130",
      "PX5-130",
      "PX6-130",
      "PX4-131",
      "PX5-131",
      "PX6-131",
      "PX4-132",
      "PX5-132",
      "PX6-132",
      "PX4-133",
      "PX5-133",
      "PX6-133",
      "PX4-134",
      "PX5-134" ,
      "PX6-134",
      "PX4-136",
      "PX5-136",
      "PX6-136"
    )
  )) %>%
  pivot_longer(cols = 2:91,
               names_to = "spp",
               values_to = "biomass") %>%
  mutate_at("spp", str_replace, "Anabaena circinalis", "Dolichospermum circinale") %>%
  mutate_at("spp", str_replace, "Melosira granulata", "Aulacoseira granulata") %>%
  mutate_at("spp", str_replace, "Synedra acus", "Ulnaria acus") %>%
  mutate_at("spp", str_replace, "Synedra ulna", "Ulnaria ulna") %>%
  mutate_at("spp", str_replace, "Nostoc communes", "Nostoc commune") %>%
  mutate_at("spp", str_replace, "Microspora floccose", "Microspora floccosa") %>%
  mutate_at("spp", str_replace, "Surirella lineatrs", "Surirella linearis") %>%
  mutate_at("spp", str_replace, "Peridinium cunningtonnii", "Parvodinium cunningtonii") %>%
  mutate_at("spp", str_replace, "Trachelomonas curta", "Trachelomonas stagnalis") 

  
#plotting-----------------------------------------------------------------------

#biomass plot
phylum_total_p <- algae_phylum_cut %>%
  group_by(new_ID, site_day, state, new) %>%
  summarise(biomass = sum(biomass)) %>%
  mutate(state = str_replace(state, "Pre-bloom", "Winter")) %>%
  mutate(state = str_replace(state, "Algal bloom", "Spring")) %>%
  mutate(state = str_replace(state, "Monsoon", "Summer")) %>%
  mutate(state = factor(state, levels = c("Winter", "Spring", "Summer"))) %>%
  ggplot(aes(y = biomass, x = site_day, fill = biomass)) +
  geom_col(position = "stack",
           size = 0.01,
           show.legend = TRUE) +
  labs(y = expression(paste("Total biomass",  ~ (mg ~ L^-1))),
       x = NULL,
       fill = "Biomass") +
  scale_y_continuous(
    trans = scales::pseudo_log_trans(base = 10),
    breaks = c(0.1, 1, 10, 50),
    limits = c(0, 50),
    oob = scales::rescale_none,
    expand = expansion(mult = c(0, .05)),
  ) +
  scale_x_discrete(expand = c(0, 0), label = delete_no_display) +
  facet_nested(
    . ~ state + new,
    scales = "free",
    space = "free",
    nest_line = element_line(linetype = 2),
    labeller = labeller(new = day.labs)
  ) +
  theme_classic_biomarker +
  theme(
    strip.text = element_text(size = 8),
    # axis.title.y = element_text(
    #   vjust = 2,
    #   size = 12,
    #   hjust = 0.5
    # ),
    plot.margin = margin(
      t = 0,
      r = 0,
      b = -2,
      l = 0,
      "line"
    ),
    axis.line = element_line(size = 0.3),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 8),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    legend.text = element_text(size = 8),
    legend.position = "right",
    legend.spacing.y = unit(.0, 'cm'),
    legend.spacing.x = unit(.1, 'cm'),
    legend.title = element_text(size = 10, vjust = 3),
    legend.justification = "left",
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(0.5, "cm")
  )


#phylum plot
phylum_rel_p <- ggplot(algae_phylum_cut, aes(fill = phylum, y = biomass, x =
                                               site_day)) +
  geom_col(position = "fill") +
  labs(y = "Phyla \nrelative biomass (%)", x = NULL, fill = "Phyla") +
  scale_fill_brewer(palette = "Spectral") +
  scale_y_continuous(
    limits = c(0, 1),
    expand = c(0, 0),
    oob = scales::rescale_none,
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_x_discrete(expand = c(0, 0), label = delete_no_display) +
  facet_nested(
    . ~ state + new,
    scales = "free",
    space = "free",
    nest_line = element_blank(),
    labeller = labeller(new = day.labs)
  ) +
  theme_classic_biomarker +
  theme(
    strip.text = element_text(colour = "white", size = .1),
    plot.margin = margin(
      t = -2,
      r = 0,
      b = 0,
      l = 0,
      "line"
    ),
    legend.spacing.y = unit(.0, 'cm'),
    legend.spacing.x = unit(.1, 'cm'),
    legend.title = element_text(size = 10, vjust = 3),
    legend.text = element_text(size = 8),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 8),
    axis.ticks.x = element_blank(),
    # axis.title.y = element_text(vjust = 2, size = 12),
    axis.ticks.y = element_line(size = 0.3),
    axis.line = element_line(size = 0.3),
    legend.justification = "left",
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(0.5, "cm")
  )


#species plot
algae_biomass_p <- algae_biomass_cut %>%
  # mutate_at("site_day", str_replace, "PX4", "PX1") %>%
  # mutate_at("site_day", str_replace, "PX5", "PX2") %>%
  # mutate_at("site_day", str_replace, "PX6", "PX3") %>%
  # mutate_at("site_day", str_replace, "MD2", "MD1") %>%
  # mutate_at("site_day", str_replace, "MD3", "MD2") %>%
  # mutate_at("site_day", str_replace, "MD4", "MD3") %>%
  group_by(new_ID) %>%
  mutate(relative_proportion = biomass / sum(biomass)) %>%
  mutate(spp_bin = ifelse(relative_proportion < 0.05, "Grouped (<0.05)", spp)) %>%
  mutate(spp_bin = str_replace(spp_bin, "^([^\\s]+\\s[^\\s]+).*", "\\1")) %>%
  mutate(spp_bin = fct_inorder(spp_bin)) %>%
  mutate(spp_bin = fct_relevel(spp_bin, "Grouped (<0.05)", after = Inf)) %>%
  group_by(new_ID, site_day, state, new, spp_bin) %>%
  summarise(biomass = sum(biomass)) %>%
  #arrange(desc(site_day)) %>%
  ggplot(aes(fill = spp_bin, y = biomass, x = site_day)) +
  geom_col(position = "fill", colour = "transparent") +
  labs(y = "Species \nrelative biomass (%)", x = NULL, fill = "Algae \nspecies") +
  scale_fill_manual(
    breaks = c(
      "Aphanizomenon flos-aquae",
      "Aulacoseira granulata",
      "Ceratium hirundinella",
      "Closterium venus",
      "Cryptomonas erosa",
      "Cryptomonas ovata",
      "Dolichospermum circinale",
      "Euglena clara",
      "Euglena viridis",
      "Melosira varians",
      "Microspora floccosa",
      "Nostoc commune",
      "Pandorina morum",
      "Parvodinium cunningtonii",
      "Peridinium volzii",
      "Phormidium tenue",
      "Surirella linearis",
      "Ulnaria acus",
      "Ulnaria ulna",
      "Trachelomonas stagnalis",
      "Trachelomonas oblonga",
      "Grouped (<0.05)"
    ),
    labels = c(
      "*Aphanizomenon flos-aquae*",
      "*Aulacoseira granulata*",
      "*Ceratium hirundinella*",
      "*Closterium venus*",
      "*Cryptomonas erosa*",
      "*Cryptomonas ovata*",
      "*Dolichospermum circinale*",
      "*Euglena clara*",
      "*Euglena viridis*",
      "*Melosira varians+*",
      "*Microspora floccosa+*",
      "*Nostoc commune+*",
      "*Pandorina morum*",
      "*Parvodinium cunningtonii*",
      "*Peridinium volzii*",
      "*Phormidium tenue*",
      "*Surirella linearis+*",
      "*Ulnaria acus*",
      "*Ulnaria ulna*",
      "*Trachelomonas stagnalis*",
      "*Trachelomonas oblonga*",
      "Grouped (<0.05)"
    ),
    values = iwanthue(seed = 100, force_init = TRUE)$hex(22)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    expand = c(0, 0),
    oob = scales::rescale_none,
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_x_discrete(expand = c(0, 0), label = delete_no_display) +
  facet_nested(
    . ~ state + new,
    scales = "free",
    space = "free",
    nest_line = element_blank(),
    labeller = labeller(new = day.labs)
  ) +
  theme_classic_biomarker +
  theme(
    strip.text = element_text(colour = "white", size = .1),
    plot.margin = margin(
      t = -2,
      r = 0,
      b = 0,
      l = 0,
      "line"
    ),
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_markdown(size = 8),
    legend.key.spacing.y = unit(0.001, "cm"),
    axis.text.x = element_text(
      size = 8,
      angle = 90,
      vjust = 0.5
    ),
    axis.text.y = element_text(size = 8),
    # axis.title.y = element_text(vjust = 2, size = 12),
    axis.ticks.y = element_line(size = 0.3),
    axis.ticks.x = element_line(size = 0.3),
    axis.line = element_line(size = 0.3)
  ) +
  guides(fill = guide_legend(ncol = 4, bycol = TRUE))

#make multipanel plot--------------------------------------------------------

#extract the legend (using temporary work around function)
algae_biomass_legend <- as_ggplot(get_legend_fix(algae_biomass_p))

#set position of the legend in the species plot
algae_biomass_p2 <- algae_biomass_p + theme(
  legend.position = "right",
  legend.key.width = unit(0.00001, "cm"),
  legend.key.height = unit(0.00001, "cm"),
  legend.title = element_text(size = 0.1, colour = "transparent"),
  legend.text = element_markdown(size = 0.1, colour = "transparent")
)


#arrange the plots vertically
combined1 <- ggarrange(
  phylum_total_p,
  phylum_rel_p,
  algae_biomass_p2,
  ncol = 1,
  heights = c(1, 1, 1),
  align = "hv",
  common.legend = FALSE,
  legend = "right",
  labels = c("A", "B", "C")
) +
  theme(plot.margin = margin(
    t = 0,
    r = 0,
    b = 1,
    l = 1,
    "line"
  ))

#combine species legend to combined plot
combined2 <- ggarrange(combined1,
                       algae_biomass_legend,
                       ncol = 1,
                       heights = c(5, 1)) + theme(plot.margin = margin(
                         t = 1,
                         r = 0,
                         b = 2,
                         l = 0,
                         "line"
                       ))


#Preview figure has incorrect space. Output figure (ggsave) has correct spacing
combined2


#save the plot------------------------------------------------------------------

ggsave(
  paste0(OutputDir, '/', 'algae_barplot_V4.png'),
  plot = combined2,
  device = 'tiff',
  width = 19,
  height = 25,
  units = 'cm'
) # Save the plot

#Required edits for image in powerpoint: 1: Legend for panel C needs to be 
#removed, 2: Dashed line and season labels above panel A need to be moved down,
#3: Insert white box around entire
#plotting area. 

