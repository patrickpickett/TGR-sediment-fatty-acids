###############################################################################
#Script for creating FA and Chla bar plots for TGR 2021
#Pat P 12/03/2025
###############################################################################

#To do: remove unused packages


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

#function to suppress unwanted labels on x-axis of plots
delete_no_display <- function(v) {
  if_else(str_detect(v, '-1'), '', v)
}


#set input directory------------------------------------------------------------
inputDir <- here::here('dataOrg', 'outputs', 'data')
OutputDir <- here::here('outputs', 'R')


#load data----------------------------------------------------------------------
df <- read_csv(file.path(inputDir, 'FA_final_2021.csv'))
chla <- read_csv(file.path(inputDir, '2021-Chla-FULL.csv'))
category_names <- read_csv(file.path(inputDir, '/FA biomarkers/FA_main_categories.csv'))


#set plot theme-----------------------------------------------------------------

my_colours <- c(
  "#DEEBF7",
  "#9ECAE1",
  "#3182BD",
  "#E5F5E0",
  "#A1D99B",
  "#31A354",
  "#DEEBF7",
  "#DEEBF7",
  "#DEEBF7",
  "#DEEBF7",
  "#DEEBF7",
  "#DEEBF7",
  "#DEEBF7",
  "#9ECAE1",
  "#9ECAE1",
  "#9ECAE1",
  "#9ECAE1",
  "#9ECAE1",
  "#9ECAE1",
  "#9ECAE1",
  "#3182BD",
  "#3182BD",
  "#3182BD",
  "#3182BD",
  "#3182BD",
  "#3182BD",
  "#3182BD"
)

names(my_colours) <- c(
  "PX4",
  "PX5",
  "PX6",
  "MD2",
  "MD3",
  "MD4",
  "PX4-129",
  "PX4-130",
  "PX4-131",
  "PX4-132",
  "PX4-133",
  "PX4-134",
  "PX4-136",
  "PX5-129",
  "PX5-130",
  "PX5-131",
  "PX5-132",
  "PX5-133",
  "PX5-134",
  "PX5-136",
  "PX6-129",
  "PX6-130",
  "PX6-131",
  "PX6-132",
  "PX6-133",
  "PX6-134",
  "PX6-136"
)



# Site order for legend
site_order <- c("PX4", "PX5", "PX6", "MD2", "MD3", "MD4")


#Customize plot
theme_edit <- theme(
  axis.ticks.y = element_line(size = 0.3),
  axis.ticks.x = element_blank(),
  axis.line = element_line(size = 0.3),
  axis.text.x = element_blank(),
  strip.text = element_text(colour = "white", size = .1),
  strip.background = element_blank(),
  legend.position = "right",
  legend.spacing.y = unit(.0, 'cm'),
  legend.spacing.x = unit(.1, 'cm'),
  legend.title = element_text(size = 10, vjust = 3),
  legend.margin = margin(6, 4, 4, 4)
)


#Use classic theme as a base & join custom theme
theme_classic_edit <- theme_classic() + theme_edit
    

#data tidy----------------------------------------------------------------------

#Fix facet labels. Applies to all bar plots below

#state labels
state.labs <- c("Winter", "Spring", "Summer")
names(state.labs) <- c("Pre-bloom", "Algal bloom", "Monsoon")

#day labels
day.labs <- c('', '', '', '', '', '')
names(day.labs) <- c('P1', 'PX4', 'PX5', 'PX6', 'MD', 'M1')


#Chla 
chla_df <- chla %>%
  filter(site %in% c("PX4", "PX5", "PX6", "MD2", "MD3", "MD4")) %>%
  mutate(date = dmy(date)) %>%
  filter(
    date >= "2021-05-09" & date <= "2021-05-16" |
      date %in% c(
        "2021-05-20",
        "2021-03-24",
        "2021-03-26",
        "2021-09-03",
        "2021-09-04"
      )
  ) %>%
  unite(new_ID, date, ID, sep = "-", remove = FALSE) %>%
  filter(depth == "surface") %>%
  rename(surface_chla = Chla_ug_L_fill) %>%
  select(new_ID, surface_chla)


#FA grouping
category_names$FA <- janitor::make_clean_names(category_names$FA) 


#This is pretty rough and maybe has some redundant script
df_cut <- df %>%
  filter(sample == "sediment") %>%
  dplyr::select(-nmg) %>%
  rename(nmg = nmg_FA_per_g_soil, new_ID = full_ID) %>%
  group_by(new_ID, ID, year, date, month, river, site, FA) %>%
  summarise(nmg = sum(nmg, na.rm = TRUE), .groups = "drop") %>%
  left_join(category_names, by = "FA") %>%
  mutate(
    category_names = coalesce(group, "unassigned"),
    date = dmy(date),
    day = as.character(yday(date)),
    state = case_when(
      month == "March" ~ "Pre-bloom",
      month == "May" ~ "Algal bloom",
      month == "September" ~ "Monsoon",
      TRUE ~ month
    ),
    day = case_when(
      day %in% c("83", "85") ~ "P1",
      day %in% c("246", "247") ~ "M1",
      day == "140" ~ "MD",
      TRUE ~ day
    ),
    state = factor(state, levels = c("Pre-bloom", "Algal bloom", "Monsoon")),
    site = factor(site, levels = c("PX4", "PX5", "PX6", "MD2", "MD3", "MD4"))
  ) %>%
  unite(site_day, site, day, sep = "-", remove = FALSE) %>%
  mutate(
    site_day = str_replace(site_day, "-(P1|M1|MD)$", ""),
    new = case_when(
      str_detect(site_day, "PX4-") ~ "PX4",
      str_detect(site_day, "PX5-") ~ "PX5",
      str_detect(site_day, "PX6-") ~ "PX6",
      day == "P1" ~ "P1",
      day == "M1" ~ "M1",
      day == "MD" ~ "MD",
      TRUE ~ NA_character_
    ),
    new = factor(new, levels = c("P1", "PX4", "PX5", "PX6", "MD", "M1")),
    site_day = factor(
      site_day,
      levels = c(
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
        "PX5-134",
        "PX6-134",
        "PX4-136",
        "PX5-136",
        "PX6-136"
      )
    )
  ) %>%
  full_join(chla_df, by = "new_ID") %>%
  select(new_ID, state, new, site_day, surface_chla, nmg, category_names)




#plotting-----------------------------------------------------------------------

#Chl-a plot
chla_p1 <- df_cut %>%
  ungroup() %>%
  distinct(new_ID, .keep_all = TRUE) %>%
  group_by(state, new, site_day) %>%
  summarise(surface_chla = sum(surface_chla)) %>%
  ggplot(aes(y = surface_chla, x = site_day, fill = site_day)) +
  geom_col() +
  labs(y = expression(paste("Chl-", italic("a"),  ~ (mu * g ~ L^-1))),
       x = NULL,
       fill = "Site") +
  scale_fill_manual(values = my_colours, breaks = site_order) +
  facet_nested(
    . ~ state + new,
    scales = "free",
    space = "free",
    nest_line = element_line(linetype = 2),
    labeller = labeller(new = day.labs, state = state.labs)
  ) +
  theme_classic_edit +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(size = 8, colour = "black")
  ) +
  scale_y_continuous(
    trans = scales::pseudo_log_trans(base = 10),
    breaks = c(1, 10, 100, 500),
    oob = scales::rescale_none,
    expand = expansion(mult = c(0, .05))
  ) 


#FA concentration plot
FAtotal_p <- df_cut %>%
  group_by(state, new, site_day) %>%
  summarise(nmg = sum(nmg)) %>%
  ggplot(aes(y = nmg, x = site_day, fill = site_day)) +
  geom_col() +
  labs(y = expression(paste("FA concentration ", (nmol ~ g^-1 ~ soil))),
       x = NULL,
       fill = "Site") +
  scale_fill_manual(values = my_colours, breaks = site_order) +
  scale_y_continuous(
    limits = c(0, 11),
    expand = c(0, 0),
    oob = scales::rescale_none
  ) +
  facet_nested(
    . ~ state + new,
    scales = "free",
    space = "free",
    nest_line = element_blank(),
    labeller = labeller(new = day.labs, state = state.labs)
  ) +
  theme_classic_edit +
  guides(fill = guide_legend(nrow = 6, byrow = TRUE))


#FA category plot
FAcat_p <- df_cut %>%
  filter(!grepl('alcohol', category_names)) %>%
  group_by(state, new, category_names, site_day) %>%
  summarise(nmg = sum(nmg)) %>%
  ggplot(aes(fill = category_names, y = nmg, x = site_day)) +
  geom_col(position = "fill") +
  labs(y = "FA category\n relative proportion (%)", x = NULL, fill = "FA\ncategory") +
  scale_fill_manual(values = c("#8DA0CB", "#8961b3", "#FC8D62")) +
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
  theme_classic_edit +
  theme(
    axis.text.x = element_text(
      size = 7,
      angle = 90,
      vjust = 0.5
    ),
    axis.ticks.x = element_line(size = 0.3)
  ) 


#remove the legend (add back in outside of R)
FAcat_p2 <- FAcat_p + theme(legend.position = "none")

#remove the legend (add back in outside of R)
FAtotal_p2 <- FAtotal_p + theme(legend.position = "none")


#extract and save legends-------------------------------------------------------

# Extract the legend
FAcat_legend <- get_legend(FAcat_p)

#Save the legend
ggsave(paste0(OutputDir, '/', 'FAcat_legend.png'), plot = FAcat_legend,
       device = 'tiff') # Save the plot

# Extract the legend
FAtotal_legend <- get_legend(FAtotal_p)

#Save the legend
ggsave(paste0(OutputDir, '/', 'FAtotal_legend.png'), plot = FAtotal_legend,
       device = 'tiff') # Save the plot


#make multipanel plot-----------------------------------------------------------

#Join the plots together
combined_plot <- ggarrange(
  chla_p1,
  FAtotal_p2,
  FAcat_p2,
  ncol = 1,
  align = "hv",
  labels = c("A", "B", "C")
)

#print combined plot
combined_plot


#save the plot------------------------------------------------------------------

#save the plot 
ggsave(
  paste0(OutputDir, '/', 'FAplot_V4.png'),
  plot = combined_plot,
  device = 'tiff',
  width = 16,
  height = 23,
  units = 'cm'
) 

#Required edits for image in powerpoint: 1: Dashed line and season labels above 
#panel A need to be moved down, 2: Add seperate legends for panels A+B and C in
#powerpoint (png for each saved in this script)
