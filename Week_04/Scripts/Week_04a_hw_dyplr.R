### This is the Week 4 a homework assignment! - dyplr
# the script below answers specific questions (see annotations for more info.)

# created by: Kat Viehl
# created on: 2026-09-21 

#--------------------------------

# load libraries
library(palmerpenguins)
library(tidyverse)
library(here)
library(ggplot2)
library(ggthemes)
library(colorBlindness)


#---------------------------------

# check out the data
head(penguins)

#--------------------------------

# Homework questions:


# 1) calculate the mean and variance of body mass by species,
#    island, and sex without any NAs

penguins |> 
  drop_na(body_mass_g) |> 
  drop_na(sex) |> 
  group_by(species, island, sex) |> 
  summarize(mean_body_mass = mean(body_mass_g),
            var_body_mass = var(body_mass_g))



#################################################
# below are commented out variations that do not get rid of all NAs. (Maybe due to no drop_na(sex)?) 
#
#penguins |> 
#  group_by(species, island, sex) |> 
#  summarize(mean_body_mass = mean(body_mass_g, na.rm = TRUE),
#            var_body_mass = var(body_mass_g, na.rm = TRUE))
#
#
#penguins |> 
#  summarize(.by = c(species, island, sex), 
#            mean_body_mass = mean(body_mass_g, na.rm = TRUE),
#            var_body_mass = var(body_mass_g, na.rm = TRUE))
#
#(Also, Iʻm wondering why the sorting is different for these two above... hmm)
##################################################

#---------------------------------------

# Next question:

# 2) -Filter out (exclude) male penguins
#    -calculate the log body mass
#    -then select only the following columns: 
#       -> species, island, sex, & log body mass
#    -use these data to make any plot. 
#    -make sure plot has clean & clear labels
#     (and follows best practices)

######################################
#I'm not sure why, but this first attempt didn't work:
#
#log_bm_female <- penguins |> 
#  filter(sex != "male") |> 
#  mutate(log_body_mass = log(body_mass_g)) |> 
#  select(species, island, sex, log_body_mass)
#
#head(log_bm_female)
#######################################

# breaking it down into small chunks:
# penguins that are not male, with log_body_mass column added:

peng_no_m_logbm <- penguins |> 
  filter(sex != "male") |> 
  mutate(log_body_mass = log(body_mass_g))

# select only columns species, island, sex & log bm:

plot_group <- peng_no_m_logbm |>
  select(species, island, sex, log_body_mass)

# make a plot with the data:

palm_log_plot <- plot_group |> 
  ggplot(mapping = aes(x = island,              # base plot mapping
                       y = log_body_mass,
                       group = species,
                       color = species,
                       fill = species))+
  geom_dotplot(binaxis = "y",                   # dotplot details
               stackdir = "center", 
               group = "species", 
               dotsize = 0.4)+
  facet_wrap(~ species, ncol = 3)+              # separate into 3 graphs
  theme_clean()+
  theme(axis.title.x = element_text(size = 15),       # increase size of axis labels
        axis.title.y = element_text(size = 15))+
  labs(title = "Log Body Mass of Female Penguins in the Palmer Archipelago",
       subtitle = "Plots Separated by Species (Adelie, Chinstrap, and Gentoo)",
       caption = "Data source: Horst AM, Hill AP, Gorman KB (2020).",
       x = "Island",
       y = "Log of Body Mass (g)",                      # labels
       fill = "Species",
       color = "Species")+
  scale_discrete_manual(aesthetics = c("color", "fill"),     # make color palette more accessible
                        values = palette.colors())


# cvdPlot(palm_log_plot) #check to see if plot is colorblind accessible


ggsave(here("Week_04","Output","hw_week04a_peng_f_logbm_plot.png"),
       width = 9, height = 6) #default is in inches

  
  