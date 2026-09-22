### Week 04 Homework b - tidyr!!

# created by: Kat Viehl
# created on: 2026-09-22 (time for speed coding!)

#-------------------------------------

# load libraries

library(tidyverse)
library(here)
library(colorBlindness)

#------------------------------------

# Homework prompt:

#Using the chemistry data:
#  
# Create a new clean script
# Remove all the NAs
# Separate the Tide_time column into appropriate columns for analysis
# Filter out a subset of data (your choice)
# Use either pivot_longer() or pivot_wider() at least once
# Calculate some summary statistics (can be anything) and 
#      export the csv file into the output folder
# Make any kind of plot (it cannot be a boxplot) and export 
#      it into the output folder
# Make sure you comment your code and your data, outputs, and 
#      script are in the appropriate folders
#

#-------------------------------------

# bring in the chem data:

ChemData <- read_csv(here("Week_04", "Data", "chemicaldata_maunalua.csv"))

# preview it:

head(ChemData)

# separate the tide_time column to appropriate columns:

fixed_ChemData <- ChemData |> 
  separate_wider_delim(cols = Tide_time,
                       delim = "_",
                       names = c("Tide","Time"))

# filter out a subset of data - how about I do in spring, 
# how did the salinity and silicate vary, day vs night? I'll also keep site and tide.

sub_fixed_ChemData <- fixed_ChemData |> 
  drop_na() |> 
  select(Site, Season, Tide, Time, Salinity, Silicate) |> 
  filter(Season == "SPRING")

# use pivot longer or wider at least once

sub_fixed_ChemData_long <- sub_fixed_ChemData |> 
  pivot_longer(cols = Salinity:Silicate,
               names_to = "Variables",
               values_to = "Values")
  
# calculate some summary statistics, specifically looking at means of
# Silica/Salinity separated out by site, tide, and time:

mean_sub_fixed_ChemData_long <- sub_fixed_ChemData_long |> 
  group_by(Variables, Site, Tide, Time) |> 
  summarise(mean_vals = mean(Values))

# export the above to a .csv file

mean_sub_fixed_ChemData_long |> 
  write_csv(here("Week_04", "Output", "hw_mean_silica-salinity.csv"))


# make any kind of plot

hw_week04b_plot <- mean_sub_fixed_ChemData_long |> 
  ggplot(aes(x = Tide,            # base plot aesthetics
             y = mean_vals,
             color = Site,
             shape = Time,
             group = interaction(Site, Time)))+  # learned interaction here!: https://www.statology.org/ggplot-group-by-two-columns/
  geom_point(size = 7, alpha = 0.5)+     # for the points
  geom_line(size = 1, alpha = 0.5)+      # for the connecting lines 
  facet_wrap(~Variables, scales = "free", 
             labeller = labeller(Variables = c(Salinity = "Average Salinity (ppt)",         # learned labeller here!: https://ggplot2.tidyverse.org/reference/labeller.html
                                               Silicate = "Average Silicate (umol/L)")))+
  theme_clean()+
  theme(title = element_text(size = 20),  # I'm not sure why, but this line doesn't appear to be working
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  scale_discrete_manual(aesthetics = c("color", "fill"),  # for accessible colors
                        values = palette.colors())+
  labs(title = "Average Salinity and Silicate Fluctuations in Spring",
       subtitle = "Differences Between Site W vs. BP, High vs. Low Tide, and Day vs. Night",
       caption = "Data source: Silbiger, N. (2026) Maunalua Chemical Data",
       x = "Tide (High or Low)",
       y = "Average Values")


#cvdPlot(hw_week04b_plot) #check to see if plot is colorblind accessible



# save plot

ggsave(here("Week_04", "Output", "hw_week04b_sal-sil_plot.png"),
       width = 8, height = 5)


