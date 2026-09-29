### Week_05 Homework!

# created by: Kat Viehl
# created on: 2026-09-22
# last updated: 2026-09-28

#------------------------------

# load libraries
library(tidyverse)
library(here)
library(dplyr)
library(ggthemes)
library(ggpattern)   # new package to make gradients!
#library(colorBlindness)    # for using the color accessibility checker at the bottom of the script

#------------------------------

# The Homework Question:

# Read in both the conductivity and depth data
# Convert date columns appropriately
# Round the conductivity data to the nearest 10 seconds to match depth data
# Join the two dataframes using inner_join() (only exact matches)
# Calculate averages of date, depth, temperature, and salinity by minute
# Make a plot using the averaged data
# Use pipes throughout (minimize separate dataframes)
# Add comments to your code!
#  Save output, data, and scripts appropriately

# 5 points for following all the above rules, 3 points for best plotting and 
#     scripting practices, 2 points for appropriate GitHub organization

#-----------------------------

# First step: Load in the data

cond_data <- read_csv(here("Week_05","Data","CondData.csv"))

depth_data <- read_csv(here("Week_05","Data","DepthData.csv"))

#-----------------------------

# Convert date columns appropriately

# first, set up the cond_data correctly:
cond_data |> 
  
  # convert to datetime format
  mutate(datetime = mdy_hms(date)) |> 
  
  # remove the original date column
  select(-date) |> 
  
  # round the datetime to the nearest 10 seconds (replacing original column):
  mutate(datetime = round_date(datetime, seconds(10))
         ) -> clned_cond_data    # assign to variable for join later

  
# then, come in with the depth_data:
depth_data |> 
  
  # convert to datetime format
  mutate(datetime = ymd_hms(date)) |> 
  
  # remove the original date column
  select(-date) |> 
  
  # inner_join with the cleaned conductivity data (from above)
  inner_join(clned_cond_data) |> 
  
  # "Calculate averages of date, depth, temperature, and salinity by minute"
  # could mean 1 of 2 things:
  #    1. round the datetime to minutes, then summarise (this one is way more likely lol)
  #    2. pull the "minutes" value from the datetime, then summarise (not gonna do this)
  
  
  # first, round to minute
  mutate(rounded_datetime = round_date(datetime, "minute")) |> 
  
  # calculate averages of date, depth, temp, and salinity, by rounded minute:
  group_by(rounded_datetime) |> 
  summarise(mean_date = mean(datetime, na.rm = TRUE),
            mean_depth = mean(Depth, na.rm = TRUE),
            mean_temp = mean(Temperature, na.rm = TRUE),
            mean_salinity = mean(Salinity, na.rm = TRUE)) |>
  
  # pivot long to make a more interesting graph
  pivot_longer(cols = mean_depth:mean_salinity,
               names_to = "Variables",
               values_to = "Values") |> 
  
  # time for ggplot:
  ggplot(mapping = aes(x = rounded_datetime,   # make the base graph
                       y = Values,
                       color = Variables,      # group according to variables
                       fill = Variables))+ 
  
  geom_area_pattern(aes(pattern_fill = Variables,   # use ggpattern to add gradient effect under the line. Need to set both colors to correspond to variable
                        pattern_fill2 = Variables),
                    pattern_fill = "NA",            # this sets the transparency behind the area.
                    pattern_orientation = "vertical",
                    pattern = "gradient")+
  
  geom_line(colour = "white",  # bigger line in white "outlines" the line on top of it to make it a bit easier to see sharp variance in the gradient
            linewidth = 1.3)+   
  
  geom_line(linewidth = 0.5)+   # make the line graph
  
  facet_wrap(~Variables, 
             scale = "free",    # break the graph into three based on the variable categories
             labeller = labeller(Variables = c(mean_depth = "Depth (m)",
                                               mean_salinity = "Salinity (ppt)",
                                               mean_temp = "Temperature (°C)")))+
  # theme for the plot
  theme_stata()+
  
  scale_color_manual(values = c("#4049AD",   # color the lines per facet wrap
                                "#97D8C4", 
                                "#F4B942"))+  
  
  scale_pattern_fill2_manual(values = c("#4049AD", # this sets the colors at the top of the gradient per facet wrap.
                                        "#97D8C4", 
                                        "#F4B942"))+  
  
  scale_fill_manual(values = c(NA,NA,NA))+   # This is how I was able to remove the weird color errors!
  
  theme(axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),    # resize labels
        plot.title = element_text(size = 20),
        plot.margin = margin(t = 8,
                             r = 20,   # increased the margins as the time on the right was running off the edge of the plot!
                             l = 8,
                             b = 8),
       panel.grid.major = element_line(colour = "gray",
                                       linewidth = 0.5),
       panel.grid.minor = element_line(colour = "#E5E4E2", 
                                       linewidth = 0.3),
       panel.grid.major.x = element_line(colour = "gray",
                                         linewidth = 0.5))+     #added nicer grid lines
  
  guides(color = "none",         # remove the legends
         fill = "none",
         pattern_fill = "none",
         pattern_fill2 = "none")+
  
  labs(title = "Mean Depth, Salinity, and Temperature per Minute",
       x = "Time (hh:mm)") #-> test   #(for checking accessibility)
  
# cvdPlot(test) # for checking accessibility (this doesn't work with ggpalette so it looks really weird, but there are no color-only identifiers)

# save plot in the outputs folder:
ggsave(here("Week_05", 
            "Outputs", 
            "hw_week05_dates-lubri_plot.png"),
       width = 8, 
       height = 5)


