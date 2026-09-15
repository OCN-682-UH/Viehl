### Week 3 homework - a good penguin graph in 1 hour

#-------------------------------

# created by: Kat Viehl
# created on: 2026-09-14

#-------------------------------

# load libraries

library(palmerpenguins)
library(tidyverse)
library(here)
library(beyonce)

#-------------------------------

# checking out the data before I begin

head(penguins)

#-------------------------------

# first script attempt (commented out as I realized I shouldn't do geom_point, please disregard this attempt)

#ggplot(data=penguins,
#       mapping = aes(x = bill_length_mm,
#                     y = flipper_length_mm,
#                     group = year,
#                     color = species,
#                     shape = sex))+
#  geom_point(size = 3, alpha = 0.5)+
#  facet_grid(year~island)+
#  theme_igray()+
#  scale_color_colorblind()+
#  labs(title = "Measurements of Three Penguin Species in the Palmer Archipelago",
#       subtitle = "On the islands of Biscoe, Dream, and Torgersen, 2007-2009",
#       x = "Bill Length (mm)", y = "Flipper Length (mm)",
#       color = "Species",
#       shape = "Sex",
#       )


#------------------------------

# second script attempt, without geom_point()

my_plot <- ggplot(data=penguins, # define the data frame & assign to my_plot
       mapping = aes(x = year, # set the x axis
                     y = flipper_length_mm, # set the y axis
                     group = year, # group by year
                     color = year, # color by year (makes the outline match the fill below)
                     fill = year))+ # fill by year (sets the fill color)
  geom_violin()+ # violin plot function
  geom_jitter(colour = "lightblue", # overlaid with a light blue jitter plot. I don't love this, but I couldn't get a boxplot overlay to work without fill.
              shape = 20, # small dot shape
              width = 0.1, # to keep the dots mostly over the "violins"
              alpha = 0.6)+ # make them transparent to see overlap
  theme_clean()+ # plot base theme
  theme(plot.title = element_text(face = "bold", # making the title big and bold
                             size = 20))+
  theme(axis.title.x = element_text(face = "bold", # making the axis titles big and bold
                                    size = 15))+
  theme(axis.title.y = element_text(face = "bold", 
                                    size = 15))+
  facet_wrap(~island)+ # separate the violins by island
  guides(fill = "none", # get rid of the legend
         color = "none")+
  labs(title = "Flipper Lengths of Penguins from the Palmer Archipelago", #labels
     subtitle = "On the Islands of Biscoe, Dream, and Torgersen from 2007-2009",
     x = "Year", 
     y = "Flipper Length (mm)",
     caption = "Data source: Gorman K.B., Williams T.D., and Fraser W.R., 2014"
     )

#----------------------------

### un-comment to check visual accessibility:

#cvdPlot(my_plot)

#--------------------------

# show the plot in the plots window:

my_plot


# save plot:

ggsave(here("Week_03","Output","Week_03_penguin_hw.png"),
       width = 10, height = 7) #default is in inches


