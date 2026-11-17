#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(here)
library(ggthemes)
library(DT)


# bring in data (this is also brought in below within the shinyapp, but it kept erroring to publish, so here it is up here as well!)

mfu_eDNA_data <- read_csv("cleanmfu_eDNA.csv")
## use below if running code not as the app
#mfu_eDNA_data <- read_csv(here("Week_12", "Data", "cleanmfu_eDNA.csv"))


# ok! Plan for this app: I will use my clean eDNA data from week 8 and make an app that allows you to interact with it!.


# shiny code! (where the inputs and fancy text goes)
ui <- fluidPage(
  
  # a title!
  h1("eDNA Data Explorer - MiFish U/E Multiplex & Master Mix Comparison"),
  "Created by: Kat Viehl",
  br(),
  "Created on: 2026-11-15",     # these are all text, and br() is a line break.
  br(),
  "Data from: me testing master mixes and primers in the ToBo lab",
  br(),
  br(),
  "Full disclosure that the base code for this was taken from my Week_08 (functions) homework submission,", 
  br(),
  "so Iʻve added a few extra inputs to make up for it. I thought a shiny app would be a great way to simplify",
  br(),
  "that function into a user interface.",
  br(),
  
  # buttons for plot format
  radioButtons("seqtype", label = h3("Select Sequence Presentation:"),
               choices = list("Number of Sequences" = "stack", "Sequence Relative Abundance" = "fill"), 
               selected = "stack"),
  
  # dropdown for family
  selectInput("select", 
              label = h3("Select a Family to Summarize:"),
              choices = sort(unique(mfu_eDNA_data$family)), 
              selected = 1),
  
  # buttons for plot format
  radioButtons("droppedornot", label = h3("Select Data Type:"),
               choices = list("Sequences Identified to Species" = "mfu_eDNA_data_clean", 
                              "Dropped Sequences" = "removed_data",
                              "All Data" = "mfu_eDNA_data"), 
               selected = "mfu_eDNA_data_clean"),
  
  "(If the plot is blank, there are no sequences of the type selected above.)",
  
  # I'm not 100% sure what this does, but the widget gallery had it at the end of every script:
  hr(),
  fluidRow(column(3, verbatimTextOutput("value"))),
  
  # output the plot with a bit bigger height than the default
  plotOutput("eDNAcol", height = "600px"),
  
  # more text
  br(),
  h1("Table of Detections in the Selected Family:"),
  br(),
  "Each row represents a specific sequence (zOTU). The numbers under each sample name", 
  br(),
  "are the number of reads (of identical sequences) detected in that sample. ʻDroppedʻ", 
  br(),
  "indicates that there were multiple genera or species identified for the same sequence,", 
  br(),
  "making it impossible to confidently assign the identity.",
  DTOutput("details")
  
)


# shiny cody part 2! (where the outputs go)

server <- function(input, output){
  output$eDNAcol <- renderPlot({
    
    
    
    # bring in data (again, I'm not sure if this is needed since it is also before the start of the app code.)
    
    mfu_eDNA_data <- read_csv("cleanmfu_eDNA.csv")
    
    # summarizing the data that is about to be removed into a new dataframe:
    
    mfu_eDNA_data |> 
      filter(species == "dropped") -> removed_data
    
    
    # remove the dropped to make the plot better:
    
    mfu_eDNA_data |> 
      filter_out(genus == "dropped") |> 
      filter_out(species == "dropped") -> mfu_eDNA_data_clean
    
    
    ######################################### 
    ######################################### Plot function below:
    
    # create eDNA function (from my week 08 homework! I want to see if I can make this more easy to interact with using dropdowns and buttons)
    plot_eDNA <- function(data, first_sample_ID, plotby_taxa = phylum, stack_or_fill = "stack",
                          domains = "none", kingdoms = "none", phyla = "none", classes = "none",
                          orders = "none", families = "none", genera = "none", specieses = "none") {
      
      # set up labels for later based on argument inputs
      if (stack_or_fill == "stack") {
        y_axis <- "Number of Sequences"
        title_text <- "eDNA Sequence Summary Plot"
      } else if (stack_or_fill == "fill") {
        y_axis <- "Relative Amount of Sequences"
        title_text <- "eDNA Relative Sequence Summary Plot"
      }
      
      ################ Filter by identities:
      
      # if domain is specified, filter the data by the domains specified
      if (length(domains) == 1 && domains == "none") {
        data <- data
      } else {
        data <- filter(data, domain %in% domains)
      }
      
      # if kindom is specified, filter the data by the kingdoms specified
      if (length(kingdoms) == 1 && kingdoms == "none") {
        data <- data
      } else {
        data <- filter(data, kingdom %in% kingdoms)
      }
      
      # if phylum is specified, filter the data by the phyla specified
      if (length(phyla) == 1 && phyla == "none") {
        data <- data
      } else {
        data <- filter(data, phylum %in% phyla)
      }
      
      # if class is specified, filter the data by the classes specified
      if (length(classes) == 1 && classes == "none") {
        data <- data
      } else {
        data <- filter(data, class %in% classes)
      }
      
      # if order is specified, filter the data by the orders specified
      if (length(orders) == 1 && orders == "none") {
        data <- data
      } else {
        data <- filter(data, order %in% orders)
      }
      
      # if family is specified, filter the data by the families specified
      if (length(families) == 1 && families == "none") {
        data <- data
      } else {
        data <- filter(data, family %in% families)
      }
      
      # if genus is specified, filter the data by the genera specified
      if (length(genera) == 1 && genera == "none") {
        data <- data
      } else {
        data <- filter(data, genus %in% genera)
      }
      
      # if species is specified, filter the data by the species specified 
      # hilariously, I had to make this argument "specieses" as I can't have it be "species" D:
      if (length(specieses) == 1 && specieses == "none") {
        data <- data
      } else {
        data <- filter(data, species %in% specieses)
      }
      
      # call data
      data |> 
        
        # retrieve the sample column names again
        select({{first_sample_ID}}:last_col()) |> 
        colnames() -> sample_cols
      
      # call it again
      data |> 
        
        #pivot the data longer to better plot with
        pivot_longer(cols = sample_cols,
                     names_to = "site",
                     values_to = "num_sequences") -> long_eDNA
      
      # and plot!
      long_eDNA |> 
        ggplot(mapping = aes(x = site,
                             y = num_sequences,         # base plot, fill by specified taxa
                             fill = {{plotby_taxa}})) +
        geom_col(position = stack_or_fill) +
        theme_clean() +
        coord_flip() +                               #theming and aethetics
        scale_fill_viridis_d() +
        theme(title = element_text(face = "bold",
                                   size = 25),
              axis.title = element_text(size = 15)) +
        labs(title = title_text,
             subtitle = "Use arguments above to visualize specific families",
             caption = "All samples above contain the same template DNA. All differences are due to master mix changes and/or PCR stochasticity.",
             x = "Sample Name",               # labels
             y = y_axis) -> eDNA_plot
      
      return(eDNA_plot)
      
    }
    
    ######################################### Plot function complete
    ######################################### 
    
    
    
    # have to do this (switch()) in order to get the dataframe names correct instead of listed as a string of their name.
    to_be_plotted <- reactive({
      switch(input$droppedornot, 
             "mfu_eDNA_data_clean" = mfu_eDNA_data_clean, 
             "removed_data" = removed_data,
             "mfu_eDNA_data" = mfu_eDNA_data
             )
    })
    
    
    # use the function created above to plot the user inputs:
    plot_eDNA(to_be_plotted(), 
              first_sample_ID = EQ_MFE_1, 
              plotby_taxa = species, 
              families = input$select, 
              stack_or_fill = input$seqtype)
    # If I can figure out how to eliminate the first_sample_ID requirement, I could make this app compatible with submitting other eDNA datasets...
    


  })
  
  # subset the entire dataframe based on what the user inputs for family
  df_subset <- reactive({
    a <- subset(mfu_eDNA_data, family == input$select)
    return(a)
  })
  
  
  # output the table of subsetted data
  output$details <- renderDT({
    
    # assign this so it can be required to have data
    subsetted <- df_subset()
    req(subsetted, nrow(subsetted) > 0)  # this makes it so that the table is only rendered if data exists! (I think technically this is not necessary as there are no selectable options that generate zero rows)
    
    # present the data table with options!
    datatable(df_subset(),
              options = list(paging = FALSE,    # this option removes the pages, as the subsets are small enough that it is not needed
                             scrollX = TRUE))   # this option adds a scroll bar on the X axis for better aesthetics! Source: https://forum.posit.co/t/scrollable-datatable-in-bslib-in-shiny-app/173866/4
    
    })

  
  
}

shinyApp(ui = ui, server = server)



