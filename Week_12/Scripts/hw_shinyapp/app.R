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


# bring in data (this is also brought in below within the shinyapp, but it kept erroring to publish, so here it is up here as well!)

mfu_eDNA_data <- read_csv("cleanmfu_eDNA.csv")


# ok! Plan for this app: I will use my clean eDNA data from week 8 and make an app that allows you to interact with it!.


# shiny code! (where the inputs go)
ui <- fluidPage(
  
  # a title!
  h1("eDNA Data Explorer - MiFish U/E Multiplex & Master Mix Comparison"),
  "Created by: Kat Viehl",
  br(),
  "Created on: 2026-11-15",
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
                              "Dropped Sequences (Unknown genera/species within the family; does not work if there is no dropped data.)" = "removed_data",
                              "All Data" = "mfu_eDNA_data"), 
               selected = "mfu_eDNA_data_clean"),
  
  hr(),
  fluidRow(column(3, verbatimTextOutput("value"))),
  plotOutput("eDNAcol", height = "600px"),
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
  dataTableOutput("details")
  
)


# shiny cody part 2! (where the outputs go)

server <- function(input, output){
  output$eDNAcol <- renderPlot({
    
    
    
    # bring in data
    
    mfu_eDNA_data <- read_csv("cleanmfu_eDNA.csv")
    
    # summarizing the data that is about to be removed into a new dataframe:
    
    mfu_eDNA_data |> 
      filter(species == "dropped") -> removed_data
    
    
    # remove the dropped to make the plot better:
    
    mfu_eDNA_data |> 
      filter_out(genus == "dropped") |> 
      filter_out(species == "dropped") -> mfu_eDNA_data_clean
    
    
    
    
    # create eDNA function (from my week 08 homework! I want to see if I can make this more easy to interact with using dropdowns)
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
             x = "Site / Sample Name",               # labels
             y = y_axis) -> eDNA_plot
      
      return(eDNA_plot)
      
    }
    
    # have to do this in order to get the dataframe names back
    to_be_plotted <- reactive({
      switch(input$droppedornot, 
             "mfu_eDNA_data_clean" = mfu_eDNA_data_clean, 
             "removed_data" = removed_data,
             "mfu_eDNA_data" = mfu_eDNA_data
             )
    })
    
    plot_eDNA(to_be_plotted(), 
              first_sample_ID = EQ_MFE_1, 
              plotby_taxa = species, 
              families = input$select, 
              stack_or_fill = input$seqtype)
    
    


  })
  
  df_subset <- reactive({
    a <- subset(mfu_eDNA_data, family == input$select)
    return(a)
  })
  
  
  
  output$details <- renderDataTable({
    subsetted <- df_subset()
    
    req(subsetted, nrow(subsetted) > 0)
    
    df_subset()
    
    })

  
  
}

shinyApp(ui = ui, server = server)



