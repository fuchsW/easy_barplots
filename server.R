
#
# This is a ShinyApp designed to help non-programmers to plot stuff using R.
# Done by FFW. If you have questions, just write: fuchsf@fbmc.fcen.uba.ar
#

library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(shinyBS)
library(ggpubr)
library(shinythemes)
library(readxl)
library(shinyjqui)

# ------------------------------------------------------------------------
# Pre-made list of themes for the plot
# ------------------------------------------------------------------------

# First of all, we'll need the list of themes for the plot
themes <- list("Pubr" = theme_pubr(),
               "Pubclean" = theme_pubclean(),
               "Classic" = theme_classic(),
               "Void" = theme_void(),
               "Light" = theme_light(),
               "Dark" = theme_dark(),
               "Minimal" = theme_minimal())

# ------------------------------------------------------------------------
# Server side computations
# ------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Buttons to go through tabs ---------------------------------------------
  
  #sidebarPanel
  
  # Tab 1
  observeEvent(input$Next1, {
    updateTabItems(session, "tabs", "Customize the plot")
  })    
  
  # Tab 2
  observeEvent(input$Next2, {
    updateTabItems(session, "tabs", "Statistics")
  })    
  observeEvent(input$Previous1, {
    updateTabItems(session, "tabs", "Table upload")
  })    
  
  # Tab 3
  observeEvent(input$Previous2, {
    updateTabItems(session, "tabs", "Customize the plot")
  })    
  
  # Table ------------------------------------------------------------------
  
  # Show the uploaded table
  output$contents <- renderDataTable({
    
    # Select the user uploaded table
    inFile <- input$file1
    
    # Show nothing until the user uploaded the table
    if (is.null(inFile))
      return(NULL)
    
    # Read the table using read_excel
    read_excel(inFile$datapath, sheet = as.numeric(input$Sheet_number))
  },
  
  # These parameters define many characteristics of the table
  selection = 'single',
  rownames= FALSE,
  options = list(dom = 't', paging = FALSE)
  )
  
  # Make the Plot -------------------------------------------------------------------
  
  # First, make a reactive function for the theme of the plot
  plot_theme <- reactive({themes[[input$theme]]})
  
  # Plot using Conditions for the X axis and Value for the Y axis
  plotInput <- function(){
    
    # Select the user uploaded table
    inFile <- input$file1
    
    # Show nothing until the user uploaded the table
    if (is.null(inFile))
      return(NULL)
    
    # Read the table using read_excel
    df <- read_excel(inFile$datapath, sheet = as.numeric(input$Sheet_number))
    
    # Prepare the comparison tandems for statistics
    my_comparisons <- split(t(combn(levels(as.factor(df$Condition)), 2)), seq(nrow(t(combn(levels(as.factor(df$Condition)), 2)))))
    
    my_comparisons1 <- list(c(input$C1, input$C2))
    
    my_comparisons2 <- list(c(input$C1, input$C2),
                            c(input$C3, input$C4))
    
    my_comparisons3 <- list(c(input$C1, input$C2),
                            c(input$C3, input$C4),
                            c(input$C5, input$C6))
    
    
    # Build the plot
    ggbarplot(
      data = df, 
      x = "Condition", 
      y = "Value",
      add = c("mean_se", if(input$jitter == TRUE) {"jitter"}),  # Show error bar with SEM
      fill = "Condition",  palette = input$palette, # Define the color of the bars
      alpha=0.8, # Transparency of the bars
    )+
      
      # Show NO legends
      guides(fill="none")+
      
      # Names of the axis
      xlab(input$x_axis)+ ylab(input$y_axis)+
      
      # Plot title
      ggtitle(input$plot_title)+
      
      # Add statistics to the plot
      stat_compare_means(method = input$test,
                         comparisons = switch(input$comparisons,
                                              "Do all pairs" = my_comparisons,
                                              "Compare one pair" = my_comparisons1,
                                              "Compare two pairs" = my_comparisons2,
                                              "Compare three pairs" = my_comparisons3
                         ),
                         label= input$pvalue, 
                         tip.length = 0,
                         step.increase = 0.1)+
      
      # Choose the desired theme
      plot_theme() + theme(text=element_text(size = input$text_size, family = input$font),
                           axis.text.x = element_text(angle = input$angle, hjust = input$hjust),
                           axis.title.x = element_text(size = input$x_size),
                           axis.title.y = element_text(size = input$y_size),
                           plot.title = element_text(hjust = 0.5, size = input$title_size))
  }
  
  # Show Plot  -------------------------------------------------------------------
  
  output$plot <- renderPlot(print(plotInput()))
  
  # Download Plot  -------------------------------------------------------------------
  
  # First do the name of the plot
  plotname <- reactive({paste("Shinyplot", input$format, sep = ".")})
  
  output$downloadPlot <- downloadHandler(
    filename = plotname,
    content = function(file) {
      ggsave(file, dpi = "retina", 
             units = "mm",
             width = input$width, 
             height = input$height, 
             device = input$format)
    })    
  
}
