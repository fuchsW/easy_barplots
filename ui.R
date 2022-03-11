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
# User interface computations
# ------------------------------------------------------------------------
ui <- fluidPage(theme = shinytheme("united"),
                
                # Default zoom out
                tags$style("
                              body {
                                    -moz-transform: scale(0.85, 0.85); /* Moz-browsers */
                                    zoom: 0.85; /* Other non-webkit browsers */
                                    zoom: 85%; /* Webkit browsers */
                                    }
                "),
                
                # Title for the dashboard
                titlePanel("Easy Barplots"),
                
                # Create the side panel, where all the inputs will be
                sidebarPanel(
                  
                  # Create tabs on the side panel, for different groups of inputs
                  tabsetPanel( id = "tabs", type = "pills", 
                               
                               # Tab 1, for uploading the table
                               tabPanel( title = "Table upload",
                                         
                                         # Horizontal line ---
                                         tags$hr(),
                                         
                                         # Input 0) Select the sheet to be read from the excel file
                                         textInput(inputId = "Sheet_number",
                                                   label = "Type the sheet number in your Excel file you would like to load (default is 1)",
                                                   value = 1),
                                         
                                         # Input 1) Table upload by the user
                                         fileInput(inputId = "file1", 
                                                   label = "Upload your table",
                                                   accept = c(".xlsx"),
                                                   buttonLabel = "Browse"
                                         ),
                                         
                                         # Info about the table
                                         helpText("The table ought to have the following columns: Condition & Value; and be formatted in .xlsx (Excel)"),
                                         
                                         # Horizontal line ---
                                         tags$hr(),
                                         
                                         # Button for next tab
                                         span( actionButton( inputId ="Next1", 
                                                             label = icon("arrow-right")),
                                               style = "position:absolute;right:2em;")
                               ),
                               
                               # Tab 2, for uploading the table
                               tabPanel( title = "Customize the plot",
                                         
                                         # Horizontal line ---
                                         tags$hr(),
                                         
                                         # Make a sub panel menu
                                         navlistPanel(  
                                           
                                           # SubTab 1 
                                           tabPanel( title = "Text font and size",
                                                     
                                                     # Input 2) Let the user choose a title and title size
                                                     textInput(inputId = "plot_title",
                                                               label = "Type the title of the plot",
                                                               placeholder = "Plot Title"),
                                                     sliderInput(inputId = "title_size",
                                                                 label = "Select title size",
                                                                 min = 1,
                                                                 max = 30,
                                                                 value = 15,
                                                                 width = "100%"),
                                                     
                                                     # Input 3) Let the user name the axis and axis title size
                                                     textInput(inputId = "x_axis",
                                                               label = "Type the label of the X-axis",
                                                               placeholder = "X-axis name"),
                                                     sliderInput(inputId = "x_size",
                                                                 label = "Select the X-axis title size",
                                                                 min = 1,
                                                                 max = 30,
                                                                 value = 12,
                                                                 width = "100%"),
                                                     textInput(inputId = "y_axis",
                                                               label = "Type the label of the Y-axis",
                                                               placeholder = "Y-axis name"),
                                                     sliderInput(inputId = "y_size",
                                                                 label = "Select the Y-axis title size",
                                                                 min = 1,
                                                                 max = 30,
                                                                 value = 12,
                                                                 width = "100%"),
                                                     
                                                     # Input 4) Let the user choose the text size of the title and axis
                                                     sliderInput(inputId = "text_size",
                                                                 label = "Select axis text size",
                                                                 min = 1,
                                                                 max = 30,
                                                                 value = 15), 
                                                     
                                                     # Input 5) Let the user choose the font
                                                     textInput( inputId = "font",
                                                                label = "Write down the name of your favorite font",
                                                                value = "Myriad Pro"),
                                           ),
                                           
                                           # SubTab 2
                                           tabPanel( title = "Color palette and theme",
                                                     
                                                     # Input 6) Let the user choose a color palette
                                                     selectInput(inputId = "palette",
                                                                 label = "Choose a color palette for the plot",
                                                                 choices = c("grey", "npg", "aaas", "nejm", "lancet", "jco", "ucscgb", "uchicago", "simpsons", "rickandmorty")),
                                                     
                                                     # Input 7) Let the user choose a plot theme
                                                     selectInput(inputId = "theme",
                                                                 label = "Choose a theme for the plot",
                                                                 choices = names(themes)),
                                           ),
                                           
                                           # SubTab 3
                                           tabPanel( title = "X-axis rotation",
                                                     
                                                     # Input 8) Let the user rotate the x-axis labels
                                                     sliderInput(inputId = "angle",
                                                                 label = "Would you like to rotate the X-axis labels? (Degrees)",
                                                                 min = 0,
                                                                 max = 90,
                                                                 value = 0),
                                                     
                                                     # Input 9) Let the user adjust horizontally the x-axis labels
                                                     sliderInput(inputId = "hjust",
                                                                 label = "Adjust the X-axis labels horizontally",
                                                                 min = 0,
                                                                 max = 1,
                                                                 value = 0.5),
                                           ),
                                           
                                           
                                           # SubTab 4
                                           tabPanel( title = "Add jitter",
                                                     
                                                     # Input 10) Add jitter?
                                                     checkboxInput(inputId = "jitter",
                                                                   label = "Would you like to add jitter to the bars?"),
                                           ),
                                         ),     
                                         # Horizontal line ---
                                         tags$hr(),
                                         
                                         # Button for next tab
                                         span( actionButton( inputId ="Next2", 
                                                             label = icon("arrow-right")),
                                               style = "position:absolute;right:2em;"),
                                         
                                         # Button for previous tab
                                         span( actionButton( inputId ="Previous1", 
                                                             label = icon("arrow-left")),
                                               style = "position:absolute;left:2em;")
                               ),
                               
                               # Tab 3, for uploading the table
                               tabPanel( title = "Statistics",
                                         
                                         # Horizontal line ---
                                         tags$hr(),
                                         
                                         # Input 11) Let the user choose between t.test and wilcox
                                         selectInput(inputId = "test",
                                                     label = "What kind of test would you like to perform?",
                                                     choices = list("T-Test" = "t.test",
                                                                    "Wilcoxon Test" = "wilcox.test")),
                                         
                                         # Input 12) Let the user choose between showing p-values or asterisks
                                         selectInput(inputId = "pvalue",
                                                     label = "Would you like to show p-values or asterisks?",
                                                     choices = list("Asterisk" = "p.signif", 
                                                                    "P-Value" = "p.format")),
                                         
                                         # Info about the tests
                                         helpText("For both the T test and the Wilcoxon test unpaired comparisons are done, with a two-sided alternative hypothesis"),
                                         
                                         # Horizontal line ---
                                         tags$hr(),
                                         
                                         # Input 13) Let the user make whatever comparisons wished (max 3 pairs)
                                         selectInput(inputId = "comparisons",
                                                     label = "Select the number of comparisons to be done",
                                                     choices = c("Do all pairs",
                                                                 "Compare one pair",
                                                                 "Compare two pairs",
                                                                 "Compare three pairs"
                                                     )),
                                         
                                         # Input 14) Let the user write down whatever comparisons wished (max 3 pairs)
                                         
                                         #One comparison
                                         conditionalPanel(
                                           condition = "input.comparisons == 'Compare one pair'",
                                           
                                           # Info about the comparisons
                                           h5("Write the name of the conditions you wish to compare"),
                                           helpText("(it needs to be written exacly like in the table -caps sensitive-)"),
                                           
                                           # Inputs
                                           h6("Compare A vs B"),
                                           textInput(inputId = "C1",
                                                     label = "A",
                                                     width = "50%"),
                                           textInput(inputId = "C2",
                                                     label = "B",
                                                     width = "50%")
                                         ),
                                         
                                         #Two comparison
                                         conditionalPanel(
                                           condition = "input.comparisons == 'Compare two pairs'",
                                           
                                           # Info about the comparisons
                                           h5("Write the name of the conditions you wish to compare"),
                                           helpText("(it needs to be written exacly like in the table -caps sensitive-)"),
                                           
                                           # Inputs
                                           h6("Compare A vs B"),
                                           textInput(inputId = "C1",
                                                     label = "A",
                                                     width = "50%"),
                                           textInput(inputId = "C2",
                                                     label = "B",
                                                     width = "50%"),
                                           h6("Compare C vs D"),
                                           textInput(inputId = "C3",
                                                     label = "C",
                                                     width = "50%"),
                                           textInput(inputId = "C4",
                                                     label = "D",
                                                     width = "50%")
                                         ),
                                         
                                         #Three comparison
                                         conditionalPanel(
                                           condition = "input.comparisons == 'Compare three pairs'",
                                           
                                           # Info about the comparisons
                                           h5("Write the name of the conditions you wish to compare"),
                                           helpText("(it needs to be written exacly like in the table -caps sensitive-)"),
                                           
                                           # Three comparisons
                                           h6("Compare A vs B"),
                                           textInput(inputId = "C1",
                                                     label = "A",
                                                     width = "50%"),
                                           textInput(inputId = "C2",
                                                     label = "B",
                                                     width = "50%"),
                                           h6("Compare C vs D"),
                                           textInput(inputId = "C3",
                                                     label = "C",
                                                     width = "50%"),
                                           textInput(inputId = "C4",
                                                     label = "D",
                                                     width = "50%"),
                                           h6("Compare E vs F"),
                                           textInput(inputId = "C5",
                                                     label = "E",
                                                     width = "50%"),
                                           textInput(inputId = "C6",
                                                     label = "F",
                                                     width = "50%"),
                                         ),
                                         
                                         # Button for previous tab
                                         span( actionButton( inputId ="Previous2", 
                                                             label = icon("arrow-left")),
                                               style = "position:absolute;left:2em;")
                               )
                  ),
                ),
                
                # Create the main panel, which will show the users table
                mainPanel(
                  
                  # Create tabs on the main panel, for different groups of inputs
                  tabsetPanel( id = "maintabs", 
                               
                               # Show credits
                               footer = div(HTML("<em>This app was developed by FFW, for questions contact fuchsf@fbmc.fcen.uba.ar</em>")),
                               
                               # Tab 1, for showing the table
                               tabPanel( title = "Your Table",
                                         
                                         # Show the table the user uploaded
                                         dataTableOutput(outputId = "contents")
                               ),
                               
                               # Tab 2, for showing the plot
                               tabPanel( title = "Your Plot",
                                         
                                         # Show the plot on the left
                                         column(6, 
                                                
                                                # Show plot
                                                plotOutput(outputId = "plot"),
                                                
                                                # Infor about the asterisks
                                                conditionalPanel(condition = "input.pvalue == 'p.signif'", 
                                                                 helpText("ns: p > 0.05\n*: p <= 0.05\n**: p <= 0.01\n***: p <= 0.001\n****: p <= 0.0001"))),
                                         
                                         # Offer different personalizations for the download
                                         column(6,
                                                sliderInput(inputId = "width",
                                                            label = "Select the width of the plot to be downloaded (mm)",
                                                            min = 10,
                                                            max = 300,
                                                            value = 100),
                                                sliderInput(inputId = "height",
                                                            label = "Select the height of the plot to be downloaded (mm)",
                                                            min = 10,
                                                            max = 300,
                                                            value = 100),
                                                selectInput(inputId = "format",
                                                            label = "Select a format for the plot image",
                                                            choices = c("png", "tiff", "jpeg", "bmp")),
                                                
                                                # Download button
                                                downloadButton(outputId = 'downloadPlot', 
                                                               label = 'Download'),
                                                
                                                # Draw a line around
                                                style='margin-bottom:30px;border:0.3px solid; padding: 10px;'
                                         ),
                                         
                                         
                               )
                  )  
                )
                
)
