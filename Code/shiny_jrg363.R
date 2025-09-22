#Create my own shiny basic version to better understand. Perhaps a checkbox that does scatter size
install.packages("shiny")
install.packages("bslib")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggExtra")
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(ggExtra)

penguins_csv <- "https://raw.githubusercontent.com/jcheng5/simplepenguins.R/main/penguins.csv"

df <- readr::read_csv(penguins_csv)
# Find subset of columns that are suitable for scatter plot
df_num <- df |> select(where(is.numeric), -Year)

#Creating an object.
ui <- page_sidebar(
  sidebar = sidebar(
    #Creating a sidebar.
    varSelectInput("xvar", "X variable", df_num, selected = "Bill Length (mm)"),
    varSelectInput("yvar", "Y variable", df_num, selected = "Bill Depth (mm)"),
    #Create a checkbox. None of these variables need to be factor.
    checkboxGroupInput(
      "species", "Filter by species",
      choices = unique(df$Species),
      selected = unique(df$Species)
    ),
    hr(), # Add a horizontal rule
    #More checkbox. But what do these do? How are they configured.
    checkboxInput("by_species", "Show species", TRUE),
    checkboxInput("show_margins", "Show marginal plots", TRUE),
    checkboxInput("smooth", "Add smoother"),
    checkboxInput("size", "size of dots"),
  ),
  plotOutput("scatter")
)



#Now creating the app. And how to configure?
#Creating a function.
#Input is dataset?
server <- function(input, output, session) {
  subsetted <- reactive({
    req(input$species)
    df |> filter(Species %in% input$species)
  })

  output$scatter <- renderPlot({
    p <- ggplot(subsetted(), aes(!!input$xvar, !!input$yvar)) + list(
      theme(legend.position = "bottom"),
      if (input$by_species) aes(color = Species),
      geom_point(),
      if (input$smooth) geom_smooth(),
      if (input$size) aes(size = `Flipper Length (mm)`)
    )

    if (input$show_margins) {
      margin_type <- if (input$by_species) "density" else "histogram"
      p <- ggExtra::ggMarginal(p, type = margin_type, margins = "both",
                               size = 8, groupColour = input$by_species, groupFill = input$by_species)
    }

    p
  }, res = 100)
}

shinyApp(ui, server)
