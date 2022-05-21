library(shinymanager)
library(shiny)
library(leaflet)

# Define UI for app that draws a histogram ----
ui <- fluidPage(

  # App title ----
  titlePanel("Fit!"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

	fileInput("file", h3("Data file input")),
	#div(style="display: inline-block;vertical-align:top; width: 33%;", textInput("C","Enter C", "0.1")),
	div(style="display: inline-block;vertical-align:top; width: 16%;",    checkboxInput('kmh','km/h',FALSE),),
	downloadButton("downloadData", "Download"),
    ),
    # Main panel for displaying outputs ----
    mainPanel(

      leafletOutput("mymap",width = "100%", height = "800px"),
	hr(),
      plotOutput("Fit_plot",width = "100%", height = "800px"),
	hr(),
	plotOutput("Box_plot",width = "100%", height = "800px"),
	hr(),
	verbatimTextOutput("verb"),
      tableOutput("Fit_table")

    )
  )
)


#secure ui

# create_db(
#  credentials_data = credentials,
#  sqlite_path = "./database.sqlite", # will be created
#)



#-------------

ui <- secure_app(ui, enable_admin=TRUE)

#-------------

