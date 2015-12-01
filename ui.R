library(shiny)
# library(shinydashboard)
library(leaflet)


shinyUI(
  navbarPage("Annual Ground Snow Maxima", id = "nav",
    
    tabPanel("Interactive map",
      div(class="outer",
        
        tags$head(
          # Include custom CSS
          includeCSS("include/styles.css")
        ),
        
        leafletOutput("map", height = "100%"),
        
        # POINT - Control of selected gridpoint related outputs
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
          draggable = TRUE, top = 60, left = "auto", right = 50, bottom = "auto",
          width = 330, height = "auto",
          
          br(),
          HTML(paste("<b>Characteristic value:<br> <h2> s<sub>k</sub> =", textOutput("char", inline = TRUE), " [kN/m<sup>2</sup>]</h2> </b>")),
          br(),
          
          HTML(paste("<b>Exceptional snow:<br> <h2> k =", textOutput("exc_k", inline = TRUE)," [-] </h2> </b>")),
          
          selectInput("distr_type", "Distribution type:", 
            choices = c("GEV" = "gev", "Gumbel" = "gumbel"),
            selected = "gev"
          ),
          
          helpText("Click on the Info tab for further information."),
          checkboxInput("advanced_point", HTML("<i>Advanced data exploration</i>")),
          
          # Advanced data exploration (descriptive statistics, RP-RV plots, duration curves, etc)
          conditionalPanel(condition = "input.advanced_point == true",
            tabsetPanel(
              tabPanel("RP-RV plot",
                
                # plotOutput("point_hist"),
                plotOutput("rprv"),
                textOutput("rp_rv_warn"),
                
                checkboxInput("add_control_RPRV", HTML("<i>Additional control</i>")),
                
                conditionalPanel(condition = "input.add_control_RPRV == true",
                  sliderInput("alpha_ci", "Confidence level", min = 0.01, max = 0.99, value = 0.9, step = 0.01)
                )
              ),
              tabPanel("Trend",
                       plotOutput("point_trend"),
                       br(),
                       HTML(paste("Average change per decade: <br>", textOutput("trend10", inline = TRUE), " [kN/m<sup>2</sup>/10-year]</h2>"))
              ),
              tabPanel("Descriptive",
                       br(),
                       HTML(paste("Mean:", textOutput("mean", inline = TRUE), "[kN/m<sup>2</sup>] <br><br>",
                                  "Standard deviation:", textOutput("sd", inline = TRUE), "[kN/m<sup>2</sup>] <br><br>",
                                  "Coefficient of variation:", textOutput("cov", inline = TRUE), "[-] <br><br>",
                                  "Skewness:", textOutput("skew", inline = TRUE), "[-] <br><br>" ,
                                  "Winter seasons with snow:", textOutput("n_snow", inline = TRUE), " out of 49"))
              )
            )
          )
        ),
        
        # AREA - Control of area/map related ouputs
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
          draggable = TRUE, top = 60, left = 50, right = "auto", bottom = "auto",
          width = 330, height = "auto",
          
          # area/map plot
          selectInput("parm2map", "Parameter to map", 
            choices = c("Coefficient of variation" = "cov", "Mean" = "mean", "Skewness" = "skew"),
            selected = "cov"
          ),
          
          # show meteorological stations
          checkboxInput("show_stations", HTML("<i>Show meteorological stations</i>")),
          conditionalPanel(condition = "input.show_stations == true",
                           helpText(HTML("&#9675; manual </br>
                                         &#9651; automatic </br>
                                         &#9633; composite"))
          ),
          
          checkboxInput("add_control_map", HTML("<i>Additional control</i>")),
          conditionalPanel(condition = "input.add_control_map == true",
            sliderInput("opacity", "Opacity", min = 0, max = 1, value = 0.5, step = 0.1)
          )
        ),
        
        tags$div(id="cite",
          'Data from the ', tags$em(tags$a(href = "http://www.carpatclim-eu.org/pages/home/", target="_blank", 'CARPATCLIM'), 
            ': Climate of the Carpathian Region'), '(1961-2010) research project.'
        )
      )
    ),
    
    tabPanel("Data explorer",
             downloadButton('download_annual_max', 'Download'),
             DT::dataTableOutput("annual_max_table")
    ),
    tabPanel("Info",
      withMathJax(),
      includeMarkdown("info.Rmd")
    ),
    tabPanel("About",
      includeMarkdown("about.Rmd")
    )
  )
)