##TODO: 
## - rendering text with multiple reactive values?
## - 
##

# map projection??

library(shiny)
library(leaflet)
source("global.R")


shinyServer(function(input, output, session) {
  
  ##------------------------------------------------------
  ## MAP TAB
  ##------------------------------------------------------
  
  # static basemap
  output$map <- renderLeaflet({
    
    leaflet() %>% addProviderTiles("CartoDB.Positron", group = "positron_tile") %>%
      setMaxBounds(lng1 = 7, lat1 = 34, lng2 = 37, lat2 = 60) %>%
      addPolygons(
        lat = bound_lat,
        lng = bound_lng,
        weight = 1,
        fillColor = "transparent",
        color = "gray56") %>%
      addMarkers(data = stations,
                 icon = ~station_icons[StationType],
                 lng = ~Actual.Longitude,
                 lat = ~Actual.Latitude,
                 popup = ~Name.Station,
                 group = "stations")
  })
  
  
  
  # control displayed plot/rasterImage
  parm2map_flag <- eventReactive(input$parm2map, {
    
    parm2map_flag = runif(1)
  })
  
  
  
  # show meteorological stations
  observeEvent(input$show_stations, {
    # hideGroup("map", "stations")
    if (input$show_stations == TRUE){
      leafletProxy("map", session) %>%
        showGroup("stations")
    } else {
      leafletProxy("map", session) %>%
        hideGroup("stations")
    }
  })
  
  
  
  # update map with raster image
  observeEvent({input$opacity | parm2map_flag()}, { # 

    # r = raster(paste("data/raster_map_",input$parm2map,".grd", sep = ""))
    # r = readRDS(paste("data/raster_map_",input$parm2map,".rds", sep = ""))
    if (input$parm2map == "mean"){
      r = r_mean
    } else if (input$parm2map == "cov"){
      r = r_cov
    } else if (input$parm2map == "skew"){
      r = r_skew
    }
    
    min_r  = min(values(r), na.rm = TRUE)
    max_r  = max(values(r), na.rm = TRUE)

    domain = c(min_r - sign(min_r)*0.01*min_r, max_r + sign(max_r)*0.01*max_r)
    
    pal = colorNumeric(c("#0C2C84", "#f7f7f7", "#F98009"), domain = domain,
                       na.color = "transparent")
    
    leafletProxy("map", session) %>% 
      addRasterImage(r, colors = pal, opacity = input$opacity, layerId = "raster_image", project = T) %>%
      addLegend(pal = pal, values = values(r), title = input$parm2map, layerId = "legend")
  
  })
  
  
  
  # get the closest grid point coordinates to mouse click
  closest_gridpoint <- eventReactive(input$map_shape_click, {
    
    # coordinates
    click = data.frame(lat = input$map_shape_click$lat, lng = input$map_shape_click$lng)
    coord = round(click,1) #WARNING! related to gridded source data
    
    # index in data
    lidx_lat = A_MAX$code['lat'] == coord$lat
    lidx_lng = A_MAX$code['lng'] == coord$lng
    idx = which(as.logical(lidx_lat*lidx_lng))
    coord$idx = idx
    
    return(coord)
  })
  
  
  # add icon on mouse click - closesest grid point
  observeEvent(input$map_shape_click, {
    leafletProxy("map", session) %>% 
      addMarkers("map", 
                 lng = input$map_shape_click$lng,
                 lat = input$map_shape_click$lat,
                 popup = HTML(paste("<u>Clicked point:</u> <br>",
                                    "Lat:", round(input$map_shape_click$lat,2), "<br>",
                                    "Lng:", round(input$map_shape_click$lng,2), "<br><br>",
                                    "<u>Closest gridpoint:</u> <br>",
                                    "Lat:", closest_gridpoint()$lat, "<br>",
                                    "Lng:", closest_gridpoint()$lng)
                              )
      )
  })
  
  # get the data related to the mouse click (closest grid point)
  selected_data <- reactive({
    
    # select the corresponding dataset and plot it
    idx   = closest_gridpoint()$idx
    data  = A_MAX$value[idx,]
    data  = data[!is.na(data)]
    
    return(data)
  })
  
  
#   mle_fit <- eventReactive(input$map_shape_click, {
#     
#   })
  
  repr_val <- reactive({
    repr_val = get_repr_values(data = selected_data(), distr_type = input$distr_type)
  })
  
  lin_model <- reactive({
    data = selected_data()
    tt = 1:length(data)
    lin_model = lm(data~tt)
  })
  
  
  # descriptive statistics - with snowfree seasons as well!
  descr_stat <- reactive({
    data = selected_data()
    
    m      = mean(data)
    s      = sd(data)
    sk     = skewness(data, type = 2)
    n_snow = sum(selected_data() != 0) # number of winter seasons with snow cover
    
    descr = list(mean = m, sd = s, cov = s/m, skew = sk, n_snow = n_snow)
    return(descr)
  })
  
  
  # rp_rv plot
  rp_rv <- reactive({

    # a bit wasteful since in case of warning and errors the function is evaluated
    pp = tryCatch(plot_rprv(data = selected_data(), distr_type = input$distr_type, input),
                  warning=function(w) return(list(plot_rprv(data = selected_data(), distr_type = input$distr_type, input),w)))

    
    # not too elegant warning catcher
    # no warnings -> 3 element list
    if (length(pp) == 2) {
      w = pp[[2]]$message
      p = pp[[1]]
    } else {
      w = NULL
      p = pp
    }
    # browser()
    rp_rv = list(plot = p, warn = w)

    return(rp_rv)
  })
  
  ##------------------------------------------------------
  ## OUTPUTS
  ##------------------------------------------------------
  
  # characteristic value
  output$char <- renderText({
    char = repr_val()$char
    sprintf("%.2f", signif(char,3))
  })
  
  
  # exceptional snow check - 1998 European snow research
  output$exc_k <- renderText({
    exc_k = repr_val()$exc_k
    sprintf("%.2f", signif(exc_k,2))
  })
  
  # characteristic value
  output$rp_rv_warn <- renderText({
    # browser()
    rp_rv()$warn
  })
  
  
  # numeric outputs
  output$mean <- renderText(
    sprintf("%.2f", signif(descr_stat()$mean,3))
  ) 
  output$sd <- renderText(
    sprintf("%.2f", signif(descr_stat()$sd,3))
  )
  output$cov <- renderText(
    sprintf("%.2f", signif(descr_stat()$cov,3))
  )
  output$skew <- renderText(
    sprintf("%.2f", signif(descr_stat()$skew,3))
  )
  output$n_snow <- renderText(
    sprintf("%.0f", descr_stat()$n_snow)
  )
  output$trend10 <- renderText(
    sprintf("%.2f", signif(lin_model()$coefficients[2]*10,3))
  )
  
  
  # histogram plot
  output$point_hist <- renderPlot({
    validate(
      need(input$map_shape_click$lat, 'Click somewhere on the map!')
    )
    plot_point_hist(closest_gridpoint(), A_MAX)
  })
  
  
  # observation plot with time trend
  output$point_trend <- renderPlot({
    validate(
      need(input$map_shape_click$lat, 'Click somewhere on the map!')
    )
    
    data = selected_data()
    tt = 1:length(data)
    m = lin_model()$coefficients
    
    plot(tt, data, type = "b", xlab = '', ylab = expression(paste('Annual maximum [kN/m'^2, ']', sep = "")), xaxt = "n")
    lines(tt, m[1] + m[2]*tt, col = "red")
    axis(1, at = c(1,11,21,31,41,49), labels = A_MAX$year[c(1,11,21,31,41,49)], las = 2)
  })
  

  # RP-RV plot
  output$rprv <- renderPlot({
    validate(
      need(input$map_shape_click$lat, 'Click somewhere on the map!')
    )
    rp_rv()$plot
  })
  
  
  ##------------------------------------------------------
  ## DATA EXPLORATION TAB
  ##------------------------------------------------------
  
  output$download_annual_max <- downloadHandler(
    filename = 'ground_snow_annual_maxima.csv', 
    content = function(file) {
      data = data.frame(SWE = selected_data())
      data$year = A_MAX$year
      write.csv2(data, file, dec = ".")
    }
  )
  
  output$annual_max_table <- DT::renderDataTable({
    data = data.frame(selected_data())
    data$year = A_MAX$year
    DT::datatable(data, class = 'cell-border stripe', rownames = FALSE, colnames = c("SWE", "Year"),
                  caption = HTML("Annual ground snow maxima for the selected grid point, snow water equivalent [kN/m<sup>2</sup>]"),
                  selection = "none",
                  options = list(searching = F, ordering = T, paging = F, info = F, 
                                 autowidth = TRUE))
  })
  
})
