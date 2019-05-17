
server <- function(input, output, session) {
  
  #Percent Positive Pools by Year
  output$mosq_plot <- renderPlotly({
    #Format mosquito data for plot
    mosq_data <- perc_pos_by_week %>%
      mutate(Week_Start = format(Week_Start,"%b-%d" )) %>%
      mutate(Week_Start = factor(Week_Start, levels = week_starts)) %>%
      mutate(Week_Start_Year = MMWRweek2Date(as.numeric(as.character(Year)), as.numeric(as.character(CollectionWeek)))) %>%
      mutate(Week_Start_Year = format(Week_Start_Year, "%b %d")) %>%
      group_by(Year) %>%
      complete(Week_Start) %>%
      filter(as.character(Year) %in% input$mosq_plot_year) 
      
    disp_years = c(year-3, year-2, year-1, year) %>% as.character
    year_colors = c(greys[1], greys[2],  greys[4], color_pal[1])
    names(year_colors) = disp_years
    plot_colors = year_colors[unique(mosq_data$Year)]
    
    year_lines = c("dot", "dash", "dashdot", "solid")
    names(year_lines) = disp_years
    plot_lines = year_lines[unique(mosq_data$Year)]
    
    #Plot
    plot_ly(data = mosq_data, x = ~Week_Start, y = ~Percent, color = ~Year,  
            linetype = ~Year, linetypes = plot_lines,
            mode = 'lines+markers', colors = plot_colors, type = "scatter",
            text = paste0("Week ", mosq_data$CollectionWeek,", ", mosq_data$Year, "\nStart Date: ", mosq_data$Week_Start_Year,
                          "\n", round(mosq_data$Percent, 1), "% of pools were positive for WNV"), hoverinfo = "text") %>%
      layout(xaxis = list(title = "Week Start Date", showgrid = FALSE, tickangle = 270,
                          range = c(1,22)),
             yaxis = list(title = "% of Pools Positive for WNV", showgrid = FALSE),
             title = "Percent of Mosquito Pools Positive for West Nile Virus")
    
  })
  
  #Weekly mosquito summary table
  output$weekly_summary <- renderTable({
    weekly_summary_final
  })
  
  #Year to date mosquito summary table
  output$ytd_summary <- renderTable({
    ytd_summary_final
  })
  
  #MIR by year plot
  output$mir_year <- renderPlotly({
    #subset MIR to only selected years
    mir_by_year = mir_by_year %>%
      filter(MMWRyear %in% input$mir_years)%>%
      mutate(MMWRyear = factor(MMWRyear, levels = input$mir_years))
    
    #Set plot colors and line types
    plot_years = input$mir_years
    print(plot_years)
    n = length(plot_years)
    dash_types = c("dot", "dash", "dashdot")
    plot_cols = rep(greys,10)[1:n]
    plot_lines = rep(dash_types,10)[1:n]
    print(plot_cols)
    print(plot_lines)
    
    if(as.character(year) %in% plot_years){
      plot_cols[n] = color_pal[1]
      plot_lines[n] = "solid"
    }

    # year_colors = c("2016" = greys[1], "2017" = greys[2], "2018" = greys[4], "2019" = color_pal[1])
    # plot_colors = year_colors[unique(mir_by_year$MMWRyear)]
    # 
    # year_lines = c("2016" = "dot", "2017" = "dash", "2018" = "dashdot", "2019" = "solid")
    # plot_lines = year_lines[unique(mir_by_year$MMWRyear)]
    
    #produce plot
    plot_ly(data = mir_by_year, x = ~Week_Start, y = ~MIR, color = ~MMWRyear, 
            colors = plot_cols, type = "scatter",
            linetype = ~MMWRyear, linetypes = plot_lines,
            text = paste0("Week ", mir_by_year$MMWRweek,", ", mir_by_year$MMWRyear, "\nStart Date: ", mir_by_year$Week_Start_Year,
                          "\nMinimum Infection Rate: ", round(mir_by_year$MIR, 1), " per 1,000 Mosquitos"), hoverinfo = "text")%>%
      layout(xaxis = list(title = "Week Start Date", showgrid = FALSE, tickangle = 270,
                          range = c(1,22)),
             yaxis = list(title = "WNV MIR (per 1,000 Mosquitos)", showgrid = FALSE),
             title = "West Nile Virus Minimum Infection Rate")
    
    
  })
  
  #MIR by district plot
  output$mir_district <- renderPlotly({
    plot_ly(data = mir_district, x = ~Week_Start, y = ~MIR, color = ~District, colors = color_pal[1:4],
            mode = 'lines',  type = "scatter") %>%
      layout(xaxis = list(title = "Week Start Date", showgrid = FALSE, tickangle = 270, range = c(1,22)),
             yaxis = list(title = "WNV MIR (per 1,000 Mosquitos)", showgrid = FALSE),
             title = "West Nile Virus Minimum Infection Rate by Suburban Cook County District",
             hovermode = 'compare')
  })
  
  #district map
  output$district_map <- renderLeaflet({
    
    colors = color_pal[1:4]
    
    leaflet(cc, options = leafletOptions(minZoom = 8.5)) %>% addProviderTiles(providers$CartoDB.Positron) %>%  setView(lng = -87.86, lat = 41.83, zoom = 9) %>%
      addPolygons(
        fillColor = as.character(colors[cc$DIST]),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(weight = 4, color = "white", dashArray = "1", fillOpacity = 0.7, bringToFront = TRUE),
        label = cc$CITY,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px", direction = "auto")
      )%>%
      addLegend("topright", colors = colors, labels = c("North", "West", "Southwest", "South"),
                title = "District",
                opacity = 1)%>%
      addScaleBar(position = "bottomleft") 
    
    
  })
  
  #MIR vs. human cases plot
  output$MIRvHumans <- renderPlotly({
    years = 2005:(year-1)
    human_cases_year = sapply(years, function(x){
      i = grep(as.character(x), colnames(humans))
      n = sum(humans[,i])
      return(n)
    })
    peak_mir_year = sapply(years, function(x){
      peak = max(mir_by_year$MIR[mir_by_year$MMWRyear == x], na.rm = TRUE)
      return(peak)
    }) %>% round(2)
    
    plot_ly(x=peak_mir_year, y=human_cases_year, type = "scatter",
            text = paste0("Year: ", years, "\nPeak MIR: ", round(peak_mir_year,2), "\nHuman Cases: ", human_cases_year),
            hoverinfo = "text", marker = list(color = color_pal[1], size = 12, 
                                              line = list(color =color_pal[2], width = 3)
                                              )
            ) %>%
      layout(xaxis = list(title = "Season Peak MIR"),
             yaxis = list(title = "Number of Human WNV Cases"),
             title = "Mosquito MIR is Associated with Number of Human WNV Cases in a Season"
             )
    
  })
  
  #human cases and mosquito percent pos/mir plot
  output$human_mosq_plot <- renderPlotly({
    #human_mosq_plot
    cur_year_cases <- humans %>%
      select(CollectionWeek, paste0("Yr_", year)) %>%
      set_colnames(c("CollectionWeek", "Cases")) %>%
      mutate(Week_Start = week_starts) %>%
      mutate(Week_Start = factor(Week_Start, levels = Week_Start)) %>%
      replace_na(list(Cases = 0)) %>%
      mutate(MIR = mir_week$MIR) %>%
      mutate(perc_pos = num_per$percent_positive) %>%
      mutate(Cases = ifelse(CollectionWeek>week, NA, Cases))
    
    ymax = max(cur_year_cases$Cases, 20, na.rm = T)
    
    if(input$human_perc_or_mir == "% Pos Pools"){
      yvar2 = cur_year_cases$perc_pos
      name2 = "% Pos Pools"
      title2 = "Percent of Mosquito Pools Positive for WNV"
      max2 = 100
    }
    else if(input$human_perc_or_mir == "MIR"){
      yvar2 = cur_year_cases$MIR
      name2 = "MIR"
      title2 = "Mosquito Minimum Infection Rate per 1,000"
      max2 = max(yvar2, 20, na.rm = T)
    }
    
    
    plot_ly(data = cur_year_cases, x = ~Week_Start, y = ~Cases, type = "bar", marker = list(color = color_pal[4]),
            name = "Human Cases") %>%
        add_trace(x = ~Week_Start, y = yvar2, name = name2, yaxis = "y2",
                  type = "scatter", mode = "lines+markers", line = list(color = color_pal[2]),
                  marker = list(color = color_pal[2])) %>%
      layout(xaxis = list(title = "Week Start Date", tickangle = 270, showgrid = F, range = c(0,22)),
             yaxis = list(title = "Human WNV Cases", range = c(0,ymax), showgrid = F),
             yaxis2 = list(title = title2, side = "right", overlaying = "y",
                           range = c(0,max2), showgrid = F),
             legend = list(orientation = 'v', x = 0.8, y = 1),
             margin = list(b = 100, l = 100, r = 100, t = 100),
             hovermode = 'compare',
             title = paste("Human WNV Cases and Mosquito Surveillance in Suburban Cook County,", year)
             )
      
  })
  
  #cases by year bar/line plots
  output$cases_plot <- renderPlotly({
    #cases_plot
    if(is.null(input$human_year)){
      
    }else if(length(input$human_year) == 1 & input$human_year != "3 Year Average"){
      plot_year = input$human_year
      year_col = grep(plot_year, colnames(humans))
      plot_data = humans[,c(1,year_col)] %>%
        set_colnames(c("CollectionWeek", "Cases")) %>%
        replace_na(list(Cases = 0)) %>%
        mutate(Week_Start = week_starts) %>%
        mutate(Week_Start = factor(Week_Start, levels = Week_Start)) %>%
        mutate(Week_Start_Year = do.call(c, map(as.numeric(CollectionWeek), MMWRweek2Date, MMWRyear = as.numeric(plot_year)))) %>%
        mutate(Week_Start_Year = format(Week_Start_Year, "%b-%d")) %>%
        mutate(Week_Start_Year = factor(Week_Start_Year, levels = Week_Start_Year))
        
      ymax = max(plot_data$Cases, 20, na.rm = T)
      
      plot_ly(data = plot_data, x = ~Week_Start_Year, y = ~Cases, marker = list(color = color_pal[4]), type = "bar") %>%
        layout(xaxis = list(title = "Week Start Date", tickangle = 270),
               yaxis = list(title = "Human WNV Cases", range = c(0,ymax)),
               title = paste("Human WNV Cases in Suburban Cook County in", plot_year),
               hovermode = 'compare'
               )
    } else {
      plot_years = input$human_year
      #plot_years = c("2012", "2018", "2019")
      plot_data = humans_long %>%
        filter(Year %in% plot_years) %>%
        mutate(Week_Start = week_starts[as.character(CollectionWeek)]) %>%
        mutate(Week_Start = factor(Week_Start, levels = week_starts)) %>%
        filter(Year != year)
      
      plot_cols = rep(color_pal,5)[1:length(plot_years)]
      plot_lines = c(rep("solid",5), rep("dash",5), rep("dashdot",5))[1:length(plot_years)]
      
      p<- plot_ly(data = plot_data, x = ~Week_Start, y = ~Cases, type = "scatter", mode = "lines",
              color = ~Year, colors = plot_cols, linetype = ~Year, linetypes = plot_lines) %>%
        layout(xaxis = list(title = "Week Start Date", tickangle = 270, showgrid = F),
               yaxis = list(title = "Human WNV Cases", showgrid = F),
               title = "Human WNV Cases in Suburban Cook County",
               hovermode = 'compare'
        )
      
      if(year %in% input$human_year){
        plot_this_year = humans_long %>%
          filter(Year == year)%>%
          mutate(Week_Start = week_starts[as.character(CollectionWeek)]) %>%
          mutate(Week_Start = factor(Week_Start, levels = week_starts))
        
        p <- p %>%
          add_trace(data = plot_this_year, x = ~Week_Start, y = ~Cases, type = "bar", marker = list(color = color_pal[4]),
                    name = year)
      }
      suppressWarnings(p)
    } 
  })
  
  #cases by year heatmap
  output$cases_heatmap <- renderPlotly({
    
    h <- humans %>%
      filter(CollectionWeek %in% 20:41) %>%
      select(-c("CollectionWeek", "Avg_3")) %>%
      set_colnames(c(2005:year)) %>%
      set_rownames(as.character(week_starts)) %>%
      t() %>%
      as.matrix()
    
    hovtext = sapply(colnames(h), function(week){
      sapply(rownames(h), function(year){
        return(
          paste("Week start:", week, "\nYear:", year, "\nWNV Cases:", h[year,week])
        )
      })
    })
    
    plot_ly(z = h, x = colnames(h), y = rownames(h), type = "heatmap", colors = "Blues",
            text = hovtext, hoverinfo = "text") %>%
      layout(
        xaxis = list(title = "Week Start Date", showgrid = F, tickangle = 270),
        yaxis = list(title = "Year", showgrid = F, dtick = 1),
        title = "Human WNV Cases by Season in Suburban Cook County"
      )
  })
  
  
  #Pos traps/birds/humans map
  # output$wnv_map <- renderLeaflet({
  #   cook <- cook[-(which(cook$municipali == "Chicago")),]
  #   
  #   mosq_pos_year <- mosquito_year_table %>%
  #     filter(WNVpos == 1) %>%
  #     group_by(City) %>%
  #     summarise(pos_mosqs = sum(WNVpos))
  #   
  #   mosq_points <- apply(mosq_pos_year, 1, function(x){
  #     city = x["City"]
  #     count = x["pos_mosqs"] %>% as.numeric()
  #     polygon = cook[which(toupper(cook$municipali) == city),]
  #     pnts = spsample(polygon, count, "random", iter=10) %>% as.data.frame() 
  #     colnames(pnts) = c("x", "y")
  #     return(pnts)
  #   }) 
  #   
  #   mosq_points = do.call(rbind, mosq_points)
  #   
  #   bird_towns = c("Northbrook" = 1) %>% 
  #     as.data.frame() %>% 
  #     mutate(City = "Northbrook") %>%
  #     set_colnames(c("Count", "City"))
  #   
  #   
  #   bird_points <- apply(bird_towns, 1, function(x){
  #     city = x["City"]
  #     count = x["Count"] %>% as.numeric()
  #     polygon = cook[which(cook$municipali == city),]
  #     pnts = spsample(polygon, count, "random", iter=10) %>% as.data.frame() 
  #     colnames(pnts) = c("x", "y")
  #     return(pnts)
  #   }) 
  #   
  #   bird_points = do.call(rbind, bird_points)
  #   
  #   
  #   human_towns = c("Northbrook" = 1, "Rolling Meadows" = 2, "Alsip" = 1, "Des Plaines" = 3) %>% 
  #     as.data.frame() %>% 
  #     mutate(City = c("Northbrook", "Rolling Meadows", "Alsip", "Des Plaines") ) %>%
  #     set_colnames(c("Count", "City"))
  #   
  #   
  #   human_points <- apply(human_towns, 1, function(x){
  #     city = x["City"]
  #     count = x["Count"] %>% as.numeric()
  #     polygon = cook[which(cook$municipali == city),]
  #     pnts = spsample(polygon, count, "random", iter=10) %>% as.data.frame() 
  #     colnames(pnts) = c("x", "y")
  #     return(pnts)
  #   }) 
  #   
  #   human_points = do.call(rbind, human_points)
  #   human_icon <- makeAwesomeIcon(icon = "user", library = "glyphicon", markerColor = "darkred", iconColor = "white")
  #   bird_icon <- makeAwesomeIcon(icon = "twitter", library = "fa", markerColor = "orange", iconColor = "white")
  #   
  #   leaflet(cook, options = leafletOptions(minZoom = 9.4)) %>% addProviderTiles(providers$CartoDB.Positron) %>%  setView(lng = -87.86, lat = 41.81, zoom = 10) %>%
  #     addPolygons(
  #       fillColor = color_pal[1],
  #       weight = 2,
  #       opacity = 6,
  #       color = "white",
  #       dashArray = "3",
  #       fillOpacity = 0.3,
  #       highlight = highlightOptions(weight = 4, color = "white", dashArray = "1", fillOpacity = 0.7, bringToFront = FALSE),
  #       #label = paste(cc$CITY, "<br/> Chlamydia Cases:", cl_2017),
  #       label = cook$municipali,
  #       labelOptions = labelOptions(
  #         style = list("font-weight" = "normal", padding = "3px 8px"),
  #         textsize = "15px", direction = "auto")
  #     ) %>%
  #     addScaleBar(position = "bottomleft") %>%
  #     addCircles(mosq_points[,1], mosq_points[,2],
  #                color = color_pal[2], fillOpacity = 1, stroke = F,
  #                radius = 300) %>%
  #     # addCircles(bird_points[,1], bird_points[,2],
  #     #            color = color_pal[3], fillOpacity = 1, stroke = F,
  #     #            radius = 750) %>%
  #     # addCircles(human_points[,1], human_points[,2],
  #     #            color = color_pal[4], fillOpacity = 1, stroke = F,
  #     #            radius = 750) %>%
  #     addAwesomeMarkers(human_points[,1], human_points[,2],
  #                       icon = human_icon)  %>%
  #     addAwesomeMarkers(bird_points[,1], bird_points[,2],
  #                       icon = bird_icon)  %>%
  #     addLegend("topright", colors = color_pal[c(2,3,4)], 
  #               labels = c("WNV Positive Mosquito Pool", "WNV Positive Bird", "Human WNV Case"),
  #               opacity = 1)
  #   
  # })
  
  #Pos this week/previous week map
  map_week <- reactive({
    MMWRweek(input$map_week)[[2]]
  })
  
  map_week_date <- reactive({
    format(input$map_week, "%B %d, %Y")
  })
  
  pos_traps_week_data <- reactive({
    ever_pos = mosquito_year_table %>% 
      filter(MMWRweek <= map_week()) %>%
      group_by(LocationID) %>%
      dplyr::summarise(ever_pos = sum(WNVpos) > 0,
                pos_this_week = (TRUE %in% (WNVpos == 1 & MMWRweek == map_week())),
                last_pos = ifelse(sum(WNVpos) > 0, max(Collection_Date[WNVpos == 1]), NA)
      ) %>%
      filter(ever_pos == TRUE) %>%
      mutate(last_pos = format(as.Date(last_pos, origin = "1970-01-01"), "%b %d")) %>%
      merge(trap_locations, by = "LocationID") %>%
      ungroup() %>%
      mutate(fill_col = ifelse(pos_this_week == TRUE, "#dd1c1a", "#e58b89")) %>%
      mutate(City = simpleCap(City))
  })
  
  mir_traps_week_data <- reactive({
    mir_traps <- mosquito_year_table %>%
      mutate(LocationID = factor(LocationID, levels = levels(mosquito_week_table$LocationID))) %>%
      filter(MMWRweek == map_week()) %>%
      complete(LocationID) %>%
      group_by(LocationID) %>%
      dplyr::summarise(pos_pools = sum(WNVpos), mosqs_tested = sum(Number_Mosquitoes)) %>%
      mutate(MIR = pos_pools/mosqs_tested*1000) %>%
      merge(trap_locations, by = "LocationID", all.x = TRUE) %>%
      filter(!(is.na(x)) & !(is.na(y))) %>%
      mutate(City = simpleCap(City))
    mir_traps_cat <- mir_traps %>% mutate(MIR_cat = case_when(MIR > 25 ~ "> 25 infected mosquitos per 1000",
                                                              MIR <= 25 & MIR > 15 ~ "16-25 infected mosquitos per 1000",
                                                              MIR <= 15 & MIR > 5 ~ "6-15 infected mosquitos per 1000",
                                                              MIR <= 5 & MIR > 0 ~ "< 5 infected mosquitos per 1000",
                                                              MIR == 0 ~ "Negative",
                                                              TRUE ~ "Not Tested"))
    
    mir_traps$MIR_cat <- factor(mir_traps_cat$MIR_cat, levels = c("> 25 infected mosquitos per 1000", "16-25 infected mosquitos per 1000",
                                                                  "6-15 infected mosquitos per 1000", "< 5 infected mosquitos per 1000",
                                                                  "Negative", "Not Tested"), ordered = TRUE)
    return(mir_traps)
  })
  
  output$wnv_map <- renderLeaflet({
    updateSelectInput(session, "map_type")
    cook = cook[-which(cook$municipali == "Chicago"),]
    leaflet(cook, options = leafletOptions(minZoom = 9.4)) %>% addProviderTiles(providers$CartoDB.Positron) %>%  setView(lng = -87.86, lat = 41.81, zoom = 10) %>%
      addPolygons(
        fillColor = color_pal[1],
        weight = 2,
        opacity = 6,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.3,
        label = cook$municipali,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px", direction = "auto"),
        highlight = highlightOptions(weight = 4, color = "white", dashArray = "1", fillOpacity = 0.7, bringToFront = FALSE)
      ) %>%
      addScaleBar(position = "bottomleft") 
    

  })
  

  
  observeEvent({
    input$map_week
    input$map_type
    input$menu == "Mosquito Traps Map"
  },{
    
    if(input$map_type == "Positive Traps"){
      leafletProxy("wnv_map", data = pos_traps_week_data()) %>%
        clearGroup("circles") %>%
        addCircles(~x, ~y, fillColor = ~fill_col,
                   fillOpacity = 0.85, stroke = F, 
                   radius = 750,
                   group = "circles",
                   label = sprintf("%s<br/>%s", paste("Trap Location:", pos_traps_week_data()$City), 
                                   paste("Most Recent Positive Pool:",pos_traps_week_data()$last_pos))%>% lapply(htmltools::HTML),
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px"),
                     textsize = "15px", direction = "auto")
        )%>%
        clearControls() %>%
        addLegend("topright", colors = c("#dd1c1a", "#e05e5c"), 
                  labels = c(paste("Positive for WNV Week of", map_week_date()), "Positive for WNV this Season"),
                  opacity = 0.85, title = "Mosquito Traps")
    } else if(input$map_type == "Trap Minimum Infection Rate"){
      
      
      mir_cols = c("#ff4000", "#ff8000", "#ffbf00", "#ffff00", "#33cc00", "#c9c9c9")[as.numeric(mir_traps_week_data()$MIR_cat)]
      
      leafletProxy("wnv_map", data = mir_traps_week_data()) %>%
        clearGroup("circles") %>%
        addCircles(~x, ~y, fillColor = mir_cols,
                   fillOpacity = 0.85, stroke = F,
                   radius = 750,
                   group = "circles",
                   label = sprintf("%s<br/>%s", paste("Trap Location:", mir_traps_week_data()$City), 
                                   paste("MIR:",mir_traps_week_data()$MIR_cat))%>% lapply(htmltools::HTML),
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px"),
                     textsize = "15px", direction = "auto")
        )%>%
        clearControls() %>%
        addLegend("topright", colors = c("#ff4000", "#ff8000", "#ffbf00", "#ffff00", "#33cc00", "#c9c9c9"), 
                  labels = levels(mir_traps_week_data()$MIR_cat),
                  opacity = 0.85,
                  title = paste("Week of", map_week_date()))
    }

  })
  
  #bird testing waffle chart
  output$birds <- renderPlot({
    negative = sum(birds$Negative, na.rm = TRUE)
    positive = sum(birds$Positive, na.rm = TRUE)
    untestable = sum(birds$Untestable, na.rm = TRUE)
    
    bird_results = c("Negative WNV" = negative, "Positive WNV" = positive, "Untestable" = untestable)
    nrow = ceiling(sum(negative, positive, untestable)/10)
    
    waffle(bird_results, rows = nrow, colors = c("#33cc00", "#ff4000",  "#c9c9c9"),
           reverse = F, flip = F, xlab = "1 square = 1 bird", legend_pos = "bottom")
    

  })
  
  #Bird tests text
  output$bird_text <- renderText({
    submitted = sum(birds$Submitted, na.rm = TRUE) %>% replace_number() %>% simpleCap()
    negative = sum(birds$Negative, na.rm = TRUE) %>% replace_number()
    positive = sum(birds$Positive, na.rm = TRUE) %>% replace_number()
    untestable = sum(birds$Untestable, na.rm = TRUE) %>% replace_number() %>% simpleCap()
    pos_cities = birds[,grep("City",colnames(birds))] 
    pos_cities = pos_cities[!is.na(pos_cities)] %>% table()
    pos_birds = paste0(as.numeric(pos_cities) %>% replace_number() %>% simpleCap(), 
                       " bird(s) tested positive from ", names(pos_cities), ".") %>%
      paste(collapse = " ")
    out = paste0(submitted, " bird(s) have been submitted for testing and ",
                 negative, " have tested negative. ", pos_birds,
                 " ", untestable, " bird(s) were untestable.")
    
  })
  

}#Server function closure


