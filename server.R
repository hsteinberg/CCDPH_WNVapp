
server <- function(input, output, session) {

  #==========================================MOSQUITO SURVEILLANCE (SERVER)=============================================================#          
  
  #Percent Positive Pools by Year
  output$mosq_plot <- renderPlotly({
    plot_years = input$mosq_plot_year
    
    #Format mosquito data for plot
    mosq_data <- perc_pos_by_week %>%
      mutate(Week_Start = format(Week_Start,"%b-%d" )) %>%
      mutate(Week_Start = factor(Week_Start, levels = week_starts)) %>%
      mutate(Week_Start_Year = MMWRweek2Date(as.numeric(as.character(MMWRyear)), as.numeric(as.character(MMWRweek)))) %>%
      mutate(Week_Start_Year = format(Week_Start_Year, "%b %d")) %>%
      group_by(MMWRyear) %>%
      complete(Week_Start) %>%
      filter(as.character(MMWRyear) %in% plot_years) %>%
      ungroup() %>%
      mutate(MMWRyear = factor(MMWRyear, levels = plot_years))

    
    #Set plot colors and line types
    
    n = length(plot_years)
    dash_types = c("dot", "dash", "dashdot")
    plot_cols = rep(greys,10)[1:n]
    plot_lines = rep(dash_types,10)[1:n]

    
    if(as.character(year) %in% plot_years){
      plot_cols[n] = color_pal[1]
      plot_lines[n] = "solid"
    }
    
    #Plot
    plot_ly(data = mosq_data, x = ~Week_Start, y = ~percent_positive, color = ~MMWRyear,  
            linetype = ~MMWRyear, linetypes = plot_lines,
            mode = 'lines+markers', colors = plot_cols, type = "scatter",
            text = paste0("Week ", mosq_data$MMWRweek,", ", mosq_data$MMWRyear, "\nStart Date: ", mosq_data$Week_Start_Year,
                          "\n", round(mosq_data$percent_positive, 1), "% of pools were positive for WNV"), hoverinfo = "text") %>%
      layout(xaxis = list(title = "Week Start Date", showgrid = FALSE, tickangle = 270,
                          range = c(-1,22)),
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
  
  
#============================================MIR (SERVER)================================================================# 
  
  #MIR by year plot
  output$mir_year <- renderPlotly({
    #subset MIR to only selected years
    plot_years = input$mir_years
    mir_by_year = mir_by_year %>%
      filter(MMWRyear %in% plot_years)%>%
      mutate(MMWRyear = factor(MMWRyear, levels = plot_years))
    
    #Set plot colors and line types

    n = length(plot_years)
    dash_types = c("dot", "dash", "dashdot")
    plot_cols = rep(greys,10)[1:n]
    plot_lines = rep(dash_types,10)[1:n]

    
    if(as.character(year) %in% plot_years){
      plot_cols[n] = color_pal[1]
      plot_lines[n] = "solid"
    }

    
    #produce plot
    plot_ly(data = mir_by_year, x = ~Week_Start, y = ~MIR, color = ~MMWRyear, 
            colors = plot_cols, type = "scatter",
            linetype = ~MMWRyear, linetypes = plot_lines,
            text = paste0("Week ", mir_by_year$MMWRweek,", ", mir_by_year$MMWRyear, "\nStart Date: ", mir_by_year$Week_Start_Year,
                          "\nMinimum Infection Rate: ", round(mir_by_year$MIR, 1), " per 1,000 Mosquitos"), hoverinfo = "text")%>%
      layout(xaxis = list(title = "Week Start Date", showgrid = FALSE, tickangle = 270,
                          range = c(-1,22)),
             yaxis = list(title = "WNV MIR (per 1,000 Mosquitos)", showgrid = FALSE),
             title = "West Nile Virus Minimum Infection Rate")
    
    
  })
  
  #MIR by district plot
  output$mir_district <- renderPlotly({
    plot_ly(data = mir_district, x = ~Week_Start, y = ~MIR, color = ~District, colors = color_pal[1:4],
            mode = 'lines',  type = "scatter") %>%
      layout(xaxis = list(title = "Week Start Date", showgrid = FALSE, tickangle = 270, range = c(-1,22)),
             yaxis = list(title = "WNV MIR (per 1,000 Mosquitos)", showgrid = FALSE,
                          range = c(-0.1, (max(5,max(mir_district$MIR)+1)))),
             title = paste("West Nile Virus Minimum Infection Rate \nby Suburban Cook County District,",year),
             hovermode = 'compare',
             margin = list(t = 60))
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
      n = sum(humans_long$Cases[humans_long$Year == x])
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
             title = "Mosquito MIR is Associated with\nNumber of Human WNV Cases in a Season",
             margin = list(t = 60)
             )
    
  })
  
  
#==========================================HUMAN SURVEILLANCE (SERVER)=============================================================#
  
  #human cases and mosquito percent pos/mir plot
  output$human_mosq_plot <- renderPlotly({
    #human_mosq_plot
    cur_year_cases <- humans_long %>%
      filter(Year == year) %>%
      select(CollectionWeek, Cases) %>%
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
      title2 = "Percent of Mosquito Pools\nPositive for WNV"
      max2 = 100
    }
    else if(input$human_perc_or_mir == "MIR"){
      yvar2 = cur_year_cases$MIR
      name2 = "MIR"
      title2 = "Mosquito Minimum Infection\nRate per 1,000"
      max2 = max(yvar2, 20, na.rm = T)
    }
    
    
    plot_ly(data = cur_year_cases, x = ~Week_Start, y = ~Cases, type = "bar", marker = list(color = color_pal[4]),
            name = "Human Cases") %>%
        add_trace(x = ~Week_Start, y = yvar2, name = name2, yaxis = "y2",
                  type = "scatter", mode = "lines+markers", line = list(color = color_pal[2]),
                  marker = list(color = color_pal[2])) %>%
      layout(xaxis = list(title = "Week Start Date", tickangle = 270, showgrid = F, range = c(-1,22)),
             yaxis = list(title = "Human WNV Cases", range = c(0,ymax), showgrid = F),
             yaxis2 = list(title = title2, side = "right", overlaying = "y",
                           range = c(0,max2), showgrid = F),
             legend = list(orientation = 'v', x = 0.95, y = 0.95,  xanchor = "right"),
             margin = list(b = 100, l = 100, r = 100, t = 100),
             hovermode = 'compare',
             title = paste("Human WNV Cases and Mosquito Surveillance\nin Suburban Cook County,", year)
             )
      
  })
  
  
  #cases by year bar/line plots
  output$cases_plot <- renderPlotly({
    #cases_plot

      plot_years = input$human_year
      #plot_years = c("2012", "2018", "2019")
      plot_data = humans_long %>%
        filter(Year %in% plot_years) %>%
        mutate(Week_Start = week_starts[as.character(CollectionWeek)]) %>%
        mutate(Week_Start = factor(Week_Start, levels = week_starts)) %>%
        mutate(Year = factor(Year, levels=plot_years)) %>%
        mutate(Cases = ifelse(Year==year&CollectionWeek>week, NA, Cases)) %>%
        #mutate(Week = MMWRweek2Date(as.numeric(as.character(Year)), CollectionWeek))
        mutate(Week = Week_Start)
        
        #mutate(Cases = ifelse(Year==year&CollectionWeek>week, NA, Cases))
      
      plot_colors = c(color_pal, greys)[1:length(plot_years)]
      
      plot_rows = ceiling(length(plot_years)/4)
      
      p<- ggplot(data=plot_data, aes(x=Week, y=Cases,group=Year, fill =Year)) +
        geom_bar(stat = "identity")+
        scale_fill_manual(values=plot_colors)+
        facet_wrap(~Year, nrow = plot_rows) +
        theme(axis.text.x = element_text(angle = 90,color=rep(c("black","transparent"),11)),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.background = element_rect(fill = "white", colour = "grey50"),
              panel.grid.major = element_line(colour = "grey80"),
              plot.margin = margin(t=10,r=10,b=10,l=50)
        ) +
        xlab("") +
        ylab("Human WNV Cases") 

      ggplotly(p,  tooltip=c("x", "y")) %>%
        layout(showlegend = FALSE)
    
  })
  
  output$human_year_ui <- renderUI({
    plotlyOutput("cases_plot", height = ifelse((ceiling(length(input$human_year)/4) ==1),500,750 ))
  })
  
 
#========================================== MAP (SERVER)=============================================================#
  

  #Get week selected in map week slider
  map_week <- reactive({
    MMWRweek(input$map_week)[[2]]
  })
  
  #Format selected week as its start date
  map_week_date <- reactive({
    format(input$map_week, "%B %d, %Y")
  })
  
  #Format data for positive traps map
  pos_traps_week_data <- reactive({
    ever_pos = mosquito_year_table %>% 
      filter(MMWRweek <= map_week()) %>%
      group_by(LocationID) %>%
      dplyr::summarise(ever_pos = sum(WNVpos) > 0,
                pos_this_week = (TRUE %in% (WNVpos == 1 & MMWRweek == map_week())),
                last_pos = ifelse(ever_pos == TRUE, max(Collection_Date[WNVpos == 1]), NA)
      ) %>%
      filter(ever_pos == TRUE) %>%
      mutate(last_pos = format(as.Date(last_pos, origin = "1970-01-01"), "%b %d")) %>%
      merge(trap_locations, by = "LocationID") %>%
      ungroup() %>%
      mutate(fill_col = ifelse(pos_this_week == TRUE, "#dd1c1a", "#e58b89")) %>%
      mutate(City = simpleCap(City))
  })
  
  #Format data for MIR map
  mir_traps_week_data <- reactive({
    mir_traps <- mosquito_year_table %>%
      mutate(LocationID = factor(LocationID, levels = trap_locations$LocationID[trap_locations$City != "CHICAGO"])) %>%
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
  
  #Traps base map
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
      addScaleBar(position = "bottomleft") %>%
    
    #download button
    onRender(
      "function(el, x) {
      L.easyPrint({
      sizeModes: ['Current', 'A4Landscape', 'A4Portrait'],
      filename: 'Suburban Cook County Mosquito Surveillance Map',
      exportOnly: true,
      hideClasses: ['leaflet-control-easyPrint'],
      hideControlContainer: false,
      
      }).addTo(this);
  }"
        )

  })
  

  #Update map dots when map week or map type is changed, or when tab is selected
  observeEvent({
    input$map_week
    input$map_type
    input$menu == "Mosquito Activity by Trap"
  },{
    
    if(input$map_type == "Positive Traps"){
      leafletProxy("wnv_map", data = pos_traps_week_data()) %>%
        clearGroup("circles") %>%
        addCircles(~x, ~y, fillColor = ~fill_col,
                   fillOpacity = 0.85, stroke = F, 
                   radius = 750,
                   group = "circles",
                   label = sprintf("%s<br/>%s<br/>%s", paste("Trap Location:", pos_traps_week_data()$City), 
                                   paste("Agency:", pos_traps_week_data()$Agency),
                                   paste("Most Recent Positive Pool:",pos_traps_week_data()$last_pos)
                                   )%>% lapply(htmltools::HTML),
                   labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px"),
                     textsize = "15px", direction = "auto")
        )%>%
        clearControls() %>%
        addLegend("topright", colors = c("#dd1c1a", "#e05e5c"), 
                  labels = c(paste("Positive week of", map_week_date()), "Positive this season"),
                  opacity = 0.85, title = "Mosquito Traps")
    } else if(input$map_type == "Trap Minimum Infection Rate"){
      
      
      mir_cols = c("#ff4000", "#ff8000", "#ffbf00", "#ffff00", "#33cc00", "#c9c9c9")[as.numeric(mir_traps_week_data()$MIR_cat)]
      
      leafletProxy("wnv_map", data = mir_traps_week_data()) %>%
        clearGroup("circles") %>%
        addCircles(~x, ~y, fillColor = mir_cols,
                   fillOpacity = 0.85, stroke = F,
                   radius = 750,
                   group = "circles",
                   label = sprintf("%s<br/>%s<br/>%s", paste("Trap Location:", mir_traps_week_data()$City), 
                                   paste("Agency:", mir_traps_week_data()$Agency),
                                   paste("MIR:",mir_traps_week_data()$MIR_cat)
                                   )%>% lapply(htmltools::HTML),
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
  
  
#==========================================BIRD SURVEILLANCE (SERVER)=============================================================#
  
  #bird testing waffle chart
  output$birds <- renderPlot({
    negative = birds$Negative[birds$Year == year]
    positive = birds$Positive[birds$Year == year]
    untestable = birds$Untestable[birds$Year == year]
    
    bird_results = c("Negative WNV" = negative, "Positive WNV" = positive, "Untestable" = untestable)
    nrow = ceiling(sum(negative, positive, untestable)/10)
    
    if(nrow > 0){
    waffle(bird_results, rows = nrow, 
           colors = c("#4DCC64", "#FF5121","#c9c9c9"),
           reverse = F, flip = F, xlab = "", legend_pos = "bottom")+
      xlab("1 square = 1 bird")+
      theme(legend.text = element_text(size = 14),
            axis.title.x = element_text(size=14))
    }

  })
  


}#Server function closure


