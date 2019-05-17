

ui <- fluidPage(
  
  # NOTE - Functions to access the www folder (e.g. theme = and src = for images) were unsuccessful (folder permissions maybe?)
  
  #Bring in extra CSS to style application
  includeCSS("app.css"),
  
  #Add Google analytics global tracking code
  # tags$head(HTML('<script async src="https://www.googletagmanager.com/gtag/js?id=UA-107917571-1"></script>')),
  # tags$head(tags$script(HTML(" window.dataLayer = window.dataLayer || [];
  #                             function gtag(){dataLayer.push(arguments);}
  #                             gtag('js', new Date());
  #                             gtag('config', 'UA-107917571-1')"))),
  
  #Building the header 
  fluidRow(class = "header",
           column(class = "headimg", 2, align = "center", img(class = "imggen", src="http://cookcountypublichealth.org/files/images/CCDPH_logo-full.jpg", alt="CCDPH Logo")), 
           column(class = "headtitle", 10, HTML('
                                                <h1 style="font-weight: 700; font-size: 40px">Weekly West Nile Virus Surveillance Data</h1>
                                                '))
           ),
  #Starting nav bar
  navbarPage("Menu", id = "menu", windowTitle = "Cook County West Nile Virus Surveillance",
    tabPanel("Home",
             #Building Vertical strip of images on home page
             fluidRow(
               column(2, style = "padding-right: 50px;",
                      fluidRow(
                        column(12, class = "homestrip", img(src="https://phil.cdc.gov//PHIL_Images/17325/17325_lores.jpg", class = "img-responsive imggen"))
                      ),
                      fluidRow(
                        column(12, class = "homestrip", img(src="https://phil.cdc.gov//PHIL_Images/17325/17325_lores.jpg", class = "img-responsive imggen"))
                      ),
                      fluidRow(
                        column(12, class = "homestrip", img(src="https://phil.cdc.gov//PHIL_Images/17325/17325_lores.jpg", class = "img-responsive imggen"))
                      )
               ),
               #Building home page text    
              
#########################################################################################################################################
#UPDATE RISK LEVEL WHEN NEEDED. CHANGE ID TO LOWRISK, MEDRISK, OR HIGHRISK TO MATCH RISK LEVEL#
#########################################################################################################################################
               
               column(7, 
                    p(id = "lowrisk", "As of ",strong(paste("Week", week))," the risk of human West Nile Virus in Suburban Cook
                           County is ", strong("HIGH.")),
                     h4(strong("What is West Nile virus?"), style = "padding-bottom: 3px; padding-top: 15px"),
                     p("West Nile virus (WNV) is a serious disease most commonly spread by infected mosquitoes. Mosquitoes become infected when 
                       they feed on infected birds. Infected mosquitoes can then spread the virus to humans and other animals. Symptoms are usually 
                       mild and include fever, nausea, headache, and muscle aches. Four out of five people infected with WNV won't show any symptoms 
                       at all. However, in rare cases, severe illness including encephalitis (inflammation of the brain), meningitis (inflammation 
                       of the lining of the brain and spinal cord), or even death, can occur. More information on West Nile virus can be found on 
                       the Centers for Disease Control and Prevention website at ",
                       a(href = "https://www.cdc.gov/westnile", "https://www.cdc.gov/westnile"), ".", align = "justify", style = "padding-bottom: 10px"),
                     h4(strong("Why do surveillance for West Nile virus?"), style = "padding-bottom: 3px; padding-top: 5px"),
                     p("Most people get infected with West Nile virus after being bitten by an infected mosquito. However, sometimes symptoms don't 
                       begin until two weeks after the bite. West Nile Virus activity in mosquitoes and birds can provide an earlier signal of 
                       increased risk in the community. Research shows mosquito surveillance can warn health departments an outbreak is coming 
                       several weeks before a human case is reported. Simple activities like reducing the mosquito population or preventing mosquito 
                       bites can reduce the risk of infection. ", align = "justify", style = "padding-bottom: 10px"),
                     h4(strong("What is this application?"), style = "padding-bottom: 3px; padding-top: 5px"),
                     p("During periods when increased West Nile virus activity is expected (generally from June through October*), suburban Cook County's 
                       four mosquito abatement districts (MADs) - ",
                       a(href = "https://www.nsmad.com/", "the North Shore MAD"), ", ", 
                       a(href = "https://www.nwmadil.com/", "the Northwest MAD"), ", ", 
                       a(href = "https://www.desplainesvalleymad.com/", "the Desplaines Valley MAD"), ", and ", 
                       a(href = "https://www.sccmad.org/", "the South Cook County MAD"), "  - and the Cook County Department of Public Health and the Illinois 
                       Department of Public Health, trap and test mosquitos for West Nile Virus. The Cook County Department of Public Health also tests dead birds 
                       for West Nile virus and conducts disease surveillance in humans. Surveillance data is compiled into a ",
                       a(href = "http://cookcountypublichealth.org/data-reports/communicable-diseases", "weekly report"), " and shared with our partners in 
                       mosquito control, the healthcare community, and the public. This application is a companion to our weekly report. Comments or questions 
                       on the application can be submitted", 
                       a(href = "mailto:hannah.steinberg@cookcountyhhs.org?Subject=Shiny%20WNV%20App", "here."), " The Cook County Department of Public 
                        Health would like to thank all of our surveillance partners for their help in collecting this information.", align = "justify", style = "padding-bottom: 10px"),
                     h4(strong("What can I do to protect myself?"), style = "padding-bottom: 3px; padding-top: 5px"),                     
                     p("When outdoors between dusk and dawn, cover skin with lightly colored lose fitting clothing and use mosquito repellent with DEET, 
                       picaridin or oil of lemon eucalyptus. When applying repellent, always follow the directions on the product label. Get rid of standing water 
                       around your home in pet bowls, flower pots, old tires, baby pools and toys. Water that is allowed to stagnate for three or four days becomes 
                       a breeding ground for mosquitoes. Make sure your doors and windows have tightly fitting screens and repair any tears or other openings. 
                       Keep weeds and grass cut short and keep gutters clean and free of debris.", align = "justify", style = "padding-bottom: 20px"),
                     # p(id = "info", "For more information on WNV, please visit the Centers for Disease Control and Prevention at ",
                     #   a(href = "https://www.cdc.gov/westnile/", "https://www.cdc.gov/westnile/"), "."),
                     tags$small("*The Centers for Disease Control and Prevention aggregates WNV data by ", a(href = "https://wwwn.cdc.gov/nndss/document/W2018-19.pdf",
                        "MMWR Weeks. "), "MMWR Weeks run from Sunday to Saturday. For simplicity, graphs in this application typically display the first day of the MMWR Week 
                        on the x axis. Starting dates are accurate for 2019 but are approximations for all other years displayed."),
                     br(), br(), br()
               )
             ) #fluid Row closure
    ),#Home Tab Panel closure
#==========================================MOSQUITO SURVEILLANCE (UI)=============================================================#          

    tabPanel("Mosquito Surveillance", id = "Mosq", #ids added to potentially use in Google Analytics event tracking, may need modification for code to function
  #Building the intro title and data source description
      fluidRow(
           column(3, h3(strong("Mosquito Surveillance"), style = "padding-bottom: 10px; padding-top: 5px"), 
                  p("Mosquitos are collected and tested for West Nile virus by Cook County Department of Public Health, North West 
                    Mosquito Abatement District, North Shore Mosquito Abatement District, Des Plaines Valley Mosquito 
                    Abatement District, South Cook County Mosquito Abatement District, and the Illinois Department of Public Health.
                    Increased West Nile virus activity in mosquitoes is often observed several weeks before human cases are reported.",
                    align = "justify", style = "padding-bottom: 10px"),
                  p("When mosquitoes are tested for West Nile Virus, they are tested in groups, or 'pools', of up to 50 mosquitoes. 
                    Therefore when a mosquito pool tests positive, the number of WNV positive mosquitoes in that pool is unknown. 
                    It is only known that at least one mosquito in the pool was positive.", 
                    align = "justify", style = "padding-bottom: 10px")),
           column(4, br(), br(), br(), 
                  tableOutput("weekly_summary")
           ),
           column(5, br(), br(), br(), 
                  tableOutput("ytd_summary")
           )
      ),
      hr(),
      sidebarLayout(
        sidebarPanel(width = 3,
          checkboxGroupInput("mosq_plot_year", "Display Mosquito Data by Season for Suburban Cook County",
                             sort(unique(perc_pos_by_week$Year)),
                             selected = c("2018", "2019")
                             )
        ),
        mainPanel(width = 9,
          plotlyOutput("mosq_plot"),
          br(), br()
        )
      )

    ),#ED TabPanel closure


#============================================MIR (UI)================================================================# 
    tabPanel("Mosquito Infection Rate", id = "Mosq_Inf",
             fluidRow(
               column(4, h3(strong("Minimum Infection Rate"), style = "padding-bottom: 10px; padding-top: 5px"), 
                      p("An important measure of West Nile virus activity in mosquitoes is the minimum infection rate.
                        The minimum infection rate (MIR) is calculated by dividing the number of positive mosquito pools 
                        by the total number of mosquitoes tested and multiplying by 1000. It is the most conservative estimate 
                        of West Nile Virus activity in mosquitoes and can be a good indicator of human risk in the coming weeks. 
                        Seasons where MIR is higher than 15 infected mosquitoes per 1000 are associated with increased numbers 
                        of human cases.", 
                        align = "justify", style = "padding-bottom: 10px"))
               ),
             hr(),
             sidebarLayout(
               sidebarPanel(width = 3,
                            checkboxGroupInput("mir_years", "Display MIR Data by Season for Suburban Cook County",
                                               sort(unique(mir_by_year$MMWRyear)),
                                               selected = c("2018", "2019")
                            )
               ),
               mainPanel(width = 9,
                         plotlyOutput("mir_year"),
                         br(), br()
               )
             ),
             hr(),
             fluidRow(
               column(3, h4(("Suburban Cook County Districts")),
                      leafletOutput("district_map")),
               column(9, br(), br(),
                      plotlyOutput("mir_district"))
               
             ),
             hr(), br(),
             fluidRow(
               column(3, h4(("MIR is Correlated with WNV Cases")),
                      p("Mosquito minimum infection rate (MIR) can be an accurate predictor of human risk 
                        for West Nile Virus. In past seasons since 2005, high infection rates in mosquitos 
                        have been correlated with more human West Nile Virus cases in Suburban Cook County.", 
                        align = "justify", style = "padding-bottom: 10px")
                      ),
               column(9, plotlyOutput("MIRvHumans"))
             ),
             hr(),
             br(),
             br()
             ),#ED Map TabPanel closure
#==========================================MIR MAP (UI)=============================================================#
   
    tabPanel("Mosquito Activity by Trap", id = "Map",
             
      fluidRow(
        column( 4, h3(strong("Mosquito Activity by Trap"), style = "padding-bottom: 10px; padding-top: 5px"), 
                             p("Mosquito traps in suburban Cook County are operated by the North Shore MAD, the Northwest MAD, the Desplaines
                               Valley MAD, the South Cook County MAD, the Cook County Department of Public Health, and the Illinois Department 
                               of Public Health. During WNV season, traps are typically collected at least once, but often several times, a week.
                               Mosquitos collected from the traps are separated into pools of up to 50 mosquitos and each pool is tested for
                               West Nile virus. Choose from the options below to show whether a trap had any pools test positive for West Nile
                               or to show the trap-specific Minimum Infection Rate (see the Infection Rate tab for details on how MIR is calculated).", 
                               align = "justify", style = "padding-bottom: 10px"),
                              p("Remember that even if a trap in your area is negative for WNV, you should still take steps to protect yourself from 
                                mosquito bites through the summer season and especially at dawn and dusk.", 
                                align = "justify", style = "padding-bottom: 10px")
                )
        
        ),
      hr(),
      sidebarLayout(
        sidebarPanel(width = 3,
                     sliderInput("map_week", "Week Start Date",
                                        min = MMWRweek2Date(year, 20), max = MMWRweek2Date(year, week),
                                        value = MMWRweek2Date(year, week), animate = T, step = 7, ticks = FALSE,
                                 timeFormat = "%b %d"
                     ),
                     radioButtons("map_type", "Display Options",
                                  c("Positive Traps", "Trap Minimum Infection Rate")
                                  ),
                     tags$div(tags$small("* Reasons a trap might not be tested include battery or other equipment failure or 
                                         no mosquitoes present in the trap.",
                                         style = "font-style: italic")) #, style = "padding-bottom: 10px")
      ),
      mainPanel(width = 7,
        leafletOutput("wnv_map", height = "800px"),
        br(), br(), br()
      )
    )
    ),


#==========================================HUMANS (UI)=============================================================#
tabPanel("Human Surveillance", id = "Humans",
         
         fluidRow(
           column(4, h3(strong("Human West Nile Cases"), style = "padding-bottom: 10px; padding-top: 5px"),
                   p("People with a positive laboratory test for WNV, or people who are suspected to have WNV based on their 
                     symptoms, must be reported to the local health department where the patient resides. The Cook County 
                     Department of Public Health jurisdiction includes all of suburban Cook County, excluding Evanston, 
                     Skokie, Oak Park, and Stickney township.", align = "justify", style = "padding-bottom: 10px"),
                   p("Based on the type of symptoms the person experienced, WNV cases are categorized into neuroinvasive 
                     disease and non-neuroinvasive disease. Cases with non-neuroinvasive disease typically have a fever and
                     might also have headache, body aches, fatigue, joint pain, or gastrointestinal symptoms. Neuroinvasive 
                     disease is more serious and includes meningitis, encephalitis, acute flaccid paralysis, or other
                     neurological symptoms.", align = "justify", style = "padding-bottom: 10px"),
                   p("Since Week 20, two cases of West Nile Virus have been reported in suburban Cook County; one case was neuroinvasive.", 
                     align = "justify", style = "padding-bottom: 10px"))
                   ),
         hr(),

         # fluidRow(
         #   column(12, h4(strong("Seasonal Comparison of Human WNV Cases"), style = "padding-bottom: 10px; padding-top: 5px"))
         # ),
         br(),
         sidebarLayout(
           sidebarPanel(width = 2,
             checkboxGroupInput("human_year", "Display Season(s)",
                                choiceValues = c(2005:year, "3 Year Average"),
                                selected = "2019",
                                choiceNames = c("2005 (Outbreak Year)",
                                                2006:2011,
                                                "2012 (Outbreak Year)",
                                                2013:year,
                                                paste0("3 Year Avg. (", year-3, "-", year-1, ")"))
             )
           ),
           mainPanel(width = 10,
                     fluidRow(
                       column(6, plotlyOutput("cases_plot", height = "500px"), br(), br()),
                       column(6, plotlyOutput("cases_heatmap", height = "500px"), br(), br())
                       )
             
           )
         ),
         br(),hr(),br(),
         
         sidebarLayout(
           sidebarPanel(width = 2,
                        radioButtons("human_perc_or_mir", "Mosquito Surveillance Comparison",
                                     c("% Pos Pools", "MIR"),
                                     selected = c("% Pos Pools")
                        )
           ),
           mainPanel(width = 10,
                     plotlyOutput("human_mosq_plot")
           )
         ),
         br(), br()
      ),#icu tab closure


#==========================================BIRDS (UI)=============================================================#

tabPanel("Bird Surviellance", id = "Birds",
         
         fluidRow(
           column( 4, h3(strong("Bird Surveillance"), style = "padding-bottom: 10px; padding-top: 5px"), 
                   textOutput("bird_text")
                   )
                   ),
         hr(),
         fluidRow(
           column(6,
                  plotOutput("birds")
                  )
         )
    )#pi tab closure

  )#Navbarpage closure



)#UI fluidpage closure
