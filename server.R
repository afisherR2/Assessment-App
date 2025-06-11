# Require packages
library(shiny)
library(shinyBS)
library(shinythemes)
library(shinyvalidate)
library(shinyFeedback)
library(shinybusy)
library(dataRetrieval)
library(readxl)
library(httr)
library(tidyverse)
library(DT)
library(leaflet)
library(leaflet.esri)
library(jsonlite)
library(openxlsx)
library(htmltools)
library(sf)


# Define server logic 
shinyServer(function(input, output) {
  
# Insert App
  
# First  button
  output$nextBtn <- renderUI({
    actionButton('nextBtn', 
                 label = h4('Get Started')
                 )})
  
## Tabset panel UI output
#####
  observeEvent(input$nextBtn, {
    
  output$tabsPanels <- renderUI({
    withMathJax(
    tabsetPanel(
      
  # output WQS pulled from Criteria Search Tool
    tabPanel('Review WQS',
             br(),
             br(),
             p('Review Water Quality Standards from the Criteria Search Tool. Standards are filtered to show ONLY those
                parameters assessed for in the', tags$a(href = 'https://www.epa.gov/system/files/documents/2024-07/final-2024-305b303d-integrated-report.pdf#page=17', 
                'most current assessment methodology.', target = 'blank')),
                
              p('WQS for cadmium, chromium, copper, lead, nickel, and zink are contigent on the hardness (CaC03) of the waterbody,
                and can be calculated using the ', strong('Hardness'), ' input field. The WQS equations for these metals can be found
                in the', tags$a(href = 'https://www.epa.gov/sites/default/files/2014-12/documents/prwqs.pdf#page=35', 
                'most recent WQS', target = 'blank')),
                
              p('Similarly, WQS for total ammonia nitrogen is contigent on the pH and temperature of the waterbody,
                and can be calculated using the ', strong('pH'), ' and ', strong('Temperature'), ' input fields. The equation for 
                total ammonia nitrogen can be found in the', tags$a(href = 'https://www.epa.gov/sites/default/files/2014-12/documents/prwqs.pdf#page=48',
                'most recent WQS', target = 'blank')),
 
             br(),
             
             # hardness input
             fluidRow(
             column(4, 
             output$hard <- renderUI(
               numericInput('hard',
                            label = h3('Hardness (mg/L):'), width = '50%',
                            value = 100))),
             
             # pH input
             column(4,
             output$ph <- renderUI(
               numericInput('ph',
                            label = h3('pH:'), width = '50%',
                            value = 7))),
             
             # temp input
             column(4,
             output$temp <- renderUI(
               numericInput('temp',
                            label = h3('Temperature (C):'), width = '50%',
                            value = 23)))
             ),
             
             
             fluidRow(
               br(),
               br(),
               
               # Download Button             
               column(4, offset = 10, 
                      actionButton('CSTdwnld',
                                   label = 'Download WQS')),
               br(),
               br()),
             
               br(),
               br(),
               
    # Review standards from Criteria Search Tool
              DTOutput('wqsTable'),
                br(),
                br()
    ),

  # output WQP Data Table
    tabPanel('Review Sample Data',
             br(),
             br(),
             
             p('Review monitoring data from the Water Quality Portal. Data are filtered to show ONLY those
                parameters assessed for in the ', tags$a(href = 'https://www.epa.gov/system/files/documents/2024-07/final-2024-305b303d-integrated-report.pdf#page=17', 
                'most current assessment methodology', target = 'blank'), ' and collected within the ', strong('Date Range to Assess.')
                # p('feedback: time series plots for each parameter? Selector to look at parameters.
                #   upload button for addition data (not in the WQP) - NEED a formatted template file'),
                # p('NEED: assessment method for sample measurements that are "BELOW DETECTION.
                #   NEED to manage units and sample fractions',
                #   p('Are the lake monitoring data reported to WQX? I cannot find them (by monitoring station name)')
                #   )
                ),
             
             
             fluidRow(
               br(),
               br(),
               
            # Download Button             
               column(4, offset = 10, 
                      actionButton('WQPdwnld',
                                   label = 'Download Sample Data')),
               br(),
               br()),
  

               br(),
               br(),
            
    # WQP data table
    withMathJax(),
            DTOutput('WQPtable'),
    
              br(),
              br()

  ),
  
  # Download GIS from ATTIANS (AUs) and WQP (monitoring locations) from WQP Geoservices
    tabPanel('Download GIS',
             br(),
             br(),
             
             p('Download monitoring locations from the Water Quality Portal and assessment units from ATTAINS Geospatial Services.
                Monitoring locations are filtered to show only those with sample collected withinthe ', strong('Date Range to Assess.',
               ' Editable monitoring location lat longs.'
                # p('feedback: spatial join for AUs and monitoring sites - output in one table.
                #   Option to add coral reef map and spatially join monitoring sites to coral locations to use appropriate WQS')
                )),
  
            fluidRow(
              br(),
              br(),
              
              # Download Button             
              column(4, offset = 10, 
                     actionButton('GISdwnld',
                                  label = 'Download GIS')),
              br(),
              br()),
            
    # GIS of monitoring sites and ATTAINS GeoServices

              br(),
              br(),
              
     # Map of assessment units and monitoring sites
           leafletOutput('GISmap'),
              br(),
              br(),
    
          h4('Table of Monitoring Sites'),
    
  # monitoring sites table
          DTOutput('MonitoringSitestable'),
              br(),
              br(),
  
          h4('Table of Assessment Units'),
              br(),
              br(),
          DTOutput('AssessmentUnittable'),
              br(),
              br()
    ),
  
  
  # Download last assessment in ATTAINS
    tabPanel('Download ATTAINS',
             br(),
             br(),
             
             h4('Download simplified, most recent assessment cycle from ATTAINS. Review.'),
             
             fluidRow(
               br(),
               br(),
               
               # Download Button             
               column(4, offset = 10, 
                      actionButton('ATTAINSdwnld',
                                   label = 'Download ATTAINS Cycle')),
               br(),
               br()),
             
             br(),
             br(),
             
    # Simplified ATTAINS Table
             DTOutput('ATTAINStable'),
             br(),
             br()),
  
  # Assessment 
    tabPanel('Make Assessment',
             br(),
             br(),
             
             h4('Button that starts the assessment process. Built in Assessment Methodology.'
                # p('feedback: monitoring sample minimum input field. default = assessment methodology.
                #   QC after the assessments are made - user override')
                ),
             
             fluidRow(
               br(),
               br(),
               
               # Download Button             
               column(4, offset = 10, 
                      actionButton('AssessmentsBtn',
                                   label = 'Generate Plots for Assessment')),
               br(),
               br()),
             
             br(),
             br()
             
             # Simplified ATTAINS Table
             # DTOutput(''),
             # br(),
             # br()
             ),
  
  # Reports
    tabPanel('Export Reports',
             br(),
             br(),
             
             h4('Generate reports, maps, and files.'))
  ))
  
  })
  
})
#####  


  
##  read in assessed parameter crosswalk file
  standards_wqp_xwalk <- eventReactive(input$nextBtn, {
    if(input$TerritoryName == 'Puerto Rico'){
      
      read.csv('www/PR_standards_wqp_xwalk.csv')} else {
        read.csv('www/VI_standards_wqp_xwalk.csv')}
  }, ignoreNULL = FALSE)
  
  
  # reactive elements for hardness, ph, and temp
  hard <- reactive({
    as.numeric(input$hard)
  })
  
  ph <- reactive({
    as.numeric(input$ph)
  })
  
  temp <- reactive({
    as.numeric(input$temp)
  })
  

# download from CST and filter for assessed parameters
#####
  observeEvent(input$CSTdwnld, {
    
    
# FOR Puerto Rico
    # NEED clarification on "Pesticides (organochlorides)" from assessment methodology
    # NO WQS for NH3+NH4 as N but still assess for it?? (in methodology)
    # specific conductance
    # flow
    # chlorides?
    # Oil and Grease
    
    
# list of assessed parameters for USVI!
    # refer to assessment methodology: https://usepa.sharepoint.com/sites/TMDLandStandardSection/Shared%20Documents/Assessment,%20303(d),%20IR/USVI/2022/2022%20Assessment%20Methodology.pdf?CT=1701354153321&OR=ItemsView
    # vi_assess_param <- c('enterococci', 'turbidity', 'clarity', 'phosphorus [phosphorous (yellow)]', 'nitrogen [total nitrogen]',
    #                      'pH (acidity/alkalinity)', 'pH variation', 'dissolved oxygen','temperature', 'temperature rise above ambient')

    
    
    # download criteria form EPA OST Criteria Search Tool
    wqsraw <- read.xlsx('https://cfpub.epa.gov/wqsits/wqcsearch/criteria-search-tool-data.xlsx',
                        startRow = 206)
    
    wqsfine <- wqsraw %>% 
      
      filter(ENTITY_NAME == input$'TerritoryName') %>%
      
      select(c(POLLUTANT_NAME, CRITERION_VALUE, UNIT_NAME,
               CRITERIATYPEAQUAHUMHLTH, CRITERIATYPEFRESHSALTWATER,
               USE_CLASS_NAME_LOCATION_ETC, EFFECTIVE_DATE, LAST_ENTRY_IN_DB)) %>% 
      
      # rename columns
      rename(`Parameter` = POLLUTANT_NAME,
             `Criterion Value` = CRITERION_VALUE,
             `Unit` = UNIT_NAME,
             `Criterion Type` = CRITERIATYPEAQUAHUMHLTH,
             `Criterion Water Type` = CRITERIATYPEFRESHSALTWATER,
             `Criterion Application` = USE_CLASS_NAME_LOCATION_ETC,
             `Effective Date` = EFFECTIVE_DATE,
             `Updated` = LAST_ENTRY_IN_DB) %>% 
      
      # filter for only assessed parameters
      filter(Parameter %in% standards_wqp_xwalk()$cst) %>% 
      
      # add units to total ammonia nitrogen
      mutate(Unit = case_when(Parameter == 'total ammonia nitrogen' ~ 'mg/L',
                              .default = Unit)) %>% 
      
      # make some changes - pH wqs are read in as a range - change to single values
      mutate(`Criterion Value` = case_when(`Criterion Value` == 'See Equation' ~ '0.666', # replace all "See Equation" statements with a number
                                           Parameter == 'pH (acidity/alkalinity)' & `Criterion Application` == 'class SB waters' ~ '7.3', # recode to low end of range
                                           Parameter == 'pH (acidity/alkalinity)' & `Criterion Application` == 'class SD waters' ~ '6.0', # recode to low end of range
                                           .default = `Criterion Value`),

             
             `Criterion Value` = str_remove(`Criterion Value`, ','), # remove all commas from large numbers
             `Criterion Value` = as.numeric(`Criterion Value`)) %>% # convert all to numeric
      
      # change names of ph to low end of range
      mutate(Parameter = case_when(Parameter == 'pH (acidity/alkalinity)' & `Criterion Application` == 'class SB waters' ~ 'pH > (acidity/alkalinity)',
                                   Parameter == 'pH (acidity/alkalinity)' & `Criterion Application` == 'class SD waters' ~ 'pH > (acidity/alkalinity)',
                                   .default = Parameter))

    
    # copy and modify ph wqs
    wqsfineph <- wqsfine %>% 
      
      filter(Parameter == 'pH > (acidity/alkalinity)') %>% 
      
      # recode to high end of range
      mutate(`Criterion Value` = case_when(`Criterion Application` == 'class SB waters' ~ 8.5,
                                           `Criterion Application` == 'class SD waters' ~ 9.0)) %>% 
      
      # change names to high end of range
      mutate(Parameter = case_when(`Criterion Application` == 'class SB waters' ~ 'pH < (acidity/alkalinity)',
                                   `Criterion Application` == 'class SD waters' ~ 'pH < (acidity/alkalinity)'))

    
    # reactive table, add ph high, change metal wqs
    wqsfineEQ <- reactive({

      wqsfine %>%
        
        add_row(wqsfineph) %>% 

        # change metals based on hardness, ph, temp
        # equation from puerto rico wqs: https://www.epa.gov/sites/default/files/2014-12/documents/prwqs.pdf
        mutate(`Criterion Value` = case_when(

          # copper
          `Criterion Value` == 0.666 & Parameter == 'copper' ~ round(exp(0.8545 * (log(hard())) - 1.702), 2),

          # cadmium
          `Criterion Value` == 0.666 & Parameter == 'cadmium' ~ round(exp(0.7977 * (log(hard())) - 3.909), 2),

          # chromium iii
          `Criterion Value` == 0.666 & Parameter == 'chromium iii' ~ round(exp(0.8190 * (log(hard())) + 0.6848), 2),

          # nickel
          `Criterion Value` == 0.666 & Parameter == 'nickel' ~ round(exp(0.8460 * (log(hard())) + 0.0584), 2),

          # lead
          `Criterion Value` == 0.666 & Parameter == 'lead' ~ round(exp(1.273 * (log(hard())) - 4.705), 2),

          # zinc
          `Criterion Value` == 0.666 & Parameter == 'zinc' ~ round(exp(0.8473 * (log(hard())) + 0.884), 2),

          # TAN - but need to change!
          `Criterion Value` == 0.666 & Parameter == 'total ammonia nitrogen' ~ round(0.8876 * (0.0278 / (1 + 10^(7.688 - ph())) + (1.1994 / (1 + 10^(ph() - 7.688)))) * (2.126 * 10^(0.028 * (20 - temp()))), 2),
 

                                            .default = `Criterion Value`)) %>% 
        arrange(Parameter)
    })
    

    # output table - editable
    output$wqsTable <- renderDT(wqsfineEQ(),
                                rownames = FALSE,
                                editable = 'cell',
                                filter = 'top',
                                options = list(
                                  autoWidth = TRUE,
                                  columnDefs = list(list(# centers column names
                                    # width = '100px',
                                    className = 'dt-center', 
                                    targets = '_all')), 
                                  # scrollX = TRUE,
                                  pageLength = 51
                                ))

  })
#####  
  
# pull data from WQP
#####
  observeEvent(input$WQPdwnld, {
    
    if(input$TerritoryName == 'Puerto Rico'){
      statecode <- 'PR'} else{
        statecode <- 'VI'}
    
    
    WQPdata <- readWQPdata(statecode = statecode,
                              dataProfile = 'narrowResult',
                           startDateLo = input$dateRange[1],
                           startDateHi= input$dateRange[2])
    
    
    WQPdata_fine <- WQPdata %>% 
      filter(CharacteristicName %in% standards_wqp_xwalk()$wqp | CharacteristicName %in% standards_wqp_xwalk()$wqp.alt) %>% # filter for only assessed parameters
      select(MonitoringLocationIdentifier, ActivityStartDate, CharacteristicName, ResultMeasureValue, ResultMeasure.MeasureUnitCode,
             ResultSampleFractionText, ResultDetectionConditionText, ResultValueTypeName, ResultStatusIdentifier, ResultCommentText) %>% 
      
      rename(`Monitoring Location` = MonitoringLocationIdentifier, 
             `Sample Date` = ActivityStartDate, 
             Parameter = CharacteristicName,
             Value = ResultMeasureValue, 
             Unit = ResultMeasure.MeasureUnitCode, 
             `Sample Fraction` = ResultSampleFractionText,
             `Detection Condition` = ResultDetectionConditionText, 
             `Value Type` = ResultValueTypeName, 
             `Status Identifier` = ResultStatusIdentifier,
             Comments = ResultCommentText)
  
    
    # save data table as output variable then feed into UI output
    output$WQPtable <- renderDT(WQPdata_fine,
                                rownames = FALSE,
                                editable = 'cell',
                                filter = 'top',
                                options = list(
                                  autoWidth = TRUE,
                                  columnDefs = list(list(# centers column names
                                    # width = '200px',
                                    className = 'dt-center', 
                                    targets = '_all')), 
                                  pageLength = 15
                                ))
  })
#####  
  
# map monitoring sites and ATTAINS assessment units
#####  
  # Table of assessment units with IDs, associated monitoring stations
  # Table of monitoring locations with IDs, lat, longs, etc
 
  observeEvent(input$GISdwnld, {
    
    # site types of interest for PR
    sites_typesPR <- c('Stream', 'Estuary', 'Spring', 'Lake, Reservoir, Impoundment', 'Stream: Canal',
                       'Ocean: Coastal', 'River/Stream', 'Ocean', 'Lake', 'BEACH Program Site-Ocean',
                       'Reservoir')
    
    # site types of interest for VI
    sites_typesVI <- c('Stream', 'Estuary', 'Riverine Impoundment', 'Ocean', 'Wetland Palustrine-Forested', 
                       'BEACH Program Site-Ocean')
    
    # conditional statement for territory name
    if(input$TerritoryName == 'Puerto Rico'){
      
      statecode <- 'PR'
      statecodeGIS <- "'PR'"
      
      sites_types <- sites_typesPR
      
      # map focus point and zoom
      long <- -66.4131
      lata <- 18.2298
      zooms <- 8
      } else{
        
        statecode <- 'VI'
        statecodeGIS <- "'VI'"
        
        sites_types <- sites_typesVI
        
        # map focus point and zoom
        long <- -64.7822
        lata <- 18.0629
        zooms <- 9
          }
    
    
    # call WQX for monitoring sites in PR with the parameters in 
    Monitoringsites <- whatWQPsites(
      statecode = statecode,
      characteristicName = standards_wqp_xwalk()$wqp,
      
      # select for only those monitoring locations that are sampling during the user specified time period
    
      startDateLo = input$dateRange[1],
      startDateHi= input$dateRange[2]
    )
    
    # filter data
    MonitoringsitesFILT <- Monitoringsites %>% 
      select(OrganizationIdentifier,
             OrganizationFormalName, 
             MonitoringLocationIdentifier,
             MonitoringLocationDescriptionText, 
             MonitoringLocationName, 
             MonitoringLocationTypeName,
             LatitudeMeasure, 
             LongitudeMeasure) %>% 
      
      
      # filter for specific site types
      filter(MonitoringLocationTypeName %in% sites_types) %>%  
      
      mutate(Longitude = as.numeric(LongitudeMeasure), # change names and to numeric
             Latitude = as.numeric(LatitudeMeasure),
             .keep = 'unused') %>% 
      
      # Rename columns
      
      rename(`Organization ID` = OrganizationIdentifier,
             `Organization Name` = OrganizationFormalName,
            `Monitoring Site ID` = MonitoringLocationIdentifier,
            `Monitoring Site Name` = MonitoringLocationName,
            `Monitoring Site Description` = MonitoringLocationDescriptionText,
            `Monitoring Location Type` = MonitoringLocationTypeName)
    
  # table of monitoring sites
    output$MonitoringSitestable <- renderDT(
                                    datatable(
                                      MonitoringsitesFILT,
                                      rownames = FALSE,
                                      editable = 'cell',
                                      filter = 'top',
                                      options = list(
                                        autoWidth = TRUE,
                                        columnDefs = list(list(# centers column names
                                          className = 'dt-center', 
                                          targets = '_all')), 
                                        pageLength = 10
                                      )))
    
  # table of assessment units
    
    AUurl <- paste('https://attains.epa.gov/attains-public/api/assessmentUnits',
                   '?stateCode=', statecode,
                   '&statusCode=A',
                   sep = "")
    
    rawtable <- jsonlite::fromJSON(AUurl, simplifyVector = FALSE,
                                   flatten = TRUE)
    
    rawtibble <- tibble(info = rawtable$items)
    
    if(input$TerritoryName == 'Puerto Rico'){
      
      AUtable <- rawtibble %>% 
        unnest_wider(info) %>% 
        unnest_longer(assessmentUnits) %>% 
        unnest_wider(assessmentUnits) %>% 
        hoist(useClass,
              'useClassName') %>%
        unnest_wider(monitoringStations,
                     names_sep = '_') %>%
        unnest_wider(monitoringStations_1) %>% 
        select(assessmentUnitIdentifier, 
               assessmentUnitName,
               locationDescriptionText,
               useClassName,
               monitoringOrganizationIdentifier,
               monitoringLocationIdentifier) %>% 
        
        rename(`AU ID` = assessmentUnitIdentifier,
               `AU Name` = assessmentUnitName,
               `Location Description` = locationDescriptionText,
               `Use Class` = useClassName,
               `Monitoring Org ID` = monitoringOrganizationIdentifier,
               `Monitoring Site ID` = monitoringLocationIdentifier)
      
    } else {
      
      AUtable <- rawtibble %>% 
        unnest_wider(info) %>% 
        unnest_longer(assessmentUnits) %>% 
        unnest_wider(assessmentUnits) %>% 
        hoist(useClass,
              'useClassName') %>%
        
        select(assessmentUnitIdentifier, 
               assessmentUnitName,
               locationDescriptionText,
               useClassName)%>% 
        rename(`AU ID` = assessmentUnitIdentifier,
               `AU Name` = assessmentUnitName,
               `Location Description` = locationDescriptionText,
               `Use Class` = useClassName)
    }
    
    
    output$AssessmentUnittable <- renderDT(
                                  datatable(
                                    AUtable,
                                    rownames = FALSE,
                                    editable = 'cell',
                                    filter = 'top',
                                    options = list(
                                      autoWidth = TRUE,
                                      columnDefs = list(list(# centers column names
                                        className = 'dt-center',
                                        targets = '_all')),
                                      pageLength = 10
                                    )))
    
    # COMBINE Monitoring Sites Table and Assessment Unit Table by monitoring location ID
    
    
    # ATTAINS GeoService data
    PRurl <- 'https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/2'
    # 
    # # coral reef data layer
    COurl <- 'https://services.arcgis.com/cJ9YHowT8TU7DUyn/arcgis/rest/services/Coral_Reefs_in_Puerto_Rico_VIEW/FeatureServer'
    # 
    # create map
    output$GISmap <- renderLeaflet({

      leaflet() %>%
    setView(lng = long,
            lat = lata,
            zoom = zooms) %>%
    addEsriBasemapLayer(esriBasemapLayers$Topographic) %>%

    # ATTAINS GeoServices
    addEsriFeatureLayer(
      url = PRurl,

      # specific options from "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/2"
      labelProperty = 'assessmentunitidentifier',
      labelOptions = labelOptions(
        interactive = TRUE,
        textsize = '20pCx',
        style = list(
          textsize = '15px'
          # font
        )
      ),

      # border color, weight, opacity
      color = 'white',
      weight = 1,
      opacity = .75,

      # fill color, opacity,
      fillColor = '#0071bc',
      fillOpacity = .50,
      options = featureLayerOptions(
        where = paste0("state = ", statecodeGIS)),
      # where = "state = 'PR'"),

      useServiceSymbology = TRUE,
      group = 'Assessment Units') %>%

        # add coral reef layer
        addEsriFeatureLayer(
          url = COurl,
          group = 'Coral Reefs') %>%

        # monitoring location
        addCircles(
          data = MonitoringsitesFILT,
          radius = 2,
          color = 'black',
          group = 'Monitoring Sites',
          label = paste(
            '<b>Organization: </b>', MonitoringsitesFILT$`Organization Name`, "<br>",
            '<b>Site Name: </b>', MonitoringsitesFILT$`Monitoring Site Name`, "<br>",
            '<b>Site ID: </b>', MonitoringsitesFILT$`Monitoring Site ID`, "<br>") %>%
            lapply(HTML)) %>%

        # layer toggle
        addLayersControl(
          overlayGroups = c('Assessment Units', 'Monitoring Sites', 'Coral Reefs'),
          options = layersControlOptions(collapsed = FALSE)
        )

    })
    
  })
#####  
  
# create simplified ATTAINS table
#####
  
  observeEvent(input$ATTAINSdwnld, {
    
    # conditional statement for territory name
    if(input$TerritoryName == 'Puerto Rico'){
      
      OrgID <- 'PR_LAKES'

    } else{
      
      OrgID <- 'USVIST'
    }
    
    
    
    listingquery = paste("https://attains.epa.gov/attains-public/api/assessments?organizationId=", OrgID,
                         sep = "")
    
    listing <- jsonlite::fromJSON(listingquery, simplifyVector = FALSE)
    
    
    rawtibble <- tibble(info = listing$items)
    
    
    # the data is nested, so it has to be extracted
    ATTAINSdata <- rawtibble %>% 
      unnest_wider(info) %>% 
      unnest_longer(assessments) %>%
      select(organizationName, assessments, reportStatusCode) %>% 
      hoist(assessments,
            'assessmentUnitIdentifier',
            'epaIRCategory',
            'overallStatus') %>% 
      unnest_wider(assessments)
    
    ATTAINSuse <- ATTAINSdata %>% 
      unnest_longer(useAttainments) %>%
      unnest_longer(useAttainments) %>%
      filter(useAttainments_id == 'useName') %>%
      group_by(assessmentUnitIdentifier) %>% 
      summarise(useAttainments = str_c(useAttainments, collapse = '; '))
    
    ATTAINSparams <- ATTAINSdata %>% 
      unnest_longer(parameters) %>%
      unnest_longer(parameters) %>%
      filter(parameters_id == 'parameterName') %>%
      group_by(assessmentUnitIdentifier) %>% 
      summarise(parameters = str_c(parameters, collapse = '; '))
    
    
    ATTAINSdataSUB <- left_join(ATTAINSdata, ATTAINSuse,
                                by = join_by(assessmentUnitIdentifier)) %>% 
      left_join(ATTAINSparams,
                by = join_by(assessmentUnitIdentifier)) %>% 
      select(assessmentUnitIdentifier,
             epaIRCategory,
             overallStatus,
             useAttainments.y,
             parameters.y,
             cycleLastAssessedText) %>% 
      rename(`Cycle Last Assessed` = 'cycleLastAssessedText',
             `AU ID` = 'assessmentUnitIdentifier',
             `EPA IR Category` = 'epaIRCategory',
             `Overall Status` = 'overallStatus',
             `Designated Uses` = 'useAttainments.y',
             `Parameters` = 'parameters.y')
    
    output$ATTAINStable <- renderDT(
                              datatable(
                                ATTAINSdataSUB,
                                rownames = FALSE,
                                editable = 'cell',
                                filter = 'top',
                                options = list(
                                  autoWidth = TRUE,
                                  columnDefs = list(list(# centers column names
                                    className = 'dt-center', 
                                    targets = '_all')), 
                                  pageLength = 10
                                )))
    
    
  })
#####

# make assessment!
#####  
  observeEvent(input$AssessmentsBtn, {
    
    # Assessment function!
    # sort by parameter and plot with correct wq criterion
    
    # assessment unit timeseries plots for all parameters
    ass.plot.f <- function(df, station, crit, startDateLo, startDateHi) {
      
      # geospatially join all monitoring locations to an assessment unit
      # plot parameters for each assessment unit (facet wrap)
      # color code samples to monitoring stations
      
      df.x <- df %>% 
        filter(MonitoringLocationIdentifier %in% station) %>% # filter by station
        # filter(ResultMeasureValue >= 0) %>%    # remove values below zero - THERE ARE VALID TEMPERATURE MEASUREMENTS BELOW ZERO
        filter(!CharacteristicName == 'E. coli (MPN/100ml)')
      
      
      # sort wq criteria
      df.crit <- crit %>% 
        filter(CharacteristicName %in% unique(df.x$CharacteristicName))
      
      # create ggplot
      df.p <- ggplot(df.x, aes(x = ActivityStartDate, y = ResultMeasureValue, color = MonitoringLocationIdentifier)) +
        geom_point(size = 5, alpha = .75) +
        facet_wrap( ~ CharacteristicName, scales = 'free',
                    strip.position = 'left') +
        
        # add criteria line
        geom_hline(data = df.crit, aes(yintercept = ResultMeasureValue, color = '#C52E19'),
                   linewidth = 2, linetype = 1, alpha = .75) +
        
        theme(axis.title = element_text(size = 20),
              axis.text.x = element_text(size = 18),
              axis.text.y = element_text(size = 18),
              strip.text = element_text(size = 20, margin = unit(rep(8, 6), 'pt')), # facet label text
              strip.background = element_blank(),
              strip.placement = 'outside',
              legend.position = 'bottom',
              legend.title = element_blank(),
              legend.text = element_text(size = 15),
              panel.spacing = unit(2, 'lines'))  +
        
        
        xlab(NULL) + # x axis
        ylab(NULL) + # y axis
        
        # x axis scaling
        scale_x_date(breaks = seq(as.Date(startDateLo), as.Date(startDateHi), by = '3 months'), 
                     date_labels = '%b', date_minor_breaks = '1 month',
                     limits = as.Date(c(startDateLo, startDateHi), format = '%Y-%m-%d')) +
        
        scale_color_manual(values = c('#C52E19', wes_palette(n = length(station), name = 'Moonrise2', type = 'continuous')),
                           labels = c('Criteria', station))

      return(df.p)}
    
    
    output$ass.plot <- ass.plot.f(df = WQPdata_fine(), 
                                  crit = wqsfine(),
                                  # station = c('GRAS', 'GRASFD', 'GRASW'), 
                                  startDateLo = input$dateRange[1],
                                  startDateHi = input$dateRange[2])
    
  })
  
  
  
})
#####