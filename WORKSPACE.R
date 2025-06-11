## WORKSPACE
## This app is written to assisted in CWA 303(d) assessment of waters
## Adam Fisher
## 10/01/2023



# Load libraries
library(readxl)
library(httr)
library(tidyverse)
library(dataRetrieval)
library(wesanderson)


# NEED clarification on "Pesticides (organochlorides)" from assessment methodology
# NO WQS for NH3+NH4 as N but still assess for it?? (in methodology)
# specific conductance
# flow
# Oil and Grease



# assess_param <- c('arsenic', 'nickel', 'cadmium', 'chlorides', 'copper', 'pH (acidity/alkalinity)', 'dissolved oxygen',
#                   'selenium', 'enterococci', 'temperature', 'lead', 'total nitrogen', 'total phosphorus', 'mercury',
#                   'turbidity', 'zinc', 'nitrate and nitrite (expressed as n)', 'chromium iii', 'chromium vi (hex)')


params <- read.csv('www/PR_standards_wqp_xwalk.csv')

# download criteria form EPA OST Criteria Search Tool
GET('https://cfpub.epa.gov/wqsits/wqcsearch/criteria-search-tool-data.xlsx', 
    write_disk(tf <- tempfile(fileext = ".xlsx")))

df <- read_excel(tf,
                 skip = 209)

wqsfine <- df %>% 
  filter(ENTITY_NAME == 'Puerto Rico') %>% 
  
  select(c(ENTITY_NAME, POLLUTANT_NAME, CRITERION_VALUE, UNIT_NAME,
           CRITERIATYPEAQUAHUMHLTH, CRITERIATYPEFRESHSALTWATER,
           USE_CLASS_NAME_LOCATION_ETC, EFFECTIVE_DATE, LAST_ENTRY_IN_DB)) %>% 
  
  # rename columns
  rename(`Territory Name` = ENTITY_NAME,
         `Parameter` = POLLUTANT_NAME,
         `Criterion Value` = CRITERION_VALUE,
         `Unit` = UNIT_NAME,
         `Criterion Type` = CRITERIATYPEAQUAHUMHLTH,
         `Criterion Water Type` = CRITERIATYPEFRESHSALTWATER,
         `Criterion Application` = USE_CLASS_NAME_LOCATION_ETC,
         `Effective Date` = EFFECTIVE_DATE,
         `Updated` = LAST_ENTRY_IN_DB) %>% 
  
  # filter for only assessed parameters
  filter(Parameter %in% params$cst) %>% 
  arrange(Parameter) %>% 
  
  # join params csv
    left_join(params,
            by = join_by(Parameter == cst)) %>% 
  
  # use equation if wqs is "see equation" - set to NA fo now
  mutate(`Criterion Value` = case_when(`Criterion Value` == 'See Equation' ~ NA,
                                       .default = `Criterion Value`)) %>% 
  select(-wqp, -wqp.alt, -equation)



# names(wqs)
# str(wqs)
# head(df)
unique(wqs$STD_POLLUTANT_NAME) %>% sort()


WQPdata <- readWQPdata(statecode = 'PR',
                       dataProfile = 'narrowResult',
                       startDateLo = as.Date('2021-10-01'),
                       startDateHi= as.Date('2023-09-30'))


# filter wqp results by only those assessed for - xwalk and alternates
WQPdata_fine <- WQPdata %>% 
  
  filter(CharacteristicName %in% c('Turbidity', 'Phosphorus', 'Temperature, water','Nitrogen')) %>% 
  # filter(CharacteristicName %in% params$wqp | CharacteristicName %in% params$wqp.alt) %>% 
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

  # mutate(`Sample Date` = as.Date(`Sample Date`),
  #        Value = as.numeric(Value))
# str(WQPTEST_filt$Value)
# unique(WQPTEST_filt$Parameter) %>% sort()

# class(WQPTEST_filt$`Sample Date`)
# timeseries plot for each parameter color coded by ORG ID
# library(ggplot2)
# library(scales)

WQPTEST_filt %>% 
  filter(Parameter == 'Enterococcus') %>% 
  ggplot(aes(x = Value, y = `Monitoring Location`)) +
  geom_boxplot() +
  geom_vline(xintercept = wqs_sub$CRITERION_VALUE) +
  
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))
  
  geom_line() +
  geom_point()
  
unique(WQPTEST_filt$Parameter)

facet_wrap(~Parameter, scales = 'free')
  
  





## get wqp sites

params <- read.csv('www/PR_standards_wqp_xwalk.csv')

sites <- whatWQPsites(
  statecode = 'PR',
  characteristicName = params$wqp,
  startDateLo = as.Date('2022-01-01'),
  startDateHi= as.Date('2023-12-31'))

# unique(sites$MonitoringLocationTypeName)

# site types of interest for VI
sites_types <- c('Stream', 'Estuary', 'Riverine Impoundment', 'Ocean', 'Wetland Palustrine-Forested', 
                 'BEACH Program Site-Ocean')


# site types of interest for PR
sites_types <- c('Stream', 'Estuary', 'Spring', 'Lake, Reservoir, Impoundment', 'Stream: Canal',
                 'Ocean: Coastal', 'River/Stream', 'Ocean', 'Lake', 'BEACH Program Site-Ocean',
                 'Reservoir')

sitesFilt <- sites %>% 
  select(OrganizationIdentifier, OrganizationFormalName, MonitoringLocationIdentifier,
         MonitoringLocationDescriptionText, MonitoringLocationName, MonitoringLocationTypeName,
         LatitudeMeasure, LongitudeMeasure) %>% 
  # filter(MonitoringLocationIdentifier %in% WQPTEST_filt$`Monitoring Location`) %>%
  filter(MonitoringLocationTypeName %in% sites_types) %>%  # filter for specific site types
  mutate(longitude = as.numeric(LongitudeMeasure), # change names and to numeric
         latitude = as.numeric(LatitudeMeasure),
         .keep = 'unused')

# str(sitesFilt)
# unique(sitesFIlT$OrganizationFormalName)


# plot monitoring stations
library(leaflet)
library(leaflet.esri)
library(htmltools)
library(sf)

statecode <- "'PR'"

# url for assessment unit shapefile
PRurl <- 'https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/2'

# COurl <- 'https://services.arcgis.com/cJ9YHowT8TU7DUyn/arcgis/rest/services/Coral_Reefs_in_Puerto_Rico_VIEW/FeatureServer'

# leaflet() %>% 
#   setView(lng = -64.7822,
#           lat = 18.0629,
#           zoom = 10) %>% 
#   addEsriBasemapLayer(esriBasemapLayers$Topographic) %>% 
#   
#   # ATTAINS GeoServices
#   addEsriFeatureLayer(
#     url = PRurl,
#     
#     # specific options from "https://gispub.epa.gov/arcgis/rest/services/OW/ATTAINS_Assessment/MapServer/2"
#     labelProperty = 'assessmentunitidentifier',
#     labelOptions = labelOptions(
#       interactive = TRUE,
#       textsize = '20pCx',
#       style = list(
#         textsize = '15px'
#         # font
#       )
#     ),
#     
#     # border color, weight, opacity
#     color = 'white',
#     weight = 1,
#     opacity = .75,
#     
#     # fill color, opacity,
#     fillColor = '#0071bc',
#     fillOpacity = .50,
#     options = featureLayerOptions(
#       where = paste0("state = ", statecode)),
#     useServiceSymbology = TRUE,
#     group = 'Assessment Units') %>% 
#   
#   # monitoring location
#   addCircles(
#     data = sitesFilt,
#     radius = 2,
#     color = 'black',
#     group = 'Monitoring Sites',
#     label = paste(
#       '<b>Organization: </b>', sitesFilt$OrganizationFormalName, "<br>",
#       '<b>Site Name: </b>', sitesFilt$MonitoringLocationName, "<br>",
#       '<b>Site ID: </b>', sitesFilt$MonitoringLocationIdentifier, "<br>") %>% 
#   lapply(HTML)) %>% 
#   
#   # layer toggle
#   addLayersControl(
#     overlayGroups = c('Assessment Units', 'Monitoring Sites'),
#                       options = layersControlOptions(collapsed = FALSE)
#   )
  


# create sf object from monitoring sites for spatial join to assessment units: Table in line 338
# to be used on Get GIS tab of shiny app, and then to be used for doing the assessments!

# build url
url <- parse_url('https://gispub.epa.gov/arcgis/rest/services')
url$path <- paste(url$path, 'OW/ATTAINS_Assessment/MapServer/2/query',
                  sep = '/')

# create query for url
url$query <- list(where = paste0("state = ", statecode),
                  outFields = '*',
                  returnGeometry = 'true',
                  f = 'geojson')

request <- build_url(url)

# read in url and query
aus <- st_read(request)


# create sf object for monitoring sites

msites <- st_as_sf(sitesFilt, 
                   coords = c('longitude', 'latitude'), 
                   remove = FALSE,
                   crs = st_crs(aus)) # SET coordinate systems to be the SAME!


# build map
leaflet(aus) %>% 
  
  addPolygons(
    color = 'blue',
    group = 'Assessment Units',
    label = paste(
      '<b>AU ID: </b>', aus$assessmentunitidentifier, "<br>",
      '<b>AU Name: </b>', aus$assessmentunitname, "<br>",
      '<b>IR Category: </b>', aus$ircategory, "<br>") %>% 
      lapply(HTML)) %>% 
  
  setView(lng = -66.4131,
          lat = 18.2298,
          zoom = 8) %>% 
  
  addEsriBasemapLayer(esriBasemapLayers$Topographic) %>% 
  
  # monitoring location
  addCircles(
    data = msites,
    radius = 2,
    color = 'black',
    group = 'Monitoring Sites',
    label = paste(
      '<b>Organization: </b>', sitesFilt$OrganizationFormalName, "<br>",
      '<b>Site Name: </b>', sitesFilt$MonitoringLocationName, "<br>",
      '<b>Site ID: </b>', sitesFilt$MonitoringLocationIdentifier, "<br>") %>% 
      lapply(HTML)) %>% 
  
  
  
  # layer toggle
  addLayersControl(
    overlayGroups = c('Assessment Units', 'Monitoring Sites'),
    options = layersControlOptions(collapsed = FALSE)
  )
  
  
# spatial join
aus_msites <- st_make_valid(aus) %>%  # make valid geometry?
  st_join(msites)

Multi_aus_msites_LIM <- st_drop_geometry(aus_msites) %>% # drop geometry
  
  # select subset of data
  select(assessmentunitidentifier,
         MonitoringLocationIdentifier) %>% 
  group_by(assessmentunitidentifier) %>% 
  
  # tally duplicate assessment unit identifiers - MUST group_by first
  mutate(countUp = paste0('MultiAU_', row_number())) %>% 
  
    tibble() %>% 
  
  # pivot wider so monitoring locations are in multiple columns for a single assessment unit
  pivot_wider(names_from = countUp,
              values_from = MonitoringLocationIdentifier)
  
  # rename columns automatically
# colnames(Multi_aus_msites_LIM)
# ncol(Multi_aus_msites_LIM)

# create new column names list for each of the monitoring sites - automatically
aus_msites_rename <- Multi_aus_msites_LIM %>% 
  rename_with(
    ~ paste0('Monitoring Site ', 1:(ncol(Multi_aus_msites_LIM) -1),
             recycle0 = TRUE), 
    .cols = starts_with('Multi')) # only rename colunms that start with 'Multi'






# read in use class csv
useclass <- read.csv('www/PR_Class.csv')

useclass <- useclass %>% 
  select(ASSESSMENT_UNIT_ID,
         Class) %>% 
  rename('assessmentunitidentifier' = ASSESSMENT_UNIT_ID)




# table of assessment units with use classes and monitoring sites

aus_msites_uclase <- aus %>% 
  select(assessmentunitidentifier, 
         assessmentunitname
         # locationdescriptiontext,
         # useclassname,
         # monitoringorganizationidentifier,
         # monitoringlocationidentifier
         ) %>% 
  
  # join use classes
  inner_join(useclass,
             by = join_by('assessmentunitidentifier')) %>% 
  
  # join monitoring site columns to aus
  inner_join(aus_msites_rename,
             by = join_by("assessmentunitidentifier")) %>% 
  
  # rename columns
  rename(`AU ID` = assessmentunitidentifier,
         `AU Name` = assessmentunitname)

  






# assessment unit table
stateCode <- 'VI'

AUurl <- paste('https://attains.epa.gov/attains-public/api/assessmentUnits',
               '?stateCode=', stateCode,
               '&statusCode=A',
               sep = "")

rawtable <- jsonlite::fromJSON(AUurl, simplifyVector = FALSE,
                              flatten = TRUE)

rawtibble <- tibble(info = rawtable$items)

AUtable <- rawtibble %>% 
  unnest_wider(info) %>% 
  unnest_longer(assessmentUnits) %>% 
  unnest_wider(assessmentUnits) %>% 
  hoist(useClass,
        'useClassName') %>%
  # unnest_wider(monitoringStations,
  #              names_sep = '_') %>%
  # unnest_wider(monitoringStations_1) %>%
  select(assessmentUnitIdentifier, 
         assessmentUnitName,
         locationDescriptionText,
         useClassName,
         monitoringOrganizationIdentifier,
         monitoringLocationIdentifier)

  # rename(MonitoringLocationIdentifier = monitoringLocationIdentifier)

# join AU table and monitoring site table
# MasterTable <- AUtable %>% 
#   full_join(y = sitesFilt,
#             by = join_by(MonitoringLocationIdentifier))
# 
# str(sitesFilt$MonitoringLocationIdentifier)
# colSums(is.na(AUtable))
# str_detect(AUtable$MonitoringLocationIdentifier, 'NA') %>% 
#   length()






library(jsonlite)

# download ATTAINS information from previous cycle: parameters, TMDLs, assessment unit information
OrgID = 'PR_LAKES'

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
  rename(`AU ID` = 'assessmentUnitIdentifier',
         `EPA IR Category` = 'epaIRCategory',
         `Overall Status` = 'overallStatus',
         `Designated Uses` = 'useAttainments.y',
         `Parameters` = 'parameters.y',
         `Cycle Last Assessed` = 'cycleLastAssessedText')



# Assessment function!
# sort by parameter and plot with correct wq criterion

# split aus_msites_uclass into two - one for aus with monitoring stations, and one without
aus_wms <- aus_msites_uclase %>% 
  filter(!is.na(`Monitoring Site 1`) == TRUE) %>% 
  # filter(`Monitoring Site 1` %in% c('USGS-50057025', 'USGS-50038100', 'USGS-50139000')) %>% 
  filter(`AU Name` %in% c('RIO YAGUEZ', 'RIO GRANDE DE MANATI', 'RIO GURABO')) %>% 
  rename(`Monitoring Location` = `Monitoring Site 1`)
  
# for aus with monitoring stations,
  
  
#####
# assessment unit timeseries plots for all parameters
# ass.plot.f <- function(df, station, crit, startDateLo, startDateHi) {
  
  # geospatially join all monitoring locations to an assessment unit
  # plot parameters for each assessment unit (facet wrap)
  # color code samples to monitoring stations
  

  df.x <- WQPdata_fine %>% 
    filter(`Monitoring Location` %in% unique(aus_wms$`Monitoring Location`)) %>% 
    
    mutate(Parameter = case_when(Parameter == 'Turbidity' ~ 'turbidity',
                                 Parameter == 'Temperature, water' ~ 'temperature',
                                 Parameter == 'Nitrogen' ~ 'total nitrogen',
                                 Parameter == 'Phosphorus' ~ 'total phosphorus',
                                 .default = Parameter)) %>% 
    
    left_join(aus_wms, by = join_by(`Monitoring Location`))
    # filter(MonitoringLocationIdentifier %in% station) %>% # filter by station
    # filter(ResultMeasureValue >= 0) %>%    # remove values below zero - THERE ARE VALID TEMPERATURE MEASUREMENTS BELOW ZERO
    # filter(!CharacteristicName == 'E. coli (MPN/100ml)')
  
  
  # sort wq criteria
  df.crit <- wqsfine %>%
    filter(Parameter %in% unique(df.x$Parameter)) %>% 
    
   rename(Value = `Criterion Value`)
  
  
  # ggplot for each assessment unit
  for (i in 1:length(unique(df.x$`AU Name`))){
    
    df.y <- df.x %>% 
      filter(`AU Name` == unique(df.x$`AU Name`)[i])
    
    # create ggplot
    print(ggplot(df.y, aes(x = `Sample Date`, y = as.numeric(Value), color = `AU Name`)) +
      geom_point(size = 5, alpha = .75) +
      facet_wrap( ~ Parameter, scales = 'free',
                  strip.position = 'left') +
      
      # add criteria line
      geom_hline(data = df.crit, aes(yintercept = as.numeric(Value), color = '#C52E19'),
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
      scale_x_date(breaks = seq(as.Date('2021-10-01'), as.Date('2023-09-30'), by = '3 months'), 
                   date_labels = '%b', date_minor_breaks = '1 month',
                   limits = as.Date(c('2021-10-01', '2023-09-30'), format = '%Y-%m-%d')))
    
    # scale_color_manual(values = c('#C52E19', wes_palette(n = length(unique(aus_wms$`AU Name`)), name = 'Moonrise2', type = 'continuous')),
    #                    labels = c('Criteria', unique(aus_wms$`AU Name`)))
    
    
    # print(df.p)
  }
  

  
  # return(df.p)}


ass.plot <- ass.plot.f(df = WQPdata_fine, 
                              crit = wqsfine,
                              # station = c('GRAS', 'GRASFD', 'GRASW'), 
                              startDateLo = as.Date('2021-10-01'),
                              startDateHi = as.Date('2023-09-30'))


# for each au, create a plot showing the samples collected and the relevant wqs

# 102 aus with monitoring stations
