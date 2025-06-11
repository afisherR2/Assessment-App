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

# Define UI for application
shinyUI(
  fluidPage(
    
    
    theme = shinytheme("yeti"),
    tags$head(tags$link(rel = "stylesheet",
                        type = "text/css", href = "style.css"),
              tags$style("* {font-family:'Merriweather' !important;}")), # set ALL font to Merriweather
    
  
    ##### Header
    #####
    HTML
    (" <header class='masthead clearfix' role='banner'>
	   <img alt='' class='site-logo' src='https://www.epa.gov/sites/all/themes/epa/logo.png'>
       <div class='site-name-and-slogan'>
       <h1 class='site-name'><a href='https://www.epa.gov/' rel='home' title='Go to the home page'><span>US EPA</span></a></h1>
       <div class='site-slogan'>
       United States Environmental Protection Agency
       </div>
       </div>
       <div class='region-header'>
       <div class='block-epa-core-gsa-epa-search' id='block-epa-core-gsa-epa-search'> 
	   </div>
       </div>
	   </header>
       <nav class='nav main-nav clearfix' role='navigation'>
       <div class='nav__inner'>
       <h2 class='element-invisible'>Main menu</h2>
       <ul class='menu' role='menu'>
       <li class='expanded active-trail menu-item' role='presentation'>
       <a class='active-trail menu-link' href='https://www.epa.gov/environmental-topics' role='menuitem' title='View links to the most popular pages for each of EPA&#8217s top environmental topics.'>Environmental Topics</a></li>
       <li class='menu-item' role='presentation'>
       <a class='menu-link' href='https://www.epa.gov/laws-regulations' role='menuitem' title='View links to regulatory information by topic and sector, and to top pages about environmental laws, regulations, policies, compliance, and enforcement.'>Laws &amp; Regulations</a></li>
       <li class='expanded menu-item' role='presentation'>
       <a class='menu-link' href='https://www.epa.gov/aboutepa' role='menuitem' title='Learn more about our mission, organization, and locations.'>About EPA</a></li>
       </ul>
       </div>
       </nav>
       <div class='mobile-nav' id='mobile-nav'>
       <div class='mobile-bar clearfix'>
       <label class='menu-button' for='mobile-nav-toggle'>Menu</label>
       </div><input checked id='mobile-nav-toggle' type='checkbox'>
       <div class='mobile-links element-hidden' id='mobile-links' style='height:2404px;'>
       <ul class='mobile-menu'>
       <li class='expanded menu-item'><a class='menu-link' href='https://www.epa.gov/environmental-topics' tabindex='-1' title='View links to the most popular pages for each of EPA&#8217s top environmental topics.'>Environmental Topics</a></li>
       <li class='menu-item'><a class='menu-link' href='https://www.epa.gov/laws-regulations' tabindex='-1' title='View links to regulatory information by topic and sector, and to top pages about environmental laws, regulations, policies, compliance, and enforcement.'>Laws &amp; Regulations</a></li>
       <li class='expanded menu-item'><a class='menu-link' href='https://www.epa.gov/aboutepa' tabindex='-1' title='Learn more about our mission, organization, and locations.'>About EPA</a></li>
       </ul>
       </div>
       </div>
       <section class='main-content clearfix' id='main-content' lang='en' role='main' tabindex='-1'>
       <div class='region-preface clearfix'>
       <div class='block-views-revision-hublinks-block' id='block-views-revision-hublinks-block'>
       <div class='view view-revision-hublinks view-id-revision_hublinks'>
       <ul class='menu pipeline'>
       </ul>
       </div>
       </div>
       <div class='block block-pane block-pane-epa-web-area-connect' id='block-pane-epa-web-area-connect'>
       <ul class='menu utility-menu'>
       </ul>
       </div>
       </div>
       <div class='main-column clearfix'><!--googleon:all-->
	   <h1  class='page-title'>Puerto Rico Water Quality Assessment App BETA</h1>
       <div class='panel-pane pane-node-content'>
       <div class='pane-content'>
       <div class='node node-page clearfix view-mode-full'>   
	   </div>
	   </div>
	   </div>

	"),
    #####
	
  # Insert RShiny App here
	hr(),
	
	
	# shinyFeedback
	useShinyFeedback(),
	
	# add progress spinner
	add_busy_spinner(spin = 'hollow-dots',
	                 color = '#0071bc',
	                 position = 'top-right',
	                 onstart = TRUE,
	                 margins = c('50vh', '50vw')), # CSS code to display spinner in middle of screen
	
	
	# Collapsible Overview and Instructions
	bsCollapse(id = 'Overview',
	           
	           
	           bsCollapsePanel(title = h2(strong('Overview')), value = 'Overview',
	                           p('The Puerto Rico Water Quality Assessment App is an R Shiny app developed to support
	                             Clean Water Act 303(d) assessment of waters in Puerto Rico.
	                             The app pulls water quality standards from the EPA Office of Science and Technlogy',
	                             tags$a(href = 'https://www.epa.gov/wqs-tech/state-specific-water-quality-standards-effective-under-clean-water-act-cwa', 
	                                    'Criteria Search Tool', target = 'blank'), 
	                             ', water quality sample data and monitoring locations from the ',
	                             tags$a(href = 'https://www.waterqualitydata.us/', 
	                                    'Water Quality Portal', target = 'blank'),
	                             ', and assement unit GIS and assessment information from ',
	                             tags$a(href = 'https://www.epa.gov/waterdata/attains', 
	                                    'ATTAINS', target = 'blank')),
          
          p('No warranty, express or implied, is made by EPA or any other agency of 
          the U.S. Government regarding the accuracy, completeness, or currency 
          of this information.'),
          
          p('For questions, feedback, bugs, or modification suggestions, 
            contact Adam Fisher at', tags$a(href = 'mailto:fisher.adam@epa.gov', 
                                            'fisher.adam@epa.gov', target = 'blank', '.'))),
          
          bsCollapsePanel(title = h2(strong('Instructions')), value = 'Instructions',
                          p('To start, enter a date range in the ', strong('Date Range to Assess'), ' field
          and select the Get Started button. Follow the below tabs from left to right. More instructions coming soon.'))), 
	
	
	
# Territory Name
	fluidRow(
	  
	  hr(),
	  br(),
	  br(),
	  
	  column(5, selectInput('TerritoryName',
	                     label = h2('Territory Name'),
	                     choices = c('Puerto Rico',
	                                 'U.S. Virgin Islands'),
	                     selected = 'Puerto Rico',
	                     multiple = FALSE)),
	  
# Start date/End data
	  column(4, dateRangeInput('dateRange', 
	                          label = h2('Date Range to Assess'),
	                          start = '2021-10-01',
	                          end = '2023-09-30'))
	         

	  ),
    br(),

	
# First button  
	fluidRow(
	  column(2, offset = 10,
	         uiOutput('nextBtn'))),
  br(),
  br(),

## Main Panel
  fluidRow(
    withMathJax(
      uiOutput('tabsPanels')
    )
  ),
	
	# Footer
	#####
	HTML("

	  </section>
		<footer class='main-footer clearfix' role='contentinfo'>
		</footer>
	")
	#####
    
    
  )
)