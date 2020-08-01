##### Global: options #####
Production = T 
options(scipen = 1000, expressions = 10000)
appVersion = "v2.0"
appName = "UK COVID-19 Tracker"
appLongName = "UK COVID-19 Tracker"
lastUpdate = Sys.Date()

loader <- tagList(
  waiter::spin_loaders(42),
  br(),
  h3("Loading data")
)

jsToggleFS <- 'shinyjs.toggleFullScreen = function() {
var element = document.documentElement,
enterFS = element.requestFullscreen || element.msRequestFullscreen || element.mozRequestFullScreen || element.webkitRequestFullscreen,
exitFS = document.exitFullscreen || document.msExitFullscreen || document.mozCancelFullScreen || document.webkitExitFullscreen;
if (!document.fullscreenElement && !document.msFullscreenElement && !document.mozFullScreenElement && !document.webkitFullscreenElement) {
enterFS.call(element);
} else {
exitFS.call(document);
}
}'

source("appFiles/packageLoad.R")
source("appFiles/dataLoad.R")
source("appFiles/CSS.R", local = TRUE)
source("appFiles/dashboardPage.R", local = TRUE)

pcases <- read.csv("Dailycases")
deaths <- read.csv("Deaths")
corona_local <- read.csv("data/corona_local.csv")
r_rate <- read.csv("data/uk_data1.csv")

#Make icons for UK map
library(leaflet)
MapIcons <- iconList(
  virus = makeIcon("https://i.ibb.co/pKvycCW/virus.png", "https://i.ibb.co/ncH3QPV/shadow.png", 30, 30),
  recovery = makeIcon("https://i.ibb.co/VYJT0WK/Recovery.png", "https://i.ibb.co/ncH3QPV/shadow.png", 18, 18)
)
r_rate$type <- factor(ifelse(r_rate$max.growth_rate > 0.01, "virus", "recovery"),c("recovery", "virus"))
#define legend
html_legend <- "<img src='https://i.ibb.co/pKvycCW/virus.png', style = 'width:30px; height:30px;'> Warning regions <br/> 
                &nbsp <img src='https://i.ibb.co/VYJT0WK/Recovery.png', style = 'width:20px; height:20px;'> &nbsp Safer regions"



#define dataframeTotal for server
dataframeTotal <- coronavirus %>% 
  dplyr::group_by(countryName) %>%
  slice(n()) %>%
  ungroup() %>%
  dplyr::mutate(Unrecovered = Confirmed - ifelse(is.na(Recovered), 0, Recovered) - ifelse(is.na(Deaths), 0, Deaths)) %>%
  dplyr::arrange(-Confirmed) %>%
  dplyr::ungroup() %>%
  select(-c(date,region,lat,lon))

##### User interface #####
ui <- tagList( # dependencies
  use_waiter(),
  useSweetAlert(),
  useShinyjs(),
  extendShinyjs(text = jsToggleFS),
  waiter::waiter_show_on_load(loader, color = "#000"),
# shows before anything else
  ##### CSS and style functions #####
  CSS, #CSS.R
  # Loading message
  argonDash::argonDashPage(
    title = appLongName,
    header = argonDash::argonDashHeader(
      gradient = T,
      color = NULL,
      top_padding = 2,
      bottom_padding = 0,
      background_img = "corona.png",
      height = 70,
      argonRow(
        argonColumn(width = 8,
                    h4(appLongName, style = 'color:white;
                       text-align:left;
                       vertical-align: middle;
                       font-size:40px;')
                    ),
        argonColumn(
          width = 4,
          h6(HTML(paste0("TEAM OF GOOD PEOPLE")), style = 'color:white;
                                  text-align: right;
                                  font-size:15px;
                                  margin-bottom: 0em'),
          h6(HTML(paste0("UoSheff Data Science")), style = 'color:white;text-align: right;font-size:15px;')
        )
                    )
      
      
      ),
    sidebar = NULL,
    body = argonDashBody(
      tags$head( tags$meta(name = "viewport", content = "width=1600"),uiOutput("body")),
      tags$br(),
           dashboardUI
    )
  )
  )

##### server #####
server <- function(input, output, session) {
  printLogJs = function(x, ...) {
    logjs(x)
    T
  }
  # addHandler(printLogJs)
  if (!Production) options(shiny.error = recover)
  options(shiny.sanitize.errors = TRUE, width = 160)
  
  session$onSessionEnded(function() {
    stopApp()
    # q("no")
  })
  source("appFiles/dashboardServer.R", local = TRUE)
  # Hide the loading message when the rest of the server function has executed
  waiter_hide() # will hide *on_load waiter
}

# Run the application
shinyApp(ui = ui, server = server)