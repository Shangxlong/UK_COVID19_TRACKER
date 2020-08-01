results = reactiveValues(
  dataframeTotal = NULL,
  dfDaily = NULL,
  newCases = NULL,
  dataframeTotalOldCases = NULL,
  newCasesDeath = NULL,
  dataframeFinal = NULL,
  newCasesRecovered = NULL,
  dataframeOldCases = NULL,
  modelFit = NULL,
  resultTable = NULL
)
library(coronavirus)

output$dashboard = renderUI({
  argonTabSet(
    id = "analysisSettingsTabs",
    card_wrapper = T,
    horizontal = TRUE,
    circle = F,
    size = "sm",
    width = 12,
    iconList = list(
      icon("home"),
      icon("tachometer-alt"), 
      icon("chart-line")
    ),
    argonTab(
      tabName = "Home",
      active = T,
      argonRow(
        argonColumn(
          width = 12,
          h1("Latest News:",style = 'color:#529f9d;font-size:25px;text-align:left;'),
          a(img(src='second.JPG',width = "100%"), href="https://www.bbc.co.uk/news/uk-53602362"),
          p("Click img to access news",style = 'text-align:center;
             font-size: 10px;color: grey;'),
          h6("Source: Google",style = 'text-align:right;
             font-style: italic;font-weight: bold;'),
          h2("Home visits banned in parts of northern England",style = 'text-align:left;
             font-weight: bold; color:#529f9d;'),
          p("Separate households will not be allowed to meet indoors in Greater Manchester, Leicester, East Lancashire and parts of West Yorkshire from midnight, the government has announced. Health Secretary Matt Hancock said an increasing rate of transmission had been identified in those areas. But pubs, restaurants and some other facilities are to be allowed to reopen from Monday in Leicester - where a local lockdown has been in place since last month - sources have told the BBC.",style = 'text-align:justify;'),
          tags$hr(),
          a(img(src='isolation.JPG',width = "100%"), href="https://www.bbc.co.uk/news/uk-53548893"),
          p("Click img to access news",style = 'text-align:center;
             font-size: 10px;color: grey;'),
          h6("Source: Google",style = 'text-align:right;
             font-style: italic;font-weight: bold;'),
          h2("Travellers returning from Spain need to self-isolate for 14 days",style = 'text-align:left;
             font-weight: bold; color:#529f9d;'),
          p("The new coronavirus travel rule was announced on Saturday evening following a spike in the number of new cases in Spain this week.
            It came into force less than six hours after it was confirmed by the government, and requires travellers returning from all parts of Spain - including the islands of Majorca, Menorca and Ibiza - to provide an address where they will self-isolate for 14 days or risk a fine.
            During those two weeks, people must not go out to work, school, or public areas, or have visitors except for essential support. They should not go out to buy food if they can rely on others.",style = 'text-align:justify;'),
          tags$hr(),
          a(img(src='maskp.jpg',width = "100%"), href="https://www.bbc.co.uk/news/uk-politics-53397617"),
          p("Click img to access news",style = 'text-align:center;
             font-size: 10px;color: grey;'),
          h6("Source: Google",style = 'text-align:right;
             font-style: italic;font-weight: bold;'),
          h2("Face coverings will be compulsory in shops",style = 'text-align:left;
             font-weight: bold;color:#529f9d;'),
          ),
        
        argonColumn(
          width = 12,
          p("Wearing a face covering in shops and supermarkets in England is to become mandatory from 24 July. Those who fail to comply with the new rules will face a fine of up to £100, the government has announced.",style = 'text-align:justify;'),
          tags$hr(),
          a(img(src = 'rules.png', width = "100%"), href="https://www.bbc.co.uk/news/explainers-52530518"),
          p("Click img to access news",style = 'text-align:center;
             font-size: 10px;color: grey;'),
          h6("Source: BBC news", style = 'text-align:right;
             font-style: italic;font-weight: bold;
             '),
          h2("Lockdown new measures",style = 'text-align:left;
             font-weight: bold;color:#529f9d;'),
          p("Places are now open in England: pubs, haircut places, outdoor gyms, children's playgrounds, other outdoor spaces, libraries, community centres, bingo halls, cinemas, museums, galleries, funfairs and theme parks, amusement arcades, outdoor skating rinks, social clubs, model villages, places of worship can open for prayers and services, including weddings with up to 30 guests - subject to social distancing."),
          tags$br(),
          p("This monitor was developed to make the data and key visualizations of COVID-19 trends available to everyone.",style = 'color:grey;font-size:10px;text-align:justify;')
        )
        
          ),
      tags$br(),
      h4("Note:",style = 'color:#d46e82;font-size:15px;text-align:Left;'),
      p("1. The data used in this dashboard taken from GOV.UK. In case of any discrepancy in the numbers please contact with developer.",style = 'color:grey;font-size:12px;text-align:Left;'),
      p(paste0("2. News will be updated once every weekend and dashboard will be updated on daily basis synchronized with GOV.UK. "),style = 'color:grey;font-size:12px;text-align:Left;')
      
    ),
    # analysis setting tab -----
    argonTab(
      tabName = "Dashboard",
      active = F,
      tags$head(tags$style(type = "text/css", "
             #loadmessage {
                           position: fixed;
                           top: 150px;
                           left: 50px;
                           width: 93%;
                           padding: 5px 0px 5px 0px;
                           text-align: center;
                           font-weight: bold;
                           font-size: 100%;
                           color: #000000;
                           background-color: #CCFF66;
                           z-index: 105;
}
  ")),
      argonRow(
        argonColumn(
          width = 12,
          uiOutput("cardUI") %>% withSpinner()
        )
      ),
      tags$hr(),
      argonRow(
        argonColumn(
          width = 12,
          uiOutput("chartUI") %>% withSpinner()
        )
      ),
      conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                       tags$div("Loading Page...",id = "loadmessage")),
      tags$hr(),
      argonRow(
        argonColumn(
          width = 12,
          h2("Latest confirmed cases among upper tier local authority", style = "color:#529f9d;font-size: 25px;"),
          dataTableOutput("countytable") %>% withSpinner()
          
        )
      )
    ),

  argonTab(
    tabName = "Forecasting",
    active = F,
    uiOutput("forecastUI") %>% withSpinner()
  )
  )
  # argonTab(
  #   tabName = "Debug",
  #   active = T,
  #   icon =  icon("wrench"),
  #   tagList(
  #       argonRow(
  #         argonColumn(
  #           width =  12,
  #           argonRow(
  #             argonColumn(
  #               width =  12,
  #               textInput(
  #                 "runRCode",
  #                 paste0("Run R code",ifelse(Production," (disabled in production version)","")),
  #                 width = "100%"
  #               )
  #             )
  #           ),
  #           argonRow(
  #             argonColumn(
  #               width =  12,
  #               actionButton("runRCodeButton","Submit code")
  #             )
  #           )
  #         )
  #       ),
  #       argonRow(
  #         argonColumn(
  #           width =  12,
  #           br(),
  #           verbatimTextOutput("runRCodeOutput")
  #         )
  #       )
  #   )
  # )
})
outputOptions(output, "dashboard", suspendWhenHidden = FALSE)



output$confirmedCount <- renderCountup({
  results$dataframeFinal = coronavirus
  dataframeTotal <- coronavirus %>% 
    dplyr::group_by(countryName) %>%
    slice(n()) %>%
    ungroup() %>%
    dplyr::mutate(Unrecovered = Confirmed - ifelse(is.na(Recovered), 0, Recovered) - ifelse(is.na(Deaths), 0, Deaths)) %>%
    dplyr::arrange(-Confirmed) %>%
    dplyr::ungroup() %>%
    select(-c(date,region,lat,lon))
  # browser()
  results$dataframeTotal = dataframeTotal
  totalConfirmed = sum(results$dataframeTotal$Confirmed,na.rm = T)
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "Confirmed ")
  countup(
    totalConfirmed,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})

output$activeCount <- renderCountup({
  totalUnrecovered = sum(results$dataframeTotal$Unrecovered,na.rm = T)
  totalConfirmed = sum(results$dataframeTotal$Confirmed,na.rm = T)
  activeCasesPer = round(((totalUnrecovered/totalConfirmed)*100),1)
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "Active ",
               suffix = paste0(" (",activeCasesPer,"%)")
  )
  countup(
    totalUnrecovered,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})

output$recoveredCount <- renderCountup({
  totalRecovered = sum(results$dataframeTotal$Recovered,na.rm = T)
  totalConfirmed = sum(results$dataframeTotal$Confirmed,na.rm = T)
  Unrecovered = results$dataframeTotal$Unrecovered
  totalRecoveredPer = round(((totalRecovered/totalConfirmed)*100),1)
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "Recovered ",
               suffix = paste0(" (",totalRecoveredPer,"%)")
               )
  countup(
    totalRecovered,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})
output$deathCount <- renderCountup({
  totalDeath = sum(results$dataframeTotal$Deaths,na.rm = T)
  totalConfirmed = sum(results$dataframeTotal$Confirmed,na.rm = T)
  totalDeathPer = round(((totalDeath/totalConfirmed)*100),1)
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "Deaths ",
               suffix = paste0(" (",totalDeathPer,"%)"))
  countup(
    totalDeath,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})
output$countryCount <- renderCountup({
  x = results$dataframeTotal %>%
      filter(Confirmed > 0) %>%
      select(countryName) %>%
      unique() %>%                                                                              nrow()
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "Total countries affected: "
              )
  countup(
    x,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})

output$cardUI = renderUI({
  df_daily <- coronavirus %>% 
    dplyr::group_by(date) %>%
    dplyr::summarise(totalConfirmed = sum(Confirmed, na.rm = TRUE),
                     totalRecovered = sum(Recovered,na.rm = TRUE),
                     totalDeaths = sum(Deaths,na.rm = T)
    ) %>%
    dplyr::arrange(date) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(totalUnrecovered = totalConfirmed - totalRecovered - totalDeaths) 
  results$dfDaily = df_daily
  
  max_date <- as.Date(max(coronavirus$date)) 
  newCases = coronavirus %>% 
    dplyr::filter(date == max_date | date == max_date - 1) %>%
    dplyr::group_by(countryName) %>%
    mutate(ConfirmedNew = Confirmed - shift(Confirmed,1)) %>% 
    mutate(RecoveredNew = Recovered - shift(Recovered,1)) %>%
    mutate(DeathsNew = Deaths - shift(Deaths,1)) %>%
    slice(n()) %>%
    ungroup() %>%
    select(countryName,ConfirmedNew,RecoveredNew,DeathsNew)
  
  results$newCases = newCases
  dataframeTotalOldCases = coronavirus %>%
    dplyr::filter(date == max_date - 1) %>%
    dplyr::mutate(Unrecovered = Confirmed - Recovered - Deaths) %>%
    summarise(totalConfirmed = sum(Confirmed,na.rm = T),
              totalDeath = sum(Deaths,na.rm = T),
              totalRecovered = sum(Recovered,na.rm = T),
              totalUnrecovered = sum(Unrecovered,na.rm = T)
    )
  results$dataframeTotalOldCases = dataframeTotalOldCases
  dataframeOldCases = coronavirus %>%
    dplyr::filter(date == max_date - 1) %>%
    dplyr::mutate(Unrecovered = Confirmed - Recovered - Deaths)
  results$dataframeOldCases = dataframeOldCases
  tagList(
    argonRow(
      argonColumn(
        width = 3,
        argonInfoCard(
          value = "Total Cases\n302,301",
          icon = icon("users"),
          icon_background = "default",
          hover_lift = F,
          shadow = T,
          background_color = "warning",
          gradient = T,
          width = 12
        ),
        h6(paste0("Yesterday: ",
                  "301,455"
                  ), 
                  style = 'text-align:center;
                           font-size:15px;')
      ),
      argonColumn(
        width = 3,
        argonInfoCard(
          value = "Total deaths number\n45,999",
          icon = icon("heart"),
          icon_background = "default",
          hover_lift = F,
          shadow = T,
          background_color = "danger",
          gradient = T,
          width = 12
        ),
        h6("Yesterday: ","45,961",
           style = 'text-align:center;
                           font-size:15px;')
      ),
      argonColumn(
        width = 3,
        argonInfoCard(
          value = "Daily Cases \n 846",
          icon = icon("hospital"),
          icon_background = "default",
          hover_lift = F,
          shadow = T,
          background_color = "info",
          gradient = T,
          width = 12
        ),
        h6("Yesterday: ","581",
                  style = 'text-align:center;
                           font-size:15px;')
      ),
      
      argonColumn(
        width = 3,
        argonInfoCard(
          value = "Daily deaths\n38",
          icon = icon("heartbeat"),
          icon_background = "default",
          hover_lift = F,
          shadow = T,
          background_color = "white",
          gradient = T,
          width = 12
        ),
        h6("Yesterday: ","119",
                  style = 'text-align:center;
                           font-size:15px;')
      )
    )
  )

  })

output$chartUI = renderUI({
  tagList(
  argonRow(
    argonColumn(
      width = 12,
      argonRow(
        argonColumn(
          width = 12,
          tags$strong("Click on the region to see R value (reproduction rate) and Max growth Rate", style = "color:#529f9d;font-size: 25px;")
        )
      ),
      leafletOutput("UKMap",width = "100%") %>% withSpinner(),
      tags$hr()
    ),
    argonColumn(
      width = 6,
      argonRow(
        argonColumn(
          width = 12,
          tags$strong("Daily Numbers", style = "color:#529f9d;font-size: 25px;"),
          p("caption: gov.uk/guidance", style = 'color:grey;font-size: 15px;text-align:left;')
        )
      ),
      argonRow(
        argonColumn(
          width = 12,
          plotlyOutput("newCasesPlot",width = "200%") %>% withSpinner()
          )
        ),
      argonRow(
        argonColumn(
          width = 12,
            plotlyOutput("newCasesDeathsPlot",width = "200%") %>% withSpinner()
          )
        )
    )
  )
)
})

library(leaflet)
output$UKMap <- renderLeaflet({
  leaflet(r_rate) %>% 
    addTiles() %>% 
    setView(lng = -3, lat = 53.35, zoom = 4.5 ) %>% 
    addMarkers(~long, ~lat, popup = paste("R Value:", r_rate$R, "<br>",
                                          "Max growth rate:", r_rate$growth_rate,"<br>") , label = ~ Region, icon = ~ MapIcons[type]) %>%
    addControl(html = html_legend, position = 'topleft')
  })

output$countytable = renderDataTable({
  corona_local
})

library(tidyverse)
library(ggplot2)
library(plotly)

output$newCasesPlot = renderPlotly({
    print(
      ggplotly(
      pcases %>% filter(pcases$metric == "pos_num04") %>% 
        ggplot(aes(x=as.Date(date, "%Y-%m-%d"), number)) + 
        geom_line(color = "#db4259", alpha = 0.6) +
        geom_col(position = "identity", fill = "orange", alpha = 0.6) +
        theme_bw() +
        theme(panel.grid.major=element_line(colour=NA)) +
        labs(title = "Daily recorded cases", x= "Month", y = "confirmes cases", caption = "gov.uk/guidance") 
      )
    )
 })

output$newCasesDeathsPlot = renderPlotly({
  print(
    ggplotly(
      deaths %>% filter(deaths$metric == "d_num04") %>% 
        ggplot(aes(x=as.Date(date, "%Y-%m-%d"), number)) + 
        geom_line(color = "orange", alpha = 0.6) +
        geom_col(position = "identity", fill = "#db4259", alpha = 0.6) +
        theme_bw() +
        theme(panel.grid.major=element_line(colour=NA)) +
        labs(title = "Daily deaths published by DHSC", x= "Month",y = "confirmes deaths",caption = "gov.uk/guidance") 
    )
  )
})







#### Forecast UI ----

output$forecastUI = renderUI({
  tagList(
    h1("Prediction of nCov-19 cases in United Kingdom", style = "text-align:center; color:#377372"),
    p("Linear Regression Modelling", style = "text-align:center; font-size:15px; font-weight:bold; color:#529f9d"),
    tags$br(),
    argonRow(
      argonColumn(
        width = 3,
        numericInput(
          inputId = "populationInput",
          label = strong("Population (N)", style = "color:#529f9d;"),
          value = 67886011,
          min = 0,
          width = "100%"
        )
      ),
      argonColumn(
        width = 3,
        numericInput(
          inputId = "fatalityInput",
          label = strong("Fatality rate (in %): Deaths/Confirmed",style = "color:#529f9d;"),
          value = 15.332277,
          min = 0,
          width = "100%"
        )
      ),
      argonColumn(
        width = 3,
        numericInput(
          inputId = "severeInput",
          label = strong("Severe cases (in %): Assumption",style = "color:#529f9d;"),
          value = 5,
          min = 0,
          width = "100%"
        )
      ),
      argonColumn(
        width = 3,
        dateInput( inputId = "dateForecast", 
                   label = strong("Forecast till which date:", style = "color:#529f9d;"),
                   value = Sys.Date() + 60,
                   min = Sys.Date(),
                   format = "dd-mm-yyyy",
                   width = "100%"
        )
      )
    ),
    tags$hr(),
    uiOutput("forecastBadge") %>% withSpinner(),
    tags$br(),
    argonRow(
      argonColumn(
        width = 6,
        highchartOutput("currentScenario") %>% withSpinner()
      ),
      argonColumn(
        width = 6,
        h2("The three phases in a pandemic",style = 'text-align:left; font-weight: bold;color: #529f9d'),
        p("click the gif for more information", style = 'color: grey;font-size: 10px;'),
        a(img(src = 'Covidcurves.gif', width = "85%", height = "85%"), href = "https://thespinoff.co.nz/society/09-03-2020/the-three-phases-of-covid-19-and-how-we-can-make-it-manageable/"),
        h6("Source: Siouxsie Wiles and Toby Morris",style = 'text-align:center;font-style: italic;font-weight: bold;')
      )
      
    )
  )
})

observeEvent("United Kingdom",{
  x = dataframeTotal %>%
    select(-countryCode) %>%
    arrange(desc(Confirmed)) %>%
    mutate(totalActivePer = Unrecovered/Confirmed) %>%
    mutate(totalRecoveredPer = Recovered/Confirmed) %>%
    mutate(totalDeathPer = Deaths/Confirmed) %>%
    select(Country = countryName, Confirmed = Confirmed, Active = Unrecovered,Recovered = Recovered,Deaths = Deaths,"Active (%)" = totalActivePer,"Recovered (%)" = totalRecoveredPer,"Deaths (%)" = totalDeathPer)
  results$resultTable = x
  value = population %>%
            filter(Country == "United Kingdom") %>%
            .[,2] %>%
            gsub(",", "",.) %>%
            as.numeric()
  valueDeath = results$resultTable %>%
                  filter(Country == "United Kingdom") %>%
                  .["Deaths (%)"] %>%
                  as.numeric() * 100 %>%
                  round(.,2)
  updateNumericInput(session,
                     inputId = "populationInput",
                     value = value)
  updateNumericInput(session,
                     inputId = "fatalityInput",
                     value = valueDeath)
})


output$currentScenario = renderHighchart({
  req(!is.null(coronavirus))
  data = coronavirus %>% 
            filter(countryName == "United Kingdom") %>%
            filter(Confirmed > 0) %>%
            select(date,Confirmed,Recovered)
  day = 1:(nrow(data))
  lmModel <- augment(lm(log10(data$Confirmed) ~ day, data = data))
  hc <- highchart() %>% 
           hc_subtitle(text = paste0("Cumulative Infected Cases in United Kingdom"),
                       align = "left",
                       style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = data$date) %>%
    hc_yAxis(title = list(text = "Infected Cases (Log-scale)"),type = "logarithmic", color = "#529f9d") %>%
    hc_add_series(name = "Actual",data = data$Confirmed,type = "area", color = '#EBBA95') %>% 
    hc_add_series(name = "Fitted",data = 10^lmModel$.fitted,type = "line", color = "#529f9d")
  
  hc %>% 
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
  
})


output$forecastBadge = renderUI({
  req(!is.null(coronavirus))
  req(!is.na(input$populationInput))
  data = coronavirus %>% 
    filter(countryName == "United Kingdom") %>%
    filter(Confirmed > 0) %>%
    select(date,Confirmed,Recovered)
  I = data$Confirmed[1]
  R = data$Recovered[1]
  N = input$populationInput
  Day = 1:length(data$Confirmed)
  SIR <- function(time, state, parameters) {
    par <- as.list(c(state, parameters))
    with(par, {
      dS <- -beta/N * I * S
      dI <- beta/N * I * S - gamma * I
      dR <- gamma * I
      list(c(dS, dI, dR))
    })
  }
  init <- c(S = N - I, I = I, R = R)
  RSS <- function(parameters) {
    names(parameters) <- c("beta", "gamma")
    out <- ode(y = init, times = Day, func = SIR, parms = parameters)
    fit <- out[ , 3]
    sum((data$Confirmed - fit)^2)
  }
  model = optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1),hessian = F)
  modelPar <- setNames(model$par, c("beta", "gamma"))
  t = 1:(input$dateForecast - as.Date(min(data$date)))
  fitValue <- data.frame(ode(y = init, times = t, func = SIR, parms = modelPar))
  fitValue = fitValue %>%
    mutate(date = as.Date(min(data$date)) + time)
  results$modelFit = list(params = modelPar,fitValue = fitValue)
  req(!is.null(results$modelFit))
  req(!is.null(input$fatalityInput))
  req(!is.null(input$severeInput))
  r0 = results$modelFit$params[1] / results$modelFit$params[2]
  pandemicHeight = max(results$modelFit$fitValue$I)
  deathForecast = pandemicHeight * (input$fatalityInput/100)
  severeForecast = pandemicHeight * (input$severeInput/100)
  argonRow(
    argonColumn(
      width = 3,
      argonBadge(
        text = paste0("Reproductive rate = ",round(r0,2)),
        pill = T,
        status = ifelse(as.numeric(r0) < 1,"success",
                        ifelse(as.numeric(r0) < 1.5,"warning",
                               "danger")
                        ),
        src = "https://www.gov.uk/guidance/the-r-number-in-the-uk"
      )
    ),
    argonColumn(
      width = 3,
      argonBadge(
        text = paste0("Deaths till height= ",prettyNum(round(deathForecast,0),big.mark = ",")),
        pill = T,
        status = "danger"
      )
    ),
    argonColumn(
      width = 3,
      argonBadge(
        text = paste0("Pandemic height = ",prettyNum(round(pandemicHeight,0),big.mark = ",")),
        pill = T,
        status = "primary"
      )
    ),
    argonColumn(
      width = 3,
      argonBadge(
        text = paste0("Severe cases = ",prettyNum(round(severeForecast,0),big.mark = ",")),
        pill = T,
        status = "warning"
      )
    )
  )
})


##### debug server logic #####
output$runRCodeOutput = renderPrint({
  req(rcode())
  isolate({
    eval(parse(text = rcode()$text))
  })
})
rcode = reactiveVal()
observeEvent(input$runRCodeButton, {
  rcode(list("text" = input$runRCode, "type" = "runRCode", "rand" = runif(1)))
}, ignoreNULL = TRUE, ignoreInit = TRUE)