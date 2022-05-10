library(shinydashboard)
library(fpp3)
data(canadian_gas)
library(shiny)
library(plotly)
gasvolume <- canadian_gas %>%
  select(Volume)

gasvolumetrain <- canadian_gas %>%
  filter_index("1970 Jan" ~ "2000 Dec")

gasvolume %>% model(NAIVE()) %>% forecast(h = 60) -> naivemodel



gasvolume %>% model(MEAN()) %>% forecast(h = 60) -> meanmodel


gasvolume %>% model(SNAIVE(Volume ~ lag("year"))) %>% forecast(h = 60) -> seasnaivemodel


gasvolume %>% model(RW(Volume ~ drift())) %>% forecast(h = 60) -> driftmodel

ui <- dashboardPage(
  dashboardHeader(title = "Midterm Project"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro"),
      menuItem("Full-Time Series", tabName = "firsttab"),
      menuItem("Full-Time Series Interpretation", tabName = "fullseriesinterp"),
      menuItem("Seasonality", tabName = "seasonality"),
      menuItem("Seasonality Interpretation", tabName = "seasoninterp"),
      menuItem("Autocorrelation", tabName = "autocorr"),
      menuItem("Autocorrelation Interpretation",tabName = "autocorrinterp"),
      menuItem("Decomposition", tabName = "decomp"),
      menuItem("Decomposition Interpretation",tabName = "decompinterp"),
      menuItem("Added Feature - Seasonal Subseries Plot", tabName = "subseries"),
      menuItem("Seasonal Subseries Plot Interpretation", tabName = "subseriesinterp"),
      menuItem("Naive Plot", tabName = "naiveplot"),
      menuItem("Seasonal Naive Plot", tabName = "seasonalnaiveplot"),
      menuItem("Mean Model", tabName = "meanmodel"),
      menuItem("Drift Model", tabName = "driftmodel"),
      menuItem("Holts Model", tabName = "holtsmodel"),
      menuItem("Holts/Winters Model", tabName = "holtswintersmodel"),
      menuItem("Auto ARIMA", tabName = "autoarima"),
      menuItem("Manual ARIMA", tabName = "manualarima")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",
              h2("This app takes a look at monthly Canadian Gas production over many years. The following plots indicate to the user the trends, seasonalities, and other characteristics of this dataset. This app takes a look at thirteen different plots to allow the user to better understand these characteristics. The user can click on the left whichever plot they would like to see and immediately below is an interpretation (for the first five plots) describing the data shown on the specific plot.")
    ),
      tabItem(tabName = "firsttab",
              fluidRow(
                box(title = "Canadian Gas Time Series", plotOutput("full_time_series"))
    )
  ),
      tabItem(tabName = "fullseriesinterp",
              h2("The time series fully plotted shows us that there seems to be a gradual increase in the Canadian gas production. Midway through the graph there seems to be increasing heterskedasticity, but at the end of the graph this seems to level off. The graph shows us that seasonality is evident in this industry. This is because the sale of gas to consumer also has been shown to be somewhat seasonal.")),
      tabItem(tabName = "seasonality",
              fluidRow(
                box(title = "Canadian Gas Seasonality", plotOutput("seasonality"))
              )),
      tabItem(tabName = "seasoninterp",
              h2("Gas production looks to slightly dip in the Summer. The production begins to rise in early fall and continues to increase at the end of winter. This could be due to the fact that gas production companies are stocking up in order to have enough gas to sell in the Summer. The Summer months are where gas sales increases in most regions.")),
      tabItem(tabName = "autocorr",
              fluidRow(
                box(title = "Canadian Gas Autocorrelation", plotOutput("acf"))
              )),
      tabItem(tabName = "autocorrinterp",
              h2("This graph shows that there is a trend and seasonality with this graph. The trend is shown by the gradual decline of the ACF as the lags increase. The seasonality is a bit harder to see, but you can see small dips and peaks within the graph.")),
      tabItem(tabName = "decomp",
              fluidRow(
                box(title = "Canadian Gas Decomposition", plotOutput("decomp"))
              )
  
),
      tabItem(tabName = "decompinterp",
              h2("The decomposition is perfect to see the trend, seasonality, and any randomness in the model. The trend shows that there is a steady increase in gas production. The seasonality is very prevelant in this model which is supported by the other graphs as well. The randomness seems to be getting larger which is not a good thing for this model. Forecasting may be difficult due to there being an increase in randomness for this model.")),
      tabItem(tabName = "subseries",
              fluidRow(
                box(title = "Canadian Gas Seasonal Subseries", plotOutput("subseries"))
              )),
      tabItem(tabName = "subseriesinterp",
              h2("This plot more clearly shows the user the seasonality of the data. The blue line shows the mean for each month. You can look straight across the plot and see where the gas production decreases then gradually increases back up. This plot is easier on the user in order to understand the seasonality of a dataset.")),
      tabItem(tabName = "naiveplot",
        fluidRow(
          box(title = "Canadian Gas Naive Model Plot", plotOutput("naiveplot"))
        )),
      tabItem(tabName = "seasonalnaiveplot",
        fluidRow(
          box(title = "Canadian Gas Seasonal Naive Model Plot", plotOutput("seasonalnaiveplot"))
        )),
      tabItem(tabName = "meanmodel",
        fluidRow(
          box(title = "Canadian Gas Mean Model Plot", plotOutput("meanmodel"))
        )),
      tabItem(tabName = "driftmodel",
        fluidRow(
          box(title = "Canadian Gas Drift Model Plot", plotOutput("driftmodel"))
        )),
      tabItem(tabName = "holtsmodel",
        fluidRow(
          box(title = "Canadian Gas Holt's Model Plot", plotOutput("holtsmodel"))
        )),
      tabItem(tabName = "holtswintersmodel",
        fluidRow(
          box(title = "Canadian Gas Holt's/Winter's Model Plot", plotOutput("holtswintersmodel"))
        )),
      tabItem(tabName = "autoarima",
        fluidRow(
          box(title = "Canadian Gas Auto Arima Model Plot", plotOutput("autoarima"))
        )),
      tabItem(tabName = "manualarima",
        fluidRow(
          box(title = "Canadian Gas Manual Arima Model Plot", plotOutput("manualarima"))
        ))

)
)
)

server <- function(input, output, session) {
  output$full_time_series <- renderPlot(autoplot(canadian_gas))
  output$seasonality <- renderPlot(gg_season(canadian_gas))
  canadian_gas %>% 
    model(
      classical_decomposition(Volume, type = "additive")
    ) %>%
    components() %>%
    autoplot() %>% renderPlot() -> output$decomp
  canadian_gas %>% ACF(Volume) %>% autoplot() %>% renderPlot() -> output$acf
  output$subseries <- renderPlot(gg_subseries(canadian_gas))
  (naivemodel %>% 
      autoplot(gasvolumetrain, level = NULL) + 
      autolayer(canadian_gas,
        colour = "black"
      )) %>% renderPlot() -> output$naiveplot
  (meanmodel %>% 
      autoplot(gasvolumetrain, level = NULL) + 
      autolayer(canadian_gas,
        colour = "black"
      )) %>% renderPlot() -> output$meanmodel
  (seasnaivemodel %>% 
      autoplot(gasvolumetrain, level = NULL) + 
      autolayer(canadian_gas,
        colour = "black"
      )) %>% renderPlot() -> output$seasonalnaiveplot
  (driftmodel %>% 
      autoplot(gasvolumetrain, level = NULL) + 
      autolayer(canadian_gas,
        colour = "black"
      )) %>% renderPlot() -> output$driftmodel
  canadian_gas %>%
    model(
      `Holt's method` = ETS(Volume ~ error("A") +
                              trend("A") + season("N")),
      `Damped Holt's method` = ETS(Volume ~ error("A") +
                                     trend("Ad", phi = 0.9) + season("N"))
    ) %>%
    forecast(h = 60) %>%
    autoplot(canadian_gas, level = NULL) %>% renderPlot() -> output$holtsmodel
  canadian_gas %>%
    model(
      additive = ETS(Volume ~ error("A") + trend("A") +
                       season("A")),
      multiplicative = ETS(Volume ~ error("M") + trend("A") +
                             season("M"))
    ) %>% forecast(h = 60) %>% autoplot(canadian_gas, level = NULL) %>% renderPlot() -> output$holtswintersmodel
  canadian_gas %>%
    model(ARIMA(Volume)) %>% forecast(h=60) %>%
    autoplot(canadian_gas) %>% renderPlot() -> output$autoarima
  canadian_gas %>%
    model(arima210 = ARIMA(Volume ~ pdq(2,1,0)),
          arima013 = ARIMA(Volume ~ pdq(0,1,3)),
          stepwise = ARIMA(Volume),
          search = ARIMA(Volume, stepwise=FALSE)) %>% 
    forecast(h=60) %>%
    filter(.model=='arima210') %>%
    autoplot(canadian_gas) %>% renderPlot() -> output$manualarima
}

shinyApp(ui, server)
