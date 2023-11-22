#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
# Data
# https://iea.blob.core.windows.net/assets/9a698da4-4002-4e53-8ef3-631d8971bf84/NetZeroRoadmap_AGlobalPathwaytoKeepthe1.5CGoalinReach-2023Update.pdf
thisYear<-2022
years<-28
pvGWThisYear<-1145
# difference between 2021 and 2022
pvGrowthPerc<-(1145-925)/925
print(paste0("pv growth percent 2021 to 2022: ",pvGrowthPerc))
pvGrowthto30<-23
pvGrowth30to50<-12

makeExpProduction<-function(percent,peryear,years) {
  function(year) { peryear*((1+percent/100)^year); }
}
makeRecycle<-function(percent,GWcapnow) {
  function(failed,year) {
    capacity<-GWcapnow*((1+percent/100)^year)
    ifelse(failed<capacity,failed,capacity)
  }
}
makeFail<-function(mtbf) {
  lambda<-1/mtbf
  function(n,p) {
    (1-exp(-lambda*(n-1)))*p
  }
}



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Solar Waste Tonnage over time"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30),
            sliderInput("pvGrowthRate1","Annual PV growth rate  to 2030 (%)",min = 1, max = 30, value = 23),
            sliderInput("pvGrowthRate2","Annual PV growth rate 2030 to 50 (%)",min = 1, max = 30, value = 12),
            sliderInput("pvTonnagePerGW","Tonnage per GW of PV panels ('000 tonnes)",min = 30, max = 150, value = 70)
        ),

        # Show a plot of the generated distribution
        
        mainPanel(
           markdown("
### Solar panel wastage generation           

We model the global production of solar PV panel waste based on growth rates in both production and
recycling. We assume magic recycling whereby every tonne of panels is removed from the waste stream 
and returned directly to service.

PV Growth Rates follow the IEA netzero by 2050 plan in that they aren't constant from now to 2050, but
have two values, one from now to 2030 and the second rate from 2030 to 2050. This makes intuitive sense because it 
is much easier to grow by some percentage when you are small than when you are large; the bottleneck typically being
the flow of materials.

                    "),
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
      pvProdto2030<-makeExpProduction(input$pvGrowthRate1,pvGWThisYear,8)
      df1<-tibble(
        produced=(1:8 %>% map_dbl(pvProdto2030)),
        year=seq(ymd('2023-01-01'),ymd('2030-01-01'),by='1 year')
      )
      pvProdto2050<-makeExpProduction(input$pvGrowthRate2,df1$produced[8],19)
      df2<-tibble(
        produced=(1:20 %>% map_dbl(pvProdto2050)),
        year=seq(ymd('2031-01-01'),ymd('2050-01-01'),by='1 year')
      )
      df<-bind_rows(df1,df2)
      print(df,n=30)
      df %>% ggplot(aes(x=year,y=produced))+geom_col()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
