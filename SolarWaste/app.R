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
comma<-function(x) prettyNum(signif(x,digits=4),big.mark=",")

# This global solar capacity factor was chosen so that the TWh for 2021 in the World Energy Stats (ex-BP) matches
# the GW in the IEA Net Zero plan
solarCF<-0.13
# Data
# https://iea.blob.core.windows.net/assets/9a698da4-4002-4e53-8ef3-631d8971bf84/NetZeroRoadmap_AGlobalPathwaytoKeepthe1.5CGoalinReach-2023Update.pdf
# Now we get the World Energy Stats TWh from the 2023 edition and use the capacity factor (solarCF) to estimate
# the installed GW for that year
solarTWh<-read_csv("solartwh.csv") 
solarGW<-solarTWh %>% mutate(Year=ymd(paste0(Year,"-01-01")),cumGWinstalled=TWh*1e12/(solarCF*24*365)/1e9) %>%
  select(cumGWinstalled,Year)
print(solarGW)
firstYear<-2012
lastYear<-2050
thisYear<-2022
years<-28
pvGWThisYear<-1161
# difference between 2021 and 2022
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
#----------------------------------------------------------------------
# we have p items and the probability of failure before time t=n-1 is
# 1 - exp(-lambda*t)
#----------------------------------------------------------------------
makeFail<-function(mtbf) {
  lambda<-1/mtbf
  function(n,p) {
    (1-exp(-lambda*(n-1)))*p
  }
}
nyears<-lastYear-firstYear+1
bFailed<-rep(0,nyears)
bRecycled<-rep(0,nyears)
bRecycledGWh<-rep(0,nyears)
genWasteData<-function(input) {
      recycleFun<-makeRecycle(input$pvRecyclingCAGR,input$pvRecycling22)
      failFun<-makeFail(input$pvLifeSpan)
      pvProdto2030<-makeExpProduction(input$pvGrowthRate1,pvGWThisYear,8)
      df1<-tibble(
        cumGWinstalled=(1:8 %>% map_dbl(pvProdto2030)),
        Year=seq(ymd('2023-01-01'),ymd('2030-01-01'),by='1 year')
      )
      pvProdto2050<-makeExpProduction(input$pvGrowthRate2,df1$cumGWinstalled[8],19)
      df2<-tibble(
        cumGWinstalled=(1:20 %>% map_dbl(pvProdto2050)),
        Year=seq(ymd('2031-01-01'),ymd('2050-01-01'),by='1 year')
      )
      df<-bind_rows(solarGW,df1,df2)
      df <- df %>% mutate(produced=cumGWinstalled-lag(cumGWinstalled))
      df$produced[1]=0
      print(df,n=60)
      write_csv(df,"production.csv")
      
      #-----------------------------------------------------------------------------------------------------------
      # TESTING the reliability function
      # We set up 100 widgets in year 1 and we should have 63% fail at year 26 (given MTBF of 25 years by default)
      #-----------------------------------------------------------------------------------------------------------
      #trel<-rep(0,nyears)
      #tfail<-rep(0,nyears)
      #trel[1]<-100
      #for(i in 1:nyears) {
      #  if (i<nyears) {
      #    for(n in (i+1):nyears) {
      #      tfail[n]<-tfail[n]+failFun(n,trel[i])
      #      cat(paste0("i,n,trel[i],tfail[n]:",i,",",n,",P=",trel[i],",F=",tfail[n],"\n"))
      #    }
      #  }
      #}
      
      print("==================================================")
      for(i in 1:nyears) {
        if (i<nyears) {
          for(n in (i+1):nyears) {
            bFailed[n]<-bFailed[n]+failFun(n,df$produced[i])
            # cat(paste0("i,n,produced[i],failed[n]:",i,",",n,",P=",df$produced[i],",F=",bFailed[n],"\n"))
          }
        }
      }
      df$failed=bFailed
      df$recycled<-map2_dbl(bFailed,1:nyears,recycleFun)
      write_csv(df %>% mutate(cumfailed=failed,cumrecycled=recycled) %>% select(produced,cumfailed,cumrecycled),"recycled.csv")
      
      df2 <- df %>% mutate(operational=cumGWinstalled-failed+recycled,cumFailed=failed-recycled,cumInstalled=cumGWinstalled) %>%
        
        pivot_longer(cols=c("produced","cumFailed","recycled","operational","cumInstalled"),names_to="State",values_to="GW")
      
      rccagr<-input$pvRecyclingCAGR
      rc22<-input$pvRecycling22
      df3<-df2 %>% select(Year,State,GW) %>% pivot_wider(names_from=State,values_from=GW)
      #write_csv(df3,paste0("solarpv-statetable",rccagr,"pc-",rc22,"GW.csv"))
      
      
      df2
}



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Solar Waste Tonnage over time"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("pvGrowthRate1","Growth rate to 2030 (%)",min = 1, max = 30, value = 23),
            sliderInput("pvGrowthRate2","Growth rate 2030 to 2050 (%)",min = 1, max = 30, value = 11),
            sliderInput("pvTonnagePerGW","Panel tonnage per GW ('000 tonnes)",min = 30, max = 150, value = 70),
            sliderInput("pvLifeSpan","Average lifespan (years))",min = 15, max = 200, value = 30),
            sliderInput("pvRecycling22","Recycling Capacity 2022 (GW)",min = 10, max = 200, value = 10),
            sliderInput("pvRecyclingCAGR","Recycling CAGR (%)",min = 5, max = 50, value = 5)
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

### IEA Net Zero target 

The IEA Net Zero by 2050 target is for 18,750 GW ([as updated in 2023](https://iea.blob.core.windows.net/assets/9a698da4-4002-4e53-8ef3-631d8971bf84/NetZeroRoadmap_AGlobalPathwaytoKeepthe1.5CGoalinReach-2023Update.pdf)).

### Guide to the graph 

PV panels are assumed to be produced and installed according to two rates; the 
rate from now to 2030 and the rate from 2030 to 2050. They have an average lifespan which determines the
failure rate using a exponential failure model (one of a number of standard failure models). Any failed panel
can be recycled upto the limit of the recycling capacity which is determined by an estimate of the capacity
in 2022 and a specified growth rate. 

So the chart gives various numbers.

1. cumfailed ... these are panels which have failed but can't be recycled because of a lack of capacity 
1. cuminstalled ... total GW included failed panels  
1. operational ... cuminstalled - cumfailed + recycled
1. produced ... new production from freshly mined materials 
1. recycled ... production from recycled material; assumed to be perfect 


                    "),
           plotOutput("distPlot"),
           uiOutput("notes")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$notes <- renderUI({
      df<-genWasteData(input)
      op2050<-df %>% filter(State=="operational") %>% summarise(mx=max(GW))
      waste2050<-df %>% filter(State=="cumFailed") %>% summarise(mx=max(GW))
      markdown(paste0(
        "GW of operational solar in 2050: ",comma(op2050),"\n\n",
        "Accumulated waste by 2050: ",comma(waste2050*1000*input$pvTonnagePerGW/1e6)," million tonnes\n"
        ))
    })
    output$distPlot <- renderPlot({
      df<-genWasteData(input)
      mx<-df %>% filter(State=="operational") %>% summarise(mx=max(GW))
      print(mx)
      df %>% ggplot(aes(x=Year,y=GW,fill=State))+
        geom_col(position="dodge")+labs(y="Gigawatts") +
        annotate('text',x=ymd("2030-01-01"),y=30000,label=paste0("Operational GW of PV in 2050:",comma(mx)))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
