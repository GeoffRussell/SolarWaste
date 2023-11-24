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
bRecycled<-rep(0,nyears)
bRecycledGWh<-rep(0,nyears)
bFailed<-rep(0,nyears)



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
            sliderInput("pvLifeSpan","Average lifespan (years))",min = 15, max = 50, value = 30),
            sliderInput("pvRecycling22","Recycling Capacity 2022 (GW)",min = 10, max = 50, value = 10),
            sliderInput("pvRecyclingCAGR","Recycling CAGR (%)",min = 5, max = 20, value = 5),
            dateInput("pvStartYear","Pick start date",
                           value=ymd("2010-01-01"),
                           min=ymd("2010-01-01"),max=ymd("2050-01-01"),
                           format="yyyy-mm-dd"),
            markdown("Geoff Russell, Alpha Testing, V0.4 November 2023")
        ),

        # Show a plot of the generated distribution
        
        mainPanel(
           markdown("## Gigawatts of solar PV panels"),
           plotOutput("distPlot"),
           uiOutput("notes"),
           markdown("## Material required"),
           plotOutput("tonnagePlot"),
markdown("

## Model assumptions 

The graph above, with its initial slider settings, 
models the global rollout of solar PV panels according to the IEA Net Zero by 2050.
This IEA plan proposes some 18,750 GW of solar PV ([as updated in 2023](https://iea.blob.core.windows.net/assets/9a698da4-4002-4e53-8ef3-631d8971bf84/NetZeroRoadmap_AGlobalPathwaytoKeepthe1.5CGoalinReach-2023Update.pdf)).
As you can see, the model illustrated in the graph above, yields an operational level of 17,440 GW by 2050. The 
small difference may be down to some minor variations in the PV panel failure mechanism I've used, or the
initial conditions; the number of GW of solar rolled out between 2012 and 2022. 
I use the World Energy Statistics data for historical PV capacity; which may not be what the 
IEA use.

The initial settings mirror the assumptions in the IEA netzero by 2050 plan
and envisage compound annual growth rates (CAGR) of 23% out to 2030 and 11% between 2030 and
2050. These two numbers determine the growth in panel production. 

The IEA don't discuss PV module failure in the NZ2050 report, unless I missed it. But one is implied by
their numbers. Those CAGRs without any panel failure would result in a much bigger 
installed gigawatt (GW) base by 2050.

**PV panel failure**

The failure model merely takes panels out of the installed base according to a fairly standard industrial
model based on the mean time to failure of panels (ie. average panel lifespan).

The initial value of this lifespan parameter is 30 years. 

**PV recycling**

Recycling is modelled by magic whereby failed panels are simply added back to the
operational base until the recycling capacity for the year is reached. We allow the recycling capacity and its growth
rate to be changed; like all the other model parameters.

A more realistic model would dial back the production growth rate as the recycling rate increased. This would keep
the installed base tracking the IEA planning target.

I don't really have any idea how much PV recycling is being done at present. I see media report of small or very small
projects. So my guess at 10 GW per year would amount to about 30 million panels a year. 

**Panel weight and waste**

The initial setting of 70,000 tonnes of mined materials per GW or 
PV panels is based on reading a stack of specification sheets. Panels used in solar farms are typically heavier than
those used on rooftops.

No account is made of the aluminium, steel and possibly concrete
used for mounting panels, either on rooftops or in utility scale farms. The weight of mounting materials often exceeds that of
the panels themselves. For utility scale farms, the soil and weather will determine if concrete footings are 
required; the tendency seems, for obvious reasons, to try and avoid that expense.

### Guide to the graph 

All the bars on the chart are in gigawatts (GW). The calculated tonnage of waste below the graph is just estimated
by multiplying the GW by the tonnage per factor figure in the slider.

The bars are as follows:

1. cumFailed ... these are panels which have failed but haven't been recycled because of a lack of capacity 
1. cumInstalled ... total GW included failed panels  
1. operational ... cuminstalled - cumfailed + recycled
1. produced ... new production from freshly mined materials 
1. recycled ... production from recycled material; assumed to be perfect 





                    ")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    genWasteData<-reactive({
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
    })
    output$notes <- renderUI({
      df<-genWasteData()
      op2050<-df %>% filter(State=="operational") %>% summarise(mx=max(GW))
      waste2050<-df %>% filter(State=="cumFailed") %>% summarise(mx=max(GW))
      markdown(paste0(
        "Operational PV panels in 2050: **",comma(op2050),"** GW\n\n",
        "Accumulated PV panel waste by 2050: **",comma(waste2050*1000*input$pvTonnagePerGW/1e6),"** million tonnes\n"
        ))
    })
    output$distPlot <- renderPlot({
      df<-genWasteData()
      mx<-df %>% filter(State=="operational") %>% summarise(mx=max(GW))
      print(mx)
      y1<-ymd(as.character(input$pvStartYear))
      print(y1)
      df %>% ggplot(aes(x=Year,y=GW,fill=State))+
        geom_col(position="dodge")+labs(y="Gigawatts") + xlim(y1,ymd("2050-10-10")) +
        annotate('text',x=ymd("2030-01-01"),y=40000,vjust=0,label=paste0("Operational PV in 2050: ",comma(mx),"GW"),size=5)
    })
    output$tonnagePlot <- renderPlot({
      df<-genWasteData()
      y1<-ymd(as.character(input$pvStartYear))
      df %>% ggplot(aes(x=Year,y=GW*input$pvTonnagePerGW*1000/1e6,fill=State))+ xlim(y1,ymd("2050-10-10")) +
        geom_col(position="dodge")+labs(y="million tonnes") 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
