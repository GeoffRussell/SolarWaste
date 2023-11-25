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
library(markdown)
comma<-function(x) prettyNum(signif(x,digits=4),big.mark=",")
markdownFile<-function(filename) {
  #t<-read_file(pipe(paste0("date >>.m4.log && cat m4defs.txt ",filename," | m4 2>>.m4.log")))
  #t<-read_file(pipe(paste0("cat m4defs.txt ",filename," | m4 ")))
  t<-read_file(filename)
  #  s<-str_replace_all(t,'\\[1\\] "(.*)"','\\1')
  markdown(t)
}





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
# generalised to Weibull
makeWeibullFail<-function(mtbf,k) {
  lambda<-1/mtbf
  function(n,p) {
    (1-exp(-(lambda*(n-1))^k))*p
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
            sliderInput("pvQuality","PV failure parameter (see notes)",min = 1.0, max = 7.0, step=0.1,value = 1),
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
           markdownFile("../model-notes.txt"),
           imageOutput("irenaimage",height=400),
           markdownFile("../model-notes1.txt")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    genWasteData<-reactive({
      recycleFun<-makeRecycle(input$pvRecyclingCAGR,input$pvRecycling22)
      failFun<-makeWeibullFail(input$pvLifeSpan,input$pvQuality)
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
      df$produced[1]=df$cumGWinstalled[1]
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
    output$irenaimage<-renderImage(list(src="../IRENA-2016-japan.png",height="400px"),deleteFile=FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
