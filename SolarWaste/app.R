#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(modelr)
library(markdown)
library(plotly)
comma<-function(x) prettyNum(signif(x,digits=4),big.mark=",")
markdownFile<-function(filename) {
  t<-read_file(filename)
  markdown(t)
}

# This global solar capacity factor so that the TWh for 2021 in the World Energy Stats (ex-BP) matches
# the GW in the IEA Net Zero plan is solarCF<-0.13
# Data
# https://iea.blob.core.windows.net/assets/9a698da4-4002-4e53-8ef3-631d8971bf84/NetZeroRoadmap_AGlobalPathwaytoKeepthe1.5CGoalinReach-2023Update.pdf
#-----------------------------------------------------------------------------------------------
# Read file of GW by country from 2005 to 2022 from WES
#-----------------------------------------------------------------------------------------------
dfgw<-read_csv("GWByCountry.csv") %>% mutate(Country=str_replace(Country,"Total ",""))
countries<-dfgw$Country %>% unique
#------------------------------------------------------------------------------------------------
firstYear<-2005
lastYear<-2050
thisYear<-2022
years<-35
#---------------------------------------------------------------------------------------------
# The following figure is from IEA NZ2050 2023 ... the WES2023 figure puts it at 1053 GW
#---------------------------------------------------------------------------------------------
pvGWThisYear<-1145    # we don't use this anymore, but read it from the file
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
# We have two failure closures here, but only use the second, the first is what
# we started with, but it just a special case of the second function
#----------------------------------------------------------------------
makeFail<-function(mtbf) {
  lambda<-1/mtbf
  function(n,p) {
    (1-exp(-lambda*(n-1)))*p
  }
}
#-------------------------------------
# generalised to Weibull distribution
#-------------------------------------
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
dcols<-c("operational","cumInstalled","cumFailed","recycled","produced")

#-------------------------------------------------------------------------------------------
# Set up plotly
#-------------------------------------------------------------------------------------------
ggplconfig <- function(.data) {
  config(.data,displayModeBar = "static", displaylogo = FALSE, 
         modeBarButtonsToRemove = list("autoScale2d", 
                                       "hoverClosestCartesian", "hoverCompareCartesian", 
                                       "select2d", "zoomIn2d", "zoomOut2d","lasso2d","toggleSpikelines"))
}



# Define UI for application 
ui <- fluidPage(theme = shinytheme("cerulean"),

    # Application title
    titlePanel("Solar Panel Waste Tonnage over time"),
    imageOutput("banner",height="200px"),

    sidebarLayout(
        #-----------------------------------------------------------------------------------
        # Sidebar with a sliders
        #-----------------------------------------------------------------------------------
        sidebarPanel(
            selectInput("region","Country or Region",countries,selected="World"),
            checkboxInput("predict", label = "Add a prediction based on 2005-22", value = FALSE),
            checkboxGroupInput("stylegraph", label = "Choose your graph bars",choices=dcols,selected=dcols),
            sliderInput("pvGrowthRate1","Growth rate to 2030 (%)",min = 1, max = 30, value = 20),
            sliderInput("pvGrowthRate2","Growth rate 2030 to 2050 (%)",min = 1, max = 30, value = 9),
            sliderInput("pvTonnagePerGW","Panel tonnage per GW ('000 tonnes)",min = 30, max = 150, value = 70),
            sliderInput("pvLifeSpan","Average lifespan (years))",min = 10, max = 50, value = 30),
            sliderInput("pvFailParm","PV failure parameter (see notes)",min = 1.0, max = 15.0, step=0.1,value = 1),
            sliderInput("pvUtilityRatio","Percentage of utility PV (%)",min = 10, max = 100, value = 60),
            sliderInput("pvHaPerMW","Hectares per MW",min = 1, max = 3, value = 2.5,step=0.1),
            sliderInput("pvRecycling22","Recycling Capacity 2022 (GW)",min = 0, max = 50, value = 0),
            sliderInput("pvRecyclingCAGR","Recycling CAGR (%)",min = 5, max = 20, value = 5),
            dateInput("pvStartYear","Pick start date",
                           value=ymd("2005-01-01"),
                           min=ymd("2005-01-01"),max=ymd("2050-01-01"),
                           format="yyyy-mm-dd"),
            markdown("Geoff Russell, Beta Testing, V0.5 November 2023")
        ),

        #-----------------------------------------------------------------------------------
        # now the main panel has the plots
        #-----------------------------------------------------------------------------------
        mainPanel(
           uiOutput("topnotes"),
           markdown("## Gigawatts of solar PV panels"),
           plotlyOutput("distPlot"),
           uiOutput("notes"),
           plotlyOutput("failPlot"),
           markdown("## Material required"),
           plotlyOutput("tonnagePlot"),
           markdownFile("model-notes.txt"),
           markdownFile("model-notes1.txt")
        )
    )
)

#---------------------------------------------------------------------------------
# and the server to glue it together
#---------------------------------------------------------------------------------
server <- function(input, output) {
    genWasteData<-reactive({
      #print(input$region)
      #regionGW<-dfgw %>% filter(Country==input$region & Year>="2012") %>% mutate(Year=ymd(paste0(Year,"01-01")),cumGWinstalled=GW) %>% select(cumGWinstalled,Year)
      regionGW<-dfgw %>% filter(Country==input$region) %>% mutate(Year=ymd(paste0(Year,"01-01")),cumGWinstalled=GW) %>% select(cumGWinstalled,Year)
      #print(regionGW)
      pvGWThisYear<-regionGW %>% summarise(mx=last(cumGWinstalled))
      
      #print(pvGWThisYear)
      
      recycleFun<-makeRecycle(input$pvRecyclingCAGR,input$pvRecycling22)
      pvProdto2030<-makeExpProduction(input$pvGrowthRate1,as.numeric(pvGWThisYear),8)
      df1<-tibble(
        cumGWinstalled=(1:8 %>% map_dbl(pvProdto2030)),
        Year=seq(ymd('2023-01-01'),ymd('2030-01-01'),by='1 year')
      )
      pvProdto2050<-makeExpProduction(input$pvGrowthRate2,df1$cumGWinstalled[8],19)
      df2<-tibble(
        cumGWinstalled=(1:20 %>% map_dbl(pvProdto2050)),
        Year=seq(ymd('2031-01-01'),ymd('2050-01-01'),by='1 year')
      )
      df<-bind_rows(regionGW,df1,df2)
      
      #-----------------------------------------------------------------------------
      # Calculating the failed panels over time 
      # First we transform the cumulative installation into the amount produced each
      # year. That's the line with the lag function.
      # We set the first year manually, because the lag function would miss it 
      # then we loop. For each year we calculate the failures in all subsequent years 
      # according to the failure function
      #-----------------------------------------------------------------------------
      failFun<-makeWeibullFail(input$pvLifeSpan,input$pvFailParm)
      df <- df %>% mutate(produced=cumGWinstalled-lag(cumGWinstalled))
      df$produced[1]=df$cumGWinstalled[1]
      # print(df,n=60)
      # write_csv(df,"production.csv")
      
      ftab<-data.frame(matrix(0,nrow=nyears,ncol=nyears))
      ftab[,1]=df$produced
      for(i in 1:nyears) {
        if (i<nyears) {
          for(n in (i+1):nyears) {
            nf<-failFun(n-i,df$produced[i])
            bFailed[n]<-bFailed[n]+nf
            ftab[i,n]=nf
            # cat(paste0("i,n,produced[i],failed[n]:",i,",",n,",P=",df$produced[i],",F=",bFailed[n],"\n"))
          }
        }
      }
      # write_csv(ftab,"failuretable.csv")
      df$failed=bFailed
      # done
      #----------------------------------------------------------------------------------
      # the rest is easy, recycling just returns failed stuff to the operational state 
      #----------------------------------------------------------------------------------
      df$recycled<-map2_dbl(bFailed,1:nyears,recycleFun)
      # write_csv(df %>% mutate(cumfailed=failed,cumrecycled=recycled) %>% select(produced,cumGWinstalled,cumfailed,cumrecycled),"recycled.csv")
      
      df2 <- df %>% mutate(operational=cumGWinstalled-failed+recycled,cumFailed=failed-recycled,cumInstalled=cumGWinstalled) %>%
        
        pivot_longer(cols=dcols,names_to="State",values_to="GW")
      
      rccagr<-input$pvRecyclingCAGR
      rc22<-input$pvRecycling22
      df3<-df2 %>% select(Year,State,GW) %>% pivot_wider(names_from=State,values_from=GW)
      # write_csv(df3,paste0("solarpv-statetable",rccagr,"pc-",rc22,"GW.csv"))
      # write_csv(df3,"df3.csv")
      df2
    })
    output$topnotes <- renderUI({
      markdown(paste0(
        "\n</br>\n",
        "Estimate waste streams and land used under various scenarios; use the sliders to change assumptions.\n", 
        "Keep in mind that not all assumptions are equal, some are definitely better than others!\n",
        "Read the model assumptions below for details on the initial parameter choices."
      ))
    })
    output$notes <- renderUI({
      df<-genWasteData()
      op2050<-df %>% filter(State=="operational") %>% summarise(mx=last(GW))
      cumInstalled<-df %>% filter(State=="cumInstalled") %>% summarise(mx=last(GW))
      waste2050<-df %>% filter(State=="cumFailed") %>% summarise(mx=last(GW))
      rec2050<-df %>% filter(State=="recycled") %>% summarise(mx=last(GW))
      dfw<-df %>% filter(State=="cumFailed")
      # print(dfw,n=100)
      
      msg<-""
      if (input$region=="World" && op2050<18749) {
        msg<-"**Note: your settings have resulted in an operational level of PV below the IEA Net Zero by 2050 plan (18,750 GW)**" 
      }
      hectares<-op2050*1000*(input$pvHaPerMW*(input$pvUtilityRatio/100))
      w<-waste2050*input$pvTonnagePerGW*1000
      wmsg<-""
      if (w>1e6) {
        wmsg<-paste0(comma(waste2050*input$pvTonnagePerGW*1000/1e6),"** million tonnes\n\n")
      }
      else {
        wmsg<-paste0(comma(waste2050*input$pvTonnagePerGW*1000),"** tonnes\n\n")
      }
      markdown(paste0(
        "Operational PV panels in 2050: **",comma(op2050),"** GW\n\n",
        "Accumulated PV panel waste by 2050: **",wmsg,
        "Total PV tonnage deployed by 2050: **",comma(cumInstalled*input$pvTonnagePerGW*1000/1e6),"** million tonnes\n\n",
        "Total Utility scale land use: **",comma(hectares),"** hectares (based on operational GW)\n\n",
        "Recycled PV panels by 2050: **",comma(rec2050*input$pvTonnagePerGW*1000),"** tonnes\n\n",
        msg,"\n\n",
        "## Failure model\n\n",
        "Change the \"PV failure parameter\" slider to see the impact of different assumptions. The class of models is widely used in product reliability models.\n"
        ))
    })
    output$failPlot <- renderPlotly({
      nyears<-100
      failWeibullRef<-makeWeibullFail(input$pvLifeSpan,1)
      failWeibull<-makeWeibullFail(input$pvLifeSpan,input$pvFailParm)
      reffailed<-rep(0,nyears)
      failed<-rep(0,nyears)
      df<-tibble(
        panels=c(100,rep(0,nyears-1)),
        produced=c(100,rep(0,nyears-1))
      )
      for(i in 1:nyears) {
        if (i<nyears) {
          for(n in (i+1):nyears) {
                reffailed[n]<-reffailed[n]+failWeibullRef(n,df$produced[i])
                failed[n]<-failed[n]+failWeibull(n,df$produced[i])
          }
        }
      }
      dfout<-tibble(
        expfail=reffailed,
        wfail=failed,
        x=seq(1,100)
      )
      if (input$pvFailParm==1) {
        p<-dfout %>% ggplot()+
        geom_line(aes(x=x,y=expfail),color="blue") +
        labs(x="Years",y="Percentage Failed",title=paste0("Basic failure model with average panel life of ",input$pvLifeSpan," years"))
        ggplotly(p) %>% ggplconfig
      } else {
        p<-dfout %>% ggplot()+
        geom_line(aes(x=x,y=expfail),color="blue") +
        geom_line(aes(x=x,y=wfail),color="red")+
        annotate('text',x=24,y=50,label="k=1",color="blue")+
        annotate('text',x=22,y=25,label=paste0("k=",input$pvFailParm),color="red")+
        labs(x="Years",y="Percentage Failed",title=paste0("Various failure models with average panel life of ",
                                                          input$pvLifeSpan,"years
Reference (k=1) compared with selected"))
        ggplotly(p) %>% ggplconfig
      }
    })
    output$distPlot <- renderPlotly({
      df<-genWasteData()
      dfw<-df %>% pivot_wider(names_from=`State`,values_from=`GW`) 
      model<-lm(cumGWinstalled~poly(as.numeric(Year),2),dfw[1:17,])
      df<-df %>% add_predictions(model,var="Predicted") 
      # write_csv(df,"distdf.csv")
      
      mx<-df %>% filter(State=="operational") %>% summarise(mx=last(GW))
      # print(mx)
      y1<-ymd(as.character(input$pvStartYear))
      # print(y1)
      p<-df %>% filter(State %in% input$stylegraph) %>% ggplot(aes(x=Year,y=GW,fill=State))+
        geom_col(position="dodge")+labs(y="Gigawatts") + xlim(y1,ymd("2050-10-10")) +
        annotate('text',x=ymd("2030-01-01"),y=as.numeric(mx),vjust=0,label=paste0("Operational PV in 2050: ",comma(mx),"GW"),size=5)
      if (input$predict) {
         ggplotly(p+geom_line(aes(x=Year,y=Predicted),color="red")) %>% ggplconfig
      }
      else {
         ggplotly(p) %>% ggplconfig
      }
    })
    output$tonnagePlot <- renderPlotly({
      df<-genWasteData()
      y1<-ymd(as.character(input$pvStartYear))
      p<-df %>% ggplot(aes(x=Year,y=GW*input$pvTonnagePerGW*1000/1e6,fill=State))+ xlim(y1,ymd("2050-10-10")) +
        geom_col(position="dodge")+labs(y="million tonnes") 
      ggplotly(p) %>% ggplconfig
    })
    output$irenaimage<-renderImage(list(src="IRENA-2016-japan.png",height="400px"),deleteFile=FALSE)
    output$banner<-renderImage(list(src="solar-smashed-virgin-islands-s.jpg",height="200px"),deleteFile=FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)