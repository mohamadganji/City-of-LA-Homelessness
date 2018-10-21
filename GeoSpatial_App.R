library(shiny)
library(leaflet)
library(dplyr)
library(rgdal)
library(ggmap)
library(lubridate)


load("crime.Rdata")
load("calls311.RData")


ui=fluidPage(
  titlePanel("Geo-Spatial Visulization",windowTitle="Geo-Spatial Visulization"),
  sidebarPanel(
    textInput(inputId = "target_zone", label = "Search For a Place" , ""),
    
    selectInput(inputId = "mapping",
                label = "Choose the Mapping Level",
                choices = list("Census Tract","Council District"),
                selected="Census Tract"
    ),
    
    checkboxInput("county", label = "Show the Whole LA County", value = FALSE),
    
    selectInput(inputId = "select", label = "Select the Measure to Show", 
                choices = list("Crime"="num_crime" , "Shelter"="num_shelt","311 Calls"="num_call","Homeless Count"="num_homeless","Combined Measure")),
    
    conditionalPanel(
      condition = "input.select == 'num_shelt'",
      radioButtons("shelt_var", label = "Choose a Variable",
                   choices = list("Absolute Number of Shelters" = "num_shelt_abs", "Number of shelters/Number of Homeless")
      )
    ),    
    conditionalPanel(
      condition = "input.select == 'num_crime'",
      dateRangeInput(inputId = 'DateRange_Crime',
                     label = 'Date range for crime data: yyyy-mm-dd',
                     start = min(crime$DateOccurred), end = max(crime$DateOccurred)
      ),
      radioButtons("crime_var", label = "Choose a Variable",
                   choices = list("Absolute Number of Crimes" = "num_crime_abs", "Number of Crimes/Number of Homeless")
      )
    ),
    
    
    conditionalPanel(
      condition = "input.select == 'num_call'",
      dateRangeInput(inputId = 'DateRange_Call',
                     label = 'Date range for 311 calls: yyyy-mm-dd',
                     start = min(calls$CreatedDate), end = max(calls$CreatedDate)
      ),
      radioButtons("call_var", label = "Choose a Variable",
                   choices = list("Absolute Number of Calls" = "num_call_abs","Number of Calls/Number of Homeless" = "num_call_normal", "Average Response Time" = "response")
                   
      )
    ),
    
    conditionalPanel(
      condition = "input.select == 'num_homeless'",
      radioButtons("homeless_year", label = "Choose a Year",
                   choices = list("2017", "2016","2015")
      ),
      radioButtons("homeless_var", label = "Choose a Variable",
                   choices = list("Homeless Count" = "num_homeless","Number of Homeless/Population"="num_homeless_normal","Unsheltered Rate" = "unshelt_rate")
      )
    ),
    
    conditionalPanel(
      condition="input.select == 'Combined Measure'",
      radioButtons("measure_year", label = "Choose a Year",
                   choices = list("2017", "2016","2015")
      ),
      helpText("Choose the Weights For Different Variables"),
      numericInput(inputId="crime_w", label="Crime Weight", value=1,min = 0,max=100),
      numericInput(inputId="shelt_w", label="Shelter Weight", value=1,min = 0,max=100),
      numericInput(inputId="homeless_w", label="Homeless Count Weight", value=1,min = 0,max=100),
      numericInput(inputId="call_w", label="311 Calls Weight", value=1,min = 0,max=100)
    )
    
  ),
  mainPanel(
             leafletOutput(outputId = "map",height = 340),
             plotOutput(outputId = "bar",height = 140)
      
    )
)


server = function(input,output,session) {
  
  
  View=reactive({
    if(input$target_zone==""){
      return(c(9,34,-118.3650))
    }else{
      target_pos=geocode(input$target_zone)
      return(c(13,target_pos$lat,target_pos$lon))
    }
  })
  
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles("OpenStreetMap.BlackAndWhite")
    
  })
  
  shape= reactive({
    if (input$mapping=="Census Tract"){   #Census Tract Level
      shape=readRDS(file="Census-Tracts")
      b=crime%>%
        filter(DateOccurred<=input$DateRange_Crime[2]&DateOccurred>=input$DateRange_Crime[1])%>%
        group_by(CT10)%>%
        summarize(num_crime=n())
      c=calls%>%
        filter(CreatedDate<=input$DateRange_Call[2]&CreatedDate>=input$DateRange_Call[1])%>%
        group_by(CensusTract)%>%
        summarize(num_call_abs=n(),response=as.numeric(mean(ResponsePeriod,na.rm=TRUE)))
      shape=merge(shape,b,by.x="ct10",by.y="CT10")
      shape@data$num_crime[is.na(shape@data$num_crime)]=0
      shape=merge(shape,c,by.x="ct10",by.y="CensusTract")
      shape@data$num_call_abs[is.na(shape@data$num_call_abs)]=0
      shape@data$response[is.na(shape@data$response)]=0
    }else{
      shape=readRDS(file="Council-Districts")
      b=crime%>%
        filter(DateOccurred<=input$DateRange_Crime[2]&DateOccurred>=input$DateRange_Crime[1])%>%
        group_by(CD)%>%
        summarize(num_crime=n())
      shape=merge(shape,b,by.x="district",by.y="CD")
      shape@data$num_crime[is.na(shape@data$num_crime)]=0
      
      c=calls%>%
        filter(CreatedDate<=input$DateRange_Call[2]&CreatedDate>=input$DateRange_Call[1])%>%
        group_by(CD)%>%
        summarize(num_call_abs=n(),response=as.numeric(mean(ResponsePeriod,na.rm=TRUE)))
      shape=merge(shape,c,by.x="district",by.y="CD")
      shape@data$num_call_abs[is.na(shape@data$num_call_abs)]=0
      shape@data$response[is.na(shape@data$response)]=0
      return(shape)
    }
    
    if (input$county==FALSE){
      shape=shape[shape$CD != 0,]
    }
    return(shape)
  })
  
  labels= reactive({
    if (input$mapping=="Census Tract"){   #Census Tract Level
      labels=
        sprintf(
          "<strong>Tract Number: </strong>%s<br/>
          <strong>Number of Shelters: </strong>%g<br/>
          <strong>Number of Crimes: </strong>%g<br/>
          <strong>Number of 311 Calls: </strong>%g<br/>
          <strong>Average Response Time: </strong>%g<br/>
          <strong>Total count of Homeless: </strong>%g<br/>
          <strong>Rate of Unshelteredness : </strong>%g",
          shape()$ct10, shape()$num_shelt, shape()$num_crime,
          shape()$num_call_abs, shape()$response,
          shape()[[paste("num_homeless",input$homeless_year,sep="_")]],
          shape()[[paste("rate",input$homeless_year,sep="_")]]) %>% 
        lapply(htmltools::HTML)
      return(labels)
    }else{
      labels=
        sprintf(
          "<strong>Council District Number: </strong>%g<br/>
          <strong>Number of Shelters: </strong>%g<br/>
          <strong>Number of Crimes: </strong>%g<br/>
          <strong>Number of 311 Calls: </strong>%g<br/>
          <strong>Average Response Time: </strong>%g<br/>
          <strong>Total count of Homeless: </strong>%g<br/>
          <strong>Rate of Unshelteredness : </strong>%g",
          shape()$district, shape()$num_shelt, shape()$num_crime,
          shape()$num_call_abs, shape()$response,
          shape()[[paste("num_homeless",input$homeless_year,sep="_")]],
          shape()[[paste("rate",input$homeless_year,sep="_")]])   %>% 
        lapply(htmltools::HTML)
      return(labels)
    }
  })

  ################################## Selected DATA  #################################  
  observe({
    if(input$select=="Combined Measure"){
      updateDateRangeInput(session,inputId = "DateRange_Call",start=paste(input$measure_year,"-01-01",sep=""),end=paste(input$measure_year,"-12-31",sep=""))
      updateDateRangeInput(session,inputId = "DateRange_Crime",start=paste(input$measure_year,"-01-01",sep=""),end=paste(input$measure_year,"-12-31",sep=""))
    }
  })
  
  
  Data <- reactive({
    if(input$select=="num_call"){
      if(input$call_var=="num_call_normal"){
        a=shape()[["num_call_abs"]]/shape()[["num_homeless_2017"]]
        a[which(a==Inf)]=(shape()[["num_call_abs"]][which(a==Inf)]+1)/(shape()[["num_homeless_2017"]][which(a==Inf)]+1)  # To avoid Infinity
        return(a)
      }else{
      return(shape()[[input$call_var]])
      }
    }else if(input$select=="num_homeless"){
      if(input$homeless_var=="num_homeless"){
        return(shape()[[paste("num_homeless",input$homeless_year,sep="_")]])
      }else if(input$homeless_var=="unshelt_rate"){
        return(shape()[[paste("rate",input$homeless_year,sep="_")]])
      }else{
        a=shape()[[paste("num_homeless",input$homeless_year,sep="_")]]/shape()[["Pop"]]
        a[which(a==Inf)]=(shape()[[paste("num_homeless",input$homeless_year,sep="_")]][which(a==Inf)]+1)/(shape()[["Pop"]][which(a==Inf)]+1)
        return(a)
      }
      
    }else if(input$select=="num_crime"){
      if(input$crime_var=="num_crime_abs"){
        return(shape()[["num_crime"]])
      }else{
        a=shape()[["num_crime"]]/shape()[["num_homeless_2017"]]
        a[which(a==Inf)]=(shape()[["num_crime"]][which(a==Inf)]+1)/(shape()[["num_homeless_2017"]][which(a==Inf)]+1)
        return(a)
      }
      
    }else if(input$select=="num_shelt"){
      if(input$shelt_var=="num_shelt_abs"){
        return(shape()[["num_shelt"]])
      }else{
        a=shape()[["num_shelt"]]/shape()[["num_homeless_2017"]]
        a[which(a==Inf)]=(shape()[["num_shelt"]][which(a==Inf)]+1)/(shape()[["num_homeless_2017"]][which(a==Inf)]+1)
        return(a)
      }
    }else{
      a=input$shelt_w*(1-shape()[["num_shelt"]]/sum(shape()[["num_shelt"]])) +
        input$crime_w*shape()[["num_crime"]]/sum(shape()[["num_crime"]]) +
        input$call_w*shape()[["num_call_abs"]]/sum(shape()[["num_call_abs"]]) +
        input$homeless_w*shape()[[paste("num_homeless",input$measure_year,sep="_")]]/sum(shape()[[paste("num_homeless",input$measure_year,sep="_")]])
      return(a)
    }
  })
  
  
  pal= reactive({
    
      colorBin("YlOrRd",domain = Data(),bins =4)
    
  })
  
  observe({
    
    leafletProxy("map",session) %>%
      
      clearShapes() %>%
      
      addPolygons(data=shape(),weight=.5,col = 'black',
                  fillColor = ~pal()(Data()),
                  highlightOptions = highlightOptions(color = "red", weight = 2,
                                                      fillOpacity = .03,bringToFront = TRUE),
                  label = labels(),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")
      )
    
  })
  
  observe({
    leafletProxy("map",session) %>%
      clearControls() %>%
      addLegend(pal = pal(),values =Data() , opacity = 0.7, title =NULL,
                position = "bottomright")
  })
  
  observe({
    leafletProxy("map",session) %>%
      setView(lng = View()[3],lat = View()[2],zoom = View()[1])
  })
  
  plot_data=reactive({
    if(input$mapping=="Census Tract"){
      return(cbind(id=shape()$ct10,var=Data())%>%
               as.data.frame()%>%
               filter(!is.na(var))%>%
               arrange(-var)%>%
               slice(1:15))
    }else{
      return(cbind(id=shape()$district,var=Data())%>%
               as.data.frame()%>%
               filter(!is.na(var))%>%
               arrange(-var)%>%
               slice(1:5))
    }
  })
  
  output$bar=renderPlot({
    plot_data() %>%
      ggplot(aes(x=reorder(id,-var),y=var,label=round(var,digits = 2)))+
      geom_col(fill="darkred")+
      labs(x="Area Number",y="Selected Measure",title="Top Areas based on the Selected Measure")+
      geom_text(size = 4,color="white" ,position = position_stack(vjust = .5),fontface = "bold")+
      theme(panel.background = element_rect(fill="grey"),
            panel.grid = element_line(size = .1))
  })
  
}

shinyApp(ui,server)



