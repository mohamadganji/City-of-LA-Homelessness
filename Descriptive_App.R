library(tidyr)
library(dplyr)
library(ggplot2)
library(shiny)
library(lubridate)

load("calls311.RData")
load("crime.Rdata")
load("homeless_count.Rdata")

ui <- fluidPage(
  titlePanel("Descriptive Analysis",
             windowTitle = "Descriptive Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "select",
                  label = "Choose the Dataset",
                  choices = list("311 Calls","Crime","Homeless Count"),
                  selected="311 Calls"
      ),
      textInput(inputId = "tract", label = "Enter Tract Number (Leave it empty for having the whole data)" , ""),
      textInput(inputId = "CD", label = "Enter Council District Number (Leave it empty for having the whole data)" , ""),
      #############################    #############################    #############################      
      conditionalPanel(
        condition = "input.select == 'Crime'",
        radioButtons(inputId = "crime_year", 
                    label = "Choose a Year",
                    choices = list("2017", 
                                   "2016", 
                                   "2015"), 
                    selected = "2017"),
        selectInput(inputId = "crime_var", 
                    label = "Choose a Variable",
                    choices = list("Month", 
                                   "Week", 
                                   "Age", 
                                   "Race"), 
                    selected = "Month")
      ),
      #############################    #############################    #############################      
      conditionalPanel(
        condition = "input.select == '311 Calls'",
        radioButtons(inputId = "year_obs",
                     label = "Choose a Year: ",
                     choices = list("2017" = 2017,
                                    "2016" = 2016,
                                    "2015" = 2015),
                     selected = "2017"),
        selectInput(inputId = "category",
                    label = "Source and Time Analysis",
                    choices = list("Request by Source" = "a_req",
                                   "Efficiency by Source" = "b_req",
                                   "Total Request by Month" = "c_req",
                                   "Total Request by Weekday" = "d_req",
                                   "Request Source by Month" = "e_req",
                                   "Request Source by Weekday" = "f_req"),
                    selected = "Request by Source")
        
        
      )       

    ),
    
    mainPanel(
      plotOutput(outputId = "plot")
    )

  )
  
  
)

server <- function(input,output){
  
  tract=reactive({
    as.numeric(input$tract)
  })
  cd=reactive({
    as.numeric(input$CD)
  })
  
  output$plot<- renderPlot({
    
    if(input$select=="311 Calls"){
      
      whichData = reactive ({
        a= calls %>%
          filter(years == as.numeric(input$year_obs))
        
        if(!is.na(tract())&tract()%in%calls$CensusTract ){
         a = a %>%
           filter(CensusTract==tract())
        }else if(!is.na(cd()) & 0<cd() & cd()<16){
          a = a %>%
            filter(CD==cd())
        }
        return(a)
      })
      
      if(input$category == "a_req"){
        # Request by Source
        ggplot(data = whichData() %>%
                 group_by(RequestSource) %>%
                 summarise(count = n()) %>%
                 arrange(-count), 
               aes(x=reorder(RequestSource,-count),y=count)) +
          geom_bar(stat="identity",fill = "darkred")+
          theme(axis.text.x = element_text(size= 16,angle = 30, hjust = 1),
                axis.text.y = element_text(size = 16),
                plot.title = element_text(hjust = 0.5),
                title = element_text(size=17, face='bold', hjust = 0.5),
                panel.background = element_rect(fill="grey"),
                panel.grid = element_line(size = .1))+
          xlab("")+
          ylab("Number of Requests")+
          ggtitle("Total Requests By source")+
          geom_text(aes(label = count),color="white", position = position_stack(vjust = .5),size=5,fontface = "bold") 
      }else if(input$category == "b_req"){
        # Efficiency by Source
        ggplot(data = whichData() %>%
                 group_by(RequestSource)%>%
                 summarise(mean=round(mean(UpdateRate),2)),
               aes(x=reorder(RequestSource,-mean),y=mean))+
          geom_bar(stat="identity",fill = "darkred")+
          ggtitle("Request Efficiency by Request Source") +
          theme(axis.text.x = element_text(size = 16,angle = 30, hjust = 1),
                axis.text.y = element_text(size = 16),
                plot.title = element_text(hjust = 0.5),
                title = element_text(size=17, face='bold', hjust = 0.5),
                panel.background = element_rect(fill="grey"),
                panel.grid = element_line(size = .1))+
          xlab("Reqeust Source")+
          ylab("Update Duration (Hours)")+
          geom_text(aes(label = round(mean,0)),color="white", position = position_stack(vjust = .5),size=5,fontface = "bold")
      }else if(input$category == "c_req"){
        # Total Request by Month 
        ggplot(data = whichData(), aes(x = factor(MonthCreated))) +
          geom_bar(fill = "darkred") +
          ylab("Number of Requests") +
          xlab("") +
          ggtitle("Total Requests by Month") +
          theme_classic() +
          theme(axis.text.x = element_text(size = 16),
                axis.text.y = element_text(size = 16),
                axis.title = element_text(size=16),
                plot.title = element_text(hjust = 0.5),
                title = element_text(size=17, face='bold', hjust = 0.5),
                panel.background = element_rect(fill="grey"),
                panel.grid = element_line(size = .1))+
          geom_text(stat='count',aes(label = ..count..),color="white", position = position_stack(vjust = .5),size=5,fontface = "bold")
        
        
      }else if(input$category == "d_req"){
        # Total Request by Weekday
        ggplot(data = whichData(),aes(x = factor(WeekdayCreated))) +
          geom_bar(fill = "darkred") +
          ylab("Number of Requests") +
          xlab("") +
          ggtitle("Total Requests by Weekday") +
          theme_classic() +
          theme(axis.text.x = element_text(size = 16),
                axis.text.y = element_text(size = 16),
                axis.title = element_text(size=16),
                plot.title = element_text(hjust = 0.5),
                title = element_text(size=17, face='bold', hjust = 0.5),
                panel.background = element_rect(fill="grey"),
                panel.grid = element_line(size = .1))+
          geom_text(stat='count',aes(label = ..count..),color="white", position = position_stack(vjust = .5),size=5,fontface = "bold")
        
        
      }else if(input$category == "e_req"){
        # Request Source by Month
        request_cbm = whichData() %>%
          group_by(RequestSource, MonthCreated) %>%
          summarise(count = n())
        
        ggplot(request_cbm, aes(x = reorder(RequestSource, -count), 
                                y = factor(MonthCreated), 
                                fill = count)) +
          geom_tile() +
          scale_fill_gradient(low = "mistyrose", high = "deeppink3", 
                              breaks = NULL, labels = NULL) +
          theme_classic() +
          ylab("") +
          xlab("") +
          ggtitle("Request Source by Month") +
          theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
                axis.text.y = element_text(size = 16),
                axis.title = element_text(size=16),
                plot.title = element_text(hjust = 0.5),
                title = element_text(size=17, face='bold', hjust = 0.5),
                panel.background = element_rect(fill="grey"),
                panel.grid = element_line(size = .1))
        
      }else if(input$category == "f_req"){
        # Request Source by Weekday
        request_cbwh = whichData() %>%
          group_by(RequestSource, WeekdayCreated) %>%
          summarise(count = n())
        
        ggplot(request_cbwh, aes(x = reorder(RequestSource,-count), 
                                 y = factor(WeekdayCreated), 
                                 fill = count)) +
          geom_tile() +
          scale_fill_gradient(low = "mistyrose", high = "deeppink3", 
                              breaks = NULL, labels = NULL) +
          theme_classic() +
          ylab("") +
          xlab("") +
          ggtitle("Request Source by Weekday") +
          theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
                axis.text.y = element_text(size = 16),
                axis.title = element_text(size=16),
                title = element_text(size=17, face='bold', hjust = 0.5),
                panel.background = element_rect(fill="grey"),
                panel.grid = element_line(size = .1))
      }  
      

      
#############################      #############################    #############################    
    }else if(input$select=="Crime"){
      
      ###First plot
      data1=reactive({ 
        a= crime %>%
          filter(year == as.numeric(input$crime_year))
        
        if(!is.na(tract())&tract()%in%crime$CT10 ){
          a = a %>%
            filter(CT10==tract())
        }else if(!is.na(cd()) & 0<cd() & cd()<16){
          a= a %>%
            filter(CD==cd())
        }
        return(a)
      })
      
      if(input$crime_var=="Month"){
        ggplot(data1(),aes(x=month(DateOccurred,label=T,abbr=T)))+
          geom_bar(fill="darkred")+
          xlab("")+
          ylab("Count")+
          ggtitle("Number of Crimes by Month")+
          theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
                axis.text.y = element_text(size = 16),
                axis.title = element_text(size=16),
                title = element_text(size=17, face='bold', hjust = 0.5),
                panel.background = element_rect(fill="grey"),
                panel.grid = element_line(size = .1))+
          geom_text(stat='count',aes(label = ..count..),color="white", position = position_stack(vjust = .5),size=5,fontface = "bold")
      }
      
      
      #second plot
      else if(input$crime_var=="Week"){
        ggplot(data1(),aes(x=wday(DateOccurred,label=T,abbr=T)))+
          geom_bar(fill="darkred")+
          xlab("")+
          ylab("Count")+
          ggtitle("Number of Crimes by Weekday")+
          theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
                axis.text.y = element_text(size = 16),
                axis.title = element_text(size=16),
                title = element_text(size=17, face='bold', hjust = 0.5),
                panel.background = element_rect(fill="grey"),
                panel.grid = element_line(size = .1))+
          geom_text(stat='count',aes(label = ..count..),color="white", position = position_stack(vjust = .5),size=5,fontface = "bold")
      }else if(input$crime_var=="Age"){
        
        # data
        data3=data1() %>%
          filter(!is.na(VictimAge))%>%
          filter(!is.na(VictimSex))%>%
          filter(!VictimSex=="X")
        
        # data1$VictimSex=as.factor(data1$VictimSex)
        # levels(data1$VictimSex)=c("Female","Male")
        
        ##third plot
        ggplot(data3,aes(x=VictimAge))+
          geom_histogram(binwidth=10,fill="darkred",color="black")+
          scale_x_continuous(breaks=seq(0,100,by=10))+
          labs(x="Victim Age",title="Male and Female Victims Age Distribution",y="Count")+
          facet_wrap(~VictimSex)+
          theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
                axis.text.y = element_text(size = 16),
                axis.title = element_text(size=16),
                title = element_text(size=17, face='bold', hjust = 0.5),
                strip.text = element_text(size=16),
                panel.background = element_rect(fill="grey"),
                panel.grid = element_line(size = .1))
      }else if(input$crime_var=="Race"){
        #data
        data4=data1() %>%
          group_by(VictimDescent)%>%
          summarise(count=n())%>%
          arrange(count)
        
        # fourth plot
        ggplot(data4,aes(x=reorder(factor(VictimDescent),-count),y=count))+
          geom_bar(stat="identity",fill="darkred")+
          ylab("Count")+
          xlab("Race")+
          ggtitle("Number of Crime by Race")+
          theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
                axis.text.y = element_text(size = 16),
                axis.title = element_text(size=16),
                title = element_text(size=17, face='bold', hjust = 0.5),
                panel.background = element_rect(fill="grey"),
                panel.grid = element_line(size = .1))+
          geom_text(aes(label = count),color="white", position = position_stack(vjust = .5),size=5,fontface = "bold")
      }
      
      
      
#############################    #############################    #############################    
    }else{
    
      data=homeless_count
      
      if(!is.na(tract())&tract()%in%homeless_count$Tract ){
        data = data %>%
          filter(Tract==tract())
      }else if(!is.na(cd()) & 0<cd() & cd()<16){
        data= data %>%
          filter(CD==cd())
      } 
      
      
        data %>%
          group_by(year)%>%
          summarise(Unsheltered=sum(Unsheltered),Sheltered=sum(Sheltered))%>%
          gather(2:3,key = "condition",value = "count") %>%
          ggplot(aes(x=as.factor(year),y=count,fill=condition,label=count))+
          geom_col()+
          labs(x="",y="Count",title="Homeless Count by Year")+
          geom_text(size = 9,color="white" ,position = position_stack(vjust = 0.5),fontface = "bold")+
          scale_fill_manual(name="",values=c("Darkblue","darkred"))+
          theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
                axis.text.y = element_text(size = 16),
                axis.title = element_text(size=16),
                title = element_text(size=17, face='bold', hjust = 0.5),
                strip.text = element_text(size=16),
                panel.background = element_rect(fill="grey"),
                panel.grid = element_line(size = .1))
        

      
      
      
    }
 
})   
}

shinyApp(ui,server)

