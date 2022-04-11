library(shiny)
library(tidyverse)
library(scales)
library(plotly)
load(url("https://raw.githubusercontent.com/mattdemography/STA_6233_Spring2021/master/Data/fake_data.RData"))

#### Define UI for application that plots features of fake data ----------- ####
ui <- fluidPage(
  # Sidebar layout with a input and output definitions --------------
  sidebarLayout(
    
    # Inputs: Select variables to plot ------------------------------
    sidebarPanel(
      
      # Select variable for y-axis
      textInput(inputId = "plot_title",
                label= "Enter Title Here:",
                placeholder = "Enter Text"),
      
      hr(), #Horizontal Line for visual separation
      
      selectInput(inputId = "Reg", 
                  label = "Region:",
                  choices = c("North", "South", "East", "West"), 
                  selected = "North"),
      
      # Select Colors
      selectInput(inputId = "color_p", 
                  label = "Choose Point Color:",
                  choices = c("Red", "Blue", "Black", "Green"), 
                  selected = "Black"),
      
      selectInput(inputId = "color_l", 
                  label = "Choose Line Color:",
                  choices = c("Red", "Blue", "Black", "Green"), 
                  selected = "Black"),
      
      # Set alpha level
      sliderInput(inputId = "alpha", 
                  label = "Point Transparency:", 
                  min = 0, max = 1, 
                  value = 0.5),
      
      hr(), #Horizontal Line for visual separation
      
      # Set min/max of Cohort Values
      selectInput(inputId = "min", 
                  label = "Choose Cohort Range (Min):", 
                  choices = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018),
                  selected= 2011),
      
      selectInput(inputId = "max",
                  label = "Choose Cohort Range (Max):", 
                  choices = c(2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019),
                  selected= 2018)
    ),
    
    # Output: Show scatterplot --------------------------------------
    mainPanel(
      tabsetPanel(
        tabPanel("Organization: Helping Here", plotlyOutput(outputId = "scatterplot_1")),
        tabPanel("Organization: Mike's Tots", plotlyOutput(outputId = "scatterplot_2")),
        tabPanel("Organization: Silver and Black Give Back", plotlyOutput(outputId = "scatterplot_3")),
        tabPanel("Organization: Purple Cross", plotlyOutput(outputId = "scatterplot_4")),
        tabPanel("Organization: Texas Best", plotlyOutput(outputId = "scatterplot_5")),
        tabPanel("Organization: Tri South", plotlyOutput(outputId = "scatterplot_6")),
        tabPanel("Organization: Tri West", plotlyOutput(outputId = "scatterplot_7")),
        
        tabPanel("Data",  DT::dataTableOutput(outputId="datasheet"))
      )
    )
  )
)

# Define server function required to create the scatterplot ---------
server <- function(input, output) {
  
  dat1 <- reactive({
    ds1 <- d[d$Region %in% input$Reg, ]
    return(ds1)
  })
  
  # Create scatterplot object the plotOutput function is expecting --
  
  output$scatterplot_1 <- renderPlotly({
    d1<-subset(dat1(), dat1()$Orgs=="Helping Here")
    ggplot(data = d1, aes_string(x = d1$Years, y = d1$Cost)) +
      geom_point(colour=input$color_p, alpha=input$alpha) + theme_classic() +
      scale_y_continuous(labels = dollar) + xlim(as.numeric(input$min), as.numeric(input$max)) +
      xlab("Year") + labs(title=input$plot_title)
  }) 
  
  
  d_mike<-subset(d, d$Orgs=="Mike's Tots")
  d2<- aggregate(d_mike$Cost, by=list(d_mike$Region, d_mike$Years, d_mike$Months), FUN=mean)
  d2<-plyr::rename(d2, c(Group.2="Years", Group.1="Region", x="Cost", Group.3="Months"))
  dat2 <- reactive({
    d2a <- d2[d2$Region %in% input$Reg,]
    return(d2a)
  })
  output$scatterplot_2<-renderPlotly({
    ggplot(data = dat2(), aes(x = dat2()$Years , y = dat2()$Cost, text=dat2()$Months)) + geom_point(colour=input$color_p, alpha=input$alpha) +
      theme_classic() + scale_y_continuous(labels = dollar) + xlim(as.numeric(input$min), as.numeric(input$max)) +
      xlab("Year") + geom_line(aes(group=dat2()$Months), colour=input$color_l) + labs(title=input$plot_title)
  })
  
  d_silver<-subset(d, d$Orgs=="Silver and Black Give Back")
  d3<- aggregate(d_silver$Cost, by=list(d_silver$Region, d_silver$Years, d_silver$Months), FUN=mean)
  d3<-plyr::rename(d3, c(Group.2="Years", Group.1="Region", x="Cost", Group.3="Months"))
  dat3 <- reactive({
    d3a <- d3[d3$Region %in% input$Reg,]
    return(d3a)
  })
  output$scatterplot_3<-renderPlotly({
    ggplot(data = dat3(), aes(x = dat3()$Years , y = dat3()$Cost, text=dat3()$Months)) + geom_point(colour=input$color_p, alpha=input$alpha) +
      theme_classic() + scale_y_continuous(labels = dollar) + xlim(as.numeric(input$min), as.numeric(input$max)) +
      xlab("Year") + geom_line(aes(color=dat3()$Months))
    #geom_line(aes(linetype=sex, color=sex))+
      
  })
  
  d_purple<-subset(d, d$Orgs=="Purple Cross")
  d4<- aggregate(d_purple$Cost, by=list(d_purple$Region, d_purple$Years, d_purple$Months), FUN=mean)
  d4<-plyr::rename(d4, c(Group.2="Years", Group.1="Region", x="Cost", Group.3="Months"))
  dat4 <- reactive({
    d4a <- d4[d4$Region %in% input$Reg,]
    return(d4a)
  })
  output$scatterplot_4<-renderPlotly({
    ggplot(data = dat4(), aes(x = dat4()$Years , y = dat4()$Cost, text=dat4()$Months)) + geom_point(colour=input$color_p, alpha=input$alpha) +
      theme_classic() + scale_y_continuous(labels = dollar) + xlim(as.numeric(input$min), as.numeric(input$max)) +
      xlab("Year") + geom_line(aes(group=dat4()$Months), colour=input$color_l) 
  })
  
  
  d_tb<-subset(d, d$Orgs=="Texas Best")
  d5<- aggregate(d_tb$Cost, by=list(d_tb$Region, d_tb$Years, d_tb$Months), FUN=mean)
  d5<-plyr::rename(d5, c(Group.2="Years", Group.1="Region", x="Cost", Group.3="Months"))
  dat5 <- reactive({
    d5a <- d5[d5$Region %in% input$Reg,]
    return(d5a)
  })
  output$scatterplot_5<-renderPlotly({
    ggplot(data = dat5(), aes(x = dat5()$Years , y = dat5()$Cost, text=dat5()$Months)) + geom_point(colour=input$color_p, alpha=input$alpha) +
      theme_classic() + scale_y_continuous(labels = dollar) + xlim(as.numeric(input$min), as.numeric(input$max)) +
      xlab("Year") + geom_line(aes(group=dat5()$Months), colour=input$color_l) 
  })
  
  
  d_ts<-subset(d, d$Orgs=="Tri South")
  d6<- aggregate(d_ts$Cost, by=list(d_ts$Region, d_ts$Years, d_ts$Months), FUN=mean)
  d6<-plyr::rename(d6, c(Group.2="Years", Group.1="Region", x="Cost", Group.3="Months"))
  dat6 <- reactive({
    d6a <- d6[d6$Region %in% input$Reg,]
    return(d6a)
  })
  output$scatterplot_6<-renderPlotly({
    ggplot(data = dat6(), aes(x = dat6()$Years , y = dat6()$Cost, text=dat6()$Months)) + geom_point(colour=input$color_p, alpha=input$alpha) +
      theme_classic() + scale_y_continuous(labels = dollar) + xlim(as.numeric(input$min), as.numeric(input$max)) +
      xlab("Year") + geom_line(aes(group=dat6()$Months), colour=input$color_l) 
  })
  
  
  
  d_tw<-subset(d, d$Orgs=="Tri West")
  d7<- aggregate(d_tw$Cost, by=list(d_tw$Region, d_tw$Years, d_tw$Months), FUN=mean)
  d7<-plyr::rename(d7, c(Group.2="Years", Group.1="Region", x="Cost", Group.3="Months"))
  dat7 <- reactive({
    d7a <- d7[d7$Region %in% input$Reg,]
    return(d7a)
  })
  output$scatterplot_7<-renderPlotly({
    ggplot(data = dat7(), aes(x = dat7()$Years , y = dat7()$Cost, text=dat7()$Months)) + geom_point(colour=input$color_p, alpha=input$alpha) +
      theme_classic() + scale_y_continuous(labels = dollar) + xlim(as.numeric(input$min), as.numeric(input$max)) +
      xlab("Year") + geom_line(aes(group=dat7()$Months), colour=input$color_l) 
  })
  
  output$datasheet<-DT::renderDataTable({
    DT::datatable(data=d[,1:5],
                  options=list(pageLength= 20),
                  rownames=FALSE)
  })
}

# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)

