##########################################

# Author: Liesl Oeller
# email: eoeller@uidaho.edu
# Last update: Nov 29, 2021

# Script is for ENVS 504 Final
# shiny app- eelgrass abundance

##########################################


library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(plotrix)

grass_data<- read.csv("eelgrass_data.csv")
substrate_data<- read.csv("eelgrass_data_substrate.csv")

model1<- lm(grass_cover~water_temp, data= grass_data)
model2<- lm(grass_cover~salinty, data= grass_data)

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  theme = shinytheme("slate"),
  titlePanel("Eelgrass abundance and distribution in Izembek National Wildlife Refuge, Alaska"),
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      
      navlistPanel(
        
        widths = c(12, 12),
        "",
        
        tabPanel(
          selectInput(inputId = "bay",
                      label = "Site Name",
                      selected = "Izembek",
                      choices = c("Izembek", "Kinzarof")),
          # radioButtons(
          #   inputId = "bay",
          #   label = "Site Name",
          #   selected = "Kinzarof",
          #   choices = list("Izembek",
          #               "Kinzarof")),

          sliderInput(
            inputId = "year", 
            label = "Year", 
            min = min(grass_data$year), 
            max = max(grass_data$year),
            value = min(grass_data$year),
            step = 1L,
            sep = "")
          
        )
      )),
    
    mainPanel =  mainPanel(
      navbarPage(title = " ",
                 tabPanel(h4("Eelgrass cover x Water temp"),
                          tabsetPanel(
                            tabPanel("Plot",
                                     plotOutput("plot1")),
                            tabPanel("Stats",
                                     tableOutput("table1")
                            )
                          )
                 ),
                 tabPanel(h4("Eelgrass cover x Water salinity"),
                          tabsetPanel(
                            tabPanel("Plot",
                                     plotOutput("plot2")),
                            tabPanel("Stats",
                                     tableOutput("table2")
                            )
                          )
                 ),
                 tabPanel(h4("Avg Eelgrass cover x Substrate type"),
                          tabsetPanel(
                            tabPanel("Plot",
                                     plotOutput("plot3")),
                            tabPanel("Stats",
                                     tableOutput("table3"))
                          )
                 )
      )
    )
    
  )
)


# Server ------------------------------------------------------------------


server= function(input, output){
  
  selectedData <- reactive({
    grass_data%>%
      filter(bay== input$bay,
             year== input$year)
  })
  
  output$plot1 = renderPlot({
    
    p1<- ggplot(selectedData(), aes(x= water_temp, y= grass_cover))+
      geom_point()+
      labs(x= "Water Temp (C)", y= "Eelgrass Cover (%)")+
      theme_classic()+
      theme(legend.position= "none", axis.text.x= element_text(size= 12),
            axis.title.x = element_text(size= 15), 
            axis.text.y= element_text(size= 12),
            axis.title.y= element_text(size= 15))
    p1
    
  })
  
  
  output$table1 <- renderTable({
   
    t1 <- capture.output(summary(model1), file= NULL, append= FALSE)
    t1 <- data.frame(t1)
    
    t1
    
  })
  
  output$plot2= renderPlot({
    
    p2<- ggplot(selectedData(), aes(x= salinty, y= grass_cover))+
      geom_point()+
      labs(x="Salinity (PPT)", y= "Eelgrass Cover (%)")+
      theme_classic()+
      theme(legend.position= "none", axis.text.x= element_text(size= 12),
            axis.title.x = element_text(size= 15), 
            axis.text.y= element_text(size= 12),
            axis.title.y= element_text(size= 15))
    
    p2
    
  })
  
  output$table2 <- renderTable({
    
    t2 <- capture.output(summary(model2), file= NULL, append= FALSE)
    t2 <- data.frame(t2)
    
    t2
    
  })
  
  output$plot3= renderPlot({
    
    substrate_summary <- substrate_data %>% 
      group_by(substrate) %>% 
      summarize(mean_grass = mean(grass_cover),
                se_grass = std.error(grass_cover))
    
    p3<- ggplot(substrate_summary, aes(x= substrate, y= mean_grass, fill= substrate))+
      geom_col()+
      labs(x="Substrate Type", y= "Avg Grass Cover (%)")+
      geom_errorbar(data = substrate_summary,
                    aes(x = substrate,
                        ymin = mean_grass - se_grass,
                        ymax = mean_grass + se_grass), width= 0.2)+
      theme_classic()+
      theme(legend.position= "none", axis.text.x= element_text(size= 12, angle = 90, vjust = 0.5, hjust=0.4), 
            axis.title.x = element_text(size= 15),
            axis.text.y= element_text(size= 12),
            axis.title.y= element_text(size= 15))+
      scale_x_discrete(labels = c("cobble" = "Cobble", "cobble/sand" = "Cobble/Sand",
                                  "mud"= "Mud",  "mud/sand"= "Mud/Sand",  "sand"= "Sand",  "sand/mud"= "Sand/Mud",
                                  "sand/shells"= "Sand/Shells",  "shells"= "Shells"))
    
    p3
    
    
  })
  
  output$table3 <- renderTable({
    t3 <- data.frame(
      Substrate = c("Cobble", "Cobble/Sand", "Mud", "Mud/Sand", "Sand", "Sand/Mud", "Sand/Shells", "Shells"), 
      Mean = c(19.2, 47.5, 80, 58.4, 20.2, 48.7, 0, 0),
      SE= c(1.75, 12.09, 0.61, 1.41, 0.78, 1.22, 17.1, 9.87),
      stringsAsFactors = FALSE
    )
    
    t3
    
  })
  
  
}

shinyApp(ui= ui, server= server)