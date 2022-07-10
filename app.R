
library(shiny)
library(ggplot2)
library(ggthemes)
library(plotly)
library(rsconnect)
library(shinythemes)
library(tidyverse)
library(dplyr)

# setwd("~/Current Work/R_projects/BLOCKING_GIS_2")

ui <- fluidPage(

  titlePanel("MACO MINE3")
  # theme = shinythemes::shinytheme('flatly'),
  # sidebarLayout(
  #   sidebarPanel(
  #     selectInput('VEIN', 'Select VEIN', POS_FACE_MAP_AVERAGE$fn_ROCKCODE,selected=as.factor(levels(POS_FACE_MAP_AVERAGE$fn_ROCKCODE)[1]))),
  # mainPanel(
  #     tabsetPanel(
  #       tabPanel("MAP",plotlyOutput("plot2", height = 960, width = 1500)),
  #       tabPanel("VEIN",plotlyOutput("BLOCKING_PLOT",height = 960, width = 1500))
  #     )
  #   )
  # )
)




server <- function(input, output) {


  # p1 <-  function(){
  #   plot_ly(data = df_points) %>%
  #   add_sf(type = "scatter", color = ~fn_ROCKCODE, text = ~HOLE_ID)%>%
  #   layout(title = "AMCI Face Samples",
  #          plot_bgcolor='#e5ecf6',
  #          xaxis = list(
  #            zerolinecolor = '#ffff',
  #            zerolinewidth = 2,
  #            gridcolor = 'ffff',
  #            dtick = 5),
  #          yaxis = list(
  #            zerolinecolor = '#ffff',
  #            zerolinewidth = 2,
  #            gridcolor = 'ffff'))
  # 
  # }
  # #
  # output$plot2 <- plotly::renderPlotly({p1()})
  # 
  # #
  # # label = paste(HOLE_ID,COMP_AU,sep = " ")
  # 
  # p2 <- function(){
  #   POS_FACE_MAP  %>%
  #     filter(fn_ROCKCODE == input$VEIN) %>%
  #     mutate(COMP_AU = signif(COMP_AU,3)) %>%
  #   ggplot(aes(x = POS_N_S, y = LEVEL, label = HOLE_ID)) +
  #     # geom_text(hjust = 0, vjust = 0,nudge_x = 0.5,nudge_y = 0.5) +
  #     geom_point(aes(x = BLOCK_LOCATIONX, y = LEVEL))  +
  #     geom_text(aes(x = BLOCK_LOCATIONX, y = LEVEL, label = COMP_AU),hjust = 0, vjust = 0,nudge_x = 0.5,nudge_y = 0.5) +
  # 
  #   scale_x_continuous(name="POSITION", breaks=seq(-100,120,5)) +
  #     scale_y_continuous(name="LEVEL", breaks=seq(380,1010,15))
  # 
  #   # geom_label(aes(label = HOLE_ID))
  #   # geom_text(aes(label = paste(HOLE_ID,COMP_AU, sep = " ")), parse = TRUE)
  # 
  # 
  # 
  #     # geom_text(hjust = 0, vjust = 0, aes(colour = factor(AVE)))+
  #     # scale_colour_brewer(palette = "BuGnRd")
  # 
  # }
  # #
  # output$BLOCKING_PLOT<- plotly::renderPlotly({p2()})

}

shinyApp(ui, server)