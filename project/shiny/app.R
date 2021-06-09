# Load packages---------------------------
library(shiny)
library(tidyverse)
library(data.table)
library(ggmap)

# Load data---------------------------

clusterdata = data.table::fread("dataset/cluster_data.csv")
bike_road = fread("dataset/bike_road.csv")

GN = fread("dataset/GNclust.csv")
GS = fread("dataset/GSclust.csv")
JN = fread("dataset/JNclust.csv")
NW = fread("dataset/NWclust.csv")
SC = fread("dataset/SCclust.csv")
GD = fread("dataset/GDclust.csv")
YDP = fread("dataset/YDPclust.csv")

load("dataset/GNmap.rda")
load("dataset/GSmap.rda")
load("dataset/JNmap.rda")
load("dataset/NWmap.rda")
load("dataset/SCmap.rda")
load("dataset/GDmap.rda")
load("dataset/YDPmap.rda")


# UI ---------------------------------------
ui <- fluidPage(
  titlePanel("공공자전거 대여소의 지역적 특징을 이용한 자전거 도로 신설 제안"),
  
  sidebarLayout(
    sidebarPanel(
      
      p(strong(span("지역구 선택", style = "color:blue")), "에서 자전거 도로를 신설할 입지를 확인하실 수 있습니다."),
      p("또한  아래의 세 개 체크 박스를 통하여 추가적인 정보를 On/Off 하실 수 있습니다."),
      p(strong(span("초록색 원", style = "color:green")),"의 경우는 서울시 공공자전거 따릉이의 대여소이며 지역구를 선택하신 후 아래", strong("대여소 정보 표시"),
        "를 체크하시면 선정된 구역의 대여소 정보에 대하여 확인하실 수 있습니다."),
      p(strong(span("주황색 선", style = "color:orange")),"의 경우에는 현재 설치된 자전거 우선도로 및 자전거 전용도로입니다. 이 역시 아래 체크박스를 통해 On/Off 하실 수 있습니다."),
      
      
      
      # 지역구 선택
      selectInput("GU_bikeroad",
                  label = strong(span("지역구 선택", style = "color:blue")),
                  choices = c("강남구", "강서구", "서초구", "종로구",
                              "강동구", "노원구", "영등포구"),
                  selected = "강남구"
      ),
      
      
      # 자전거 도로 표시
      checkboxInput("bikeroad",
                    strong(span("자전거 도로 표시", style = "color:orange")),
                    value = T),
      
      # 대여소 표시
      checkboxInput("place",
                    strong(span("대여소 표시",style = "color:green")),
                    value = T),
      
      p("추가적인 정보를 원하시면 아래의 체크 박스도 조작해보세요."),
      
      # 대여소 정보 표시
      checkboxInput("place_info",
                    strong("대여소 정보 표시"),
                    value = F)
    ),
    
    mainPanel(
      
      plotOutput("map"),
      dataTableOutput("table")
    )
  )
)



# Server ---------------------------------------
server <- function(input, output) {
  
  show_table  = reactive({
    
    if (input$place_info == T){
      return(clusterdata %>% filter(GU == input$GU_bikeroad))
      
    }else{
      return()
    }
    
  })
  
  plot_map = reactive({
    
    if(input$GU_bikeroad == "강남구"){
      data = GN
      if (input$bikeroad == T & input$place == T){
        
        
        return(ggmap(GNmap) + 
                 geom_polygon(data = bike_road, aes(x=long,y=lat,group=group), color = "#ff7859", size= .8) + 
                 
                 geom_point(data = data, 
                            aes(x=long, y=lat), color = "green1", size = 4, alpha = .8) + 
                 
                 geom_point(data = data, 
                            aes(x=long, y=lat), color = "white", size = 2.5) + 
                 
                 
                 labs(
                   title = str_c(input$GU_bikeroad,"의 자전거 도로신설 제안 구역과 공공자전거 대여소")
                 ) + 
                 theme_void() + 
                 theme(
                   plot.title = element_text(face= "bold", hjust = .5, size= 15)
                 ))
        
        
      }
      if(input$bikeroad == T & input$place == F){
        
        return(ggmap(GNmap) + 
                 geom_polygon(data = bike_road, aes(x=long,y=lat,group=group), color = "#ff7859", size= .8) + 
                 
                 
                 labs(
                   title = str_c(input$GU_bikeroad,"의 자전거 도로신설 제안 구역과 공공자전거 대여소")
                 ) + 
                 theme_void() + 
                 theme(
                   plot.title = element_text(face= "bold", hjust = .5, size= 15)
                 ))
        
      }
      if (input$bikeroad == F & input$place == T) {
        return( ggmap(GNmap) + 
                  
                  geom_point(data = data , 
                             aes(x=long, y=lat), color = "green1", size = 4, alpha = .8) + 
                  
                  geom_point(data = data , 
                             aes(x=long, y=lat), color = "white", size = 2.5) + 
                  
                  
                  labs(
                    title = str_c(input$GU_bikeroad,"의 자전거 도로신설 제안 구역과 공공자전거 대여소")
                  ) + 
                  theme_void() + 
                  theme(
                    plot.title = element_text(face= "bold", hjust = .5, size= 15)
                  ))
      }
      if (input$bikeroad == F & input$place == F){
        return( ggmap(GNmap) + 
                  
                  labs(
                    title = str_c(input$GU_bikeroad,"의 자전거 도로신설 제안 구역과 공공자전거 대여소")
                  ) + 
                  theme_void() + 
                  theme(
                    plot.title = element_text(face= "bold", hjust = .5, size= 15)
                  ))
        
      }
      
    }
    
    if(input$GU_bikeroad == "강서구"){
      data = GS
      if (input$bikeroad == T & input$place == T){
        
        
        return(ggmap(GSmap) + 
                 geom_polygon(data = bike_road, aes(x=long,y=lat,group=group), color = "#ff7859", size= .8) + 
                 
                 geom_point(data = data, 
                            aes(x=long, y=lat), color = "green1", size = 4, alpha = .8) + 
                 
                 geom_point(data = data, 
                            aes(x=long, y=lat), color = "white", size = 2.5) + 
                 
                 
                 labs(
                   title = str_c(input$GU_bikeroad,"의 자전거 도로신설 제안 구역과 공공자전거 대여소")
                 ) + 
                 theme_void() + 
                 theme(
                   plot.title = element_text(face= "bold", hjust = .5, size= 15)
                 ))
        
        
      }
      if(input$bikeroad == T & input$place == F){
        
        return(ggmap(GSmap) + 
                 geom_polygon(data = bike_road, aes(x=long,y=lat,group=group), color = "#ff7859", size= .8) + 
                 
                 
                 labs(
                   title = str_c(input$GU_bikeroad,"의 자전거 도로신설 제안 구역과 공공자전거 대여소")
                 ) + 
                 theme_void() + 
                 theme(
                   plot.title = element_text(face= "bold", hjust = .5, size= 15)
                 ))
        
      }
      if (input$bikeroad == F & input$place == T) {
        return( ggmap(GSmap) + 
                  
                  geom_point(data = data , 
                             aes(x=long, y=lat), color = "green1", size = 4, alpha = .8) + 
                  
                  geom_point(data = data , 
                             aes(x=long, y=lat), color = "white", size = 2.5) + 
                  
                  
                  labs(
                    title = str_c(input$GU_bikeroad,"의 자전거 도로신설 제안 구역과 공공자전거 대여소")
                  ) + 
                  theme_void() + 
                  theme(
                    plot.title = element_text(face= "bold", hjust = .5, size= 15)
                  ))
      }
      if (input$bikeroad == F & input$place == F){
        return( ggmap(GSmap) + 
                  
                  labs(
                    title = str_c(input$GU_bikeroad,"의 자전거 도로신설 제안 구역과 공공자전거 대여소")
                  ) + 
                  theme_void() + 
                  theme(
                    plot.title = element_text(face= "bold", hjust = .5, size= 15)
                  ))
        
      }
    }
    
    if(input$GU_bikeroad == "강동구"){
      data = GD
      if (input$bikeroad == T & input$place == T){
        
        
        return(ggmap(GDmap) + 
                 geom_polygon(data = bike_road, aes(x=long,y=lat,group=group), color = "#ff7859", size= .8) + 
                 
                 geom_point(data = data, 
                            aes(x=long, y=lat), color = "green1", size = 4, alpha = .8) + 
                 
                 geom_point(data = data, 
                            aes(x=long, y=lat), color = "white", size = 2.5) + 
                 
                 
                 labs(
                   title = str_c(input$GU_bikeroad,"의 자전거 도로신설 제안 구역과 공공자전거 대여소")
                 ) + 
                 theme_void() + 
                 theme(
                   plot.title = element_text(face= "bold", hjust = .5, size= 15)
                 ))
        
        
      }
      if(input$bikeroad == T & input$place == F){
        
        return(ggmap(GDmap) + 
                 geom_polygon(data = bike_road, aes(x=long,y=lat,group=group), color = "#ff7859", size= .8) + 
                 
                 
                 labs(
                   title = str_c(input$GU_bikeroad,"의 자전거 도로신설 제안 구역과 공공자전거 대여소")
                 ) + 
                 theme_void() + 
                 theme(
                   plot.title = element_text(face= "bold", hjust = .5, size= 15)
                 ))
        
      }
      if (input$bikeroad == F & input$place == T) {
        return( ggmap(GDmap) + 
                  
                  geom_point(data = data , 
                             aes(x=long, y=lat), color = "green1", size = 4, alpha = .8) + 
                  
                  geom_point(data = data , 
                             aes(x=long, y=lat), color = "white", size = 2.5) + 
                  
                  
                  labs(
                    title = str_c(input$GU_bikeroad,"의 자전거 도로신설 제안 구역과 공공자전거 대여소")
                  ) + 
                  theme_void() + 
                  theme(
                    plot.title = element_text(face= "bold", hjust = .5, size= 15)
                  ))
      }
      if (input$bikeroad == F & input$place == F){
        return( ggmap(GDmap) + 
                  
                  labs(
                    title = str_c(input$GU_bikeroad,"의 자전거 도로신설 제안 구역과 공공자전거 대여소")
                  ) + 
                  theme_void() + 
                  theme(
                    plot.title = element_text(face= "bold", hjust = .5, size= 15)
                  ))
        
      }
    }
    
    if(input$GU_bikeroad == "종로구"){
      data = JN
      if (input$bikeroad == T & input$place == T){
        
        
        return(ggmap(JNmap) + 
                 geom_polygon(data = bike_road, aes(x=long,y=lat,group=group), color = "#ff7859", size= .8) + 
                 
                 geom_point(data = data, 
                            aes(x=long, y=lat), color = "green1", size = 4, alpha = .8) + 
                 
                 geom_point(data = data, 
                            aes(x=long, y=lat), color = "white", size = 2.5) + 
                 
                 
                 labs(
                   title = str_c(input$GU_bikeroad,"의 자전거 도로신설 제안 구역과 공공자전거 대여소")
                 ) + 
                 theme_void() + 
                 theme(
                   plot.title = element_text(face= "bold", hjust = .5, size= 15)
                 ))
        
        
      }
      if(input$bikeroad == T & input$place == F){
        
        return(ggmap(JNmap) + 
                 geom_polygon(data = bike_road, aes(x=long,y=lat,group=group), color = "#ff7859", size= .8) + 
                 
                 
                 labs(
                   title = str_c(input$GU_bikeroad,"의 자전거 도로신설 제안 구역과 공공자전거 대여소")
                 ) + 
                 theme_void() + 
                 theme(
                   plot.title = element_text(face= "bold", hjust = .5, size= 15)
                 ))
        
      }
      if (input$bikeroad == F & input$place == T) {
        return( ggmap(JNmap) + 
                  
                  geom_point(data = data , 
                             aes(x=long, y=lat), color = "green1", size = 4, alpha = .8) + 
                  
                  geom_point(data = data , 
                             aes(x=long, y=lat), color = "white", size = 2.5) + 
                  
                  
                  labs(
                    title = str_c(input$GU_bikeroad,"의 자전거 도로신설 제안 구역과 공공자전거 대여소")
                  ) + 
                  theme_void() + 
                  theme(
                    plot.title = element_text(face= "bold", hjust = .5, size= 15)
                  ))
      }
      if (input$bikeroad == F & input$place == F){
        return( ggmap(JNmap) + 
                  
                  labs(
                    title = str_c(input$GU_bikeroad,"의 자전거 도로신설 제안 구역과 공공자전거 대여소")
                  ) + 
                  theme_void() + 
                  theme(
                    plot.title = element_text(face= "bold", hjust = .5, size= 15)
                  ))
        
      }
    }
    
    if(input$GU_bikeroad == "서초구"){
      data = SC
      if (input$bikeroad == T & input$place == T){
        
        
        return(ggmap(SCmap) + 
                 geom_polygon(data = bike_road, aes(x=long,y=lat,group=group), color = "#ff7859", size= .8) + 
                 
                 geom_point(data = data, 
                            aes(x=long, y=lat), color = "green1", size = 4, alpha = .8) + 
                 
                 geom_point(data = data, 
                            aes(x=long, y=lat), color = "white", size = 2.5) + 
                 
                 
                 labs(
                   title = str_c(input$GU_bikeroad,"의 자전거 도로신설 제안 구역과 공공자전거 대여소")
                 ) + 
                 theme_void() + 
                 theme(
                   plot.title = element_text(face= "bold", hjust = .5, size= 15)
                 ))
        
        
      }
      if(input$bikeroad == T & input$place == F){
        
        return(ggmap(SCmap) + 
                 geom_polygon(data = bike_road, aes(x=long,y=lat,group=group), color = "#ff7859", size= .8) + 
                 
                 
                 labs(
                   title = str_c(input$GU_bikeroad,"의 자전거 도로신설 제안 구역과 공공자전거 대여소")
                 ) + 
                 theme_void() + 
                 theme(
                   plot.title = element_text(face= "bold", hjust = .5, size= 15)
                 ))
        
      }
      if (input$bikeroad == F & input$place == T) {
        return( ggmap(SCmap) + 
                  
                  geom_point(data = data , 
                             aes(x=long, y=lat), color = "green1", size = 4, alpha = .8) + 
                  
                  geom_point(data = data , 
                             aes(x=long, y=lat), color = "white", size = 2.5) + 
                  
                  
                  labs(
                    title = str_c(input$GU_bikeroad,"의 자전거 도로신설 제안 구역과 공공자전거 대여소")
                  ) + 
                  theme_void() + 
                  theme(
                    plot.title = element_text(face= "bold", hjust = .5, size= 15)
                  ))
      }
      if (input$bikeroad == F & input$place == F){
        return( ggmap(SCmap) + 
                  
                  labs(
                    title = str_c(input$GU_bikeroad,"의 자전거 도로신설 제안 구역과 공공자전거 대여소")
                  ) + 
                  theme_void() + 
                  theme(
                    plot.title = element_text(face= "bold", hjust = .5, size= 15)
                  ))
        
      }
    }
    
    if(input$GU_bikeroad == "영등포구"){
      data = YDP
      if (input$bikeroad == T & input$place == T){
        
        
        return(ggmap(YDPmap) + 
                 geom_polygon(data = bike_road, aes(x=long,y=lat,group=group), color = "#ff7859", size= .8) + 
                 
                 geom_point(data = data, 
                            aes(x=long, y=lat), color = "green1", size = 4, alpha = .8) + 
                 
                 geom_point(data = data, 
                            aes(x=long, y=lat), color = "white", size = 2.5) + 
                 
                 
                 labs(
                   title = str_c(input$GU_bikeroad,"의 자전거 도로신설 제안 구역과 공공자전거 대여소")
                 ) + 
                 theme_void() + 
                 theme(
                   plot.title = element_text(face= "bold", hjust = .5, size= 15)
                 ))
        
        
      }
      if(input$bikeroad == T & input$place == F){
        
        return(ggmap(YDPmap) + 
                 geom_polygon(data = bike_road, aes(x=long,y=lat,group=group), color = "#ff7859", size= .8) + 
                 
                 
                 labs(
                   title = str_c(input$GU_bikeroad,"의 자전거 도로신설 제안 구역과 공공자전거 대여소")
                 ) + 
                 theme_void() + 
                 theme(
                   plot.title = element_text(face= "bold", hjust = .5, size= 15)
                 ))
        
      }
      if (input$bikeroad == F & input$place == T) {
        return( ggmap(YDPmap) + 
                  
                  geom_point(data = data , 
                             aes(x=long, y=lat), color = "green1", size = 4, alpha = .8) + 
                  
                  geom_point(data = data , 
                             aes(x=long, y=lat), color = "white", size = 2.5) + 
                  
                  
                  labs(
                    title = str_c(input$GU_bikeroad,"의 자전거 도로신설 제안 구역과 공공자전거 대여소")
                  ) + 
                  theme_void() + 
                  theme(
                    plot.title = element_text(face= "bold", hjust = .5, size= 15)
                  ))
      }
      if (input$bikeroad == F & input$place == F){
        return( ggmap(YDPmap) + 
                  
                  labs(
                    title = str_c(input$GU_bikeroad,"의 자전거 도로신설 제안 구역과 공공자전거 대여소")
                  ) + 
                  theme_void() + 
                  theme(
                    plot.title = element_text(face= "bold", hjust = .5, size= 15)
                  ))
        
      }
    }
    
    if(input$GU_bikeroad == "노원구"){
      data = NW
      if (input$bikeroad == T & input$place == T){
        
        
        return(ggmap(NWmap) + 
                 geom_polygon(data = bike_road, aes(x=long,y=lat,group=group), color = "#ff7859", size= .8) + 
                 
                 geom_point(data = data, 
                            aes(x=long, y=lat), color = "green1", size = 4, alpha = .8) + 
                 
                 geom_point(data = data, 
                            aes(x=long, y=lat), color = "white", size = 2.5) + 
                 
                 
                 labs(
                   title = str_c(input$GU_bikeroad,"의 자전거 도로신설 제안 구역과 공공자전거 대여소")
                 ) + 
                 theme_void() + 
                 theme(
                   plot.title = element_text(face= "bold", hjust = .5, size= 15)
                 ))
        
        
      }
      if(input$bikeroad == T & input$place == F){
        
        return(ggmap(NWmap) + 
                 geom_polygon(data = bike_road, aes(x=long,y=lat,group=group), color = "#ff7859", size= .8) + 
                 
                 
                 labs(
                   title = str_c(input$GU_bikeroad,"의 자전거 도로신설 제안 구역과 공공자전거 대여소")
                 ) + 
                 theme_void() + 
                 theme(
                   plot.title = element_text(face= "bold", hjust = .5, size= 15)
                 ))
        
      }
      if (input$bikeroad == F & input$place == T) {
        return( ggmap(NWmap) + 
                  
                  geom_point(data = data , 
                             aes(x=long, y=lat), color = "green1", size = 4, alpha = .8) + 
                  
                  geom_point(data = data , 
                             aes(x=long, y=lat), color = "white", size = 2.5) + 
                  
                  
                  labs(
                    title = str_c(input$GU_bikeroad,"의 자전거 도로신설 제안 구역과 공공자전거 대여소")
                  ) + 
                  theme_void() + 
                  theme(
                    plot.title = element_text(face= "bold", hjust = .5, size= 15)
                  ))
      }
      if (input$bikeroad == F & input$place == F){
        return( ggmap(NWmap) + 
                  
                  labs(
                    title = str_c(input$GU_bikeroad,"의 자전거 도로신설 제안 구역과 공공자전거 대여소")
                  ) + 
                  theme_void() + 
                  theme(
                    plot.title = element_text(face= "bold", hjust = .5, size= 15)
                  ))
        
      }
    }
    
  })
  
  
  output$map <- renderPlot({
    
    plot_map()
    
  })
  
  output$table <- renderDataTable({
    
    show_table()
    
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
