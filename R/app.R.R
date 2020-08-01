## app.R ##

# packages if required
#install.packages('shinydashboard')
#install.packages("devtools")
#devtools::install_github("hrbrmstr/hrbrthemes")
#install.packages('dplyr')
#install.packages('plotly')
#install.packages('choroplethr')
#install.packages('choroplethrAdmin1')
#install.packages("devtools") 
#devtools::install_github("cardiomoon/Kormaps")
#install.packages('tidyverse')
#install.packages('sf')
#install.packages('shinycssloaders')
#install.packages('shinyWidgets')
#install.packages('treemap')
#install.packages('highcharter')
#install.packages('reshape')
#install.packages('viridis')
#install.packages('tableHTML')
#install.packages('shinydashboardPlus')

library(shinydashboard)
library(shiny)
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(plotly)
library(choroplethr)
library(choroplethrAdmin1)
library(Kormaps)
library(tidyverse)
library(sf)
library(leaflet)
library(shinycssloaders)
library(shinyWidgets)
library(treemap)
library(highcharter) 
library(reshape)
library(viridis)  
library(tableHTML)
library(shinydashboardPlus)


# time info
time <- read.csv("Time.csv", header=T)
time$date <- as.Date(time$date,'%d/%m/%Y')

# region info - confirmed cases
region <- read.csv("TimeProvince.csv", header=T)
region$province <- gsub('Sejong','Sejong-si',region$province)
region_m <- read.csv("TimeProvince.csv", header=T)
region_m$province <- gsub('Sejong','Sejong-si',region_m$province)

region$date <- as.Date(region$date,'%d/%m/%Y')
region_m$date <- as.Date(region_m$date,'%d/%m/%Y')
region1 <- region[1548:nrow(region),c(2,5)]
names(region1)[1]<-"CTP_ENG_NM"
names(region1)[2]<-"confirmed"

# region info - deceased cases
region2 <- region[1548:nrow(region),c(2,6)]
names(region2)[1]<-"CTP_ENG_NM"
names(region2)[2]<-"deceased"


#admin1_map("south korea")
korea_sf <- st_read("CTPRVN.shp")
korea_sf$CTP_KOR_NM <- NULL
korea_sf$CTP_ENG_NM <- iconv(korea_sf$CTP_ENG_NM, from = "CP949", to = "UTF-8", sub = NA, mark = TRUE, toRaw = FALSE)


# web mercator
korea_sf <- st_transform(korea_sf, "+proj=longlat +datum=WGS84")
korea_df <- korea_sf %>% 
    st_set_geometry(NULL)

#join
confirmed_case <- korea_sf %>% left_join(region1, by = "CTP_ENG_NM")
deceased_case <- korea_sf %>% left_join(region2, by = "CTP_ENG_NM")

# palette
pal1 <- colorNumeric(
    palette = colorRampPalette(c('skyblue1', 'navyblue','black'))(length(confirmed_case$confirmed)), 
    confirmed_case$confirmed)

pal2 <- colorNumeric(
    palette = colorRampPalette(c('wheat1', 'red','black'))(length(deceased_case$deceased)), 
    deceased_case$deceased)

# label
label1 <- sprintf(
    "<strong>%s</strong><br/>%g cases</sup>",
    confirmed_case$CTP_ENG_NM, confirmed_case$confirmed
) %>% lapply(htmltools::HTML)

label2 <- sprintf(
    "<strong>%s</strong><br/>%g cases</sup>",
    deceased_case$CTP_ENG_NM, deceased_case$deceased
) %>% lapply(htmltools::HTML)

# case
case <- read.csv("Case.csv", header=T)

# patient
patient <- read.csv("PatientInfo.csv", header=T)
patient$date <- as.Date(patient$date,'%d/%m/%Y')
patient_new <- patient%>%filter(infection_case != "")

#routes
routes <- read.csv("PatientRoute.csv")
types.summary<-table(routes$type)
types.summary.m<-melt(types.summary)

# place types
loc <- routes %>%
  group_by(type,latitude,longitude, province) %>%
  summarize(count=n())


loc_df <- as.data.frame(loc)

#palette
pal_r <- colorFactor(
  palette = "viridis",
  domain = loc_df$type
)

#label
label_r <- sprintf(
  "<strong>Place Type: %s <br/> Region: %s <br/> Number of visits: %g </strong></sup>",
  loc_df$type, loc_df$province, loc_df$count
) %>% lapply(htmltools::HTML)

#region&place type
pt <- routes %>%
  group_by(type,province) %>%
  summarize(count=n())

pt_df <- as.data.frame(pt)

pt_mat <- pt_df %>%pivot_wider(names_from = province, values_from = count)
pt_mat[is.na(pt_mat)] <- 0

pt_mat <- as.data.frame(pt_mat)

type_region = melt(pt_mat, id = c("type"))




## ui.R ##
ui <- dashboardPage(
    skin = "red",
    dashboardHeader(title = "Covid-19 in South Korea",
                    titleWidth=250),   
    # tabs
    dashboardSidebar(width = 250,
                     sidebarMenu(
                         menuItem("Covid-19", tabName = "Covid19", icon = icon("readme")),
                         menuItem("Number of cases", tabName = "Number_of_cases", icon = icon("calendar")), 
                         menuItem("Regions", tabName = "Map", icon = icon("map-marked-alt"),
                                  menuSubItem("Confirmed cases", tabName = "confirmed_region"),
                                  menuSubItem("Deceased cases", tabName = "deceased_region")),
                         menuItem("Infection Cases", tabName = "type", icon = icon("user-friends")),
                         menuItem("Patients' Routes", tabName = "route", icon = icon("project-diagram"))
                     )
    ),
    
    
    dashboardBody(
        tags$head(tags$style(HTML('.content-wrapper, .right-side {
                                background-color: white;
                                }'))),
        tags$style(make_css(list('.box.title', 
                                 c('font-weight'), 
                                 c('bold')))),
        tabItems(
            #tab1
            tabItem(tabName = "Covid19",  
                    h2('What is Covid-19?'),
                    HTML("<p><br></p>"),
                    HTML('<p style="font-size:15px">The world suffers from an unprecedented increase in the number of infected and deceased cases for COVID-19 pandemic. According to WHO (World Health Organization), "Coronavirus disease (COVID-19) is an infectious disease caused by a newly discovered coronavirus. Most people infected with the COVID-19 virus will experience mild to moderate respiratory illness and recover without requiring special treatment."  After the outbreak of COVID-19, the governments of each country have been trying to come up with measures to prevent the spread of COVID-19 and protect people. Therefore, identifying the situation of each country for COVID-19, will be important to reduce infected cases and set effective strategies. <b>The aim of this project is to identify the COVID-19 situation in South Korea intuitively with informative visualisations.</b></p>'),
                    HTML('<p style="font-size:15px">More information about Covid-19 can be found in the following link: </p>' ),
                    HTML("<p><a href = 'https://www.who.int/health-topics/coronavirus#tab=tab_1'>https://www.who.int/health-topics/coronavirus#tab=tab_1</a></p>"),
                    HTML('<p><img src="covid_19.png", height="350px", width="1120px"/></p>')),
            #tab2
            tabItem(tabName = "Number_of_cases",  
                    titlePanel('Number of Covid-19 cases in South Korea'),
                
                    fluidRow(
                        valueBox(563035 ,"Total Test", icon = icon("stethoscope"),color='green'),
                        valueBox(10674 ,"Total Confirmed", icon = icon("hospital"),color='blue'),
                        valueBox(236, "Total Deceased", icon = icon("bed"),color='red')
                    ),
                    
                    fluidRow(
                             
                             box(title='All',width=8, height="500px", plotlyOutput('a')),
                             
                             box(width=4,
                                 selectInput("scale","Scale Type:", c("linear","log"))),
                             box(title = HTML('<b>How has the number of tests, confirmed and deceased cases for COVID-19 been changing in South Korea? </b>'),
                                 width=4,
                                 solidHeader = TRUE,
                                 HTML('<p style="font-size:15px">All numbers showed an increasing pattern with time, but there was a difference in the degree. During a given period, the number of deceased cases was significantly less than the number of tests and confirmed cases, and the number of confirmed people was also much lower than the number of tests. Moreover, each gap between the number of tests and confirmed cases, and between that of tests and deceased cases has been widened over the given period. This indicates that the Korean government has attempted to quickly screen suspected patients through an increase in the number of tests and try to prevent the spread of corona in Korea.</p>')
                                   ),
                             box(width=4,
                                 chooseSliderSkin("Shiny"),
                                 sliderInput("date_range", "Date:", min(time$date), max(time$date), c(min(time$date),max(time$date)))
                             )
                             ),
                    
                    fluidRow(
                        box(title='Test',width=4,status='success',plotlyOutput("time_test_cases")),
                        box(title='Confirmed',width=4,status='primary',plotlyOutput("time_confirmed_cases")),
                        box(title='Deceased',width=4,status='danger',plotlyOutput("time_deceased_cases"))
                        
                    )
                    ),
            
            #tab3-1
            tabItem(tabName = "confirmed_region",                       
                    h2("Number of confirmed cases of Covid-19 by region in South Korea"),
                    
                    fluidRow(
                        
                       box(width = 9, status = "primary",title="Number of confirmed cases of Covid-19 by region and date in South Korea",plotlyOutput("region_confirmed_cases",height="575px") %>% withSpinner(color="blue")
                      ),
                      
                      box(width = 3,
                          status = "primary",
                          sliderInput("date_range_c", "Date:", min(region_m$date), max(region_m$date), c(min(region_m$date),max(region_m$date)))
                      ),
                      box(width=3, 
                          status = "primary",
                          checkboxGroupInput("region","Region:", choices = c("Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan","Sejong-si","Gyeonggi-do","Gangwon-do","Chungcheongbuk-do","Chungcheongnam-do","Jeollabuk-do","Jeollanam-do","Gyeongsangbuk-do","Gyeongsangnam-do","Jeju-do"),
                                             selected = c("Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan","Sejong-si","Gyeonggi-do","Gangwon-do","Chungcheongbuk-do","Chungcheongnam-do","Jeollabuk-do","Jeollanam-do","Gyeongsangbuk-do","Gyeongsangnam-do","Jeju-do")
                                             ))
                      ),
                    
                    fluidRow( 
                      box(title = HTML('<b> Which provinces have a large number of confirmed cases for COVID-19?  </b>'),
                          width=12,
                          status = 'primary',
                          solidHeader = TRUE,
                          HTML('<p style="font-size:15px">Over the given period, Daegu has consistently recorded the largest number of confirmed patients in Korea. Following that, Gyeongsangbuk-do, Gyeonggi-do, and Seoul in turn have recorded high numbers. However, unlike Daegu and Gyeongsangbuk-do, the large number of confirmed people in Gyeonggi-do and Seoul are proportional to their high population.</p>')
                      )
                      
                    ),
                    fluidRow(
                      leafletOutput("confirmed_map")%>% withSpinner(color="blue")
                    )
                    ),
            #tab3-2
            tabItem(tabName = "deceased_region",                       
                    h2("Number of deceased cases of Covid-19 by region in South Korea"),
                    
                    fluidRow(
                      box(width = 9, status = "danger", title= "Number of deceased cases of Covid-19 by region and date in South Korea",plotlyOutput("region_deceased_cases",height="575px")%>% withSpinner(color="red")
                      ),
                      
                      box(width = 3,
                        status = "danger",
                        sliderInput("date_range_d", "Date:", min(region_m$date), max(region_m$date), c(min(region_m$date),max(region_m$date)))
                             ),
                      box(width=3, 
                          status = "danger",
                          checkboxGroupInput("region_d","Region:", choices = c("Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan","Sejong-si","Gyeonggi-do","Gangwon-do","Chungcheongbuk-do","Chungcheongnam-do","Jeollabuk-do","Jeollanam-do","Gyeongsangbuk-do","Gyeongsangnam-do","Jeju-do"),
                                             selected = c("Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan","Sejong-si","Gyeonggi-do","Gangwon-do","Chungcheongbuk-do","Chungcheongnam-do","Jeollabuk-do","Jeollanam-do","Gyeongsangbuk-do","Gyeongsangnam-do","Jeju-do")
                          ))
                      
                      ),
                    fluidRow( 
                      box(title = HTML('<b> Which provinces have a large number of deceased cases for COVID-19?  </b>'),
                          width=12,
                          status = 'danger',
                          solidHeader = TRUE,
                          HTML('<p style="font-size:15px">Over the given period, Daegu has consistently recorded the largest number of deceased patients in Korea. Following that, Gyeongsangbuk-do, Gyeonggi-do, and Seoul in turn have recorded high numbers. However, unlike Daegu and Gyeongsangbuk-do, the large number of deceased people in Gyeonggi-do and Seoul are proportional to their high population.</p>')
                      )
                    ),
                    
                    fluidRow(
                      leafletOutput("deceased_map") %>% withSpinner(color="red")
                    )
                    ),
            #tab4
            tabItem(tabName = "type",                       
                    h2("The main cases of Covid-19 infection in South Korea"),
                    fluidRow(
                      box(width=8, title='Covid-19 infection cases in South Korea',highchartOutput("treemap")  %>% withSpinner(color="black")
                      ),
                      box(title = HTML('<b> What are the cases of infection for COVID-19 in South Korea?  </b>'),
                          width=4,
                          solidHeader = TRUE,
                          HTML('<p style="font-size:15px">"Shincheonji Church", well known as "Shincheonji", which is "the group, which has more than 1,000 churches in South Korea and boasts more than 240,000 members worldwide" and "an offshoot of Christianity", accounted for more than half of the path of infection. Following this, contact with patients, etc., oversea inflow, "Guro-gu call center" in Seoul, "Second Mi-Ju Hospital" in Daegu, "Hansarang Convalescent Hospital" in Daegu and "Cheongdo Daenam Hospital" in Gyeongsandbuk-do took up for a large percentage of infection cases. <br> <br> In other words, the most cases are from group infection. Thus, it is required for the government to strengthen the quarantine of public facilities in South Korea.</p>')
                      )
                    ),
                    fluidRow(
                             box(width=8, height = '550px', title = "Covid-19 infection cases by time in South Korea (except Daegu)", plotlyOutput("infection",height = '480px')%>% withSpinner(color="black")
                             ),
                             box(title = HTML('<b>Have there been any infection cases that have changed over time? </b>'),
                                 width=4,
                                 solidHeader = TRUE,
                                 HTML('<p style="font-size:15px">The infection cases in Daegu is not properly reflected in the graph on the left side, since the government did not release the exact number of infection routes for the public. For example, Daegu has a close relationship with the "Shincheonji Church", which was the biggest cause of COVID-19 spread in South Korea. According to experts, "the outbreak among its followers began with so-called "Patient 31", a 61-year-old female member who developed a fever on 10 February, but attended at least four further church services in Daegu". However, the data related to "Shincheonji Church" on February 10 was not revealed in the graph. <br> <br> Despite the disadvantage, the useful information about the temporal change of the infection case types can be found. For instance, the frequency of overseas inflow infection had been increasing as time passed. From this information, it can be seen that the Korean government should put more efforts into preventing overseas inflow infection cases in the future.</p>')
                             )
                             )
                    
                    ),
            #tab 5
            tabItem(tabName = "route", 
                    h2("Covid-19 patients' routes before self-isolation in South Korea"),
                    HTML("<p style='font-size:15px'> <b>Attention: </b><br>There are a large number of missing data especially in Daegu in the following graphs and map, since Daegu did not open the data fully related to patients' routes to the public. </p>"),
                    
                    fluidRow(
                      box(width=8, title = "Place types where Covid-19 confirmed patients have visited before isolation in South Korea", plotlyOutput("places", height = '480px')%>% withSpinner(color="black")
                          ),
                      box(title = HTML('<b>What is the type of places where the most of the COVID-19 patients has visited before isolation? </b>'),
                          width=4,
                          solidHeader = TRUE,
                          HTML('<p style="font-size:15px"> Hospital is the most frequently visited place by Covid-19 patients before self-isolation, followed by etc, store, restaurant, public transportation and so on. <br><br> Since there is no place type related to the office, it can be inferred that work places have been transferred into "etc" category. <br> <br> From this information, each local government of South Korea should alert the public to keep the social distancing. </p>')
                      )
                      ),
                    
                    fluidRow(
                      box(width=12, title = "Place types where Covid-19 patients have visited before isolation by region in South Korea", plotlyOutput("places_region", height = '800px')%>% withSpinner(color="black")
                      )
                      ),
                    fluidRow(box(title = HTML('<b>What is the type of places by province where the most of the COVID-19 patients has visited before isolation? </b>'),
                                 width=12,
                                 solidHeader = TRUE,
                                 HTML('<p style="font-size:15px"> The type of places where the large number of infected people have visited before self-isolation can be found by region. For instance, in Incheon, where Incheon International Airport is located, it can be seen that many confirmed patients went to the airport before self-isolation. <br> <br> However, the place types in Daegu are not properly reflected in the graph above. For instance, the size of the church type data in Daegu is not as large as the actual one. Thus, the situation in Daegu should be considered with more information on this website. <br><br> Furthermore, the reason why there are the highest number of visits in Seoul is because Seoul has the most floating population. <br> <br> By referring to the graph above, each local government can choose the place types where they should more focus on quarantine to prevent the spread of COVID-19 effectively. </p>')
                    )),
                    fluidRow(leafletOutput("place_map", height = '500px', width='1150px') %>% withSpinner(color="black")
                    )
                    
                    )
            
    
  )
 ),
 dashboardFooter(
   left_text = "By Soyeon Kim",
   right_text = "Seoul, 04/2020"
 )
)

## server.R ##
server <- function(input, output) {
   
  
  
    #tab2
    reactiveTime <- reactive({
        time %>% filter(date>=input$date_range[1] & date<input$date_range[2])
    })

    output$a = renderPlotly({
      
      p<- plot_ly(reactiveTime(), x = ~date) %>%
        add_trace(y = ~test, 
                  name = "test", 
                  type = "scatter", 
                  mode = "lines",
                  line = list(color = "forestgreen")
                  ) %>%
        add_trace(y = ~confirmed, 
                  name = "confirmed", 
                  type = "scatter", 
                  mode = "lines",
                  line = list(color = "blue")) %>%
        add_trace(y = ~deceased, 
                  name = "deceased", 
                  type = "scatter", 
                  mode = "lines",
                  line = list(color = "red")) %>%
        layout(xaxis = list(title = "date"),
               yaxis = list(title = "number of cases", type = input$scale))
      p
    })
    
    output$time_test_cases = renderPlotly({
        
              g=ggplot(data = reactiveTime(), aes(x=date, y=test)) +
                  theme(axis.text.x=element_text(angle=90,hjust=1)) +
                    geom_area(fill="forestgreen", alpha=0.5) +
                    geom_line(color="forestgreen") +
                    ylab("number of cases") +
                    theme_ipsum()
              
            ggplotly(g)
        })
    
    output$time_confirmed_cases = renderPlotly({
        
                g1=ggplot(data = reactiveTime(), aes(x=date, y=confirmed)) +
                    geom_area(fill="blue", alpha=0.5) +
                    geom_line(color="blue") +
                    ylab("number of cases") +
                    theme_ipsum() 
                
                ggplotly(g1)
                })
    
    output$time_deceased_cases = renderPlotly({
            
               g2= ggplot(data = reactiveTime(), aes(x=date, y=deceased)) +
                    geom_area(fill="red", alpha=0.5) +
                    geom_line(color="red") +
                    ylab("number of cases") +
                    theme_ipsum() 
                ggplotly(g2)
                 })
    #tab3
    reactiveTime_c <- reactive({
      region_m %>% filter(date>=input$date_range_c[1] & date<input$date_range_c[2] & province==input$region )
    }) 
    
    output$region_confirmed_cases = renderPlotly({
      
      c = ggplot(data=reactiveTime_c(), aes(x=date,y=confirmed, group=1)) +
        geom_line(color = 'blue') +
        facet_wrap(~province) +
        theme_minimal() +
        theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
        theme(axis.text.x = element_text(angle=90)) +
        labs(y='number of cases')
       
        
      ggplotly(c)
    })
    
    reactiveTime_d <- reactive({
      region_m %>% filter(date>=input$date_range_d[1] & date<input$date_range_d[2] & province==input$region_d)
    })
    
    output$region_deceased_cases = renderPlotly({
      
      d = ggplot(data=reactiveTime_d(), aes(x=date,y=deceased, group=1)) +
        geom_line(color = 'red') +
        facet_wrap(~province) +
        theme_minimal()+
        theme(plot.title = element_text(face="bold", size=20, hjust=0)) +
        theme(axis.text.x = element_text(angle=90)) +
        labs(y='number of cases')
        
      ggplotly(d)
    })
    #confirmed_case
    #confirm_c <- reactive({
    #  confirmed_case %>% filter(confirmed_case$CTP_ENG_NM==input$region)
    #}) 
    
    output$confirmed_map = renderLeaflet({
        leaflet(confirmed_case) %>%
            addTiles() %>% 
            addPolygons(opacity = 1.0, fillOpacity = 1.0,
                        weight = 1,
                        fillColor = ~pal1(confirmed_case$confirmed),
                        highlightOptions = highlightOptions(color = "black", weight = 3,  bringToFront = TRUE),
                        label = label1,
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto")) %>% 
            addLegend("bottomright", pal = pal1, values = ~confirmed_case$confirmed,
                      title = "Confimed cases on 20/04/2020",
                      opacity = 10)
    })
    
    output$deceased_map = renderLeaflet({
        leaflet(deceased_case) %>%
            addTiles() %>% 
            addPolygons(opacity = 1.0, fillOpacity = 1.0,
                        weight = 1,
                        fillColor = ~pal2(deceased_case$deceased),
                        highlightOptions = highlightOptions(color = "black", weight = 3,  bringToFront = TRUE),
                        label = label2,
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto")) %>% 
            addLegend("bottomright", pal = pal2, values = ~deceased_case$deceased,
                      title = "Deceased cases on 20/04/2020",
                      opacity = 10)
    })
    
    output$treemap = renderHighchart({
      tm <-treemap(case, index='infection_case',vSize ="confirmed",
                   type = "value")
      
    
      itm<- hctreemap(tm, allowDrillToNode = TRUE) %>% 
        hc_tooltip(pointFormat = "<b>{point.name}</b>:<br>
                             Confirmed cases: {point.value:.0f}") %>% 
        hc_exporting(enabled = TRUE) # enable export
      itm
      
    })
    
   
    output$infection = renderPlotly({
      
      p_plot <-  ggplot(patient_new)+geom_point(aes(x=date,y=infection_case, color=infection_case, 
                                                    text = paste('Date: ', date, '<br>Infection case: ',infection_case)))+
        theme_minimal() +
        theme(axis.text.x=element_text(angle=90,hjust=1)) + 
        theme(legend.position= "none") +
        labs(y="Infection case") 
      
      
      ggplotly(p_plot, tooltip="text")
    })
    
    output$places = renderPlotly({
      types.summary.m
      t_plot <- ggplot(types.summary.m,aes(x=reorder(as.factor(Var.1),-value),y=value,fill=Var.1,
                                           text = paste('Place type: ', Var.1, '<br>Number of visits: ', value)))+
        geom_bar(stat='identity')+
        scale_fill_viridis(discrete=TRUE)+
        theme_minimal() +
        theme(axis.text.x=element_text(angle=90,hjust=1)) +
        theme(legend.position= "none") +
        labs(x='place type',y='number of visits')
      
      ggplotly(t_plot, tooltip="text")
    })
    
    output$places_region = renderPlotly({
      
      place_region = ggplot(type_region, aes(x = type, y = variable, 
                                             text = paste('Place type: ', type, '<br>Region: ',variable, '<br>Number of visits: ', value))) + 
        geom_point(aes(size = value, fill = type), alpha = 0.75, shape = 21) + 
        scale_size_continuous(limits = c(0.1, 900), range = c(1,10), breaks = c(1,100,200,300,400,500,600,700,800,900)) + 
        labs( x= "place type", y = "region", size = "Number of visits", fill = "")  + 
        theme(legend.key=element_blank(), 
              axis.text.x = element_text(colour = "black", size = 12, face = "bold", angle = 90, vjust = 0.3, hjust = 1), 
              axis.text.y = element_text(colour = "black", face = "bold", size = 11), 
              legend.text = element_text(size = 10, face ="bold", colour ="black"), 
              legend.title = element_text(size = 12, face = "bold"), 
              panel.background = element_blank(), panel.border = element_rect(colour = "black", fill = NA, size = 1.2), 
              legend.position = "topright") +  
        scale_fill_viridis(discrete=TRUE)+
        scale_y_discrete(limits = rev(levels(type_region$variable))) 
      
      ggplotly(place_region, tooltip="text")
      
    })
    
    output$place_map = renderLeaflet({
      leaflet(data = loc_df) %>% 
        addTiles() %>%
        # Korean into English: https://leaflet-extras.github.io/leaflet-providers/preview/
        addProviderTiles(providers$Stamen.Terrain) %>%
        addCircles(~longitude, ~latitude, weight = 1, radius = ~sqrt(count)*3000,  
                   label = label_r,labelOptions = labelOptions(
                     style = list("font-weight" = "normal", padding = "3px 8px"),
                     textsize = "15px",
                     direction = "auto"),
                   color = ~pal_r(type), fillOpacity = 0.8) %>%
         
        addLegend("bottomright", pal = pal_r, values = ~type,
                  title = "Place type",
                  opacity = 8)
      
    })
}

shinyApp(ui, server)


