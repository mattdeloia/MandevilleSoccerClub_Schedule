library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(readxl)
library(gt)
library(janitor)
library(tidyverse)
library(lubridate)
library(rsconnect)
library(bslib)
library(thematic)
library(shinymanager)

credentials <- data.frame(
  user = c("msc", "MSC"), # mandatory
  password = c("msc", "MSC"), # mandatory
  # start = c("2019-04-15"), # optinal (all others)
  # expire = c(NA, "2019-12-31"),
  # admin = c(FALSE, TRUE),
  # comment = "Simple and secure authentification mechanism 
  # for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)

df_schedule <- read_xlsx("MSCGameSchedule_test.xlsx", sheet = "Boys_7U") %>%
  clean_names()  %>% 
  arrange(date, time) %>% 
  mutate(time = format(time, "%I:%M %p" )) %>% 
  separate(time, into = c("game_hour", "minutes"), remove = FALSE) %>% 
  mutate(game_hour = as.numeric(game_hour)) %>% 
  mutate(today = today()) %>% 
  mutate(next_game = if_else(today <= date & (today+6)>date, "*", " ")) %>% 
  mutate(date = format(date,"%A, %b %d"))


df_roster <- read_xlsx("MSCGameSchedule_test.xlsx", sheet = "Players") %>% 
  clean_names() 


# Define UI for application that draws a histogram
ui <- 
   navbarPage(
       theme = shinytheme("cerulean"),
       header=tagList(useShinydashboard()),
       
       # Application title
       title = "Mandeville Soccer Club Game Schedule App",
       
  
    # Sidebar with a slider input for number of bins 
    fluidRow(
        
        box(title="Player Name", footer = "*Please use name from registration", status="success", solidHeader = FALSE, width = 12,
            textInput("first",
                  "Player First Name:"), 
            textInput("last",
                        "Player Last Name:")
 
            ) ),
    
    fluidRow(
           
        box(title="Player Info", status="success", solidHeader = FALSE,width = 12,
            infoBoxOutput("league"),
            infoBoxOutput("team")
        ) 
    ),
    
    fluidRow(
        box (status="success", solidHeader = FALSE,width = 12,
        gt_output("table"),
        helpText("sports info hotline# 985-626-1444"),
        helpText("Weather data provided by Visual Crossing Weather web services (free API plan)."
    ) ))
        
)

ui <- secure_app(ui)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
 
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  df_weather <- reactive({read.csv(
   "https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/forecast?aggregateHours=1&combinationMethod=aggregate&contentType=csv&unitGroup=us&locationMode=single&key=4KBDXYSGKFWGLEK6DMHK9Z7UF&dataElements=default&locations=mandeville%2C%20LA")
  })

  df_weather2 <- reactive({
    df_weather()%>%  
    select(Date.time, Temperature, Conditions, Chance.Precipitation....) %>% 
      separate(Date.time, into= c("date", "time"), sep = " ", remove=FALSE) %>% 
    rename(precip_percent = Chance.Precipitation...., temp_F = Temperature, conditions = Conditions) %>% 
    mutate(date = mdy(date)) %>% 
    filter(date< (today()+6)) %>% 
    mutate(date = format(date,"%A, %b %d")) %>% 
    separate(time, into = c(("game_hour"), "minutes", "seconds"), sep = ":", remove = TRUE) %>% 
      mutate(game_hour = as.numeric(game_hour)) %>% filter(game_hour<18 & game_hour>=9) %>% 
    mutate(game_hour = if_else(game_hour >12, (game_hour-12), game_hour))  
  })
                    
  
  df <- reactive ({ 
    df_roster %>% 
      left_join(df_schedule, by=c("league", "team"="home"), keep=TRUE) %>% 
      rbind(df_roster %>% 
              left_join(df_schedule, by=c("league", "team"="away"), keep=TRUE)
            ) %>%
      rename("league"="league.x")%>% 
      left_join(df_weather2(), by = c("date", "game_hour")) %>% 
    mutate(playerid = paste(tolower(first_name), tolower(last_name))) %>% 
    mutate(temp_F = replace_na(temp_F, "--")) %>% 
    mutate(conditions = replace_na(conditions, "--")) %>% 
      mutate(precip_percent = replace_na(precip_percent, "--"))
  })
  
  
    df_table <- reactive({
        df() %>% ungroup() %>% 
            filter(playerid == paste(tolower(trimws(input$first)), tolower(trimws(input$last))) ) %>%
                 mutate(jersey = if_else(team == home, "White", "Dark"))      })
    

   output$league <- renderInfoBox({
      infoBox(paste(df_table()$league %>% unique()), title= "League", color = "green", icon=icon("futbol"))
      })
     
     output$team <- renderInfoBox({
         infoBox(title="Team", paste("Team #", df_table()$team %>% unique()), color = "light-blue", icon=icon("flag-usa"))
     })
     
     # output$location <- renderInfoBox({
     #     infoBox(title="Field Location", paste( df_table()$location %>% unique()), color = "light-blue", icon=icon("location-arrow") )
     # })
    
    #Output Table
    output$table <- render_gt({
        df_table() %>% select(date, time, home, away, jersey, location, `temp_F`, conditions, precip_percent, next_game)  %>% 
            gt(rowname_col = "next_game") %>%
        tab_style(
          style = list(
            cell_fill(color = "skyblue")
          ), 
          locations = cells_body(
            rows = ifelse(next_game =="*", TRUE, FALSE)) ) %>%
         tab_style(
                style = list(
                    cell_text(style = "italic", color = "blue"
                )),
                locations = cells_body(columns = vars(jersey), rows = ifelse(jersey=="White", TRUE, FALSE))) %>%
        cols_align(
          align = "center", columns = TRUE) %>% 
        tab_header(title=md(paste("Game Schedule for: ", toupper(df_table()$playerid %>% unique())))) %>% 
        tab_spanner(
          label="Weather Forecast", 
          columns = vars(`temp_F`, conditions, precip_percent)
        ) %>% 
        tab_spanner(
          label="Team #", 
          columns = vars(home, away)
        )
            })
}

# Run the application 
shinyApp(ui = ui, server = server)
