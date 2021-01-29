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

#setwd("~/Youth Sports/Soccer/MSC_Spring2021_Schedule")

df_schedule <- read_xlsx("MSCGameScheduleSpring2021.xlsx", sheet = "Schedule") %>%
  clean_names()  %>% arrange(date, game_time) %>% 
  mutate(game_time = format(game_time, "%I:%M %p" )) %>% 
  separate(game_time, into = c("game_hour", "minutes"), remove = FALSE) %>% mutate(game_hour = as.numeric(game_hour)) %>% 
  mutate(today = today()) %>% 
  mutate(next_game = if_else(today < date & (today+6)>date, "*", " ")) %>% 
  mutate(date = format(date,"%A, %b %d"))

df_coach <- read_xlsx("MSCGameScheduleSpring2021.xlsx", sheet = "Coach") %>% clean_names() %>% mutate(coach_last = tolower(coach_last) )

df_roster <- read_xlsx("MSCGameScheduleSpring2021.xlsx", sheet = "Roster") %>% clean_names() %>% mutate(coach_last = tolower(coach_last))


# Define UI for application that draws a histogram
ui <- 
   navbarPage(
       theme = shinytheme("cerulean"),
       header=tagList(useShinydashboard()),
       
       # Application title
       title = "Mandeville Soccer Club Game Schedule App",
       
  
    # Sidebar with a slider input for number of bins 
    fluidRow(
        
        box(title="Player Name", status="success", solidHeader = FALSE, width = 12,
            textInput("first",
                  "Player First Name:"), 
            textInput("last",
                        "Player Last Name:"),
            actionButton("go", "Get Player Info", incon=icon("refresh"))
            
            ) ),
    
    fluidRow(
           
        box(title="Player Info", status="success", solidHeader = FALSE,width = 12,
            infoBoxOutput("league"),
            infoBoxOutput("team"),
            infoBoxOutput("location")
        ) 
    ),
    
    fluidRow(
        box (status="success", solidHeader = FALSE,width = 12,
        gt_output("table"),
        helpText("sports info hotline# 985-626-1444"
    ) ))
        
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
 
  df_weather <- reactive({ read.csv(
   "https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/weatherdata/forecast?aggregateHours=1&combinationMethod=aggregate&contentType=csv&unitGroup=us&locationMode=single&key=4KBDXYSGKFWGLEK6DMHK9Z7UF&dataElements=default&locations=mandeville%2C%20LA")
  })

  df_weather2 <- reactive({
    df_weather()%>%  
    select(Date.time, Temperature, Conditions, Chance.Precipitation....) %>% separate(Date.time, into= c("date", "time"), sep = " ", remove=FALSE) %>% 
    rename(precipitation = Chance.Precipitation...., temp_F = Temperature, conditions = Conditions) %>% 
    mutate(date = mdy(date)) %>%  
    mutate(date = format(date,"%A, %b %d")) %>% 
    separate(time, into = c(("game_hour"), "minutes", "seconds"), sep = ":", remove = TRUE) %>% mutate(game_hour = as.numeric(game_hour)) %>% filter(game_hour<14) %>% 
    mutate(game_hour = if_else(game_hour >12, (game_hour-12), game_hour)) %>% 
    mutate(temp_F = replace_na(temp_F, " ")) %>% 
             mutate(conditions = replace_na(conditions, " "))
  })
                    
  
  df <- reactive ({ 
    df_roster %>% left_join(df_schedule) %>% left_join(df_coach) %>% left_join(df_weather2(), by = c("date", "game_hour")) %>% 
    mutate(playerid = as.factor(paste(tolower(first), tolower(last)))) %>% 
    mutate(temp_F = replace_na(temp_F, "--")) %>% 
    mutate( conditions = replace_na(conditions, "--"))
  })
  
  
    df_table <- reactive({
        df() %>% ungroup() %>% 
            filter(playerid == paste(tolower(input$first), tolower(input$last)) ) %>%
                 mutate(jersey = if_else(home_away == "Home", "White (Home)", "Dark (Away)")) %>% 
            mutate(opponent = paste("Team", opponent) )
    })
    
   #Value Box
     output$league <- renderInfoBox({
        infoBox(paste(df_table()$gender %>% unique(), df_table()$age_group %>% unique()), title= "League", color = "green", icon=icon("futbol"))
        })
     
     output$team <- renderInfoBox({
         infoBox(title="Team", paste(df_table()$team %>% unique(), toupper(df_table()$coach_last %>% unique())), color = "light-blue", icon=icon("flag-usa"))
     })
     
     output$location <- renderInfoBox({
         infoBox(title="Field Location", paste( df_table()$location %>% unique()), color = "light-blue", icon=icon("location-arrow") )
     })
    
    #Output Table
    output$table <- render_gt({
        df_table() %>% select(date, game_time, jersey, opponent, `temp_F`, conditions, precipitation, next_game)  %>% 
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
                locations = cells_body(columns = vars(jersey), rows = ifelse(jersey=="White (Home)", TRUE, FALSE))) %>% 
        cols_align(
          align = "center", columns = TRUE) %>% 
        tab_header(title=md(paste("Game Schedule for: ", toupper(df_table()$playerid %>% unique())))) %>% 
        tab_spanner(
          label="Weather Forecast", 
          columns = vars(`temp_F`, conditions, precipitation)
        )
            })
}

# Run the application 
shinyApp(ui = ui, server = server)
