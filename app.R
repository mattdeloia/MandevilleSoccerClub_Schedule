library(shiny)
library(shinyMobile)
library(readxl)
library(gt)
library(janitor)
library(tidyverse)
library(lubridate)
library(rsconnect)
library(bslib)
library(thematic)

getwd()
#preview_mobile(appPath  = "~/app.R", device = "iphoneX")
#f7Gallery()

df_schedule <- read_xlsx("MSCGameSchedule_2021.xlsx", sheet = "Boys_12U") %>%
    clean_names()  %>% 
  mutate_at(vars(home,away), as.character) %>% 
    mutate(time = format(time, "%I:%M %p" )) %>% 
    separate(time, into = c("game_hour", "minutes", "AM_PM"), remove = FALSE) %>% 
    mutate(game_hour = as.numeric(game_hour)) %>% 
  mutate(game_time = paste(game_hour, ":", minutes, " ", AM_PM, sep = "")) %>% 
    mutate(today = today()) %>% 
    mutate(next_game = if_else(today <= date & (today+7)>date, "*", " ")) %>% 
    mutate(date = as.Date(date,"%A, %b %d"))

df_roster <- read_xlsx("MSCGameSchedule_2021.xlsx", sheet = "Players") %>% 
    clean_names() 

df <- df_roster %>% 
    left_join(df_schedule, by=c("league", "team"="home"), keep=TRUE) %>% 
    rbind(df_roster %>%
            left_join(df_schedule, by=c("league", "team"="away"), keep=TRUE)) %>%
    rename("league"="league.x") %>% 
    mutate(playerid = paste(tolower(first_name), tolower(last_name))) %>% 
  arrange(date) 

ui <-f7Page(
    title = "MSC Game Schedule",
    options = list(
        theme = c("ios", "md", "auto", "aurora"),
        dark = TRUE,
        filled = FALSE,
        color = "#007aff",
        touch = list(
            tapHold = TRUE,
            tapHoldDelay = 750,
            iosTouchRipple = FALSE
        ),
        iosTranslucentBars = FALSE,
        navbar = list(
            iosCenterTitle = TRUE,
            hideNavOnPageScroll = TRUE
        ),
        toolbar = list(
            hideNavOnPageScroll = FALSE
        ),
        pullToRefresh = FALSE
    ),
    preloader = FALSE, 
    loading_duration = 3,
    allowPWA = FALSE,
    
    f7TabLayout(
      navbar = f7Navbar(
        title = "Mandeville Soccer Club Recreation League",
        hairline = FALSE,
        shadow = FALSE,
        leftPanel = FALSE,
        rightPanel = FALSE),
      
      f7Tabs (
        
        f7Tab(
          tabName = "Input",
          icon = f7Icon("flag"),
          f7Block(
            strong=TRUE,

          f7AutoComplete("first", openIn = "dropdown", label=
                     "Player First Name", placeholder = "Enter first name here",  closeOnSelect = TRUE, choices = df_roster$first_name, value = "-"), 
          f7AutoComplete("last",  openIn = "dropdown", label = "Player Last Name", placeholder = "Enter last name here", closeOnSelect = TRUE, choices = df_roster$last_name),
          f7BlockFooter("*Please use name from registration")
          ),

          f7List( title = "Team Details",
                  f7Button(label = textOutput("league")),
                  br(),
                  f7Button(label = textOutput("team"))
          ) ),
          
          f7Tab(
            tabName = "Schedule",
            icon = f7Icon("table"),
            f7Block( gt_output("table"))),
            
            f7Tab(
              tabName = "Info",
              icon = f7Icon("question"),
              f7Card( title= "Information",
                      br(),
            
              f7Button(label = "Pelican Park - Mandeville", color="blue", rounded=TRUE),
              "63350 Pelican Park Blvd., Mandeville", br(),
              "Field Status: 1-985-626-1444", br(),
              f7Button(label = "Abita (Recreation District 11)", color="orange", rounded=TRUE),
              "22517 Highway 36, Abita", br(),
              "Field Status: 1-985-871-7555", br(),
              f7Button(label = "STYSA - Hammond (Chappapeela Sports Park)", color="blue", rounded=TRUE),
              "19325 Hipark Blvd., Hammond", br(),
              "Field Status: 1-504-812-6834", br(),
              f7Button(label = "Spartan Soccer Fields - Slidell", color="orange", rounded=TRUE),
              "658 Spartan Dr., Slidell", br(),
              "Field Status: 1-985-649-2175",br(),
              f7Button(label = "MYB - Covington (Coquille)", color="blue", rounded=TRUE),
              "13505 Louisiana Highway 1085, Covington", br(),
              "Field Status: 1-985-892-9836", br(),
              f7Button(label = "Friendship Park - Picayune, MS", color="blue", rounded=TRUE), br(),
              "1701 S. Haugh Ave., Picayune, MS", br(),
              "Field Status: 1-504-578-0606",br(), br(),
              f7Button(label = "Mandeville Soccer Club:", color="green", rounded=TRUE, href = "https://www.mscsoccer.org/"),br(),
              "Phone: 1-985-624-8080", br(),
              "Email: admin@mscsoccer.org")
              
              )
        
            )
        ))
      
server <- function(input, output, session) {

df_table <- reactive({
    df %>% ungroup() %>% 
        filter(playerid == paste(tolower(trimws(input$first)), tolower(trimws(input$last))) ) %>%
        mutate(jersey = if_else(team == home, "White", "Dark")) %>% 
    arrange((date))})


output$league <- renderPrint({paste("League ",df_table()$league %>% unique())
  })

output$team<- renderPrint({paste("Team ",df_table()$team %>% unique())
})

output$practice<- renderPrint({paste("Practice Field Location ",df_table()$team %>% unique())
})

observeEvent(input$togglePopup, {
  updateF7Popup(id = "popup1")
})

#Output Table
output$table <- render_gt({

    df_table() %>%
        select(date, game_time, home, away, jersey, location,
               # `temp_F`, conditions, precip_percent,
               next_game)  %>% 
    mutate(date = format(date, "%h-%d")) %>% 
        gt(rowname_col = "next_game") %>%
        tab_style(
            style = list(
                cell_fill(color = "skyblue"),
                cell_text(size=pct(100))
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
        tab_header(title=md(paste("2021 Game Schedule for: ", toupper(df_table()$playerid %>% unique())))) %>% 
       tab_spanner(
            label="Team #", 
            columns = vars(home, away)
        )
})
}

# Run the application 
shinyApp(ui = ui, server = server)
