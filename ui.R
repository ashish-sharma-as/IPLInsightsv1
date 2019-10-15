library(shinydashboard)

shinyUI(dashboardPage(
  
  
  # ---~---~---~---~---~---~---~---~---~---~---~---~
  # Dashboard HEADER
  dashboardHeader(title = 'IPL Insights'), 

  
  # ---~---~---~---~---~---~---~---~---~---~---~---~
  # Dashboard SIDEBAR  
  dashboardSidebar(
    sidebarUserPanel("Ashish Sharma"),
    sidebarMenu(
      
      menuItem("About", tabName = "about", icon = icon("pencil")),
      
      menuItem("IPL at a Glance", tabName = "iplglance", icon = icon("trophy")),
      
      menuItem("Team Highlights", tabName = "team", icon = icon("users")),
      
      menuItem("Opponent Analysis", tabName = "compareteams", icon = icon("people-carry")),
      
      menuItem("Batsman Metrics", tabName = "comparebatsman", icon = icon("screwdriver")),
      
      menuItem("Bowler Metrics", tabName = "comparebowlers", icon = icon("baseball-ball"))
      
      ) # ends sidebarMenu
    
    
  ), 
  
  
  # ---~---~---~---~---~---~---~---~---~---~---~---~
  # Dashboard BODY
  dashboardBody(
    
    tabItems(
      
      
      
      # ---~---~---~---      
      # Tab: About
      tabItem(tabName = "about",
              
              tags$h3("Indian Premier League - IPL"),
              "A professional cricket league in India played by eight teams from 8 different cities in India. Unlike traditional cricket that is played with 50 overs, IPL is played with 20 overs.",
              hr(),
              "The 8 IPL teams are typically owned by large conglomerates or Bollywood celebrities. Renowned cricket players from all over the world are sought after via auctions to form IPL teams.",
              hr(),
              "Fun Facts about the IPL*:",
              br(),
              " - Has the highest attendance among all cricket leagues in the world",
              br(),
              " - Was the first sporting event in the world to be broadcast live on YouTube in 2010",
              br(),
              " - Has a brand value of $6.9 billion.",
              br(),
              "*Source: Wikipedia",
              hr(),
              hr(),
              tags$h3("IPL Insights - EDA"),
              "This project aims to capture, highlight and bring to life some of the key facts and metrics about IPL from season 2008 till 2017. The visualization tabs are categorized based on overall IPL facts and figures as well as such metrics when analyzing one team's performance and drivers with another. Finally, the Batsman and Bowler analysis provides the engine behind the IPL games."
              
              
      ),
      
      
      
      
      # ---~---~---~---
      # Tab: Project Summary
      tabItem(tabName = "iplglance",
              
              


              fluidRow(
                box(
                  plotOutput(outputId = "S_MatchesWonPerTeam")
                ), # ends box
                
                
                
                box(
                  tableOutput('S_WinRunVenuePl')
                ) # ends box
                

              ), # ends fluid row
              
              hr(),
              
              fluidRow(infoBoxOutput("maxmatches"),
                       infoBoxOutput("countTeams"),
                       infoBoxOutput("countRuns"),
                       infoBoxOutput("countWkts"),
                       infoBoxOutput("countSixes"),
                       infoBoxOutput("countFours"))
              
      ), # ends tabItem  ("iplglance")

      

      # ---~---~---~---
      # Tab: Team Highlights
      tabItem(tabName = "team",
              fluidRow(
                
                box(
                  
                  
                  # User Select - Team - drop down
                  selectizeInput(
                    inputId = "UserChoiceTeam",
                    label = "Select a Team",
                    choices = unique(matches$team1),
                    selected = 'Mumbai Indians'
                  ),
                
                
                  # User Selection - Year(s) - Range
                  sliderInput(
                    inputId = "UserChoiceYears",
                    label = "Select Year(s)",
                    min = floor(min(matches$season)),
                    max = ceiling(max(matches$season)),
                    value = c(2008, 2017),
                    sep = "",
                    round = T,
                    ticks = F
                  ),
                
                
                
                  # Render Bar Chart - Wins and losses over user-selected team and year(s)
                  plotOutput(outputId = "T_WinsAndLossesOverYearForSelectTeam")
                
                ) # ends box
                
              ) # ends fluid row
              
      ), # ends tab: Team Highlights
      

      
      
      # ---~---~---~---      
      # Tab: Compare Teams
      tabItem(tabName = "compareteams",
              # User Select - Team - drop down
              selectizeInput(
                inputId = "UserChoiceTeamOpponent",
                label = "Select Opposing Team",
                choices = unique(matches$team1)
              ),
              
              fluidRow(
                
                column(8,
                       # Render Line Chart - Outcome categories for team and opponent
                       plotOutput(outputId = "C_OutcomeCategories")
                ), # ends column
                
                
                column(4,
                       # Render Bar Chart - Win comparison between team and opponent
                       plotOutput(outputId = "C_WinComparison")
                ) # ends column
                
              ) # ends fluid row
              
      ), # ends tab: Compare with Opponent
      

      
      
      
      # ---~---~---~---      
      # Tab: Compare Batsman
      tabItem(tabName = "comparebatsman",
              
              # User Select - Year - drop down
              selectizeInput(
                inputId = "UserChoiceSingleYear",
                label = "Select Year for comparison",
                choices = sort(unique(matches$season))
              ),
              
              fluidRow(
                
                # Plot of Strike Rate
                plotOutput(outputId = "CBt_TeamStrikeRate"),
                
                # Plot of Batting Average
                plotOutput(outputId = "CBt_TeamBattingAvg")
                
              ) # ends fluid row
              
      ), # ends tab: Compare Batsman
      
      
      
      
      
      # ---~---~---~---      
      # Tab: Compare Bowlers
      tabItem(tabName = "comparebowlers",
              
              fluidRow(
                
                # Plot of Economy Rate
                plotOutput(outputId = "CBwl_EconomyRate"),
                
                # Plot of Wickets Taken
                plotOutput(outputId = "CBwl_WktsTaken")
                
              ) # ends fluid row
              
      ) # ends tab: Compare Bowlers
      

      ) # ends "tabItems"

    
  ) # ends "dashboardBody"
  
  
  # ---~---~---~---~---~---~---~---~---~---~---~---~
  )
)
