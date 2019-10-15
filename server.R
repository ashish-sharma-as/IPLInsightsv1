shinyServer(function(input, output, session){ 
  
  observe({
    opponent <- unique(matches[matches$team1 == input$UserChoiceTeam, 'team2']) # should i do OR and swap with 'team1' 
    updateSelectizeInput(session, 
                         inputId = "UserChoiceTeamOpponent",
                         choices = opponent,
                         selected = opponent[2])
  })

  # ---~---~---~---~---~---~---~---~---~---~---~---~---~---~---~
  # Tab: Summary
  # S1 - Bar Plot - Matches won per team
  output$S_MatchesWonPerTeam <- renderPlot({
    matches %>%
      group_by(winner) %>%
      filter(result != "no result") %>%
      summarise("MatchesWon" = n()) %>%
      arrange(desc(MatchesWon)) %>%
      ggplot(data = ., aes(x = winner,
                 y = MatchesWon)) +
      geom_col(aes(fill = winner),
               stat = "identity") +
      coord_flip() +
      xlab("") +
      ylab("Number of matches won") +
      theme(legend.position = "none") +
      ggtitle(label = "Matches won by IPL Teams") +
      theme(plot.title = element_text(color = "darkblue", size = 16, face = "bold"))
  })
  
  
  
  # Table of winners, runnerups, venue per year
  output$S_WinRunVenuePl <- renderTable({
    dfWinRunVenuePl
  })
  
  
  
  output$maxmatches <- renderInfoBox({
    max_value <- max(matches[,'id']) 
    max_entity <- "Matches"
    infoBox(max_entity, max_value, icon = icon("handshake")) #icon("medium-m"))
  })
  
  output$countTeams <- renderInfoBox({
    Tvalue <- length(unique(matches$team1))
    Tentity <- "Teams"
    infoBox(Tentity, Tvalue, icon = icon("users"))
  })
  
  output$countRuns <- renderInfoBox({
    Rvalue <- sum(deliveries[,'total_runs'])
    Rentity <- "Runs"
    infoBox(Rentity, Rvalue, icon = icon("walking"))
  })
  
  output$countWkts <- renderInfoBox({
    Rvalue <-   deliveries %>% 
      filter(player_dismissed == batsman | player_dismissed == non_striker) %>% 
      summarise(n())
    Rentity <- "Wickets"
    infoBox(Rentity, Rvalue, icon = icon("hand-point-up"))
  })
  
  output$countSixes <- renderInfoBox({
    Sixvalue <- length(deliveries[deliveries$batsman_runs == 6,'match_id'])
    Sixentity <- "Sixes"
    infoBox(Sixentity, Sixvalue, icon = icon("dice-six"))
  })
  
  output$countFours <- renderInfoBox({
    Fourvalue <- length(deliveries[deliveries$batsman_runs == 4,'match_id'])
    Fourentity <- "Fours"
    infoBox(Fourentity, Fourvalue, icon = icon("dice-four"))
  })
  


 
  # ---~---~---~---~---~---~---~---~---~---~---~---~---~---~---~
  # Tab: Team
  # T1 - Bar plot - wins and losses over user-selected team and year(s)
  output$T_WinsAndLossesOverYearForSelectTeam <- renderPlot({
    matches %>%
      group_by(season, year) %>%
      filter(
        (team1 == input$UserChoiceTeam | team2 == input$UserChoiceTeam) &
          (season >= min(input$UserChoiceYears) & season <= max(input$UserChoiceYears))
      ) %>%
      summarise(
        'TotalMatches' = n(),
        'Wins' = sum(winner == input$UserChoiceTeam),
        'Losses' = sum(winner != input$UserChoiceTeam)
      ) %>%
      gather(., type, value, -c(season, year, TotalMatches)) %>%
      ggplot(aes(x = year,
                 y = value)) +
      ylab("Matches played") +
      geom_bar(aes(fill = type),
               stat = "identity",
               position = "dodge")  # See Todo-1
  })

  

  # ---~---~---~---~---~---~---~---~---~---~---~---~---~---~---~
  # Tab: Compare Teams
   
  # C1 - Line Graph - Team comparison - (Winner-RunnerUp-Top4-RoundRobin)
  output$C_OutcomeCategories <- renderPlot({
    dfarchive %>% 
      filter(Team == input$UserChoiceTeam | Team == input$UserChoiceTeamOpponent) %>% 
      gather(., key = "YEAR", value = "category", 2:11) %>% 
      ggplot(aes(x = YEAR,
               y = category,
               group = Team)) +
      geom_point(aes(color = Team)) +
      geom_line(aes(color = Team)) +
      xlab("") + 
      ylab("")
  })
  
  
  output$C_WinComparison <- renderPlot({
    matches %>%
      filter((team1 == input$UserChoiceTeam & team2 == input$UserChoiceTeamOpponent) | (team2 == input$UserChoiceTeam & team1 == input$UserChoiceTeamOpponent) ) %>%
      summarise(
        'TotalMatches' = n(),
        'Won by team ' = sum(winner == input$UserChoiceTeam),
        'Won by opponent' = sum(winner == input$UserChoiceTeamOpponent)) %>% 
      gather(., type, value,2:3) %>% 
      ggplot(aes(x=type,
                 y=value)) +
      geom_bar(aes(fill = type),
               stat = "identity",
               position = "dodge") +
      xlab("") +
      ylab("") + 
      geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.5) +
      xlab("") +
      ylab("") +
      theme(axis.text.y = element_blank(),
            axis.ticks = element_blank())
  })
  
  
  
  
  # ---~---~---~---~---~---~---~---~---~---~---~---~---~---~---~
  # Tab: Compare Batsman
  
  # ---~---~---~---~---  
  # STRIKE RATE PLOT
  # ---~---~---~---~---
    output$CBt_TeamStrikeRate <- renderPlot({
      
      # Form Team DF
      SRdf_Team <- as.data.frame(deliveries %>%
                              group_by(batsman) %>%
                              filter(batting_team == input$UserChoiceTeam & season == input$UserChoiceSingleYear & wide_runs == 0) %>%
                              summarise('BF' = n(),
                                        'Runs' = sum(batsman_runs),
                                        'StrikeRate' = round(Runs*100/BF,2)) %>%
                              filter(Runs > median(Runs)) %>%
                              arrange(desc(StrikeRate)
                              )
      )
      
      SRdf_Team$teamname <- input$UserChoiceTeam

      
      # Form Opponent DF
      SRdf_Opponent <- as.data.frame(deliveries %>%
                                   group_by(batsman) %>%
                                   filter(batting_team == input$UserChoiceTeamOpponent & season == input$UserChoiceSingleYear & wide_runs == 0) %>%
                                   summarise('BF' = n(),
                                             'Runs' = sum(batsman_runs),
                                             'StrikeRate' = round(Runs*100/BF,2)) %>%
                                   filter(Runs > median(Runs)) %>%
                                   arrange(desc(StrikeRate)
                                   )
      )
      
      SRdf_Opponent$teamname <- input$UserChoiceTeamOpponent
      
      # Form Conbined DF 
      SRdf_Team <- rbind(SRdf_Team,SRdf_Opponent)
      # Ordering by Strike Rate and assigning back
      SRdf_Team %>%
        arrange(desc(StrikeRate)) -> SRdf_Team
      
      # Plot Strike Rate comparison
      SRdf_Team %>%
        arrange(desc(StrikeRate)) %>% 
        ggplot(aes(x = Runs,
                   y = StrikeRate)) +
        geom_point(aes(color = teamname), size = 3, alpha = 0.8) +
        geom_point(data=SRdf_Team[1:5, ], aes(x=Runs, y=StrikeRate, color = teamname), size=8) +
        geom_text(aes(label=batsman,hjust=-.2,vjust=0)) +
        ggtitle(label = "Strike Rate") +
        theme(plot.title = element_text(color = "darkblue", size = 16, face = "bold"))
    })
    
    

    # ---~---~---~---~---  
    # BATTING AVERAGE PLOT
    # ---~---~---~---~---
    output$CBt_TeamBattingAvg <- renderPlot({
      
      # Form Team DF
      BAdf_Team <- as.data.frame(deliveries %>%
                                   group_by(batsman) %>%
                                   filter(batting_team == input$UserChoiceTeam & season == input$UserChoiceSingleYear & wide_runs == 0) %>%
                                   summarise('Inns' = n_distinct(match_id),
                                             'NO' = Inns - sum(player_dismissed == batsman),
                                             'Runs' = sum(batsman_runs),
                                             'BattingAverage' = round(Runs / (Inns - NO),2)
                                   ) %>%
                                   filter(Inns > median(Inns)) %>%
                                   arrange(desc(BattingAverage)
                                   )
      )
      
      BAdf_Team$teamname <- input$UserChoiceTeam
      
      
      # Form Opponent DF
      BAdf_Opponent <- as.data.frame(deliveries %>%
                                       group_by(batsman) %>%
                                       filter(batting_team == input$UserChoiceTeamOpponent & season == input$UserChoiceSingleYear & wide_runs == 0) %>%
                                       summarise('Inns' = n_distinct(match_id),
                                                 'NO' = Inns - sum(player_dismissed == batsman),
                                                 'Runs' = sum(batsman_runs),
                                                 'BattingAverage' = round(Runs / (Inns - NO),2)
                                       ) %>%
                                       filter(Inns > median(Inns)) %>%
                                       arrange(desc(BattingAverage)
                                       )
      )
      
      BAdf_Opponent$teamname <- input$UserChoiceTeamOpponent
      
      # Form Conbined DF 
      BAdf_Team <- rbind(BAdf_Team,BAdf_Opponent)
      # Ordering by Batting Average and assigning back
      BAdf_Team %>%
        arrange(desc(BattingAverage)) -> BAdf_Team
      
      # Plot Strike Rate comparison
      BAdf_Team %>%
        arrange(desc(BattingAverage)) %>% 
        ggplot(aes(x = Runs,
                   y = BattingAverage)) +
        geom_point(aes(color = teamname), size = 3, alpha = 0.8) +
        geom_point(data=BAdf_Team[1:5, ], aes(x=Runs, y=BattingAverage, color = teamname), size=8) +
        geom_text(aes(label=batsman,hjust=-.2,vjust=0)) +
        ggtitle(label = "Batting Average") +
        theme(plot.title = element_text(color = "darkblue", size = 16, face = "bold"))
    })
    
    
    
    
    
    # ---~---~---~---~---~---~---~---~---~---~---~---~---~---~---~
    # Tab: Compare Bowlers

    # ---~---~---~---~---  
    # ECONOMY RATE
    # ---~---~---~---~---
    output$CBwl_EconomyRate <- renderPlot({
      
      # Form DF
      BowlDF <- as.data.frame(deliveries %>%
                                group_by(bowler, bowling_team) %>%
                                filter((bowling_team == input$UserChoiceTeam | bowling_team == input$UserChoiceTeamOpponent) & season == input$UserChoiceSingleYear) %>% 
                                select(bowler, season, bowling_team, player_dismissed, batsman, non_striker, match_id, over, total_runs) %>% 
                                summarise('Wkts' = sum(player_dismissed == batsman | player_dismissed == non_striker),
                                          'Ov' = n_distinct(match_id,over),
                                          'Runs' = sum(total_runs),
                                          'Econ' = round((Runs / Ov),2)))
      
      # Form DF of Top bowlers based on Economy
      BowlDF %>%
        arrange(Econ) %>%
        filter(Econ < median(Econ)) -> BowlDF_top
      
      
      # Economy Rate Visualization
      BowlDF_top %>%   
        ggplot(aes(x = Runs,
                   y = Econ)) +
        geom_point(aes(color = bowling_team), size = 3, alpha = 0.8) +
        geom_point(data = BowlDF_top[1:5,],
                   aes(x = Runs, y = Econ, color = bowling_team),
                   size = 8) +
        geom_text(aes(label = bowler,hjust = -.2,vjust = 0)) +
        ggtitle(label = "Economy Rate") +
        theme(plot.title = element_text(color = "darkblue", size = 16, face = "bold"))
      
    })
    
    
    
    
    
    # ---~---~---~---~---  
    # WICKETS TAKEN
    # ---~---~---~---~---
    output$CBwl_WktsTaken <- renderPlot({
      
      # Form DF
      Bowl2DF <- as.data.frame(deliveries %>%
                                group_by(bowler, bowling_team) %>%
                                filter((bowling_team == input$UserChoiceTeam | bowling_team == input$UserChoiceTeamOpponent) & season == input$UserChoiceSingleYear) %>% 
                                select(bowler, season, bowling_team, player_dismissed, batsman, non_striker, match_id, over, total_runs) %>% 
                                summarise('Wkts' = sum(player_dismissed == batsman | player_dismissed == non_striker),
                                          'Ov' = n_distinct(match_id,over),
                                          'Runs' = sum(total_runs),
                                          'Econ' = round((Runs / Ov),2)))
      
      
      # Wickets
      Bowl2DF %>%
        arrange(desc(Wkts)) %>%
        filter(Wkts > median(Wkts)) -> WktDF_top
      
      # Wickets Taken Visualization
      WktDF_top %>%   
        ggplot(aes(x = Runs,
                   y = Wkts)) +
        geom_point(aes(color = bowling_team), size = 3, alpha = 0.8) +
        geom_point(data = WktDF_top[1:5,],
                   aes(x = Runs, y = Wkts, color = bowling_team),
                   size = 8) +
        geom_text(aes(label = bowler,hjust = -.2,vjust = 0)) +
        ggtitle(label = "Wickets Taken") +
        theme(plot.title = element_text(color = "darkblue", size = 16, face = "bold"))
      
    })
    
    
    
  })   # End shinyServer
