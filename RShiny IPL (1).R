library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readxl)
library(DT)
library(lubridate)
library(plotly)

setwd("E://PGDDS/Data Visualization/IPL Data 2008-16")

player_match=read_excel('player_match.xlsx')
player=read_excel('player.xlsx')
ball_by_ball=read_excel('ball_by_ball.xlsx')
match=read_excel('match.xlsx')
batsman_scored=read_xlsx('batsman_scored.xlsx',sheet = 1)
wicket_taken=read_xlsx('wicket_taken.xlsx')
season=read_excel("season.xlsx")
team=read_excel("team.xlsx")

match$year=year(match$match_date)

season_player_join<-left_join(season,player,by=c("orange_cap"="player_id"))

season_player_join2<-left_join(season,player,by=c("purple_cap"="player_id"))

Runs_Scored<-left_join(batsman_scored,match)

Runs_Scored$year<-year(Runs_Scored$match_date)

Wicket_taken<-left_join(wicket_taken,match)

team_win<-left_join(match,team,by=c("match_winner"="team_id"))

team_win$year=year(team_win$match_date)

team_win$year=as.character(team_win$year)
team_wise_count=data.frame()

for(i in (unique(team_win$year))){
  team_wise_count=rbind(team_wise_count,tail(team_win %>% filter(year==i) %>% 
                                               select(match_id,team_name,year),1))    }

firstjoin=left_join(batsman_scored,ball_by_ball)

result1=firstjoin %>% group_by(striker) %>% summarise(Runs=sum(runs_scored))


secondjoin=left_join(result1,player,by=c("striker"="player_id"))

l=left_join(player,player_match)

v=l %>% group_by(player_id,player_name) %>% summarise(count=n())

result2=firstjoin %>% group_by(striker) %>% summarise(Runs=sum(runs_scored))

thirdjoin=left_join(result2,v,by=c("striker"="player_id"))

thirdjoin$BattingAverage=round(thirdjoin$Runs/thirdjoin$count)

strikerate=firstjoin %>% group_by(striker) %>% summarise(Balls=n())

fourthjoin=left_join(strikerate,player,by=c("striker"="player_id"))

fourthjoin$runs<-result1$Runs

fourthjoin %>% select(player_name,Balls,runs)

fourthjoin$Strikerate<-((fourthjoin$runs/fourthjoin$Balls)*100)


##Header of Dashboard

header=dashboardHeader(title="IPL Dashboard")

sidebar=dashboardSidebar(selectInput(inputId = "Year",
                                     label = "Select the Year",
                                     choices = c('All',season_player_join$season_year),
                                     selected = "All"),
                         sliderInput("TopN",label = "Number of Batsman",min = 0,max = 20,value = 5),
                         sidebarMenu(menuItem("Dashboard",tabName = "KPI",icon = icon("Dasboard")),
                                     menuItem("Overall Batting Stats",tabName = "RowData",icon = icon("Graphs")),
                                     menuItem("Title Win",tabName = "Winners",icon = icon("Title Win")),
                                     menuItem("Caps",tabName = "Capholders",icon = icon("Caps")),
                                     menuItem("Overall Matches Won",tabName="Toss_Match",icon = icon("Toss & Match Winner"))))

##Body of the Dashboard

body=dashboardBody(
  tabItems(
    tabItem(
      tabName = "KPI",
      fluidRow(
        valueBoxOutput("value3",width = 3),
        valueBoxOutput("value4",width = 3),
        valueBoxOutput("value5",width = 3),
        valueBoxOutput("value6",width = 3)),
      fluidRow(
        box(title = "Top 10 Batsman by fifties",width = 6,collapsible = T,
            plotOutput("fifties")),
        box(title = "Top 10 Bowlers by 3 wicket hauls",width = 6,collapsible = T,
            plotOutput("wickets"))
        )
      ),
        
    tabItem(
      tabName = "RowData",
      fluidRow(
        box(title = "Top Batsman by Runs",width = 6,collapsible = T,
                   plotOutput("TopBatsman")),
        box(title = "Top Batsman by Average",width = 6,collapsible = T,
                    plotOutput("TopAverage"))),
      fluidRow(
        box(title = "Top Batsman by Strike Rate",width = 6,collapsible = T,
                    plotOutput("TopStrikeRate")),
      
        box(title = "Top Highest Scores",width = 6,collapsible = T,
                    plotOutput("TopScorer")))),
    tabItem(
      tabName = "Winners",
      fluidRow(
        box(title = "Title Winners",width = 12,collapsible = T,
            dataTableOutput("TitleStriker"))
      )
    ),
    tabItem(
      tabName = "Capholders",
      fluidRow(
        box(title = "Orange Cap Holders",width = 6,collapsible = T,DTOutput("orange")),
        box(title = "Purple Cap Holders",width = 6,collapsible = T,DTOutput("purple"))
      )
    ),
    tabItem(
      tabName = "Toss_Match",
      fluidRow(
        box(title = "Toss and Match Wins By Team",width = 12,collapsible = T,
            plotlyOutput("Match_Winners"))
      )
    )))

##Lets create a UI

UI=dashboardPage(header = header,sidebar = sidebar,body = body)

server=function(input,output){
  
  output$value3=renderValueBox({
    data<-{
      if(input$Year=="All"){
        data1<-Runs_Scored %>% filter(runs_scored==6) %>% summarise(Sixes=n())
      }else{
        data1<-Runs_Scored %>% filter(runs_scored==6 & year==input$Year) %>%  
          summarise(Sixes=n())
      }
      data1
      }
      Six=data %>% select('Sixes')
      valueBox(Six,"Sixes",color = "green")
     })
  
  output$value4=renderValueBox({
    data<-{
      if(input$Year=="All"){
        data1<-Runs_Scored %>% filter(runs_scored==4) %>% summarise(Fours=n())
      }else{
          data1<-Runs_Scored %>% filter(runs_scored==4 & year==input$Year) %>% 
            summarise(Fours=n())
        }
      data1
      }
    Four=data %>% select('Fours')
    valueBox(Four,"Fours",color = "red")
    })
  
  output$value5=renderValueBox({
    data<-{
      if(input$Year=="All"){
        data1<-match
      }else{
       data1<-filter(match,year==input$Year)
      }
      data1
      }
      valueBox(length(unique(data$match_id)),"Number of Matches",color = "orange")
     })
  
  output$value6=renderValueBox({
    data<-{
      if(input$Year=="All"){
        data1<-length(Wicket_taken$match_id)
      }else{
        data1<-Wicket_taken %>% filter(year==input$Year) %>% summarise(Wickets=n())
      }
    data1
      }
    valueBox(data,"No.of Wickets")
     })
  
  output$fifties = renderPlot({
    abc = merge(x = firstjoin,y = player %>% select(player_id,batsman_name = "player_name"),
                by.x = "striker",by.y = "player_id")
    abc1 = merge(x = abc,y = match %>% select(match_id,match_date))
    abc1$year = year(abc1$match_date)
    data<-{
      if(input$Year=="All"){
        data1<-abc1 %>% group_by(batsman_name,match_id) %>% 
          summarise(runs = sum(runs_scored,na.rm = T))
      }else{
        data1<-abc1 %>% filter(year==input$Year) %>% group_by(batsman_name,match_id) %>% summarise(runs = sum(runs_scored,na.rm = T))
      }
      data1
    }
    data %>% filter(runs>49,runs<100) %>% group_by(batsman_name) %>% 
      summarise(count=n()) %>% arrange(-count) %>% head(10) %>% 
      ggplot(aes(x = reorder(batsman_name,-count),y=count))+
      geom_bar(stat = "identity",fill = "orange")+theme_bw()+
      theme(axis.ticks.x = element_blank(),axis.text.x = element_text(angle = 90),
            axis.title.x = element_blank(),legend.position = "none")+
      geom_text(aes(label = count), position = position_dodge(width = .9), vjust = -0.25)+
      theme(axis.title.y = element_blank(), axis.text.y = element_blank(), 
            axis.ticks.y =element_blank())
  })
  
  output$wickets = renderPlot({
    cba = merge(x = ball_by_ball,y = wicket_taken)
    cba1 = merge(x = cba,y = player%>% select(player_id,bowler_name = "player_name"),
                 by.x = "bowler",by.y = "player_id")
    cba1 = merge(x = cba1,y = match %>% select(match_id,match_date))
    cba1$year = year(cba1$match_date)
    data<-{
      if(input$Year=="All"){
        data1 = cba1 %>% group_by(bowler_name,match_date) %>% summarise(wickets = n())
      }else{
        data1<-cba1 %>% filter(year==input$Year) %>% 
          group_by(bowler_name,match_date) %>% summarise(wickets = n())
      }
      data1
    }
    data1 %>% filter(wickets>3) %>% group_by(bowler_name) %>% 
      summarise(count = n()) %>% arrange(-count) %>% head(10) %>% 
      ggplot(aes(x = reorder(bowler_name,-count),y=count))+
      geom_bar(stat = "identity",fill = "steelblue")+theme_bw()+
      theme(axis.ticks.x = element_blank(),axis.text.x = element_text(angle = 90),
            axis.title.x = element_blank(),legend.position = "none")+
      geom_text(aes(label = count),position = position_dodge(width = .9), vjust = -0.25)+
      theme(axis.title.y = element_blank(), axis.text.y = element_blank(),
            axis.ticks.y =element_blank())
  })
  
  output$TopBatsman=renderPlot({
    secondjoin %>% select(Runs,player_name) %>% arrange(-Runs)  %>%
    head(input$TopN) %>% ggplot(aes(x=reorder(player_name,-Runs),y=Runs,fill=player_name))+
      geom_bar(stat = 'identity')+theme_bw()+
      theme(axis.ticks = element_blank(),axis.text = element_blank(),axis.title = element_blank(),
            legend.position = "bottom",legend.title = element_blank())+
      geom_text(aes(label = Runs), position = position_dodge(width = .9), vjust = -0.25)
  })
  
  output$TopAverage=renderPlot({
      thirdjoin %>% select(player_name,BattingAverage) %>% 
      arrange(-BattingAverage) %>% head(input$TopN) %>% ggplot(aes(x=reorder(player_name,-BattingAverage),y=BattingAverage,fill=player_name))+
      geom_bar(stat = "identity")+theme_bw()+
      theme(axis.ticks = element_blank(),axis.text = element_blank(),axis.title = element_blank(),
            legend.position = "bottom",legend.title = element_blank())+
      geom_text(aes(label = BattingAverage), position = position_dodge(width = .9), vjust = -0.25)
    
    
    })
   
  output$TopStrikeRate=renderPlot({
    fourthjoin %>% select(player_name,Strikerate) %>% arrange(-Strikerate) %>% 
      head(input$TopN) %>% ggplot(aes(x=reorder(player_name,-Strikerate),y=Strikerate,fill=player_name))+
      geom_bar(stat="identity")+theme(axis.title = element_blank())+theme_bw()+
      theme(axis.ticks = element_blank(),axis.text = element_blank(),axis.title = element_blank(),
            legend.position = "bottom",legend.title = element_blank())+
      geom_text(aes(label = round(Strikerate)), position = position_dodge(width = .9), vjust = -0.25)
  
    })
  
  output$TitleStriker=renderDataTable({
    a<-team_wise_count %>% group_by(team_name,year) %>% select(year,team_name) %>% arrange(year)
    a
    # ggplot(a,aes(x=year,y=count,fill=team_name),fill=team_name)+
    #   geom_bar(stat="identity")+theme(axis.ticks = element_blank(),axis.text.y = element_blank(),
    #                                   legend.position = "bottom",legend.title = element_blank())
  })
  
  output$TopScorer=renderPlot({
    v<-firstjoin %>% group_by(match_id,striker) %>% summarise(Run=sum(runs_scored))
    k=left_join(v,player,by=c("striker"="player_id"))
    j = k %>% arrange(-Run) %>% head(input$TopN)
    ggplot(j,aes(x=1:nrow(j),y=Run,fill = player_name))+geom_bar(stat = "identity",position=position_dodge())+
      theme_bw()+theme(axis.text = element_blank(),legend.position = "bottom",axis.ticks = element_blank(),
                       legend.title = element_blank(),axis.title = element_blank())+
      geom_text(aes(label = Run), position = position_dodge(width = .9), vjust = -0.25)
    
  })
  
  output$orange=renderDT({
    orange_cap<-season_player_join %>% select(season_year,player_name)
    datatable(orange_cap)
  })
  
  output$purple=renderDT({
    purple_cap<-season_player_join2 %>% select(season_year,player_name)
    datatable(purple_cap)
  })
  
  output$Match_Winners=renderPlotly({
    # toss<-left_join(match,team,by=c("toss_winner"="team_id"))
    # toss_winner<-toss %>% group_by(team_name) %>% summarise(Wins=n())
    # toss_winner$type="toss_winner"
    Winner<-left_join(match,team,by=c("match_winner"="team_id"))
    Match_Winner<-Winner %>% filter(team_name != "null") %>% group_by(team_name) %>% summarise(Wins=n())
    Match_Winner$type<-"Match_Winner"
    #toss_match<-rbind(toss_winner,Match_Winner)
    a = Match_Winner %>% group_by(team_name,type) %>% summarise(wins=sum(Wins)) %>% 
      ggplot(aes(x=reorder(team_name,-wins),y=wins))+
      geom_bar(stat="identity",position="dodge",fill="steelblue")+
      theme(axis.text.x =element_text(angle = 90))+
      xlab('Total Matches Won')
    ggplotly(a)
  })
  }
  

shinyApp(UI,server)
