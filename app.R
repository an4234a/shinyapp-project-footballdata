#all necessary packages to run this Shiny app
library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(forcats)
library(ggthemes)
library(fmsb) ##for radar
library(DT)
library(broom)

#the data
df <- read_csv("data/playerall.csv") %>%
#age squared and overall squared will be in regression model that predicts market value
  mutate(age.square = age^2,
         overall.square = overall^2)
##TAB #4

##choosing player name for famous players only. without this filter, 
##all player names being displayed will take a pretty long time to load
##this is for the spreadsheet of player info
player_name <- unique(df$short_name[df$international_reputation > 3])
##TAB #5

#this is the set of variables to compare across different body types & work rates
available_var_bmi <- c("Strength", "Reaction", "Pace", "Stamina", "Market value")
##TAB #2

#is the user looking at outfield player or goalkeeper for age's impact on skills
available_position <- c("Yes", "No")
##TAB #3

#the available data by season
#spreadsheet tab displays data for one season at a time
available_seasons <- c("2014-15", "2015-16", "2016-17", "2017-18", "2018-19", "2019-20")
##TAB #5

#reading the CSV file with the per season data into R
#each of the elements from 'available_seasons' corresponds to one of these CSVs
players_15 <- read_csv("data/players_15.csv")
players_16 <- read_csv("data/players_16.csv")
players_17 <- read_csv("data/players_17.csv")
players_18 <- read_csv("data/players_18.csv")
players_19 <- read_csv("data/players_19.csv")
players_20 <- read_csv("data/players_20.csv")
##TAB #5

#choosing only variables common across all players, regardless of position
#keeps spreadsheet compact instead of original 105 variables
#modifies the players_** data from the direct chunk above
choose_year <- function(df)
{
  df %>%
    select(long_name, age, height_cm, weight_kg, nationality,
           club, player_positions,
           preferred_foot, work_rate, 
           body_type, overall, pace, shooting,passing, dribbling, defending, physic) -> df_2
  df_2 <- as_tibble(df_2)
  
  
  return(df_2)
  
  
}
##TAB #5

#these output tables will actually appear spreadsheet tab of shiny app
players_15_1 <- choose_year(players_15)
players_16_1 <- choose_year(players_16)
players_17_1 <- choose_year(players_17)
players_18_1 <- choose_year(players_18)
players_19_1 <- choose_year(players_19)
players_20_1 <- choose_year(players_20)

##TAB #5

#created BMI variable for use in market value prediction
df %>%
  mutate(player_BMI = (((weight_kg)/(height_cm)/(height_cm))*10000)) -> df
##TAB #4

#create a list of world cup winner
world_cup_winners <- c("Uruguay", "Italy", "Germany", "Brazil", "England", "Argentina", "France", "Spain")
#later we want to see if coming from a world cup winning-
#country enhances a player's market value
##TAB #4

##create new variable of coming from a world cup-winning country or not in df
df$win_wc <- NULL
##TAB #4

#created variable that player comes from world cup winner countries
#1 if the player nationality is world cup winner and 0 if not
df <- df %>%
  mutate(win_wc = ifelse(nationality %in% world_cup_winners, 1,0))
##TAB #4


#the following variables changed to numerics for analysis across different
#body types and work rates
df %>% mutate(work_rate = recode(work_rate,
                                 'Low/Low'  = "2",
                                 'Low/Medium' = "3",
                                 'Low/High' = "4",
                                 'Medium/Low' = "3",
                                 'Medium/Medium' = "4",
                                 'Medium/High' = "5",
                                 'High/Low' = "4",
                                 'High/Medium' = "5",
                                 'High/High' = "6")) %>%
  mutate(power_stamina = parse_number(power_stamina)) %>%
  mutate(movement_reactions = parse_number(movement_reactions)) %>%
  mutate(power_strength = parse_number(power_strength)) -> df
##TAB #2


##the following are boxplots of various chosen variables across
##different body types and work rates

#how does pace differ across different body types and work rates
df %>%
  filter((body_type == "Normal")|(body_type=="Lean")|(body_type=="Stocky"))%>%
  ggplot(aes(x = as.factor(work_rate), y = pace, color = body_type))+
  geom_boxplot(na.rm=TRUE) +xlab("Work rate") -> haha

#how does a player's reaction/sharpness differ across body types & work rates
df %>%
  filter((body_type == "Normal")|(body_type=="Lean")|(body_type=="Stocky"))%>%
  ggplot(aes(x = as.factor(work_rate), y = movement_reactions, color = body_type))+
  geom_boxplot(na.rm=TRUE) +xlab("Work rate") +ylab("Reaction")-> lol

#how does a player's strength differ across body types and work rates
df %>%
  filter((body_type == "Normal")|(body_type=="Lean")|(body_type=="Stocky"))%>%
  ggplot(aes(x = as.factor(work_rate), y = power_strength, color = body_type))+
  geom_boxplot(na.rm=TRUE) +xlab("Work rate") +ylab("Strength")-> huhu


#does stamina vary by body type and work rate
df %>%
  filter((body_type == "Normal")|(body_type=="Lean")|(body_type=="Stocky"))%>%
  ggplot(aes(x = as.factor(work_rate), y = power_stamina, color = body_type))+
  geom_boxplot(na.rm=TRUE)  +xlab("Work rate") +ylab("Stamina") -> hehe



##does market value vary by body type and work rate
df %>%
  filter((body_type == "Normal")|(body_type=="Lean")|(body_type=="Stocky"))%>%
  ggplot(aes(x = as.factor(work_rate), y = value_eur, color = body_type))+
  geom_boxplot(na.rm=TRUE )+scale_y_log10() +xlab("Work rate") +ylab("Market value")-> hoho

##TAB #2


#above, we have formatted the data and plots to be displayed as
#outputs in tabs 2, 4, and 5



ui <- dashboardPage(
  dashboardHeader(title = "World futbol!"),
  
  #titles of tabs
  dashboardSidebar(
    sidebarMenu(
      #Tab 1
      menuItem(text = "By player", tabName = "single_pl"),
      
      #Tab 2
      menuItem(text = "By body type and work rate", tabName = "body_type_pl"),
      
      #Tab 3
      menuItem(text = "Impact of age on skills", tabName = "age_pl"),
      
      #tab 4
      menuItem(text = "Market value", tabName = "mv_pl"),
      
      #tab 5
      menuItem(text = "Appendix", tabName = "spread_sheet")
    )),
  #dashboard body
  dashboardBody(
    #these deal with inputs
    tabItems(
      tabItem(tabName = "single_pl",
              #inputs for stats by individual player
              fluidPage(
                
                tabsetPanel(
                  tabPanel("Player Stats",
                           
                           # Sidebar choosing which player and which variable 
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("idv.name","player name", player_name),
                               tableOutput("idv.table")
                             ),
                             mainPanel(
                               plotOutput("radar")
                             )
                           )),
                  
                  tabPanel("Player Progress",
                           
                           # Sidebar choosing which player and which variable 
                           sidebarLayout(
                             sidebarPanel(
                               varSelectInput("idv.var1","Variable", data=df, selected = "overall")
                             ),
                             mainPanel(
                               
                               plotOutput("idv.plot1")   
                               
                             )
                           ))
                  
                  
                  
                )
                )),
     
      tabItem(tabName = "body_type_pl",
              #the boxplots showing median of 5 variables across
              #different body types and work rates
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    radioButtons("var1", "How does ___ differ by body type and work rate?", choices = available_var_bmi)
                  ),
                  mainPanel(
                    plotOutput("bt_boxplot")
                  )
                )
      )),
      tabItem(tabName = "age_pl",
              #the effect of age on skills over time
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    #different plots for goalkeepers and outfield players
                    radioButtons("pos1", "Goalkeeper?", choices = available_position)

                  ),
                  mainPanel(
                    plotOutput("age_plot")
                  )
                )
              )),
      tabItem(tabName = "mv_pl",
              #which variables are significant in predicting market value?
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    checkboxGroupInput("mv_var", "Which variables may affect a player's market value?", 
                                       choices = c("age","overall","player_BMI",
                                                   "international_reputation","win_wc")),
                    checkboxGroupInput("mv_var.model", "Add a quadratic relationship?", 
                                       choices = c("age" = "age.square",
                                                   "overall" = "overall.square")),
                    radioButtons("generate_model", "Generate your model?", 
                                 choices = available_position, selected = "No"),
                    radioButtons("generate_residual", "Inspect model's residual plot?", 
                                 choices = available_position, selected = "No"),
                    verbatimTextOutput("hint", placeholder = TRUE)
                  ),
                  mainPanel(
                    plotOutput("mv_plot"),
                    tableOutput("lm"),
                    plotOutput("res_plot")
                  )
                )
              )),
      tabItem(tabName = "spread_sheet",
              #overall spreadsheet
              fluidPage(
                #only choose one season at a time
                radioButtons("year", "Season?", choices = available_seasons),
                
                #download button
                downloadButton('downLoadFilter',"Download the filtered data"),
                
                #the resulting table for the chosen season
                dataTableOutput("dynamic")
                )
              ))
      
      
      )
      )


server <- function(input, output, session) {

#HERE STARTS TAB #1
  
  ##main plot 
  output$idv.plot1 <- renderPlot({
    ###making a curve to show progression of mean for chosen variable
    ###over time taking into account all players
    mm.df <- df %>%
      group_by(year)%>%
      summarise(mean = mean(!!input$idv.var1, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(short_name = "Average") %>%
      select(short_name,mean,year)
    names(mm.df) <- c("short_name","val","year")
    
    ### the chosen player's data
    df.1 <- df %>%
      filter(short_name == input$idv.name) %>%
      select(short_name,input$idv.var1,year)
    names(df.1) <- c("short_name","val","year")
    
    ### bind them
    df.1 <- rbind(df.1,mm.df)
    
    ### make a plot that compares the player's rating for chosen
    ###variable versus the overall mean rating for chosen
    ##variable from 2014-2020
    df.1 %>%
      ggplot(aes(year,val, color=short_name)) + geom_point() + geom_line()+theme_bw()
  })
  
  ## make radar plot
  output$radar <- renderPlot({
    
    ### put condition: if player is retired then showed retired
    if (max(df$year[df$short_name == input$idv.name]) == 20) {
      
      ### select variable to be shown in radar
      radar <- df %>%
        filter(short_name == input$idv.name,
               year == 20) %>%
        select(pace,shooting,passing,dribbling,physic)
      
      ### make data frame with radar format
      radar <- rbind(rep(100,5) , rep(40,5) , radar)
      ## make a plot
      radarchart(radar)
    } else {
      "Retired as of 2019-2020 season"
    }
  })
  
  ##table with some information of chosen player
  output$idv.table <- renderTable({
    
    ### put condition: if player is retired then showed retired
    if (max(df$year[df$short_name == input$idv.name]) == 20) {
      df %>%
        filter(short_name == input$idv.name,
               year == 20) %>%
        select(nationality,age,height_cm,weight_kg)%>%
        gather("Stat","Value")
    } else {
      "Retired as of 2019-2020 season"
    }
    
  })

#HERE STARTS TAB #4  
    
  #output for market value vs other variable
  #Scatterplots of value_eur vs. significant predictor variables
 output$mv_plot <- renderPlot({
   df[,c("value_eur",input$mv_var)] %>%
     plot()
 })
 
 ## output model
 output$lm <- renderTable({
   if (input$generate_model == "Yes") {
   df <- df[,c("value_eur",input$mv_var,input$mv_var.model)] 
   lm <- lm(value_eur~.,df)
   tidy(lm)
   } else { print("Choose Yes to generate model")}
 })
 
 ## output residual 
 output$res_plot <- renderPlot({
   if (input$generate_residual == "Yes") {
     df <- df[,c("value_eur",input$mv_var.model)] 
     lm <- lm(value_eur~.,df)
     par(mfrow=c(2,2))
     plot(lm)
   } else { print("Choose Yes to generate model coefficient")}
   
 })
 
 output$hint <- renderText(
   "Tips: Try predict market value 
   with age, overall, 
   international reputation, 
   and second term  (squared)
   of age and overall to 
   generate best model"
 )

#HERE STARTS TAB #2
   
  output$bt_boxplot <- renderPlot({
   
    ##boxplot of pace v work rate
   if(input$var1 == "Pace")
    {
    #does pace vary by body type
     haha
    
   }
    #REACTION V WORK RATE 
    else if(input$var1 == "Reaction")
    {
      lol
    }
    
    #strength v work rate
    else if(input$var1 == "Strength")
    {
      huhu
    }
    
    #stamina v work rate
    else if(input$var1 == "Stamina")
    {
    #does stamina vary by body type
      hehe
    }
    
    #market value v work rate
    else if(input$var1 == "Market value")
    {
    ##does market value vary by body type
     hoho
    
    }
    
    
  })
  
#HERE STARTS TAB #3
  
  output$age_plot <- renderPlot({
   
    #display progession of mean rating for skills over time for goalkeepers 
    if(input$pos1 == "Yes")
    {
      df.gk <- df %>%
        filter(player_positions == "GK")
      
      #setseed
      set.seed(1000)
      df.gk %>%
        sample_n(1000) %>%
        select(age,gk_diving,gk_handling,gk_kicking,gk_reflexes,gk_speed,gk_positioning) %>%
        gather(-age, key= "skill", value = "value") %>%
        ggplot(aes(age,value,color = skill)) + geom_smooth(se= FALSE) + theme_bw()
    }
    
    #display progression of mean rating for skills over time for outfield players
    else if(input$pos1 == "No")
    {
      #outfield player
      df.outfield <- df %>%
        filter(player_positions != "GK")
      
      #setseed
      set.seed(1000)
      df.outfield %>%
        sample_n(1000) %>%
        select(age,pace,shooting,passing,dribbling,defending,physic) %>%
        gather(-age, key= "skill", value = "value") %>%
        ggplot(aes(age,value,color = skill)) + geom_smooth(se= FALSE) + theme_bw()
      
    }
  })
  
#HERE STARTS TAB #5  
  
  output$dynamic <- renderDataTable({
    
    #give the user the corresponding data table for the chosen season
    if(input$year == "2014-15")
    {
      players_15_1
    }
    else if(input$year == "2015-16")
    {
      players_16_1
    }
    else if(input$year == "2016-17")
    {
      players_17_1
    }
    else if(input$year == "2017-18")
    {
      players_18_1
    }
    else if(input$year == "2018-19")
    {
      players_19_1
    }
    else
    {
      players_20_1
    }
  },filter = 'top', options = list(scrollX = TRUE))
  
  output$downLoadFilter <- downloadHandler(
    filename = function() {
      paste('Filtered data-', Sys.Date(), '.csv', sep = '')
    },
    content = function(file){
      write.csv(thedata()[input[["ex1_rows_all"]], ],file)
    }
  )
  
  
}




shinyApp(ui, server)
