library(shiny)
library(dplyr)
library(tidyr)
library(purrr)
library(magrittr)
library(ggplot2)
library(stringr)

groups <- c(
            "PSG" = "A", "ARS" = "A",
            "NAP" = "B", "BEN" = "B", "BES" = "B",
            "BAR" = "C", "MC"  = "C",
            "ATL" = "D", "BAY" = "D",
            "MON" = "E", "B04" = "E",
            "BVB" = "F", "RM"  = "F",
            "LEI" = "G", "POR" = "G", "COP" = "G",
            "JUV" = "H", "SEV" = "H", "OL"  = "H"
            )

association <- c(
                 "PSG" = "FRA", "ARS" = "ENG",
                 "NAP" = "ITA", "BEN" = "POR", "BES" = "TUR",
                 "BAR" = "ESP", "MC"  = "ENG",
                 "ATL" = "ESP", "BAY" = "GER",
                 "MON" = "FRA", "B04" = "GER",
                 "BVB" = "GER", "RM"  = "ESP",
                 "LEI" = "ENG", "POR" = "POR", "COP" = "DEN",
                 "JUV" = "ITA", "SEV" = "ESP", "OL"  = "FRA"
                 )

teams <- tibble::tribble(
  ~short, ~name, ~group, ~position, ~association,
  "PSG", "Paris Saint-Germain",   "A",  2,  "FRA",
  "ARS", "Arsenal",               "A",  1,  "ENG",
  "NAP", "Napoli",                "B",  1,  "ITA",
  "BEN", "Benfica",               "B",  2,  "POR",
  "BAR", "Barcelona",             "C",  1,  "ESP",
  "MC",  "Manchester City",       "C",  2,  "ENG",
  "ATL", "Atletico Madrid",       "D",  1,  "ESP",
  "BAY", "Bayern Munich",         "D",  2,  "GER",
  "MON", "Monaco",                "E",  1,  "FRA",
  "B04", "Bayer Leverkusen",      "E",  2,  "GER",
  "BVB", "Borussia Dortmund",     "F",  1,  "GER",
  "RM",  "Real Madrid",           "F",  2,  "ESP",
  "LEI", "Leicester City",        "G",  1,  "ENG",
  "POR", "Porto",                 "G",  2,  "POR",
  "JUV", "Juventus",              "H",  1,  "ITA",
  "SEV", "Sevilla",               "H",  2,  "ESP"
) 

short_long <- teams$name
names(short_long) <- teams$short

#change short names to long names in a game: ARS-BEN to Arsenal - Benfica
change_to_long <- function(short){
  str_split(short, "-") %>%
    map(~short_long[.x]) %>%
    map_df(~data_frame(home = .x[1], away=.x[2])) %>% 
    mutate(vs = "-") %>%
    select(home, vs, away)
    }

teams.first <- filter(teams, position == 1) %>% mutate(dummy = 1)
teams.second <- filter(teams, position == 2) %>% mutate(dummy = 1)

#cross join
teams.cj <- full_join(teams.first, teams.second, 
                      by = "dummy", suffix = c(".first", ".second")) %>%
  select(-dummy) 

#filtered

all_games <- teams.cj %>%
  filter(group.first != group.second &
           association.first != association.second) %>%
  select(short.first, short.second)


#draw a team.first
short.firsts <- unique(all_games$short.first)

tmp <- short.firsts %>% 
  map(~filter(all_games, short.first == .x)) %>%   #je ein df mit allen mögichen gegner aus t2
  map(~set_colnames(.x,                        #colnames durchnumerieren
                    paste(colnames(.x), 
                          which(short.firsts == .x$short.first[1]), sep = "."))) %>%
  map(~mutate(.x, dummy = 1))     

#new filter-helper

t(combn(paste0("short.second.", 1:8),2)) %>% 
  tbl_df() %>% 
  transmute(fil = paste0("!(", V1, " == ", V2, ")")) %>% 
  .$fil %>% 
  paste(collapse = " & ") -> filterhelper

#calculate all possible draws:
tmp %>%         #Reduce führt nacheinander cross joins aus
  Reduce(function(dtf1,dtf2) full_join(dtf1,dtf2,by="dummy"), .) %>%
  select(-dummy) %>%
  filter_(filterhelper) -> all_draws

#calculate first probs:
map(as.character(1:8), ~select(all_draws, ends_with(.x))) %>%
  map_df(~unite(.x, "game", 1:2, sep = "-")) %>% 
  group_by(game) %>% 
  summarise(prob = n()/nrow(all_draws)) %>%
  separate(game, c("home", "away"), sep="-") -> all_probs

df_ids <- all_draws %>%
  mutate(draw_id = row_number()) %>%
  unite("g1", short.first.1, short.second.1, sep = "-") %>%
  unite("g2", short.first.2, short.second.2, sep = "-") %>%
  unite("g3", short.first.3, short.second.3, sep = "-") %>%
  unite("g4", short.first.4, short.second.4, sep = "-") %>%
  unite("g5", short.first.5, short.second.5, sep = "-") %>%
  unite("g6", short.first.6, short.second.6, sep = "-") %>%
  unite("g7", short.first.7, short.second.7, sep = "-") %>%
  unite("g8", short.first.8, short.second.8, sep = "-") %>%
  gather(gn,game, -draw_id) %>%
  select(draw_id, game)

#functions:
new_draws <- function(df, drawn_game){
  df %>%
    group_by(draw_id) %>%
    summarise(game_in_draw = drawn_game %in% game %>% 
                all()) -> pos_draws
  
  filter(all_draws, pos_draws$game_in_draw) 
}

new_probs <- function(new_draws){
  map(as.character(1:8), ~select(new_draws, ends_with(.x))) %>%
    map_df(~unite(.x, "game", 1:2, sep = "-")) %>% 
    group_by(game) %>% 
    summarise(prob = n()/nrow(new_draws)) %>%
    separate(game, c("home", "away"), sep="-")
}

new_games <- function(probs){
  filter(probs, prob < 1) %>% unite("game", 1:2, sep = "-") %>% .$game
}

drawn_matches <- function(probs){
  filter(probs, prob == 1) %>% unite("game", 1:2, sep = "-") %>% .$game
}

plot_helper <- data_frame(home = teams.cj$short.first,
                          away = teams.cj$short.second)

new_plot <- function(probs){
  new_plot_df <- full_join(probs, plot_helper, 
                           by = c("home", "away"))
  
  ggplot(new_plot_df, aes(factor(home, levels = names(sort(groups[unique(home)]))), 
                          factor(away, levels = rev(names(sort(groups[unique(away)])))))) +
    geom_tile(aes(fill = prob), color = "black") +
    geom_text(data = probs ,aes(label = paste0(round(100*prob, 1),"%")), size = 5) +
    scale_x_discrete(position = "top") +
    scale_fill_gradient(low = "white", high = "red", na.value = "lightgrey") + 
    theme_minimal() +
    labs(caption = "\n@hambue") +
    theme(panel.grid = element_blank(),
          legend.position = "none",
          axis.title = element_blank(),
          text = element_text(size = 10, face = "bold"),
          axis.text = element_text(size = rel(1.3)))
}


shinyServer(function(input, output, session) {
  
  updateSelectizeInput(session, "game1", choices = c("", new_games(all_probs)))
  
  drawn <- reactive({
    c(input$game1, input$game2, input$game3, input$game4,
      input$game5, input$game6, input$game7, input$game8) %>%
    .[cumall(str_length(.) > 0)]
    })
  
  n.draws <- reactive({
    new_draws(df_ids, drawn())
    })
  
  n.probs <- reactive({
    new_probs(n.draws())
  })
  
  n_games <- reactive({
    new_games(n.probs())
  })
  
  observeEvent(input$game1, {
    
    if(input$game1 != ""){
      updateSelectizeInput(session, "game2", choices = c("", n_games()))
    }
    else{
      updateSelectizeInput(session, "game2", choices = "")
      updateSelectizeInput(session, "game3", choices = "")
      updateSelectizeInput(session, "game4", choices = "")
      updateSelectizeInput(session, "game5", choices = "")
      updateSelectizeInput(session, "game6", choices = "")
      updateSelectizeInput(session, "game7", choices = "")
    }
  })
  
  observeEvent(input$game2, {
    
    if(input$game2 != ""){
      updateSelectizeInput(session, "game3", choices = c("", n_games()))
    }
    else{
      updateSelectizeInput(session, "game3", choices = "")
      updateSelectizeInput(session, "game4", choices = "")
      updateSelectizeInput(session, "game5", choices = "")
      updateSelectizeInput(session, "game6", choices = "")
      updateSelectizeInput(session, "game7", choices = "")
    }  
  })
  
  observeEvent(input$game3, {
    
    if(input$game3 != ""){
      updateSelectizeInput(session, "game4", choices = c("", n_games()))
    }
    else{
      updateSelectizeInput(session, "game4", choices = "")
      updateSelectizeInput(session, "game5", choices = "")
      updateSelectizeInput(session, "game6", choices = "")
      updateSelectizeInput(session, "game7", choices = "")
    }
  })
  
  observeEvent(input$game4, {
    
    if(input$game4 != ""){
      updateSelectizeInput(session, "game5", choices = c("", n_games()))
    }
    else{
      updateSelectizeInput(session, "game5", choices = "")
      updateSelectizeInput(session, "game6", choices = "")
      updateSelectizeInput(session, "game7", choices = "")
    }
  })
  
  observeEvent(input$game5, {
    
    if(input$game5 != ""){
      updateSelectizeInput(session, "game6", choices = c("", n_games()))
    }
    else{
      updateSelectizeInput(session, "game6", choices = "")
      updateSelectizeInput(session, "game7", choices = "")
    }
  })
  
  observeEvent(input$game6, {
    
    if(input$game6 != ""){
      updateSelectizeInput(session, "game7", choices = c("", n_games()))
    }
    else{
      updateSelectizeInput(session, "game7", choices = "")
    }
  })
  

  output$view <- renderPlot({
    
    
    if(input$game1 == "") new_plot(all_probs)
    else{
       new_plot(n.probs())
    }
    
  })
  
  output$table <- renderTable({
    
    if(input$game1 == "") return()
    else change_to_long(drawn_matches(n.probs()))
    
  }, colnames = F)
  
  output$downloadPlot <- downloadHandler(
    filename = "Shinyplot.png",
    content = function(file) {
        ggsave(file, new_plot(n.probs()), device = "png")
          }   
  )

})
