setwd("~/Downloads/Most Improved Player")

library(tidyverse)
library(ggplot2)
library(ggnewscale)
library(ggalt)
library(gridExtra)
library(ggimage)
library(cowplot)
library(gt)
rm(list=ls())

# Load datasets
nba21 <- read_csv(file="historical-data/nba_traditional21.csv")
nba22 <- read_csv(file="historical-data/nba_traditional22.csv")
adv21 <- read_csv(file="historical-data/nba_advanced21.csv")
adv22 <- read_csv(file="historical-data/nba_advanced22.csv")
predictions <- read_csv(file="nba_predictions_2022.csv")
teams <- read_csv(file="team-data/teams_logo.csv")
teams_d <- read_csv(file="team-data/teams_d.csv")

# Merge datasets into one dataframe
nba <- merge(x=nba21, y=nba22, by="PLAYER", all=FALSE, suffixes = c("", ".new"))
adv <- merge(x=adv21, y=select(adv22, PLAYER, `USG%`), by="PLAYER", all=FALSE, suffixes = c("", ".new"))
nba <- merge(x=nba, y=select(predictions, PLAYER, PRED_RF), by="PLAYER", all.x=TRUE)
nba <- merge(x=nba, y=select(adv, PLAYER, `USG%`, `USG%.new`), by='PLAYER', all=FALSE)

# Create new columns for per game stats, GP minimum
nba <- mutate(nba,
              d_PPG = PTS.new - PTS,
              d_AST = AST.new - AST,
              d_REB = REB.new - REB,
              d_FGP = `FG%.new` - `FG%`,
              d_3PP = `3P%.new` - `3P%`,
              d_MIN = MIN.new - MIN,
              d_USG = `USG%.new` - `USG%`,
              Qualified = case_when(
                (GP >= 41) & (GP.new >= 41) ~ "GP \u2265 41",
                TRUE ~ "GP < 41"))

# Create new dataframe of stat differentials
d_nba <- select(filter(nba, Qualified == "GP \u2265 41"),
                PLAYER,
                d_PPG, PTS, PTS.new,
                d_AST, AST, AST.new,
                d_REB, REB, REB.new,
                d_FGP, d_3PP, d_MIN, d_USG)

# Create list of MIP candidates
player_candidates <- c('Anfernee Simons',
                       'Darius Garland',
                       'Dejounte Murray', 
                       'Desmond Bane',
                       'Ja Morant', 
                       'Jordan Poole',
                       'Miles Bridges', 
                       'Tyrese Maxey')
team_candidates <- c("Portland Trail Blazers",
                     "Memphis Grizzlies", 
                     "Golden State Warriors", 
                     "Philadelphia 76ers", 
                     "Cleveland Cavaliers", 
                     "Charlotte Hornets", 
                     "San Antonio Spurs")

# Create new dataframes of only MIP candidates
candidates <- filter(nba, PLAYER %in% player_candidates)
adv_candidates <- filter(adv, PLAYER %in% player_candidates)
d_candidates <- filter(d_nba, PLAYER %in% player_candidates)
pred_candidates <- filter(predictions, PLAYER %in% player_candidates)

teams <- filter(teams, TEAM %in% team_candidates)
teams_d <- filter(teams_d, TEAM %in% team_candidates)

# Add color column to candidate dataframe
candidates$HEX_CODE <- c("#E03A3E", # Anfernee Simons
                         "#860038", # Darius Garland
                         "#C4CED4", # Dejounte Murray
                         "#5D76A9", # Desmond Bane
                         "#12173F", # Ja Morant
                         "#FFC72C", # Jordan Poole
                         "#00788C", # Miles Bridges
                         "#ED174C") # Tyrese Maxey
adv_candidates$HEX_CODE <- candidates$HEX_CODE

##################################################

# Create ggplot theme
theme_borders <- 
  theme_linedraw() +
  theme(
    plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"),
    plot.background = element_rect(
      fill = "grey90",
      color = "black"
    ),
    legend.background = element_rect(color="black"),
    title = element_text(size=20),
    legend.title = element_text(size=10),
    axis.title.x = element_text(size=15),
    axis.title.y = element_text(size=15)
  )

# Create plot for PPG differentials
candidates_ppg <- ggplot(candidates) + 
  geom_dumbbell(aes(x=PTS, xend=PTS.new, y=reorder(PLAYER, desc(PLAYER)), group=PLAYER, color=PLAYER),
                size_x=4,
                size_xend=8,
                show.legend = FALSE) +
  geom_point(aes(x=PRED_RF, y=reorder(PLAYER, desc(PLAYER)), group=PLAYER, color=PLAYER, alpha=0.5),
             size=6,
             show.legend = FALSE) +
  scale_y_discrete(limits=rev(player_candidates)) +
  scale_color_manual(breaks=player_candidates,
                     values = candidates$HEX_CODE) +
  labs(title="Top Candidates for Most Improved Player",
       x="Points Per Game (2021 vs 2022)", 
       y="Player") +
  xlim(0, 28) +
  theme_borders +
  theme(plot.title = element_text(hjust = 0.5))

# Create plot for APG differentials
candidates_ast <- ggplot(candidates) + 
  geom_dumbbell(aes(x=AST, xend=AST.new, y=reorder(PLAYER, desc(PLAYER)), group=PLAYER, color=PLAYER),
                size_x=4,
                size_xend=8,
                show.legend = FALSE) +
  scale_y_discrete(limits=rev(player_candidates)) +
  scale_color_manual(breaks=player_candidates,
                     values = candidates$HEX_CODE) +
  labs(x="Assists Per Game (2021 vs 2022)", 
       y="Player") +
  xlim(0, 10) +
  theme_borders

# Create plot for RPG differentials
candidates_reb <- ggplot(candidates) + 
  geom_dumbbell(aes(x=REB, xend=REB.new, y=forcats::fct_rev(factor(PLAYER)), group=PLAYER, color=PLAYER),
                size_x=4,
                size_xend=8,
                show.legend = FALSE) +
  scale_y_discrete(limits=rev(player_candidates)) +
  scale_color_manual(breaks=player_candidates,
                     values = candidates$HEX_CODE) +
  labs(x="Rebounds Per Game (2021 vs 2022)", 
       y="Player") +
  xlim(0, 9) +
  theme_borders

# Create plot for MIP candidates' FG% differentials, sort by FG%
candidates_fgp <- ggplot(candidates) + 
  geom_dumbbell(aes(x=`FG%`, xend=`FG%.new`, y=reorder(PLAYER, desc(PLAYER)), group=PLAYER, color=PLAYER),
                size_x=4,
                size_xend=8,
                show.legend = FALSE) +
  scale_color_manual(values = candidates$HEX_CODE) +
  labs(title='Field Goal Percentage Differentials', 
       x="Field Goal Percentage (2021 vs 2022)", 
       y="Player") +
  xlim(30, 51) +
  theme_borders

# Create plot for MIP candidates' 3P% differentials
candidates_3pp <- ggplot(candidates) + 
  geom_dumbbell(aes(x=`3P%`, xend=`3P%.new`, y=reorder(PLAYER, desc(PLAYER)), group=PLAYER, color=PLAYER),
                size_x=4,
                size_xend=8,
                show.legend = FALSE) +
  scale_color_manual(values = candidates$HEX_CODE) +
  labs(title='3-Point Percentage Differentials', 
       x="3-Point Percentage (2021 vs 2022)", 
       y="Player") +
  xlim(30, 51) +
  theme_borders

# Create plot for MIP candidates' minute differentials
candidates_min <- ggplot(candidates) + 
  geom_dumbbell(aes(x=MIN, xend=MIN.new, y=reorder(PLAYER, desc(PLAYER)), group=PLAYER, color=PLAYER),
                size_x=4,
                size_xend=8,
                show.legend = FALSE) +
  scale_color_manual(values = candidates$HEX_CODE) +
  labs(title="Minute Differentials",
       x="Minutes Per Game (2021 vs 2022)", 
       y="Player") +
  xlim(15, 36) +
  theme_borders +
  theme(plot.title = element_text(hjust = 0.5))

# Create plot for MIP candidates' usage differentials
candidates_usg <- ggplot(adv_candidates) + 
  geom_dumbbell(aes(x=`USG%`, xend=`USG%.new`, y=reorder(PLAYER, desc(PLAYER)), group=PLAYER, color=PLAYER),
                size_x=4,
                size_xend=8,
                show.legend = FALSE) +
  scale_color_manual(values = candidates$HEX_CODE) +
  labs(title="Usage Rate Differentials",
       x="Usage Rate (2021 vs 2022)", 
       y="Player") +
  xlim(15, 36) +
  theme_borders +
  theme(plot.title = element_text(hjust = 0.5))

# Create plot for teams' win differentials, sorted by W
team_colors <- c("#E03A3E", "#00788C", "#860038", "#FFC72C", "#5D76A9", "#ED174C", "#C4CED4")
team_wins <- ggplot(data=teams) +
  geom_line(aes(x=factor(Season), y=W, color=TEAM, group=TEAM), size=1) + 
  geom_image(aes(image = URL, x=factor(Season), y=W), asp=0.75, size=0.075, by="height") +
  scale_color_manual(values=team_colors)+
  labs(title='Win Differentials', 
       x="Season", 
       y="Wins") +
  ylim(21, 57) +
  theme_borders +
  theme(legend.position = "none")

##################################################

# Create function to make differentials more readable
differential_sign <- function(number) {
  if (substr( format(number, nsmall=1), 1, 1 ) != "-") {
    return( paste("+", format(number, nsmall=1), sep="") )
  } else {
    return( number )
  }
}

# Add +/- signs in front of differentials
teams_d <- 
  teams_d %>%
  arrange(desc(d_W)) %>%
  mutate(d_W = map(teams_d$d_W, differential_sign))
candidates <- mutate(candidates,
                     d_PPG = map(d_candidates$d_PPG, differential_sign),
                     d_AST = map(d_candidates$d_AST, differential_sign),
                     d_REB = map(d_candidates$d_REB, differential_sign),
                     d_FGP = map(d_candidates$d_FGP, differential_sign),
                     d_3PP = map(d_candidates$d_3PP, differential_sign),
                     d_MIN = map(d_candidates$d_MIN, differential_sign),
                     d_USG = map(d_candidates$d_USG, differential_sign))

# Display candidates' changes

productivity_table <- select(d_candidates, 
                             PLAYER, 
                             PTS, d_PPG, PTS.new, 
                             AST, d_AST, AST.new, 
                             REB, d_REB, REB.new) %>%
  gt() %>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = PLAYER)
  ) %>%
  tab_style(
    style = list(cell_text(style = "italic", size="small")),
    locations = cells_body(columns = c(d_PPG, d_AST, d_REB))
  ) %>%
  tab_spanner(
    label = "PTS",
    columns = c(PTS, d_PPG, PTS.new)
  ) %>%
  tab_spanner(
    label = "APG",
    columns = c(AST, d_AST, AST.new)
  ) %>%
  tab_spanner(
    label = "RPG",
    columns = c(REB, d_REB, REB.new)
  ) %>%
  cols_label(
    d_PPG = "",
    d_AST = "",
    d_REB = "",
    PTS = "2021",
    AST = "2021",
    REB = "2021",
    PTS.new = "2022",
    AST.new = "2022",
    REB.new = "2022"
  ) %>% 
  tab_header( 
    title = md("**Year-to-Year Comparison**"),
    subtitle = "Most Improved Player Candidates")

# Display candidates' changes
efficiency_table <- select(candidates, 
                           PLAYER, 
                           `FG%`, d_FGP, `FG%.new`, 
                           `3P%`, d_3PP, `3P%.new`) %>%
  gt() %>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = PLAYER)
  ) %>%
  tab_style(
    style = list(cell_text(style = "italic", size="small")),
    locations = cells_body(columns = c(d_FGP, d_3PP))
  ) %>%
  tab_spanner(
    label = "FG%",
    columns = c(`FG%`, d_FGP, `FG%.new`)
  ) %>%
  tab_spanner(
    label = "3P%",
    columns = c(`3P%`, d_3PP, `3P%.new`)
  ) %>%
  cols_label(
    d_FGP = "",
    d_3PP = "",
    `FG%` = "2021",
    `3P%` = "2021",
    `FG%.new` = "2022",
    `3P%.new` = "2022"
  ) %>% 
  tab_header( 
    title = md("**Year-to-Year Comparison**"),
    subtitle = "Most Improved Player Candidates")

# Display candidates' changes
role_table <- select(candidates, 
                     PLAYER, 
                     MIN, d_MIN, MIN.new, 
                     `USG%`, d_USG, `USG%.new`) %>%
  gt() %>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = PLAYER)
  ) %>%
  tab_style(
    style = list(cell_text(style = "italic", size="small")),
    locations = cells_body(columns = c(d_MIN, d_USG))
  ) %>%
  tab_spanner(
    label = "MIN",
    columns = c(MIN, d_MIN, MIN.new)
  ) %>%
  tab_spanner(
    label = "USG%",
    columns = c(`USG%`, d_USG, `USG%.new`)
  ) %>%
  cols_label(
    d_MIN = "",
    d_USG = "",
    MIN = "2021",
    `USG%` = "2021",
    MIN.new = "2022",
    `USG%.new` = "2022"
  ) %>% 
  tab_header( 
    title = md("**Year-to-Year Comparison**"),
    subtitle = "Most Improved Player Candidates")

# Display candidates' changes
teams_table <- select(teams_d, 
                      TEAM, W21, d_W, W22) %>%
  gt() %>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = TEAM)
  ) %>%
  tab_style(
    style = list(cell_text(style = "italic", size="small")),
    locations = cells_body(columns = c(d_W))
  ) %>%
  tab_spanner(
    label = "Wins",
    columns = c(W21, d_W, W22)
  ) %>%
  cols_label(
    d_W = "",
    W21 = "2021",
    W22 = "2022"
  ) %>% 
  tab_header( 
    title = md("**Year-to-Year Comparison**"),
    subtitle = "Most Improved Player Candidates' Teams")

# Display candidates' differentials for all stats
candidates$W <- c("-15", "+22", "+1", "+18", "+18", "+14", "+10", "+2")
master_table <- select(candidates, 
                       PLAYER, 
                       d_MIN, d_PPG, d_AST, d_REB, 
                       d_FGP, d_3PP, d_USG,
                       W) %>%
  gt() %>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = PLAYER)
  ) %>%
  tab_spanner(
    label = "Per-Game Stats",
    columns = c(d_MIN, d_PPG, d_AST, d_REB)
  ) %>%
  tab_spanner(
    label = "Percentages",
    columns = c(d_FGP, d_3PP, d_USG)
  ) %>%
  tab_spanner(
    label = "Team",
    columns = c(W)
  ) %>%
  cols_label(
    d_PPG = "PTS",
    d_AST = "AST",
    d_REB = "REB",
    d_FGP = "FG%",
    d_3PP = "3P%",
    d_MIN = "MIN",
    d_USG = "USG%"
  ) %>%
  tab_header( 
    title = md("**Year-to-Year Differentials**"),
    subtitle = "Most Improved Player Candidates")

##################################################

# Display plots
grid.arrange(candidates_ppg, arrangeGrob(candidates_ast, candidates_reb, ncol=2))
grid.arrange(candidates_fgp, candidates_3pp, nrow=2)
grid.arrange(candidates_min, candidates_usg, nrow=2)
team_wins

productivity_table
efficiency_table
role_table
teams_table
master_table