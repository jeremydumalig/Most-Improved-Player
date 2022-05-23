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
nba21 <- read_csv(file="https://raw.githubusercontent.com/jeremydumalig/DataBank/master/nba2021.csv")
nba22 <- read_csv(file="https://raw.githubusercontent.com/jeremydumalig/DataBank/master/nba2022.csv")
adv21 <- read_csv(file="https://raw.githubusercontent.com/jeremydumalig/DataBank/master/nba_advanced21.csv")
adv22 <- read_csv(file="https://raw.githubusercontent.com/jeremydumalig/DataBank/master/nba_advanced22.csv")
predictions <- read_csv(file="https://raw.githubusercontent.com/jeremydumalig/DataBank/master/nba_predictions_2022.csv")
teams <- read_csv(file="https://raw.githubusercontent.com/jeremydumalig/DataBank/master/teams_logo.csv")
teams_d <- read_csv(file="https://raw.githubusercontent.com/jeremydumalig/DataBank/master/teams_d.csv")
nba_model <- read_csv(file="https://raw.githubusercontent.com/jeremydumalig/DataBank/master/nba_predictions_2022.csv")

# Merge datasets into one dataframe
nba <- merge(x=nba21, y=nba22, by="PLAYER", all=FALSE, suffixes = c("", ".new"))
adv <- merge(x=adv21, y=select(adv22, PLAYER, `USG%`), by="PLAYER", all=FALSE, suffixes = c("", ".new"))
nba <- merge(x=nba, y=select(predictions, PLAYER, PRED_RF), by="PLAYER", all.x=TRUE)
nba <- merge(x=nba, y=select(adv, PLAYER, `USG%`, `USG%.new`), by='PLAYER', all=FALSE)


# Create new columns for per game stats, GP minimum
nba <- mutate(nba, 
              PPG = round(PTS / GP, 1),
              PPG.new = round(PTS.new / GP.new, 1),
              APG = round(AST / GP, 1),
              APG.new = round(AST.new / GP.new, 1),
              RPG = round(REB / GP, 1),
              RPG.new = round(REB.new / GP.new, 1),
              d_PPG = PPG.new - PPG,
              d_AST = APG.new - APG,
              d_REB = RPG.new - RPG,
              d_FGP = `FG%.new` - `FG%`,
              d_3PP = `3P%.new` - `3P%`,
              MPG = round(MIN / GP, 1),
              MPG.new = round(MIN.new / GP.new, 1),
              d_MIN = MPG.new - MPG,
              d_USG = `USG%.new` - `USG%`,
              PLAYER = fct_reorder(PLAYER, desc(PPG.new)),
              Qualified = case_when(
                (GP >= 41) & (GP.new >= 41) ~ "GP \u2265 41",
                TRUE ~ "GP < 41"))


# Create new dataframe of stat differentials
d_nba <- select(filter(nba, Qualified == "GP \u2265 41"),
                PLAYER,
                d_PPG, PPG, PPG.new,
                d_AST, APG, APG.new,
                d_REB, RPG, RPG.new,
                d_FGP, d_3PP, d_MIN, d_USG)


# Create list of MIP candidates
player_candidates <- c('Darius Garland',
                       'Dejounte Murray', 
                       'Desmond Bane',
                       'Ja Morant', 
                       'Jordan Poole',
                       'Miles Bridges', 
                       'Tyrese Maxey')
team_candidates <- c("Memphis Grizzlies", 
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
candidates$HEX_CODE <- c("#860038", # Darius Garland
                         "#C4CED4", # Dejounte Murray
                         "#5D76A9", # Desmond Bane
                         "#12173F", # Ja Morant
                         "#FFC72C", # Jordan Poole
                         "#00788C", # Miles Bridges
                         "#ED174C") # Tyrese Maxey
adv_candidates$HEX_CODE <- candidates$HEX_CODE

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
  geom_dumbbell(aes(x=PPG, xend=PPG.new, y=reorder(PLAYER, desc(PLAYER)), group=PLAYER, color=PLAYER),
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
  geom_dumbbell(aes(x=APG, xend=APG.new, y=reorder(PLAYER, desc(PLAYER)), group=PLAYER, color=PLAYER),
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
  geom_dumbbell(aes(x=RPG, xend=RPG.new, y=forcats::fct_rev(factor(PLAYER)), group=PLAYER, color=PLAYER),
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
  geom_dumbbell(aes(x=MPG, xend=MPG.new, y=reorder(PLAYER, desc(PLAYER)), group=PLAYER, color=PLAYER),
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
team_colors <- c("#00788C", "#860038", "#FFC72C", "#5D76A9", "#ED174C", "#C4CED4")
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

# Create plot for model residuals
scatter_residuals <- ggplot(data=nba_model) +
  geom_hline(yintercept=0, linetype='dashed') +
  geom_point(aes(x=PRED_RF, y=RES_RF),
             color='#C9082A',
             shape=1,
             size=3) +
  xlim(0, 32) +
  ylim(-8, 8) +
  labs(title="Model Residuals",
       x="Predicted Points Per Game",
       y="Residuals") +
  theme_borders

# Create plot for model residuals
histogram_residuals <- ggplot(data=nba_model) +
  geom_histogram(aes(x=RES_RF), 
                 color='#17408B',
                 fill='#6495ED',
                 bins=25) +
  xlim(-8, 8) +
  coord_flip() +
  labs(title="", x="", y="") +
  theme_borders


# Display candidates' changes
productivity_table <- select(d_candidates, 
                             PLAYER, PPG, PPG.new, APG, APG.new, RPG, RPG.new) %>%
  gt() %>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = PLAYER)
  ) %>%
  tab_spanner(
    label = "PTS",
    columns = c(PPG, PPG.new)
  ) %>%
  tab_spanner(
    label = "APG",
    columns = c(APG, APG.new)
  ) %>%
  tab_spanner(
    label = "RPG",
    columns = c(RPG, RPG.new)
  ) %>%
  cols_label(
    PPG = "2021",
    APG = "2021",
    RPG = "2021",
    PPG.new = "2022",
    APG.new = "2022",
    RPG.new = "2022"
  ) %>% 
  tab_header( 
    title = md("**Year-to-Year Comparison**"),
    subtitle = "Most Improved Player Candidates")

# Display candidates' changes
efficiency_table <- select(candidates, 
                           PLAYER, 
                           `FG%`, `FG%.new`, 
                           `3P%`, `3P%.new`) %>%
  gt() %>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = PLAYER)
  ) %>%
  tab_spanner(
    label = "FG%",
    columns = c(`FG%`, `FG%.new`)
  ) %>%
  tab_spanner(
    label = "3P%",
    columns = c(`3P%`, `3P%.new`)
  ) %>%
  cols_label(
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
                     MPG, MPG.new, 
                     `USG%`, `USG%.new`) %>%
  gt() %>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = PLAYER)
  ) %>%
  tab_spanner(
    label = "MIN",
    columns = c(MPG, MPG.new)
  ) %>%
  tab_spanner(
    label = "USG%",
    columns = c(`USG%`, `USG%.new`)
  ) %>%
  cols_label(
    MPG = "2021",
    `USG%` = "2021",
    MPG.new = "2022",
    `USG%.new` = "2022"
  ) %>% 
  tab_header( 
    title = md("**Year-to-Year Comparison**"),
    subtitle = "Most Improved Player Candidates")

# Display candidates' changes
teams_table <- select(arrange(teams_d, desc(W21)), 
                      TEAM, W21, W22) %>%
  gt() %>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = TEAM)
  ) %>%
  tab_spanner(
    label = "Wins",
    columns = c(W21, W22)
  ) %>%
  cols_label(
    W21 = "2021",
    W22 = "2022"
  ) %>% 
  tab_header( 
    title = md("**Year-to-Year Comparison**"),
    subtitle = "Most Improved Player Candidates' Teams")

# Display model predictions/residuals
model_table <- head(nba_model,
                    10) %>%
  gt() %>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(columns = PLAYER)
  ) %>% 
  cols_label(PPG2 = "PPG") %>%
  tab_footnote(
    footnote = "2020-21 PPG",
    locations = cells_column_labels( 
      columns = PPG
    )
  ) %>% 
  tab_footnote(
    footnote = "2021-22 PPG",
    locations = cells_column_labels( 
      columns = PPG2
    )
  ) %>% 
  tab_footnote(
    footnote = "Model Prediction",
    locations = cells_column_labels( 
      columns = PRED_RF
    )
  ) %>% 
  tab_footnote(
    footnote = "Model Residual",
    locations = cells_column_labels(columns = RES_RF)
  ) %>%
  tab_header( 
    title = md("**2021-22 Regular Season: Out-Performers**"), # Under-Performers
    subtitle = "Random Forest Model Results")

# Display candidates' differentials for all stats
master_table <- select(d_candidates, 
                       PLAYER, 
                       d_MIN, d_PPG, d_AST, d_REB, 
                       d_FGP, d_3PP, d_USG) %>%
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


# Display plots
grid.arrange(candidates_ppg, arrangeGrob(candidates_ast, candidates_reb, ncol=2))
grid.arrange(candidates_fgp, candidates_3pp, nrow=2)
grid.arrange(candidates_min, candidates_usg, nrow=2)
team_wins
plot_grid(scatter_residuals, histogram_residuals, ncol=2, rel_widths=c(3/4, 1/4))

productivity_table
efficiency_table
role_table
teams_table
model_table
master_table
