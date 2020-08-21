library(tidyverse)
library(rvest)

soccer_data <- read_csv("womens_ncaa_soccer_20182019.csv")[,-c(1,2)]
soccer_data$season <- as_factor(soccer_data$season)

### Scrape divisions from NCAA

soccer_rankings <- read_html("https://www.ncaa.com/rankings/soccer-women/d1/ncaa-womens-soccer-rpi") %>%
  html_table()

school_conf <- soccer_rankings[[1]] %>%
  select("School", "Conference")

soccer_data <- soccer_data %>%
  left_join(school_conf, by = c("team" = "School"))

conf_data <- soccer_data %>%
  group_by(Conference) %>%
  mutate(num_teams = n()) %>%
  summarise_if(is.numeric, mean) %>%
  select(Conference, num_teams, everything())

### Visualize Conference data

conf_data %>%
  ggplot(aes(x = Conference, y = ga)) +
  geom_bar(stat = "identity", width = 0.2) +
  geom_point(size = 3) +
  coord_flip() +
  theme_bw()

soccer_data %>%
  ggplot() +
  geom_point(aes(gpg, gaa)) +
  theme_bw()



###


soccer_data %>%
  select(goals, assists, points) %>%
  View()



soccer_data %>%
  ggplot() +
  geom_point(aes(pk_pct, y = gpg))

soccer_data %>%
  ggplot() +
  geom_point(aes(assists_gp, y = gpg))

soccer_data %>%
  ggplot() +
  geom_point(aes(corners_gp, y = gpg))

soccer_data %>%
  ggplot() +
  geom_point(aes(sog_gp, y = gpg))

lm(gpg ~ pk_pct + assists_gp + corners_gp + sog_gp, data = soccer_data) %>%
  summary()


lm(win_pct ~ ., data = soccer_data[,-1]) %>% summary()

soccer_data %>%
  ggplot() +
  geom_point(aes(team_min, y = win_pct, col = season))



soccer_data %>%
  ggplot() +
  geom_bar(aes(psatt)) +
  theme_bw()


soccer_data %>%
  ggplot() +
  geom_histogram(aes(win_pct)) +
  theme_bw()


soccer_data %>%
  ggplot() +
  geom_histogram(aes(fouls)) +
  theme_bw()




###

soccer_data %>%
  mutate(goal_diff = goals - ga) %>%
  ggplot() +
  geom_point(aes(goals, win_pct))


soccer_data %>%
  mutate(goal_diff = goals - ga) %>%
  ggplot() +
  geom_point(aes(ga, win_pct))

soccer_data %>%
  mutate(goal_diff = goals - ga) %>%
  ggplot() +
  geom_point(aes(goal_diff, win_pct))

soccer_data %>%
  mutate(goal_diff = goals - ga) %>%
  select(goals, ga, goal_diff, win_pct) %>%
  cor()



### Clustering Variables

soccer_cor <- soccer_data %>%
  na.omit() %>%
  mutate(goal_diff = goals - ga) %>%
  select_if(is.numeric) %>%
  cor()

soccer_dist <- 1 - abs(soccer_cor)

soccer_dist %>%
  as.dist() %>%
  hclust(method = "complete") %>%
  as.dendrogram() %>%
  ggdendro::ggdendrogram(rotate = T)


# Modeling
soccer_data <- read_csv("womens_ncaa_soccer_20182019.csv")[,-c(1,2)]
soccer_data$season <- as_factor(soccer_data$season)

soccer_data <- soccer_data %>%
  mutate(goal_diff = goals - ga,
         goal_diff_gp = goal_diff/team_games,
         save_pct = save_pct*100,
         pk_pct = pk_pct*100,
         assists_per_goal = assists/goals)


set.seed(10)
data_2019 <- soccer_data %>%
  filter(season == 2019) %>%
  mutate(test_fold = sample(rep(1:5, length.out = n())))

new_lm <- lm(goal_diff ~ sa_pg + pk_pct + corners_gp + sog_gp + saves_gp + save_pct + fouls_gp, data_2019)
init_lm <- lm(goal_diff ~ pk_pct + corners_gp + sog_gp + sog_pct + saves_gp + save_pct + fouls_gp, data_2019)
best_lm <- lm(goal_diff ~ pk_pct + corners_gp + sog_gp + saves_gp + save_pct + fouls_gp, data_2019)
best_lm %>%
  autoplot() +
  theme_bw()

VIF_data <- data.frame(VIF = vif(best_lm))
VIF_data %>%
  ggplot() +
  geom_bar(aes(x = rownames(VIF_data), y = VIF), stat = "identity") +
  annotate("segment", x = 0, xend = 7, y = 5, yend = 5, col = "red") +
  xlab("") +
  labs(title = "Checking for Multicollinearity",
       caption = "ISLR: \"A VIF value that exceeds 5 or 10 indicates a problematic amount of collinearity\"") +
  coord_flip() +
  theme_bw()

get_cv_preds <- function(model_formula, data = data_2019) {
  map_dfr(unique(data$test_fold),
          function(holdout) {
            # Separate test and training data:
            test_data <- data %>%
              filter(test_fold == holdout)
            train_data <- data %>%
              filter(test_fold != holdout)
            # Train model:
            reg_model <- lm(as.formula(model_formula), data = train_data)
            # Return tibble of holdout results:
            tibble(test_preds = predict(reg_model, newdata = test_data),
                   test_actual = test_data$goal_diff,
                   test_fold = holdout)
          })
}

get_cv_preds(goal_diff ~ corners_gp + pk_pct + sog_gp + saves_gp + save_pct + fouls_gp) %>%
  na.omit() %>%
  summarise(rmse = sqrt(mean((test_actual - test_preds)^2)))


soccer_data %>%
  na.omit() %>%
  select(goal_diff, assists_per_goal, pk_pct, corners_gp, sog_gp, sog_pct, save_pct, saves_gp, fouls_gp) %>%
  cor() %>%
  ggcorrplot::ggcorrplot(type = "lower", lab = T) +
  labs(title = "Understanding the Relationship between the Features")






### Scratch Paper

soccer_data %>%
  mutate(goal_ratio = goals/ga) %>%
  View()
  ggplot(aes(team_games, goal_ratio)) +
  geom_point(col = "#4D4D4D", alpha = 0.7) +
  xlab("Number of Games Played") +
  ylab("Goal Ratio") +
  labs(title = "Relationship between Number of Games and Goal Differential") +
  theme_bw()


soccer_data %>%
  ggplot() +
  geom_bar(aes(team_games)) +
  facet_wrap(~Conference) +
  theme_bw()

soccer_data %>%
  ggplot(aes(team_games, goal_diff)) +
  geom_point(col = "#4D4D4D", alpha = 0.7) +
  facet_wrap(~season) +
  xlab("Number of Games Played") +
  ylab("Goal Differential") +
  labs(title = "Relationship between Number of Games and Goal Differential") +
  theme_bw()


## Coefficient Plots


coef_best_lm <- best_lm %>%
  summary() %>%
  coef() %>%
  as.data.frame()

coef_best_lm <- coef_best_lm[-1,]

rownames(coef_best_lm)
coef_best_lm %>%
  ggplot(aes(x = rownames(coef_best_lm), y = Estimate)) +
  geom_bar(stat = "identity", fill = "#4D4D4D") +
  geom_errorbar(col = "#CC002B", ymin = coef_best_lm$Estimate - coef_best_lm$`Std. Error`, ymax= coef_best_lm$Estimate + coef_best_lm$`Std. Error`) +
  theme_bw() +
  xlab("") +
  labs(title = "Coefficients of our Best Linear Model")


soccer_scaled_2019 <- data_2019 %>%
  select_if(is.numeric) %>%
  scale() %>%
  as.data.frame()
scaled_lm <- lm(goal_diff ~ pk_pct + corners_gp + sog_gp + saves_gp + save_pct + fouls_gp, soccer_scaled_2019)

coef_scaled_lm <- scaled_lm %>%
  summary() %>%
  coef() %>%
  as.data.frame()

coef_scaled_lm <- coef_scaled_lm[-1,]

rownames(coef_scaled_lm)
coef_scaled_lm %>%
  ggplot(aes(x = rownames(coef_scaled_lm), y = Estimate)) +
  geom_bar(stat = "identity", fill = "#4D4D4D") +
  geom_errorbar(col = "#CC002B", ymin = coef_scaled_lm$Estimate - coef_scaled_lm$`Std. Error`, ymax= coef_scaled_lm$Estimate + coef_scaled_lm$`Std. Error`) +
  theme_bw() +
  xlab("") +
  labs(title = "Estimated coefficients of the standardized variables",
       subtitle = "Error bars indicate +/- 1 standard deviation from the estimated value",
       caption = "(Figure 1)") +
  scale_x_discrete(labels = c("pk_pct" = "PK%", "corners_gp" = "Corners", "saves_gp" = "Saves", "save_pct" = "Save%", "fouls_gp" = "Fouls", "sog_gp" = "Shots on Goal"))



soccer_data %>%
  select(goals, ga, fouls, fouls_gp) %>%
  cor()

soccer_data %>%
  group_by(team)


coef_bs <- function(data, index){
  lm(goal_diff ~ pk_pct + corners_gp + sog_gp + saves_gp + save_pct + fouls_gp, data = data, subset = index) %>%
    coef()
}

boot(data_2019, coef_bs, 5000)



best_lm %>%
  autoplot(smooth.colour = "#CC002B", label.n = 0) +
  theme_bw() +
  labs(caption = "Figure 4")

