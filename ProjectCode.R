library(tidyverse)
library(ggplot2)
library(GGally)
library(leaps)
library(broom)
library(car)

sleep_data <- read.csv("Sleep_Efficiency.csv")
head(sleep_data)

clean_data <- sleep_data %>%
  mutate(SleepEfficiency = Sleep.efficiency,
         CaffeineConsumption = Caffeine.consumption,
         AlcoholConsumption = Alcohol.consumption, 
         SmokingStatus = Smoking.status,
         ExerciseFrequency = Exercise.frequency) %>%
  select(SleepEfficiency,CaffeineConsumption,AlcoholConsumption,SmokingStatus,ExerciseFrequency) %>%
  mutate(SmokingStatus= as.factor(SmokingStatus))

head(clean_data)

anyNA(clean_data)
sum(is.na(clean_data))
sum(is.na(clean_data))/nrow(clean_data)

# Replace missing values with mean value for CaffeineConsumption
caffeine_mean <- mean(clean_data$CaffeineConsumption, na.rm = TRUE)
clean_data$CaffeineConsumption[is.na(clean_data$CaffeineConsumption)] <- caffeine_mean

# Replace missing values with mean value for AlcoholConsumption
alcohol_mean <- mean(clean_data$AlcoholConsumption, na.rm = TRUE)
clean_data$AlcoholConsumption[is.na(clean_data$AlcoholConsumption)] <- alcohol_mean

# Replace missing values with mean value for ExerciseFrequency
exercise_mean <- mean(clean_data$ExerciseFrequency, na.rm = TRUE)
clean_data$ExerciseFrequency[is.na(clean_data$ExerciseFrequency)] <- exercise_mean

anyNA(clean_data)

options(repr.plot.width = 22, repr.plot.height = 15)

pairs_plot <- clean_data %>%
  ggpairs(progress = FALSE) +
  theme(
    text = element_text(size = 20),
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )
pairs_plot


options(repr.plot.width = 15, repr.plot.height = 10)
plot_CC_SE <- clean_data %>%
  ggplot(aes(x = SmokingStatus, y = SleepEfficiency)) +
  geom_boxplot() +
  xlab("The smoking status") +
  ylab("The sleep efficiency") +
  ggtitle("Sleep Efficiency Vs Smoking Status") +
  theme(
    text = element_text(size = 20),
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )
plot_CC_SE


options(repr.plot.width = 15, repr.plot.height = 13)
corr_matrix <- clean_data %>%
  select(- c(SleepEfficiency,SmokingStatus)) %>% 
  cor() %>%
  as.data.frame() %>%
  rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "corr")


plot_corr_matrix <- corr_matrix %>%
  ggplot(aes(var1, var2)) +
  geom_tile(aes(fill = corr)) +
  scale_fill_distiller("Correlation Coefficient \n",
                       palette =  "OrRd",
                       direction = 1, limits = c(-1,1)) +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(
    angle = 15, vjust = 1,
    size = 18, hjust = 1,
    face = "bold"),
    axis.text.y = element_text(
      vjust = 1,
      size = 18, hjust = 1,
      face = "bold"
    ),
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 18),
    legend.key.size = unit(1.5, "cm")
  ) +
  coord_fixed() +
  geom_text(aes(var1, var2, label = round(corr,digits =2)), color = "black", size = 6)
plot_corr_matrix


qqnorm(clean_data$SleepEfficiency, main = "Q-Q Plot for Sleep Efficiency")
qqline(clean_data$SleepEfficiency)



set.seed(333)

training_set <- sample_n(clean_data, size = nrow(clean_data) * 0.70,
                         replace = FALSE)
testing_set <- anti_join(clean_data,
                         training_set)
head(training_set)
nrow(training_set)

head(testing_set)
nrow(testing_set)

model_selection <- regsubsets(
  x = SleepEfficiency ~ ., 
  nvmax = 4,
  data = training_set,
  method = "exhaustive"
)


selection_summary <- summary(model_selection)

selection_summary_dataframe <- tibble(
  n_input_variables = 1:4,
  RSS = selection_summary$rss,
  ADJ.R2 = selection_summary$adjr2,
)
selection_summary$which
selection_summary_dataframe


generative_fit <- lm(SleepEfficiency ~ AlcoholConsumption+SmokingStatus+ExerciseFrequency,
                     testing_set
)
glance(generative_fit) %>%
  select(r.squared, adj.r.squared)
summary(generative_fit)


variables_vif <- vif(generative_fit)
round(variables_vif,3)


qqnorm(generative_fit$residuals, main = "Q-Q Plot of Model Residuals")
qqline(generative_fit$residuals)

plot(generative_fit$fitted,generative_fit$residuals,main = "Residual Plot of the Best Additive Model")



interact_fit<-lm(SleepEfficiency ~ CaffeineConsumption+AlcoholConsumption*SmokingStatus+ExerciseFrequency,
                 testing_set)
interact_fit2<-lm(SleepEfficiency ~ CaffeineConsumption+AlcoholConsumption+ExerciseFrequency*SmokingStatus,
                  testing_set)
interact_fit3<-lm(SleepEfficiency ~ CaffeineConsumption*SmokingStatus+AlcoholConsumption+ExerciseFrequency,
                  testing_set)
interact_fit4<-lm(SleepEfficiency ~ CaffeineConsumption*SmokingStatus+AlcoholConsumption*SmokingStatus+ExerciseFrequency,
                  testing_set)
interact_fit5<-lm(SleepEfficiency ~ CaffeineConsumption*SmokingStatus+AlcoholConsumption*SmokingStatus+ExerciseFrequency*SmokingStatus,
                  testing_set)
interact_fit6<-lm(SleepEfficiency ~ AlcoholConsumption*SmokingStatus+ExerciseFrequency*SmokingStatus,
                  testing_set)
interact_fit7<-lm(SleepEfficiency ~ AlcoholConsumption+ExerciseFrequency*SmokingStatus,
                  testing_set)
interact_fit8<-lm(SleepEfficiency ~ AlcoholConsumption*SmokingStatus+ExerciseFrequency,
                  testing_set)

glance(interact_fit) %>%
  select(r.squared, adj.r.squared)
glance(interact_fit2) %>%
  select(r.squared, adj.r.squared)
glance(interact_fit3) %>%
  select(r.squared, adj.r.squared)
glance(interact_fit4) %>%
  select(r.squared, adj.r.squared)
glance(interact_fit5) %>%
  select(r.squared, adj.r.squared)
glance(interact_fit6) %>%
  select(r.squared, adj.r.squared)
glance(interact_fit7) %>%
  select(r.squared, adj.r.squared)


qqnorm(interact_fit$residuals, main = "Q-Q Plot of Model Residuals")
qqline(interact_fit$residuals)

plot(interact_fit$fitted,interact_fit$residuals,main = "Residual Plot of Best Interactive Model")


summary(interact_fit)
