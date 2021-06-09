library(tidyverse)
library(gcookbook)

heightweight %>% 
  
  #create age_group category & bmi variables
  mutate(age_group = cut(ageMonth, c(-Inf, 145, 155, Inf), right = F),
         bmi = (weightLb / heightIn^2) * 703) %>% 
  ggplot(aes(x = bmi, y = age_group, fill = sex)) + 
  geom_boxplot(alpha = 0.6, lwd = .7) +
  
  #titles
  labs(title = "HW 2 Figure",
       x="BMI",
       y = "Age Group",
       fill = "Sex") +
  
  #change labels for age_group axis
  scale_y_discrete(labels = c("<145", "[145,155)", ">=155")) +
  
  #change colors for fill argument
  scale_fill_brewer(palette = "Set1", labels = c("Female", "Male")) + 
  
  #themes
  theme_minimal() + 
  theme(
    axis.text.y = element_text(angle = 90),
    plot.title = element_text(face = "bold", size = 20),
    axis.title = element_text(face = "bold", size = 15),
    axis.text = element_text(color = "gray"),
    text = element_text(color = "darkgray"),
    legend.position = c(.9,.15),
    legend.title = element_text(face = "bold")
  ) 
