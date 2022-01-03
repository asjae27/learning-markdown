#load packages and data
library(tidyverse)
leadclean <- read_csv("leadclean.csv")
view(leadclean)

#use pivot_wider to make a colomn for each individual geo_type 
leadpivot<- leadclean %>% 
  pivot_wider(names_from = geo_type,
              values_from = geo_area_name) %>% 
  view(leadpivot)

#get the average number of children under 6 with blood lead levels of 5 or greater by borough in NYC.
avg_bll5_by_borough<- leadpivot %>% 
  group_by(Borough) %>% 
  summarise (meanbll5 = mean(bll5, na.rm = TRUE))

#make a bar chart showing average # of children with elevated blood by borough. add a title and label the axes. 
ggplot(data = avg_bll5_by_borough, 
       mapping = aes( x = Borough, y = meanbll5, fill = Borough)) + 
  geom_col() + 
  labs(title = "Avergae Number of Children With Elevated Blood Levels by Borough", y = "Mean children with elevated lead", x = "Borough") +
  scale_color_brewer("Accent")

#remove NA's from dataset 
avg_bll5_by_borough_complete <- avg_bll5_by_borough[complete.cases(avg_bll5_by_borough), ]

#reorder graph from smallest to largest average bll5. 
#Make breaks on the y axis smaller. Get rid of gray default background with theme_minimal().
ggplot(data = avg_bll5_by_borough_complete, 
       mapping = aes( x = reorder(Borough, meanbll5),
                      y = meanbll5, fill = Borough)) + 
  geom_col() + 
  scale_y_continuous(limits = c(0, 8000),
                     breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000)) +
  labs(title = "Avergae Number of Children With Elevated Blood Levels by Borough", y = "Mean children with elevated lead", x = "Borough") +
  scale_color_brewer("Accent") + 
  theme_minimal()

  
  