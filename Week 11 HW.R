#load packages and data
library(tidyverse)
library(wesanderson)
library(ggrepel)
library(ggtext)

#make an object with we anderson palette 
pal<- wes_palette("Cavalcanti1" , 5, "discrete")

#import data 
leadclean <- read_csv("leadclean.csv")

#use pivot_wider to make a colomn for each individual geo_type
#rename "time_period" variable to "year" 
leadpivot <- leadclean %>% 
  pivot_wider(names_from = geo_type,
              values_from = geo_area_name) %>% 
  rename(year = time_period)

#get the average number of children under 6 with blood lead levels of 5 or greater by borough in NYC.
avg_bll5_by_borough<- leadpivot %>% 
  group_by(Borough, year) %>% 
  summarise (meanbll5 = mean(bll5, na.rm = TRUE))
view(avg_bll5_by_borough)

#remove NA's from dataset 
avg_bll5_by_borough_complete <- avg_bll5_by_borough[complete.cases(avg_bll5_by_borough), ]

#make a line graph
avg_bll5_by_borough_complete %>% 
  ggplot(aes(x = year,
             y = meanbll5,
             color = Borough)) + 
  geom_line(key_glyph = "timeseries") + #makes the lines jagged instead of smooth 
  geom_label_repel(aes(label = ifelse(year == 2005, scales::number(meanbll5, 
                                                                   accuracy = 1), "")), 
                   nudge_x = -0.5, 
                   segment.color = 'transparent', 
                   size = 2, 
                   show.legend = FALSE) +
  geom_label_repel(aes(label = ifelse(year == 2016, scales::number(meanbll5, 
                                                                   accuracy = 1), "")), 
                   nudge_x = -0.5, 
                   segment.color = 'transparent', 
                   size = 2, 
                   show.legend = FALSE) +
scale_y_continuous(limits = c(0, 16000),
                     breaks = c(0, 2000, 4000, 6000, 8000, 10000, 12000, 14000, 16000)) +
scale_x_continuous(breaks = c(2005,2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016 )) +
scale_color_manual(values = pal) + 
labs(title = "While the average number of children with elevated blood lead levels has decreased in every borough from 2005 to 2016,<br>
     <span style='color:#D8B70A'>Brooklyn</span> continues to have the highest numbers", y = "Mean Children with Elevated Bllod Lead Levels", x = "Year") +
theme_minimal() +
theme(plot.title = element_markdown()) 

 

