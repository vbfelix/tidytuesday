# Packages ----------------------------------------------------------------

library(readxl)
library(tidyverse)

# Import ------------------------------------------------------------------
list.files()
df_or<-read_xlsx("tidy_tuesday_week2.xlsx")

df_or %>% 
  mutate(Quarterback = Quarterback/10^6) %>% 
  group_by(year) %>% 
  top_n(16,Quarterback)->df

# Plot --------------------------------------------------------------------

df %>%
  ggplot(aes(year, Quarterback))+
  geom_point()+
  labs(x = "Year",y="Average cap value ($ million)",
       caption = "Graphic: @H0Vinicius | Source: spotrac.com",
       title = "Average pay for the top 16 quarterbacks (2011-2018)",
       subtitle = "Week 2 of #TidyTuesday")+
  theme_minimal(16)+
  scale_x_continuous(breaks = 2011:2018,
                     labels = 2011:2018,
                     limits = c(2010,2018))+
  scale_y_continuous(breaks = seq(0,37.5,by=12.5),
                     labels = seq(0,37.5,by=12.5),
                     limits = c(0,37.5) )
