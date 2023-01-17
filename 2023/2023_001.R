# setup -------------------------------------------------------------------
# install.packages("tidytuesdayR")
library(dplyr)
library(ggplot2)
library(relper)
library(tidyr)
library(stringr)
library(forcats)

# import ------------------------------------------------------------------

or_data <- tidytuesdayR::tt_load(2023, week = 03)

plt_tidytuesday <-
  labs(
    subtitle = "#TidyTuesday 17/01/2023",
    caption = "Source: Lemus S., Stam H. (2022) | Graphic: @vbfelix."
  )

color_tidytuesday <- palette_qua(name = "heartstopper")[2]

artists_df <-
  or_data$artists %>% 
  rename_with(.fn = ~str_remove(.,"artist_")) %>% 
  mutate(
    period = cut(year,
                 breaks = seq(1900,2050,25),dig.lab = 5),
    white_male = if_else(gender == "Male" & race_nwi == "White","Yes","No"),
    period = fct_recode(period,"(2000,2020]" = "(2000,2025]")
    ) %>% 
  glimpse()


# male-white-artists|period -----------------------------------------------

artists_df %>%
  count(period,gender,race_nwi) %>% 
  group_by(period) %>% 
  mutate(
    perc = as_perc(n,sum = TRUE),
    label = str_c(format_num(perc),"% (",format_num(n,0),")")
    ) %>% 
  filter(gender == "Male" & race_nwi == "White") %>% 
  ggplot(aes(period, perc))+
  geom_col(col = "black", alpha = .5, fill = color_tidytuesday)+
  geom_text(aes(label = label), fontface = "bold",nudge_y = -4)+
  plt_theme_y(14)+
  scale_y_continuous(expand = c(0,0), breaks = seq(0,100,10), limits = c(0,100))+
  labs(
    x = "Period of publication.",
    y = "%",
    fill = "",
    title = "Proportion of editions by white male artists (%), by period."
  )+
  plt_flip_y_title+
  plt_water_mark(vfx_watermark)+
  plt_tidytuesday


# male-white-artists|period -----------------------------------------------

artists_df %>% 
  # filter(gender %in% "Male")
  ggplot(aes(space_ratio_per_page_total,period, fill = white_male))+
  ggridges::geom_density_ridges(alpha = .75)+
  plt_theme_x()+
  labs(
    x = "Space ratio per page total",
    y = "",
    fill = "White male artist:",
    title = "Density of space ratio (< 1), by period."
  )+
  scale_x_continuous(breaks = seq(0,5,.1), expand = c(0,0), limits = c(0,1))+
  plt_tidytuesday+
  theme(legend.text = element_text(size = 12))+
  scale_fill_manual(values = palette_qua(name = "heartstopper"))+
  plt_water_mark(vfx_watermark)
  
