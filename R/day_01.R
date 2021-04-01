# Libraries
library(tidyverse)
library(waffle)
library(ggtext)
library(stringr)
library(extrafont)
loadfonts()

# colors
color_media <- c(
    "car" = "#a36a69",
    "transit" = "#3f97d6", 
    "walking" = "#6f927c",
    "bike" =  "#9ab3a3")

# data
df <- tibble(name = c("car", "transit", "walking", "bike"),
             label = c("car", "transit", "walking", "bike"),
             value = c(32, 47, 18, 3)) %>% 
    mutate(x = c(4, 11, 18, 20)) %>% 
    mutate(x.proc = c(4, 11, 18, 20.1)) 

# plot
df %>% 
    ggplot(aes(fill = name, values = value)) +
    geom_waffle(n_rows = 5, size = 0.33, colour = "white") +
    geom_text(aes(label = label, y = 6, x = x),
              family = "Ebrima",
              size = 9) +
    geom_text(aes(label = paste0(value, "%"), y = 3, x = x.proc),
              family = "Ebrima",
              size = 10, color = "white", fontface = "bold") +
    labs(
        title = "How Warsaw residents move",
        caption = "Visualisation: Marcin Stepniak  •  Data source: Warsaw Traffic Study (2015)"
    ) +
    scale_fill_manual(values = color_media) +
    coord_cartesian(clip = "off") +
    theme_void() +
    theme(
        legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_textbox_simple(
            family="Ebrima",
            size = 24,
            lineheight = 1.5,
            halign=0.5,
            margin = unit(c(0, 0, 1, 0), "cm"),
            padding = margin(5.5, 5.5, 5.5, 5.5),
            fill = "#dd9129",
            color = "white"
            ),
        plot.background = element_rect(
            fill = "#fef7ee", 
            color = "#fef7ee"),
        plot.caption = element_text(
            family="Ebrima",
            size = 12,
            hjust = 0.93,
            margin = margin(b = 5)
            )
    )

# export to png
ggsave(here::here("img", "day_1.png"), height = 5, width = 16, dpi = 600)
