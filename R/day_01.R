# remotes::install_github("hrbrmstr/waffle")
library(tidyverse)
library(waffle)
library(trimisViz)
library(ggtext)
library(stringr)
library(extrafont)
loadfonts()

nfig <- "fig_10" 


color_media <- c(
    "car" = "#a36a69",
    "transit" = "#3f97d6", 
    "walking" = "#6f927c",
    "bike" =  "#9ab3a3")

df <- tibble(name = c("car", "transit", "walking", "bike"),
             label = c("car", "transit", "walking", "bike"),
             value = c(32, 47, 18, 3)) %>% 
    mutate(x = c(4, 11, 18, 20)) %>% 
    mutate(x.proc = c(4, 11, 18, 20.1)) 

df %>% 
    ggplot(aes(fill = name, values = value)) +
    geom_waffle(n_rows = 5, size = 0.33, colour = "white") +
    geom_text(aes(label = label, y = 6, x = x),
              family = "Ebrima",
              size = 10) +
    geom_text(aes(label = paste0(value, "%"), y = 3, x = x.proc),
              family = "Ebrima",
              size = 10, color = "white", fontface = "bold") +
    labs(
        title = "How Warsaw residents move"
    ) +
    # coord_cartesian() +
    scale_fill_manual(values = color_media) +
    coord_equal(clip = 'off') +
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
            color = "#fef7ee")
    )

# ggsave(file = here::here("img", "fig_10.png"), dpi = 600,
#        width = 16, height = 5)

ggsave(here::here("img", "day_1.png"), height = 5, width = 16, dpi = 600)
