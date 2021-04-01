# Libraries
library(tidyverse)
library(ggimage)
library(ggtext)

# Fonts
library(extrafont)
loadfonts(device = "win")
theme_set(theme_void(base_family = "Consolas"))

# Data and symbols
df <- tibble(
    x = rep(seq(0, 14*0.3, 0.3), each=10),
    y = rep(seq(0, 9*0.2, 0.2), 15),
    image = c(rep(here::here("data", "icons", "iconmonstr-car-5.png"), 5),
              rep(here::here("data", "icons", "iconmonstr-car-1.png"), 145)) ) %>% 
    mutate(y = ifelse(image == here::here("data", "icons", "iconmonstr-car-5.png"), 
                      y + 0.015, y))

# Legend 
legend <- tibble(
    x = -0.15,
    y = -0.15,
    label = "Each symbol represents **100 000** registered cars"
    )

# Figure
ggplot(df, aes(x, y)) + 
    geom_image(aes(image=image), size=0.05, asp = 24/13) +
    geom_richtext(data = legend,
                  aes(x = x, y = y, label = label),
                  size = 4.5*5/14,
                  color = "#606060",
                  hjust = 0, 
                  family = "Consolas",
                  fill = NA, label.colour = NA) +
    coord_cartesian(clip = 'off') +
    scale_x_continuous(expand = c(0,0)) +
    labs(
        title = "New registrations of <span style='color:#00a000'>electric</span> and <span style='color:black'>conventional</span> cars (2019)",
        subtitle = "The EU-27, Iceland, Norway and the United Kingdom",
        caption = "Visualisation: Marcin Stepniak  •  Data source: European Environment Agency"
    ) +
    theme(
        plot.margin = margin(t = 2, r = 8, b = 2, l = 5, unit = "mm"),
        plot.background = element_rect(
            fill = "#f8f1e1", 
            color = "#f8f1e1"),
        plot.title = element_markdown(
            color = "#888888",
            size = 11,
            margin = margin(t = 5),
            hjust = 0,
            face = "bold"
            ),
        plot.subtitle = element_markdown(
            color = "#888888",
            size = 9,
            margin = margin(t = 5, b = 10),
            hjust = 0
            ),
        plot.caption = element_markdown(
            color = "#888888",
            size = 5,
            hjust = 1.05,
            margin = margin(t = 0, l = 0)
            ),
        plot.title.position = 'plot',
        plot.caption.position = 'plot'
    )

# export to png
ggsave(
    file = here::here("img", "day_1.png"),
    height = 10,
    width = 15,
    units = "cm",
    dpi = 600) 


# export temporary images ---------------------------------------------------------------------

ggsave(
    file = here::here("img", "tests",
                      paste("day_1",
                            paste(paste(
                                gsub("-", "", str_sub(Sys.time(), 1, 10)),
                                gsub(":", "", str_sub(Sys.time(), 12, 19)), sep = "_"),
                                "png", sep = "."), sep = "_")),
    height = 10,
    width = 15,
    units = "cm",
    dpi = 600) 

