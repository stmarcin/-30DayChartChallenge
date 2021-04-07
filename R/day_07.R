library(tidyverse)
library(ggtext)
library(patchwork)
library(extrafont)
loadfonts(device = "win")

df <- readxl::read_excel(here::here("data", "day_7_games.xlsx")) %>% 
    mutate(xmin = 20-width/2) %>% 
    mutate(xmax = 20+width/2) %>% 
    mutate(ymin = 20-length/2) %>% 
    mutate(ymax = 20+length/2)

p1 <- ggplot(data = df) +
    geom_rect(
        aes(
            xmin = xmin,
            xmax = xmax,
            ymin = ymin,
            ymax = ymax
        ),
        fill = "#fffa6e",
        color = NA,
        # size = 0.1,
        alpha = 0.02) +
    geom_rect(
        aes(
            xmin = xmin,
            xmax = xmax,
            ymin = ymin,
            ymax = ymax
        ),
        fill = NA,
        color = "#FFD700",
        size = 0.15) +
    scale_x_continuous(limits = c(0,40),
                       expand = c(0, 0)) +
    scale_y_continuous(limits = c(0,40),
                       expand = c(0, 0)) +
    coord_fixed(clip = "off") +
    ggthemes::theme_solid(fill = "#0d0d00") +
    theme(
        plot.margin = margin(0,0,0,0)
          )
    
py <- ggplot(data = df) +
    geom_density(aes(y = width),
                 fill = "#fffa6e",
                 color = "#FFD700",
                 size = 0.2,
                 alpha = 0.2) +
    scale_y_continuous(limits = c(0,40),
                       expand = c(0, 0)) +
    scale_x_continuous(limits = c(0, 0.08),
                       expand = c(0, 0)) +
    ggthemes::theme_solid(fill = "#0d0d00") +
    theme(
        plot.margin = margin(0,0,0,0)
    )

px <- ggplot(data = df) +
    geom_density(aes(x = length),
                 fill = "#fffa6e",
                 color = "#FFD700",
                 size = 0.2,
                 alpha = 0.2) +
    scale_y_continuous(limits = c(0, 0.06),
                       expand = c(0, 0)) +
    scale_x_continuous(limits = c(0,40),
                       expand = c(0, 0)) +
    coord_cartesian(clip = "off") +
    ggthemes::theme_solid(fill = "#0d0d00") +
    theme(
        plot.margin = margin(0,0,0,0)
        
    )

pn <- ggplot() +
    ggthemes::theme_solid(fill = "#0d0d00") 


design <- "
  12
  34
"


px + pn + p1 + py + plot_layout(design = design,
                           width = c(10,2), heights = c(2,10)) +
    plot_annotation(
        title = "Our collection of **board games**",
        subtitle = "The chart below shows **physical** dimensions of 30 of board games from our collection:  
        + Main chart shows boxes overlapping each other, sharing the same central position.  
        + Density curves scales from 0 to 40 cm are located on the vertical and horizontal margins.",
        caption = "Visualisation: Marcin Stepniak  •  Data collection: Dominika Stepniak",
        theme = theme(plot.background = element_rect(fill  = "#0d0d00", 
                                                     color = NA),
                      plot.title = element_markdown(
                          family = "Britannic Bold",
                          size = 18,
                          color = "#FFD700",
                          hjust = 0.5,
                          margin = margin(t = 3, b = 3, unit = "mm")),
                      plot.subtitle = element_markdown(
                          family = "Ebrima",
                          size = 6,
                          color = "#fffdc5",
                          hjust = 0,
                          lineheight = 1.5,
                          margin = margin(l = 20, t = 3, b = 3, unit = "mm")),
                      plot.caption = element_markdown(
                          family = "Ebrima",
                          size = 7,
                          color = "#fffdc5",
                          hjust = 0.5,
                          margin = margin(t = 4, b = 1, unit = "mm"))
                      )
        )

# 
# ggsave(file = here::here("img", "day_7.svg"),
#        height = 14,
#        width = 12,
#        units = "cm",
#        dpi = 600)

ggsave(file = here::here("img", "day_7.png"),
       height = 14,
       width = 12,
       units = "cm",
       dpi = 600)


