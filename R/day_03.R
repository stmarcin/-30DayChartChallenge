library(tidyverse)
library(ggforce)
library(ggthemes)
library(ggtext)
library(extrafont)
loadfonts(device = "win")

df <- readr::read_csv(here::here("data", "metro_eu_capitals.csv")) %>% 
    filter(!is.na(year)) %>% 
    mutate(label = paste0(name, ": ", year)) %>% 
    arrange(year)

line_to_point <- tibble(
    x = df$year,
    y = 0,
    yend = c(6, 10, 14, 18, rep(6, 3), 10, 14, 6, 10, 14, 18, 6, 10, 14)) %>% 
    rowid_to_column("id") %>% 
    mutate(xend = x-(yend-y)*tan(60)) 

line_for_name <- tibble(
    name = df$name,
    year = df$year,
    x = line_to_point$xend,
    y = line_to_point$yend) %>% 
    mutate(xend = x - 12) 

theme_set(theme_solid(fill = "#ffdf80") )
          
theme_update(
    plot.background = element_rect(fill = "#ffdf80", color = "#ffdf80"),
    plot.title = element_text(size = 12, color = "#575147", family = "Consolas",
                              face = "bold", margin = margin(t = 2, b = 2, unit = "mm")),
    plot.subtitle = element_text(size = 11, color = "#6d685f", family = "Consolas",
                              face = "italic", margin = margin(b = 4, unit = "mm")),
    plot.caption = element_text(size = 5, color = "#6d685f", family = "Consolas",
                                face = "plain",
                                margin = margin(t = 3, b = 0, unit = "mm"))
    )


ggplot(
    data = df
    ) +
    geom_hline(
        yintercept = 0,
        size = 0.2,
        color = "#696969"
        ) +
    geom_segment(data = line_to_point,
                 aes(x = x, y = y, xend = xend, yend = yend),
                 size = 0.3,
                 color = "#696969"
                 ) +
    geom_segment(data = line_for_name,
                 aes(x = x, y = y, xend = xend, yend = y),
                 size = 0.3,
                 color = "#696969"
    ) +
    geom_richtext(
        data = line_for_name,
        aes(x = xend-1,
            y = y+0.7,
            label = paste0("**", year, ":** ", name)),
        family = "Consolas",
        size = 5*5/14,
        color = "#4b463d",
        fill = NA, 
        label.color = NA,
        hjust = 0
    ) +
    geom_point(
        aes(x = year,
            y = 0),
        shape = 21,
        size = 2,
        color = "#808080",
        fill = "#F5F5E1") +
    scale_x_continuous(
        expand = c(.005, .005),
        limits = c(1880, 2004)
        ) +
    scale_y_continuous(
        expand = c(0, 0),
        limits = c(-2, 24)
    ) +
    coord_cartesian(clip = "off") +
    labs(
        title = "The history of metro in EU capitals",
        subtitle = "When did the first metro line start to operate?",
        caption = "Visualisation: Marcin Stepniak  •  Source: wikipedia"
    )
    

ggsave(file = here::here("img", "day_3.png"), 
       height = 6,
       width = 16,
       units = "cm",
       dpi = 600
)

# temporary -----------------------------------------------------------------------------------

# ggsave(
#     file = here::here("img", "tests", "day_3",
#                       paste("day_3",paste(paste(
#                           gsub("-", "", str_sub(Sys.time(), 1, 10)),
#                           gsub(":", "", str_sub(Sys.time(), 12, 19)), sep = "_"),
#                           "png", sep = "."), sep = "_")),
#     height = 6,
#     width = 16,
#     units = "cm",
#     dpi = 600
# )
# 
#     
# ggsave(file = here::here("img", "tests", "day_3", "day_3.svg"),
#        height = 6,
#        width = 16,
#        units = "cm",
#        dpi = 600
# )