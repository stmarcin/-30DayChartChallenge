library(tidyverse)
library(ggthemes)
library(ggtext)
library(ggblur)
library(patchwork)
library(extrafont)
loadfonts(device = "win")

# data for visuals ----------------------------------------------------------------------------

set.seed(1234)

df <- readxl::read_excel(here::here("data", "day_7_games.xlsx")) %>% 
    filter(!is.na(avg_rating)) %>% 
    mutate(y = runif(21, 0.2, 0.8))

# df_text <- tibble(
#     x = c(10.2), 
#     y = c(0.6),
#     label = c("Settlers of Catan is the game with\nthe highest number of votes")
# )

# df_curve <- tibble(
#     x = 10,
#     xend = 7.7,
#     y = 0.7,
#     yend = 0.6)

px <- ggplot(data = df,
       aes(x = avg_rating, y = y)) +
    # geom_boxplot(data = df,
    #              aes(avg_rating, y = 0.5),
    #              stat = "boxplot",
    #              width=0.5,
    #              outlier.colour = NA,
    #              fill = NA, 
    #              color = "#999999",
    #              size = 0.15) +
    geom_point_blur(size = 0.4, alpha = 0.3, aes(blur_size = num_votes), color = "#ffa500") +
    
    # geom_label(data = df_text,
    #            fill = "#fff6e6",
    #            color = "#e69500",
    #            aes(x = x, 
    #                y = y,
    #                label = label),
    #            label.padding = unit(1, "mm"),
    #            family = "Ebrima",
    #            size = 4*5/14) +
    # geom_curve(data = df_curve,
    #              aes(x = x, y = y, xend = xend, yend = yend),
    #              curvature = -0.3) +
    geom_segment(
        aes(x = 0, xend = 10, y = 0, yend = 0),
        color = "#999999",
        size = 0.2) +
    scale_y_continuous(
        expand = c(0, 0),
        limits = c(0, 1)
        ) +
    scale_x_continuous(
        limits = c(0, 10),
        expand = c(0, 0),
        breaks = seq(0,10)
        ) +
    ggthemes::theme_solid(fill = "#fff6e6") +
    coord_cartesian(clip = "off") +
    theme(
        axis.ticks.length.x = unit(1, "mm"),
        axis.ticks.x = element_line(
            color = "#999999",
            size = 0.2),
        axis.text.x = element_text(
            family = "Ebrima",
            size = 4,
            color = "#999999"),
        legend.position = "none"
        
    )
    

# pt <- ggplot(data = )

px + plot_annotation(
    title = "Distribution of average ratings of **board games** from our collection",
    subtitle = "Average ratings according to boardgamegeek.com<br>The size of the blurr shows the number of votes",
    caption = "Visualisation: Marcin Stepniak  •  Data: boardgamegeek.com",
    theme = theme(
        plot.background = element_rect(fill  = "#fff6e6", 
                                       color = NA),
        plot.title = element_markdown(
            family = "Bernard MT Condensed",
            size = 10,
            color = "#e69500",
            hjust = 0.5,
            margin = margin(t = 3, b = 3, unit = "mm")
            ),
        plot.subtitle = element_markdown(
            family = "Ebrima",
            size = 6,
            color = "#e69500",
            hjust = 0.5,
            lineheight = 1.2,
            margin = margin(
                t = 0,
                b = 2,
                unit = "mm")),
        plot.caption = element_markdown(
            family = "Ebrima",
            size = 5,
            color = "#e69500",
            hjust = 0.5,
            margin = margin(t = 4, b = 1, unit = "mm"))
        )
    ) 

ggsave(file = here::here("img", "day_9.svg"),
       height = 6,
       width = 12,
       units = "cm",
       dpi = 600)
    
ggsave(file = here::here("img", "day_9.png"),
       height = 6,
       width = 12,
       units = "cm",
       dpi = 600)
