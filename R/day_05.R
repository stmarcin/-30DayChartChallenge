library(tidyverse)
library(extrafont)
loadfonts(device = "win")

df_points <- readxl::read_excel(here::here("data", "day_5_modes.xlsx"),
                         sheet = "point") %>% 
    pivot_longer(-c(mode, color), names_to = "year", values_to = "share") %>% 
    mutate(year = as.numeric(gsub("Y", "", year))) %>% 
    mutate(y = ifelse(mode == "cycling", share+1, share))

df_lines <- readxl::read_excel(here::here("data", "day_5_modes.xlsx"),
                               sheet = "slope") %>% 
    inner_join(df_points %>% distinct(mode, color),
               by = "mode")
    

ggplot() +
    geom_point(data = df_points,
               aes(
                   x = year,
                   y = share,
                   color = color),
               size = 3) +
    geom_segment(data = df_lines,
                 aes(x = before, 
                     xend = after, 
                     y = sharebefore, 
                     yend = shareafter,
                     color = color),
                 size = 1) +
    geom_text(data = filter(df_points, year == 2015),
              aes(x = 2017,
                  y = y,
                  label = paste0(mode, ": ", share, "%")),
              hjust = 0,
              family = "Segoe UI",
              color = "#FCF9D8",
              size = 8*5/14) +
    geom_text(data = filter(df_points, year == 1969 & mode != "cycling"),
              aes(x = 1968,
                  y = y,
                  label = paste0(share, "%")),
              hjust = 1,
              family = "Segoe UI",
              color = "#FCF9D8",
              size = 8*5/14) +
    scale_x_continuous(
        limits = c(1965, 2030),
        expand = c(0, 0),
        breaks = c(1969, 1980, 1993, 1998, 2005, 2015)
    ) +
    scale_color_identity() +
    labs(
        title = "Half of the century of mode share in Warsaw",
        caption = "Visualisation: Marcin Stepniak  •  Data source: Warsaw Traffic Surveys (1969, 1980, 1993, 1998, 2005 & 2015)"
        ) +
    coord_cartesian(clip = "off") +
    ggthemes::theme_solid(fill = "#471914") +
    theme(
        plot.title = element_text(
            family = "Segoe UI",
            size = 18,
            color = "#FCF9D8",
            margin = margin(t = 5, b = 10, unit = "mm")
        ),
        # plot.subtitle = element_text(
        #     family = "Segoe UI",
        #     size = 8,
        #     color = "#b0ae97"
        # ),
        panel.grid.major.x = element_line(
            color = "#FCF8D7",
            size = 0.1,
            linetype = "dashed"
        ),
        plot.caption = element_text(
            family = "Segoe UI",
            size = 8,
            hjust = 0.5,
            color = "#b0ae97",
            margin = margin(t = 10, b = 5, unit = "mm")
        ),
        axis.text.x = element_text(
            family = "Segoe UI",
            size = 7,
            color = "#FCF9D8"
        )
            
    ) 

# ggsave(file = here::here("img", "tests", "day_5.svg"),
#        height = 12,
#        width = 20,
#        units = "cm",
#        dpi = 600
# )

    
ggsave(file = here::here("img", "day_5.png"),
       height = 12,
       width = 20,
       units = "cm",
       dpi = 600
)
    
