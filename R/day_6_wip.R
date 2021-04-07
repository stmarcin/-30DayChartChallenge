library(tidyverse)
library(ggbump)
library(ggtext)
library(extrafont)
loadfonts(device = "win")

df_years <- readxl::read_excel(here::here("data", "day_5_ascensores.xlsx"),
                         sheet = "years") %>% 
    mutate(fin = ifelse(is.na(Cese_de_actividad), 2021, Cese_de_actividad)) %>% 
    rowid_to_column("id") %>% 
    mutate(start = mean(id)) %>% 
    mutate(end = ifelse(!is.na(Cese_de_actividad), NA, mean(id))) 

ggplot(df_years) +
    geom_sigmoid(
        aes(x = 1850,
            xend = 1870,
            y = mean(id),
            yend = id,
            group = factor(Nombre)),
        color = "#8d8073",
        size = 0.04) +
    geom_segment(
        aes(x = 1870, 
            y = id, 
            xend = Inauguracion,
            yend = id),
        color = "#8d8073",
        size = 0.04) +
    geom_segment(
        aes(x = Inauguracion, 
            y = id, 
            xend = fin,
            yend = id),
        size = 1.2,
        color = "#9e9082") +
    geom_sigmoid(
        data = filter(df_years, is.na(Cese_de_actividad)),
        aes(x = 2021,
            xend = 2040,
            y = id,
            yend = mean(id),
            group = factor(Nombre)),
        color = "#8d8073",
        size = 0.04) +
    labs(
        title = "The rise and decline of the Valparaíso funiculars",
        caption = "Visualisation: Marcin Stepniak  •  Source: wikipedia"
    ) +
    # geom_text(
    #     aes(x = (Inauguracion + fin)/2,
    #         y = id,
    #         label = Nombre),
    #     size = 8.5*5/14,
    #     color = "#46403a",
    #     family = "Segoe UI Light"
    # ) +
    ggthemes::theme_solid(fill = "#d8d0c8") +
    scale_x_continuous(breaks = c(1900, 1950, 2000),
                       labels = c(1900, 1950, 2000)) +
    theme(
        plot.title = element_text(
            family = "Segoe UI Historic",
            size = 10,
            face = "bold",
            color = "#585048",
            margin = margin(t = 3, b = 2, unit = "mm")),
        plot.margin = margin(b = 0),
        plot.caption = element_text(
            family = "Segoe UI Historic",
            size = 4,
            # face = "bold",
            color = "#585048",
            hjust = 0.5,
            margin = margin(t = 3, b = 2, unit = "mm")
            ),
        panel.grid.major.x = element_line(
            size = 0.1,
            # linetype = "dotted",
            color = "#efece9" # "#9e9082"
        ),
        axis.text.x = element_text(
            family = "Segoe UI",
            size = 5,
            color = "#585048"
        )
        
    )
        

ggsave(file = here::here("img", "tests", "day_6.svg"),
       height = 7,
       width = 12,
       units = "cm",
       dpi = 600)

ggsave(file = here::here("img", "day_6.png"),
       height = 6,
       width = 12,
       units = "cm",
       dpi = 600)



df_share <- tibble(
    name = c("total", "in use", "share"),
    n = c(nrow(df_years), 
          nrow(filter(df_years, !is.na(Cese_de_actividad))),
          round(nrow(filter(df_years, is.na(Cese_de_actividad)))*100/nrow(df_years)))
    ) 




# tibble(
#     year = seq(min(df_years$Inauguracion), 2020)) %>% 
#     fuzzyjoin::fuzzy_left_join(
#         df_years %>% 
#             select(Nombre, Inauguracion, fin),
#         by = c("year" = "Inauguracion",
#                "year" = "fin"),
#         match_fun = list(`>=`, `<=`)
#         )
