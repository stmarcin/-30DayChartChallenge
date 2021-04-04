# purrr oriented

library(harrypotter)
library(tidyverse)
library(stringr)
library(patchwork)
library(ggtext)
library(extrafont)
loadfonts()

# get the data --------------------------------------------------------------------------------

books <- list(
    harrypotter::philosophers_stone,
    harrypotter::chamber_of_secrets,
    harrypotter::prisoner_of_azkaban,
    harrypotter::goblet_of_fire,
    harrypotter::order_of_the_phoenix,
    harrypotter::half_blood_prince,
    harrypotter::deathly_hallows
)

titles <- tibble(
    part = seq(1, 7),
    book = c("philosophers_stone",
             "chamber_of_secrets",
             "prisoner_of_azkaban",
             "goblet_of_fire",
             "order_of_the_phoenix",
             "half_blood_prince",
             "deathly_hallows"),
    title = c("Philosopher’s Stone",
              "Chamber of Secets",
              "Prisoner of Azkaban",
              "Goblet of Fire",
              "Order of the Phoenix",
              "Half-Blood Prince",
              "Deathly Hallows"))

names(books) <- c("philosophers_stone",
                  "chamber_of_secrets",
                  "prisoner_of_azkaban",
                  "goblet_of_fire",
                  "order_of_the_phoenix",
                  "half_blood_prince",
                  "deathly_hallows")


modes <- c("ship",
           "boat",
           "train",
           "flying car",
           "bus", # includes Knight Bus
           "dragon",
           "motorcycle",
           "broomstick", 
           "broom") 

modes_variants <- purrr::map_chr(modes, function(x){
    paste(paste0(" ", x, " "), 
          paste0(" ", x, "\\."),
          paste0(" ", x, "s "),
          paste0(" ", x, "s\\."),
          paste0(" ", x, "\\'s"),
          paste0(str_to_upper(str_sub(x, 1, 1)) , str_sub(x, 2, nchar(x)), " "),
          paste0(str_to_upper(str_sub(x, 1, 1)) , str_sub(x, 2, nchar(x)), "\\."),
          paste0(str_to_upper(str_sub(x, 1, 1)) , str_sub(x, 2, nchar(x)), "\\'s"),
          sep = "|")
})

# original names in HP
modes_names <- c("Hogwarts Express",
                 "Flying Ford Anglia",
                 "Weasleys’ car",
                 "Nimbus",
                 "Firebolt",
                 "Flying carpet",
                 "Phoenix",
                 "Thestral",
                 "Hippogriff",
                 "Centaur",
                 "Vanishing cabinet",
                 "Floo Powder",
                 "Apparition",
                 "Teleportation",
                 "Portkey") 

modes_all <- c(modes_variants, modes_names)

names(modes_all) <- c(modes, modes_names)


count_modes <- purrr::map(books, function(y) purrr::map_df(modes_all, function(x) sum(stringr::str_count(y, pattern = x)))) %>% 
    dplyr::bind_rows(.id = "book")

count_modes <- tibble(book = "all") %>% 
    bind_cols(summarise_if(count_modes, is.numeric, sum, na.rm = TRUE)) %>% 
    bind_rows(count_modes) %>% 
    pivot_longer(-book, names_to = "mode") %>% 
    inner_join(
        readr::read_csv(here::here("data", "HP_modes.csv"), locale = readr::locale(encoding = "UTF-8")),
        by = c("mode" = "keyword")
    )

all_books <- count_modes %>% 
    filter(book == "all") %>% 
    group_by(category) %>% 
    summarise(mode = sum(value))

books <- count_modes %>% 
    filter(book != "all") %>% 
    group_by(book, category) %>% 
    summarise(mode = sum(value)) %>% 
    inner_join(titles,
               by = "book")

# plot ----------------------------------------------------------------------------------------



p1 <- ggplot(data = all_books, 
       aes(x=reorder(category, mode), 
           y=mode)) +
    geom_bar(stat = "identity",
             fill = "#EEBA30",
             width = 0.6) +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0, max(all_books$mode*1.2))) +
    coord_flip() +
    labs(
        title = "Parts one to seven together") +
    ggthemes::theme_solid(fill = "#740001") +
    theme(
        plot.title.position = "plot",
        plot.title = element_text(
            family = "Footlight MT Light",
            color = "#EEBA30",
            size = 11,
            margin = margin(t = 2, b = 4, unit = "mm")),
        axis.text = element_text(
            family = "Footlight MT Light",
            color = "#C0C0C0"),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 9),
        axis.line = element_line(color = "#C0C0C0",
                                 size = 0.3),
        panel.grid.major.x = element_line(color = "#C0C0C0",
                                          size = 0.15,
                                          linetype = "dotted")
        
        )


p2 <- ggplot(data = books, 
       aes(x=reorder(book, -part), 
           y=mode)) +
    geom_bar(stat = "identity",
             fill = "#EEBA30",
             width = 0.6) +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0, max(books$mode*1.1))) +
    scale_x_discrete(breaks = books$book,
                     label = books$title) +
    labs(
        title = "Appearance in the particular book",
        caption = 'Appearance of a particular word or words in the novel calculated using the following categories:<br><i>Hagrid’s flying motorcycle</i>: motorcycle; <i>Broomsticks</i>:broom, broomstick, Firebolt, Nimbus; <i>Fantastic creatures</i>: centaur, dragon, Hippogriff, Phoenix, Thestral;<br><i>Boats</i>:boats, ships; <i>Hogwarts Express</i>: train, Hogwarts Express; <i>Knight Bus</i>: bus; <i>Weasleys’ car</i>: flying car, Flying Ford Anglia, Weasleys’ car;<br><i>Magical objects</i>: Floo Powder, Flying carpet, Portkey, Vanishing cabinet; <i>Spells</i>: Apparition, Teleportation.') +
    coord_flip() +
    facet_wrap(~category) +
    theme(
        plot.background = element_rect(fill = "#740001", color = "#740001"),
        panel.background = element_rect(fill = "#740001", color = "#a9851e"),
        
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_text(
            family = "Footlight MT Light",
            color = "#EEBA30",
            size = 11,
            margin = margin(t = 2, b = 4, unit = "mm")),
        plot.caption = element_markdown(
            family = "Footlight MT Light",
            color = "#C0C0C0",
            lineheight = 1.3,
            size = 5.5,
            hjust = 0,
            margin = margin(t = 4, b = 2, unit = "mm")),
        
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color = "#C0C0C0",
                                          size = 0.1,
                                          linetype = "dotted"),
    
        axis.line = element_blank(),
        
        axis.text = element_text(
            family = "Footlight MT Light",
            color = "#C0C0C0"),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 8),
        
        axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(color = "#868686",
                                    size = 0.2),
        
        strip.background = element_rect(fill = "#a9851e", color = "#a9851e"),
        strip.text = element_text(
            family = "Footlight MT Light",
            color = "#740001",
            size = 8)
        
    )
 
p1/p2  +
    plot_layout(heights = c(0.35, 0.65)) +
    plot_annotation(
        title = "Transport modes in **Harry Potter** series",
        subtitle = "How often does a particular *transport mode* appear in <b>Harry Potter</b> series?",
        caption = "Visualisation: Marcin Stepniak<br>Harry Potter consultation: Maja Stepniak  •  Data via package bradleyboehmke/harrypotter by @bradleyboehmke",
        theme = theme(
            plot.background = element_rect(fill = "#740001", color = "#740001"),
            plot.title = element_markdown(
                family = "Algerian",
                color = "#EEBA30",
                size = 20,
                hjust = 0.5,
                margin = margin(t = 5, b = 5, unit = "mm")),
            plot.subtitle = element_markdown(
                family = "Footlight MT Light",
                color = "#EEBA30",
                size = 13),
            plot.caption = element_markdown(
                family = "Footlight MT Light",
                color = "#EEBA30", face = "plain",
                size = 8,
                lineheight = 1.5,
                hjust = 0.5),
        )
    ) 
    
# ggsave(file = here::here("img", "tests", "day_4", "day_4.svg"),
#        height = 24,
#        width = 16,
#        units = "cm",
#        dpi = 600
# )

ggsave(file = here::here("img", "day_4.png"),
       height = 24,
       width = 16,
       units = "cm",
       dpi = 600
)
