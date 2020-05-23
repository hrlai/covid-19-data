library(tidyverse)
library(lubridate)
library(scales)
library(ggrepel)
library(ggtext)
library(extrafont)

raw <- 
  read_csv("public/data/testing/covid-testing-all-observations.csv") %>%
  filter(str_detect(Entity, "Malaysia"))

ms <- 
  read_csv("public/data/owid-covid-data.csv") %>%
  filter(location == "Malaysia") %>% 
  # proportion of positive test results (unit = cases)
  mutate(p_pos = new_cases / new_tests,
         week = week(date))

write.csv(ms, "clean/malaysia_tests.csv")

ggplot(ms) +
  geom_line(aes(date, new_tests)) +
  geom_point(aes(date, new_tests)) +
  geom_line(aes(date, new_cases)) +
  geom_point(aes(date, new_cases)) 

ggplot(ms) +
  geom_line(aes(date, p_pos))
  
ggplot(ms, aes(total_tests, total_cases)) +
  geom_line() +
  geom_point() 



# Accumulation curve --------------------------------------------------------
# using weekly summary

ms_weekly <- 
  ms %>% 
  group_by(week) %>% 
  summarise(date = max(date),
            total_cases = max(total_cases),
            # total_deaths = max(total_deaths),
            total_tests = max(total_tests, na.rm = TRUE)) %>% 
  arrange(date) %>% 
  mutate(new_cases = total_cases - lag(total_cases),
         new_tests = total_tests - lag(total_tests),
         date_lab = paste0(day(date), " ", month(date, label = TRUE),
                          " (",total_cases,")")) %>% 
  filter(!is.infinite(total_tests),
         !is.infinite(new_tests),
         week >= 9)

contours <- 
  data.frame(x = seq(1, 1e5, length.out = 100)) %>% 
  mutate(y100 = x,
         y50 = 0.5 * x,
         y10 = 0.1 * x,
         y5 = 0.05 * x,
         y1 = 0.01 * x)
contour_labs <- 
  data.frame(x = rep(400, 5)) %>% 
  mutate(y = c(0.01, 0.05, 0.10, 0.50, 1) * 400,
         ylab = paste0(c(1, 5, 10, 50, 100), "%"))

p_tests <-
  ggplot() +
  geom_line(data = contours, aes(x, y100), colour = "grey", linetype = 2) +
  geom_line(data = contours, aes(x, y50), colour = "grey", linetype = 2) +
  geom_line(data = contours, aes(x, y10), colour = "grey", linetype = 2) +
  geom_line(data = contours, aes(x, y5), colour = "grey", linetype = 2) +
  geom_line(data = contours, aes(x, y1), colour = "grey", linetype = 2) +
  geom_text(data = contour_labs, aes(x, y, label = ylab),
            size = 2, colour = "darkgrey", angle = 20,
            nudge_y = 0.1) +
  geom_path(data = ms_weekly, 
            aes(new_tests, new_cases),
            colour = "skyblue", size = 1, lineend = "round", 
            arrow = arrow(length = unit(0.6, "lines"),
                          angle = 20,
                          type = "open")) +
  geom_text_repel(data = ms_weekly, 
                  aes(new_tests, new_cases, label = date_lab),
                  direction = "y", nudge_y = 0.2, nudge_x = -0.1,
                  size = 3, fontface = "italic",
                  segment.alpha = 0.6, segment.colour = "darkgrey") +
  geom_point(data = ms_weekly, 
             aes(new_tests, new_cases),
             pch = 21, fill = "white", size = 2) +
  scale_x_log10(breaks = c(500, 1000, 5000, 1e4, 3e4)) +
  scale_y_log10(breaks = c(5, 10, 50, 100, 500, 1000)) + 
  annotation_logticks(sides = "trbl")  +
  coord_cartesian(expand = FALSE, 
                  ylim = c(3, 4000), xlim = c(300, 200000)) +
  labs(x = "No. new tests", 
       y = "No. new cases (positive tests)",
       title = "Weekly trends in COVID-19 tests, Malaysia",
       subtitle = "The number of new cases per week plotted against the number of new tests per week. A week\nlapses between data points, except for the latest point. Values in parentheses next to the dates\nare total number of confirmed cases (cumulative). Note that MOH reports 'number of cases\ntested' which does not differentiate between tests on new individual and repeated tests on the\nsame individual. Also note the logarithmic scale of both axes. Dashed lines are contours\ndenoting percentage positive tests over total tests.",
       caption = paste0("MOH Malaysia data compiled by ourworldindata.org. Last updated ", max(ms_weekly$date), ".\n See https://ourworldindata.org/covid-testing for more.")) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 9),
        plot.caption = element_text(size = 8, face = "italic"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#f8f2e4"))
p_tests
ggsave("figs/malaysia_testing.png",
       p_tests,
       width = 6, height = 5, units = "in",
       dpi = 300)





# -------------------------------------------------------------------------
world <- 
  read_csv("public/data/owid-covid-data.csv") %>% 
  filter(!is.na(total_tests),
         !is.na(total_cases),
         total_cases >= 100)

# # countries with similar total cases as Malaysia
# ms_max_cases <- max(world$total_cases[world$location=="Malaysia"])
# current_total_cases <- 
#   world %>% 
#   group_by(location) %>% 
#   filter(date == max(date)) %>% 
#   mutate(diff_ms = log(total_cases) - log(ms_max_cases)) %>% 
#   arrange(abs(diff_ms))
# top_similar_cases <- current_total_cases$location[2:6]
# 
# # countries with similar total tests as Malaysia
# ms_max_tests <- max(world$total_tests[world$location=="Malaysia"])
# current_total_tests <- 
#   world %>% 
#   group_by(location) %>% 
#   filter(date == max(date)) %>% 
#   mutate(diff_ms = log(total_tests) - log(ms_max_tests)) %>% 
#   arrange(abs(diff_ms))
# top_similar_tests <- current_total_tests$location[2:6]

# slope in the past month
world_last_month <- 
  world %>% 
  group_by(location) %>% 
  filter(date >= max(date) - 28,
         # filter countries with 'sufficient' data points
         n() >= 20)
slopes <- lapply(unique(world_last_month$location), function(x) {
  dat <- filter(world_last_month, location == x)
  mod <- lm(log(total_cases) ~ log(total_tests), data = dat)
  return(coef(mod)[2])
})
slopes <- 
  data.frame(location = unique(world_last_month$location), 
             slope = unlist(slopes)) %>% 
  arrange(slope)

world_top <- world %>% filter(location %in% head(slopes$location, 5))
world_bottom <- world %>% filter(location %in% tail(slopes$location, 5))
most_cases <- world %>% group_by(location) %>% filter(total_cases == last(total_cases)) %>% ungroup %>% top_n(10, total_cases)

# textboxes
tb <- data.frame(
  label = 
    c("Blue lines are the top-5 countries with the flattest curves in the past 4 weeks (i.e. getting less positive tests)",
      "Red lines are the top-5 countries where positive tests remained high in the past 4 weeks (i.e. confirmed cases continue to rise with more tests)",
      "Grey labels highlight the top-10 countries with the most confirmed cases. <span style='font-size:7pt'>There is no test data from China.</span>"),
  x = c(4e5, 13e3, 3.5e5),
  y = c(6e2, 11e3, 2.5e5),
  hjust = c(0, 1, 1),
  vjust = c(1, 0, 0),
  colour = c("blue", "red", "darkgrey")
)

world_p <- 
  ggplot() +
  geom_line(data = world,
            aes(total_tests,
                total_cases,
                group = location),
            colour = "lightgrey", lineend = "round", size = 0.4) +
  geom_line(data = world_top,
            aes(total_tests,
                total_cases,
                group = location),
            colour = "blue", lineend = "round") +
  geom_line(data = world_bottom,
            aes(total_tests,
                total_cases,
                group = location),
            colour = "red", lineend = "round") +
  geom_line(data = world %>% filter(location == "Malaysia"), 
            aes(total_tests, 
                total_cases),
            size = 1.2, lineend = "round") + 
  geom_text_repel(data = 
                    world_top %>% 
                    group_by(location) %>% 
                    filter(date == last(date)),
                  aes(total_tests,
                      total_cases,
                      label = location),
                  colour = "blue", fontface = "italic",
                  segment.alpha = 0.5) +
  geom_text_repel(data = 
                    world_bottom %>% 
                    group_by(location) %>% 
                    filter(date == last(date)),
                  aes(total_tests,
                      total_cases,
                      label = location),
                  colour = "red", fontface = "italic",
                  segment.alpha = 0.5) +
  geom_text_repel(data = most_cases,
                  aes(total_tests,
                      total_cases,
                      label = location),
                  colour = "darkgrey", fontface = "italic",
                  segment.alpha = 0.5) +
  geom_text_repel(data = 
                    world %>% 
                    filter(location == "Malaysia") %>% 
                    top_n(1, total_cases),
                  aes(total_tests,
                      total_cases,
                      label = location),
                  nudge_x = -.3, nudge_y = 0.1,
                  colour = "black", fontface = "italic") +
  geom_textbox(data = tb, 
               aes(x, y, label = label, colour = colour,
                   hjust = hjust, vjust = vjust),
               maxwidth = unit(1.7, "in"),
               box.padding = margin(3,3,3,3),
               box.r = unit(3, "pt"),
               size = 3.5) +
  scale_discrete_identity(aesthetics = c("color")) +
  # scale_x_log10(labels = trans_format("log10", math_format(10^.x))) +
  # scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  scale_x_log10(breaks = 10^c(3:7),
                labels = c("1 K", "10 K", "100 K", "1 M", "10 M")) +
  scale_y_log10(breaks = 10^c(2:6),
                labels = c("100", "1 K", "10 K", "100 K", "1 M")) +
  # scale_x_continuous(trans = "log") +
  # scale_y_continuous(trans = "log") +
  annotation_logticks(sides = "trbl", colour = "grey") +
  labs(x = "Total tests",
       y = "Total confirmed cases",
       title = "More tests, more confirmed cases? A comparison.",
       subtitle = paste0("Total confirmed COVID-19 cases against the total number of tests across ", length(unique(world$location)), " countries, where test data are publicly available. **Malaysia** is the thick black line, ranking 10^th in having a plateauing curve ", emo::ji("clap"), ". Note that the unit for test differs among countries; they may be one of 'tests performed', 'people tested', 'samples tested', or 'units unclear or inconsistent'. Also note the logarithmic scale on both axes."),
       caption = paste0("Data compiled by Our World in Data (github.com/owid/covid-19-data).\nTotal confirmed cases from the European Centre for Disease Prevention and Control;\nTotal tests from separate national government reports. Last updated ", max(world$date))) +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(family = "CMU Sans Serif"),
        plot.title.position = "plot",
        plot.title = element_textbox_simple(face = "bold", size = 14, 
                                            family = "CMU Serif", 
                                            width = unit(6, "in"),
                                            padding = margin(0, 5, 5, 5)),
        plot.subtitle = 
          element_textbox_simple(size = 11, width = unit(6, "in"),
                                 padding = margin(0, 5, 5, 5)),
        plot.caption = element_text(size = 7),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "#f8f2e4"),
        axis.ticks = element_blank(),
        axis.text = element_text(colour = "black"))

world_p
ggsave("figs/world_testing.png",
       world_p,
       width = 6, height = 7, units = "in",
       dpi = 300)


# plotly::ggplotly()
