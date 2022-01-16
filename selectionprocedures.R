# 0. Setup ####
## 0.1. Libraries ####
library(tidyverse)
library(ggplot2)
library(ggrepel)

## 0.2. Install and load selected fonts ####
#sysfonts::font_add_google("Source Sans Pro")
windowsFonts("Source Sans Pro" = windowsFont("Source Sans Pro"))

## 0.3. Colours
blue <- "#1E64C8"
lgrey <- "#A6A6A6"


# 1. Data ####
dir <- here::here()

sackett <- 
  readxl::read_xlsx(paste0(dir, "/data/", "Sackett et al_2021.xlsx")) %>%
  type_convert() %>% janitor::clean_names() %>%
  mutate(significant90 = as_factor(significant90))

sackett_p_ord <- 
  readxl::read_xlsx(paste0(dir, "/data/", "Sackett et al_2021_p.xlsx")) %>%
  type_convert() %>% janitor::clean_names()

sackett_bw_d_ord <- 
  readxl::read_xlsx(paste0(dir, "/data/", "Sackett et al_2021_bw_d.xlsx")) %>%
  type_convert() %>% janitor::clean_names()


# 2. Data transformation ####
order_sp <-
  sackett %>%
  arrange(lower90_p) %>%
  select(selection_procedure) %>%
  unique() %>% pull()

sackett$selection_procedure <- factor(sackett$selection_procedure, levels = order_sp)

order_c <-
  sackett %>%
  arrange(order) %>%
  select(category) %>%
  unique() %>% pull()

sackett$category <- factor(sackett$category, levels = order_c)


# 3. Visuals ####
## 3.1. Cover image ####
cover <-
sackett_p_ord %>%
  ggplot(mapping = aes(x = 1, y = p)) +
  scale_y_reverse(limits = c(.42, 0), breaks = NULL) +
  scale_x_continuous(limits = c(1, 1.1)) +
  geom_text(aes(label = selection_procedure), hjust = 0, nudge_x = .003, nudge_y = .003,
            family = "Source Sans Pro",
            colour = "gray20", angle = 30) +
  geom_point(size = 2, colour = blue) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Source Sans Pro", size = 10, colour = "gray20"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.margin = margin(t = 0, r = 0, b = 100, l =0, unit = "pt"))
cover


## 3.2. Uni-dimensional graphs ####
### 3.2.1. Operational validity estimates ####
sackett.p <-
sackett_p_ord %>%
  ggplot(mapping = aes(x = 1, y = p)) +
  scale_y_continuous(limits = c(.05, .42), breaks = sackett_p_ord$p %>% unique()) +
  scale_x_continuous(limits = c(1, 1.1)) +
  geom_text_repel(aes(label = selection_procedure), hjust = 0, nudge_x = .005,
                  family = "Source Sans Pro", colour = "gray20", size = 3.5,
                  segment.colour = "gray60", segment.size = .25, min.segment.length = 0) +
  geom_point(size = 2, colour = blue) +
  labs(title = "Ranking of selection procedures as predictors of job performance",
       subtitle = "Structured interviews supersede cognitive ability measures as top-ranked selection procedure",
       y = "Validity (\U1D70C)",
       caption = paste0("Notes. Operational validity estimates originate from Sackett, Zhang, Berry, and Lievens (2021 \U2013 doi:10.1037/apl0000994).",
                        "\n",
                        "The estimates represent the correlations between the selection procedure (predictor) and job performance (criterion).",
                        "\n \n",
                        "graph by Louis Lippens")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.ticks.y = element_line(size = .25, colour = "gray60"),
        text = element_text(family = "Source Sans Pro", size = 10, colour = "gray20"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 9, family = "Source Sans Pro", colour = "gray20"),
        axis.title.y = element_text(size = 11,
          margin = margin(t = 0, r = 10, b = 0, l =0),
          angle = 0, vjust = .961, hjust = 0),
        plot.title = element_text(size = 12,
                                  margin = margin(t = 0, r = 0, b = 5, l =0),
                                  colour = "gray20"),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 11,
                                     margin = margin(t = 0, r = 0, b = 5, l =0),
                                     colour = "gray40",
                                     lineheight = 1.1),
        plot.caption = element_text(size = 9, hjust = 0, colour = "gray40",
                                    margin = margin(t = 5, r = 0, b = 0, l =0),
                                    lineheight = 1.2),
        plot.caption.position = "plot",
        )
sackett.p

### 3.2.2. Black-White mean differences ####
sackett.bwd <-
sackett_bw_d_ord %>%
  ggplot(mapping = aes(x = 1, y = bw_d)) +
  scale_y_continuous(limits = c(-.07, .79), breaks = sackett_bw_d_ord$bw_d %>% unique()) +
  scale_x_continuous(limits = c(1, 1.1)) +
  geom_text_repel(aes(label = selection_procedure), hjust = 0, nudge_x = .005,
                  family = "Source Sans Pro", colour = "gray20", size = 3.5,
                  segment.colour = "gray60", segment.size = .25, min.segment.length = 0) +
  geom_point(size = 2, colour = blue) +
  labs(title = "Ranking of selection procedures by adverse impact potential",
       subtitle = paste0("Cognitive ability and work samples have the highest adverse impact potential in selection",
                         "\n", "Personality measures and structured interviews are better to maximise (ethnic) diversity potential"),
       y = expression("Black-White " ~ italic(d)),
       caption = paste0("Notes. Black-White mean differences originate from Sackett, Zhang, Berry, and Lievens (2021 \U2013 doi:10.1037/apl0000994).",
                        "\n",
                        "The values represent the standardised subgroup differences in means for a given selection procedure (predictor).",
                        "\n \n",
                        "graph by Louis Lippens")) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.ticks.y = element_line(size = .25, colour = "gray60"),
        text = element_text(family = "Source Sans Pro", size = 10, colour = "gray20"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 9, family = "Source Sans Pro", colour = "gray20"),
        axis.title.y = element_text(size = 11,
          margin = margin(t = 0, r = 10, b = 0, l =0),
          angle = 0, vjust = .961, hjust = 0),
        plot.title = element_text(size = 12,
                                  margin = margin(t = 0, r = 0, b = 5, l =0),
                                  colour = "gray20"),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 11,
                                     margin = margin(t = 0, r = 0, b = 5, l =0),
                                     colour = "gray40",
                                     lineheight = 1.1),
        plot.caption = element_text(size = 9, hjust = 0, colour = "gray40",
                                    margin = margin(t = 5, r = 0, b = 0, l =0),
                                    lineheight = 1.2),
        plot.caption.position = "plot",
  )
sackett.bwd

## 3.3. Uncertainty around validity estimates ####
sackett.pci <-
sackett %>% filter(include == 1) %>%
  ggplot(mapping = aes(x = selection_procedure, y = p_sackett_etal_2021)) +
  geom_hline(yintercept = 0, size = .25, colour = "gray60", linetype = "dashed") +
  geom_errorbar(mapping = aes(ymin = lower90_p, ymax = higher90_p, colour = significant90),
                size = .4, width = 0) +
  geom_errorbar(mapping = aes(ymin = lower80_p, ymax = higher80_p, colour = significant90),
                size = .8, width = 0) +
  geom_point(size = 3, colour = "white") +
  geom_point(mapping = aes(colour = significant90), size = 2) +
  scale_colour_manual(values = c(lgrey, blue, blue)) +
  scale_y_continuous(limits = c(-.18, .75), breaks = seq(-.15, .75, .15), sec.axis = dup_axis()) +
  facet_grid(category~.,
             scales = "free",
             space = "free",
             switch = "y") +
  labs(title = "Uncertainty around validity estimates of selection procedures as predictors of job performance",
       subtitle = paste0("The predictive value of several procedures can vary substantially across organisational contexts"),
       x = "",
       y = "Validity (\U1D70C)",
       caption = paste0("Notes. ",
                        "Operational validity estimates and their standard errors originate from Sackett, Zhang, Berry, and Lievens (2021 \U2013 doi:10.1037/apl0000994).",
                        "\n",
                        "The dots depict the validity estimates. The thick (thin) lines illustrate the 80% (90%) confidence intervals around the estimates.",
                        "\n",
                        "The estimates represent the correlations between the selection procedure (predictor) and job performance (criterion).",
                        "\n \n",
                        "graph by Louis Lippens")) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_line(size = .25, colour = "gray60"),
        axis.title.x.top = element_blank(),
        text = element_text(family = "Source Sans Pro", size = 10, colour = "gray20"),
        axis.title = element_text(size = 11, colour = "gray20"),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l =0),
                                    hjust = .961),
        axis.text = element_text(size = 9),
        plot.title = element_text(size = 12,
                                  margin = margin(t = 0, r = 0, b = 5, l =0),
                                  colour = "gray20"),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 11,
                                     margin = margin(t = 0, r = 0, b = 20, l =0),
                                     colour = "gray40",
                                     lineheight = 1.1),
        plot.caption = element_text(size = 9, hjust = 0, colour = "gray40",
                                    margin = margin(t = 20, r = 0, b = 0, l =0),
                                    lineheight = 1.2),
        plot.caption.position = "plot",
        panel.spacing = unit(10, "points")
        )
sackett.pci

## 3.4. Trade-off between validity and adverse impact potential ####
sackett.p.bwd <-
sackett %>% filter(!is.na(bw_d)) %>%
  ggplot(mapping = aes(x = p_sackett_etal_2021, y = bw_d)) +
  geom_smooth(formula = "y ~ x", method = "lm",
              colour = "gray80", fill = "gray90", size = .75) +
  geom_segment(x = .055, y = -.11, xend = .415, yend = -.11,
               lineend = "butt", linejoin = "bevel",
               size = .35, colour = "gray80",
               arrow = arrow(length = unit(5, "points"), type = "closed")
  ) +
  geom_segment(x = .44, y = .78, xend = .44, yend = 0.01,
               lineend = "butt", linejoin = "bevel",
               size = .35, colour = "gray80",
               arrow = arrow(length = unit(5, "points"), type = "closed")
  ) +
  geom_segment(x = .44, y = -.06, xend = .44, yend = -.01,
               lineend = "butt", linejoin = "bevel",
               size = .35, colour = "gray80",
               arrow = arrow(length = unit(5, "points"), type = "closed")
  ) +
  geom_text(data = (data.frame(p_sackett_etal_2021 = .41,
                               bw_d = -.11-(.008*2.5),
                               label = "Higher values are better")),
            mapping = aes(label = label),
            hjust = 1,
            family = "Source Sans Pro", size = 3.5, colour = "gray40") +
  geom_text(data = (data.frame(p_sackett_etal_2021 = .44+.008,
                               bw_d = 0.03,
                               label = "Lower values are better")),
            mapping = aes(label = label),
            angle = 270, hjust = 1,
            family = "Source Sans Pro", size = 3.5, colour = "gray40") +
  geom_text_repel(aes(label = selection_procedure), size = 3,
                  family = "Source Sans Pro", colour = "gray20",
                  segment.colour = "gray60", segment.size = .25,
                  min.segment.length = 99, box.padding = .4, nudge_y = .005, nudge_x = -.001) +
  geom_point(mapping = aes(colour = category), size = 3) +
  scale_colour_manual(values = viridisLite::mako(n = sackett$category %>% unique() %>% length())) +
  scale_x_continuous(limits = c(.05, .45), breaks = c(0.05, seq(.15,.35,.10), .42),
                     sec.axis = dup_axis()) +
  scale_y_continuous(limits = c(-.13, .79), breaks = c(-0.07, seq(0,.70,.10), .79),
                     sec.axis = dup_axis()) +
  labs(title = "Tradeoff between operational validity and adverse impact potential of selection procedures",
       subtitle = paste0("Increased operational validity of selection procedures often comes at the expense of decreased (ethnic) diversity potential",
                         "\n",
                         "Careful consideration should be given to choosing selection procedures, in function of the organisational needs and resources"),
       x = "Validity (\U1D70C)",
       y = expression("Black-White " ~ italic(d)),
       caption = paste0("Notes. ",
                        "Operational validity estimates and Black-White mean differences originate from Sackett, Zhang, Berry, and Lievens (2021 \U2013 doi:10.1037/apl0000994).",
                        "\n",
                        "The regression line is based on the ordinary least squares (OLS) method. The greyed-out area depicts the 95% confidence interval around this slope.",
                        "\n",
                        "The validity estimates represent the correlations between the selection procedure (predictor) and job performance (criterion).",
                        "\n",
                        "The Black-White differences represent the standardised subgroup differences in means for a given selection procedure (predictor).",
                        "\n \n",
                        "graph by Louis Lippens")) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = .1, colour = "gray80"),
        axis.ticks = element_line(size = .25, colour = "gray60"),
        text = element_text(family = "Source Sans Pro", size = 10, colour = "gray20"),
        axis.title.x.top = element_blank(),
        axis.title.y.right = element_blank(),
        axis.title = element_text(size = 11, colour = "gray20"),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l =0),
                                    hjust = .91),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l =0),
                                    vjust = .968, angle = 0 ),
        axis.text = element_text(size = 9),
        plot.title = element_text(size = 12,
                                  margin = margin(t = 0, r = 0, b = 5, l =0),
                                  colour = "gray20"),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 11,
                                     margin = margin(t = 0, r = 0, b = 20, l =0),
                                     colour = "gray40",
                                     lineheight = 1.1),
        plot.caption = element_text(size = 9, hjust = 0, colour = "gray40",
                                    margin = margin(t = 20, r = 0, b = 0, l =0),
                                    lineheight = 1.2),
        plot.caption.position = "plot"
  )
sackett.p.bwd