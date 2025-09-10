library(ggplot2)
library(scales)
library(showtext)
library(tidyverse)
library(patchwork)
library(readxl)
font_add_google("Raleway", "raleway")
showtext_auto()
theme_set(theme_minimal(base_size = 14) + theme(text = element_text(family = "raleway")))
intervals <- read_excel("ennusteet.xlsx")

# Väestö Plot
vaesto_plot <- intervals |>
  filter(variable == "pop") |>
  ggplot(aes(x = as.integer(year))) +
  geom_ribbon(aes(ymin = lower_80, ymax = upper_80), fill = "steelblue", alpha = 0.3) +
  geom_ribbon(aes(ymin = lower_50, ymax = upper_50), fill = "steelblue", alpha = 0.5) +
  geom_line(aes(y = lower_80), color = "steelblue", linetype = "dashed") +
  geom_line(aes(y = upper_80), color = "steelblue", linetype = "dashed") +
  geom_line(aes(y = lower_50), color = "steelblue") +
  geom_line(aes(y = upper_50), color = "steelblue") +
  geom_line(aes(y = median), color = "steelblue", size = 1.1) +
  scale_y_continuous(labels = label_number(suffix = "m", scale = 1/1000000)) +
  labs(x = "", y = "Väestö", title = "Väestö") +
  theme_minimal(base_size = 14) +
  theme(text = element_text(family = "raleway"),panel.grid = element_blank(), axis.line = element_line())

# Työikäiset Plot
tyoikaiset_plot <- intervals |>
  filter(variable == "tyoikaiset") |>
  ggplot(aes(x = as.integer(year))) +
  geom_ribbon(aes(ymin = lower_80, ymax = upper_80), fill = "orange", alpha = 0.3) +
  geom_ribbon(aes(ymin = lower_50, ymax = upper_50), fill = "orange", alpha = 0.5) +
  geom_line(aes(y = lower_80), color = "orange", linetype = "dashed") +
  geom_line(aes(y = upper_80), color = "orange", linetype = "dashed") +
  geom_line(aes(y = lower_50), color = "orange") +
  geom_line(aes(y = upper_50), color = "orange") +
  geom_line(aes(y = median), color = "orange", size = 1.1) +
  scale_y_continuous(labels = label_number(suffix = "m", scale = 1/1000000)) +
  labs(x = "", y = "Työikäiset", title = "Työikäiset") +
  theme_minimal(base_size = 14) +
  theme(text = element_text(family = "raleway"),panel.grid = element_blank(), axis.line = element_line())

# Huoltosuhde Plot
huolto_plot <- intervals |>
  filter(variable == "huolto") |>
  ggplot(aes(x = as.integer(year))) +
  geom_ribbon(aes(ymin = lower_80, ymax = upper_80), fill = "purple", alpha = 0.3) +
  geom_ribbon(aes(ymin = lower_50, ymax = upper_50), fill = "purple", alpha = 0.5) +
  geom_line(aes(y = lower_80), color = "purple", linetype = "dashed") +
  geom_line(aes(y = upper_80), color = "purple", linetype = "dashed") +
  geom_line(aes(y = lower_50), color = "purple") +
  geom_line(aes(y = upper_50), color = "purple") +
  geom_line(aes(y = median), color = "purple", size = 1.1) +
  labs(x = "", y = "Yli 65-vuotiasta per työikäinen", title = "Huoltosuhde") +
  theme_minimal(base_size = 14) +
  theme(text = element_text(family = "raleway"),panel.grid = element_blank(), axis.line = element_line())

# Combine (example with patchwork)
combined_population <- vaesto_plot | tyoikaiset_plot | huolto_plot
combined_population

library(ggplot2)
ggsave("figs/population.svg", combined_population, width = 9, height = 3, units = "in")


# Elinajanodote (e0)
e0_plot <- intervals |>
  filter(variable == "e0") |>
  ggplot(aes(x = as.integer(year))) +
  geom_ribbon(aes(ymin = lower_80, ymax = upper_80), fill = "steelblue", alpha = 0.3) +
  geom_ribbon(aes(ymin = lower_50, ymax = upper_50), fill = "steelblue", alpha = 0.5) +
  geom_line(aes(y = lower_80), color = "steelblue", linetype = "dashed") +
  geom_line(aes(y = upper_80), color = "steelblue", linetype = "dashed") +
  geom_line(aes(y = lower_50), color = "steelblue") +
  geom_line(aes(y = upper_50), color = "steelblue") +
  geom_line(aes(y = median), color = "steelblue", size = 1.2) +
  labs(x = "", y = "Vuosia", title = "Elinajanodote e0") +
  theme_minimal(base_size = 14) +
  theme(  text = element_text(family = "raleway"),
          panel.grid = element_blank(), axis.line = element_line())

# Syntyvyys
syntyvyys_plot <- intervals |>
  filter(variable == "Syntyvyys") |>
  ggplot(aes(x = as.integer(year))) +
  geom_ribbon(aes(ymin = lower_80, ymax = upper_80), fill = "orange", alpha = 0.3) +
  geom_ribbon(aes(ymin = lower_50, ymax = upper_50), fill = "orange", alpha = 0.5) +
  geom_line(aes(y = lower_80), color = "orange", linetype = "dashed") +
  geom_line(aes(y = upper_80), color = "orange", linetype = "dashed") +
  geom_line(aes(y = lower_50), color = "orange") +
  geom_line(aes(y = upper_50), color = "orange") +
  geom_line(aes(y = median), color = "orange", size = 1.2) +
  labs(x = "", y = "Lasta per nainen", title = "Syntyvyys") +
  theme_minimal(base_size = 14) +
  theme(  text = element_text(family = "raleway"),
          panel.grid = element_blank(), axis.line = element_line())

# Maahanmuutto (Muuttaneita)
muuttaneita_plot <- intervals |>
  filter(variable == "Muuttaneita") |>
  ggplot(aes(x = as.integer(year))) +
  geom_ribbon(aes(ymin = lower_80, ymax = upper_80), fill = "purple", alpha = 0.3) +
  geom_ribbon(aes(ymin = lower_50, ymax = upper_50), fill = "purple", alpha = 0.5) +
  geom_line(aes(y = lower_80), color = "purple", linetype = "dashed") +
  geom_line(aes(y = upper_80), color = "purple", linetype = "dashed") +
  geom_line(aes(y = lower_50), color = "purple") +
  geom_line(aes(y = upper_50), color = "purple") +
  geom_line(aes(y = median), color = "purple", size = 1.2) +
  scale_y_continuous(labels = label_number(suffix = "t", scale = 1/1000)) +
  labs(x = "", y = "Nettomuuttujia", title = "Maahanmuutto") +
  theme_minimal(base_size = 14) +
  theme(  text = element_text(family = "raleway"),
          panel.grid = element_blank(), axis.line = element_line())

# Combine
combined_plot <- e0_plot | syntyvyys_plot | muuttaneita_plot
combined_plot
library(ggplot2)
ggsave("figs/elementit.svg", combined_plot, width = 12, height = 4, units = "in")


set.seed(123)  
simulaatiot <- read_excel("simulaatiot.xlsx")

ids <- sample(unique(simulaatiot$trajectory), 1000)

simulaatiot <- simulaatiot |>
  filter(trajectory %in% ids)

means <- simulaatiot |>
  filter(year >= 2024, year <= 2050) |>
  group_by(trajectory) |>
  summarize(
    e0 = e0[year == 2050],
    Syntyvyys = mean(Syntyvyys),
    Muuttaneita = mean(Muuttaneita),
    pop_2050 = pop[year == 2050], 
    tyo_2050= tyoikaiset[year == 2050], 
    huolto_2050 = huolto[year == 2050]
  ) 

# 2. Make long data for plotting
means_long <- means |>
  pivot_longer(cols = c(e0, Syntyvyys, Muuttaneita),
               names_to = "variable", values_to = "value") |> ungroup()

# 3. Separate plots for each variable with style

# e0 vs population
mean_e0_plot <- means_long |>
  filter(variable == "e0") |>
  ggplot(aes(x = value, y = pop_2050 )) +
  geom_point(color = "steelblue", alpha = 0.15, size = 1) +
  # Add red dot
  geom_point(aes(x = 86.5, y = 6177141), color = "#00008B", size = 3, alpha = 0.8) +
  
  labs(x = "Elinajanodote e0 (vuotta)", y = "Väestö 2050 (m)", title = "") +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "raleway"),
    panel.grid = element_blank(),
    axis.line = element_line()
  ) +
  scale_y_continuous(labels = label_number(suffix = "m", scale = 1/1000000))

# Syntyvyys vs population
mean_syntyvyys_plot <- means_long |>
  filter(variable == "Syntyvyys") |>
  ggplot(aes(x = value, y = pop_2050 )) +
  geom_point(color = "orange", alpha = 0.15, size = 1) +
  # Add red dot
  geom_point(aes(x = 1.26, y = 6177141), color = "#00008B",  size = 3, alpha = 0.8) +
  labs(x = "Syntyvyys (lasta per nainen)", y = "", title = "") +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "raleway"),
    panel.grid = element_blank(),
    axis.line = element_line()
  ) +
  scale_y_continuous(labels = label_number(suffix = "m", scale = 1/1000000))

# Muuttaneita vs population
mean_muuttaneita_plot <- means_long |>
  filter(variable == "Muuttaneita") |>
  mutate(value = value ) |>
  ggplot(aes(x = value, y = pop_2050 )) +
  geom_point(color = "purple", alpha = 0.15, size = 1) +
  # Add red dot
  geom_point(aes(x = 40000, y = 6177141), color = "#00008B",  size = 3, alpha = 0.8) +
  labs(x = "Nettomaahanmuuttajia/vuosi ", y = "", title = "") +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "raleway"),
    panel.grid = element_blank(), 
    axis.line = element_line(),
    axis.ticks = element_line(color = "black", size = 0.5)
  ) +
  scale_x_continuous(labels = label_number(suffix = "t", scale = 1/1000)) +
  scale_y_continuous(labels = label_number(suffix = "m", scale = 1/1000000))

# Combine the three plots
means_scatter_plot <- mean_e0_plot | mean_syntyvyys_plot | mean_muuttaneita_plot

manual_legend <- tibble(x = 1, y = 1, label = "Tilastokeskuksen ennusteen oletukset")

legend_plot <- ggplot(manual_legend, aes(x, y)) +
  geom_point(color = "#00008B", size = 3) +
  geom_text(aes(label = label), vjust = -0.5,hjust = -0.5, size = 4, family = "raleway") +
  theme_void()

# Stack plot and manual legend
means_scatter_plot_ <- means_scatter_plot / legend_plot + plot_layout(heights = c(10, 1))
means_scatter_plot_

# Save as SVG
ggsave("figs/elementit_vs_population.svg", means_scatter_plot, width = 9, height = 3, units = "in")



##probabilities
means_long |>
  filter(variable == "Muuttaneita") |>
  summarise(
    pct_over_40000 = mean(value > 40000) * 100
  )

means_long |>
  filter(variable == "e0") |>
  summarise(
    lower_10 = quantile(pop_2050, 0.10),
    upper_90 = quantile(pop_2050, 0.90)
  )

means_long |>
  filter(variable == "Muuttaneita") |>
  summarise(
    pct_over_40000 = mean(pop_2050 > 5635971) * 100
  )


means_long |>
  filter(variable == "e0") |>
  summarise(
    pct_over_40000 = mean(tyo_2050 > 3293886) * 100
  )

means_long |>
  filter(variable == "e0") |>
  summarise(
    pct_over_40000 = mean(huolto_2050 > .4034192) * 100
  )


means_long |>
  filter(variable == "e0") |>
  summarise(
    pct_over_40000 = mean(value > 85) * 100
  )

means_long |>
  filter(variable == "e0") |>
  summarise(
    pct_over_40000 = mean(value > 84) * 100
  )

means_long |>
  filter(variable == "e0") |>
  summarise(
    lower_10 = quantile(value, 0.10),
    upper_90 = quantile(value, 0.90)
  )

means_long |>
  filter(variable == "Muuttaneita") |>
  summarise(
    lower_10 = quantile(value, 0.10),
    upper_90 = quantile(value, 0.90)
  )

means_long |>
  filter(variable == "Syntyvyys") |>
  summarise(
    lower_10 = quantile(value, 0.10),
    upper_90 = quantile(value, 0.90)
  )


means_long |>
  filter(variable == "e0") |>
  summarise(threshold_20th_percentile = quantile(value, 0.20))

##taulukko  nettisivuille.
intervals_2025 <- intervals |>
  filter(as.numeric(year) >= 2025)

intervals_split <- list(
  "Koko väestö" = intervals_2025 |>
    filter(variable == "pop") |>
    mutate(across(where(is.numeric), round)),
  
  "Työikäiset 18 - 64" = intervals_2025 |>
    filter(variable == "tyoikaiset") |>
    mutate(across(where(is.numeric), round)),
  
  "Muuttaneita" = intervals_2025 |>
    filter(variable == "Muuttaneita") |>
    mutate(across(where(is.numeric), round)),
  
  "Syntyvyys" = intervals_2025 |>
    filter(variable == "Syntyvyys"),
  
  "e0" = intervals_2025 |>
    filter(variable == "e0"),
  
  "huolto" = intervals_2025 |>
    filter(variable == "huolto")
)

# Write to Excel
write_xlsx(intervals_split, "ennusteet_sorsa.xlsx")
