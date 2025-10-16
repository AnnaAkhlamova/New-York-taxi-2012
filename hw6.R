library(arrow)
library(duckplyr, warn.conflicts = FALSE)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(patchwork)

nyc <- read_parquet_duckdb("D:/Anna Akhlamova/Rsomedatasettaxi/nyc-taxi/**/*.parquet")

monthly_summary <- nyc |>
  filter(!is.na(vendor_name)) |>
  summarise(
    avg_distance = mean(trip_distance, na.rm = TRUE),
    avg_fare     = mean(fare_amount, na.rm = TRUE),
    n_trips      = n(),
    .by = c(vendor_name, month)
  ) |>
  mutate(
    month_name = month.abb[month],
    season = case_when(
      month %in% c(12, 1, 2)  ~ "Winter",
      month %in% c(3, 4, 5)   ~ "Spring",
      month %in% c(6, 7, 8)   ~ "Summer",
      month %in% c(9, 10, 11) ~ "Autumn"
    )
  )

season_summary <- monthly_summary |>
  group_by(vendor_name, season) |>
  summarise(
    avg_distance = sum(avg_distance * n_trips) / sum(n_trips),
    avg_fare     = sum(avg_fare * n_trips) / sum(n_trips),
    total_trips  = sum(n_trips),
    .groups = "drop"
  )


month_plot <- ggplot(monthly_summary,
                     aes(x = factor(month_name, levels = month.abb),
                         y = avg_fare, group = vendor_name, color = vendor_name)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_text(aes(label = round(avg_fare, 2)), vjust = -0.5, size = 3) +
  labs(title = "Середня оплата по місяцях", x = "Місяць", y = "Середня оплата") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'none')

season_plot <- ggplot(season_summary,
                      aes(x = factor(season, levels = c("Winter", "Spring", "Summer", "Autumn")),
                          y = avg_fare, group = vendor_name, color = vendor_name)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_text(aes(label = round(avg_fare, 2)), vjust = -1, size = 3) +
  labs(x = "Сезон", y = "Середня оплата") +
  theme_minimal()

combined_plot <- month_plot / season_plot +
  theme(legend.position = "bottom")

combined_plot



nyc_peaks <- nyc |>
  select(pickup_datetime, month) |> 
  filter(!is.na(pickup_datetime), !is.na(month)) |> 
  collect() |>
  mutate(
    season = case_when(
      month %in% c(12, 1, 2)  ~ "Winter",
      month %in% c(3, 4, 5)   ~ "Spring",
      month %in% c(6, 7, 8)   ~ "Summer",
      month %in% c(9, 10, 11) ~ "Autumn"
    ),
    hour = hour(pickup_datetime),
    day_of_week = wday(pickup_datetime, label = TRUE)
  )

hourly_all <- nyc_peaks |>
  group_by(season, hour) |>
  summarise(n_trips = n(), .groups = "drop")

daily_all <- nyc_peaks |>
  group_by(season, day_of_week) |>
  summarise(n_trips = n(), .groups = "drop")

hour_plot <- ggplot(hourly_all, aes(x = factor(hour), y = n_trips, fill = season)) +
  geom_col(position = "dodge") +
  labs(title = "Пікові години по сезонах", x = "Година", y = "Кількість поїздок") +
  theme_minimal() +
  scale_fill_manual(values = c("tan1", "palegreen", "khaki1", "lightblue1"))

day_plot <- ggplot(daily_all, aes(x = day_of_week, y = n_trips, fill = season)) +
  geom_col(position = "dodge") +
  labs(title = "Пікові дні тижня по сезонах", x = "День тижня", y = "Кількість поїздок") +
  theme_minimal() +
  scale_fill_manual(values = c("tan1", "palegreen", "khaki1", "lightblue1"))

peak_plot <- hour_plot / day_plot + plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

combined_plot
peak_plot
