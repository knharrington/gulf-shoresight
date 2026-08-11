# examples of figures that can be produced using the shore-based camera footage

{
  library(tidyverse)
  library(data.table)
  library(lubridate)
}

# --------------------------- options ------------------------------------------
start_date <- as.Date("2026-06-19")
end_date <- as.Date("2026-07-07")
ports <- c("Brazos Santiago", "Perdido Pass")

# plot aesthetics
{
  entering_col <- "#56CFE1"
  exiting_col <- "#FF8A65"
  highlight_col <- "#1E2D44"
  background_col <- "#0B1421"
}

# ---------------------- data preprocessing ------------------------------------

# read example datasets
brazos_raw <- fread("data/Brazos Santiago_2026-06-19 - 2026-07-07.csv")
perdido_raw <- fread("data/Perdido Pass_2026-06-19 - 2026-07-07.csv")

# combine data sources, fix attribute classes
data <- bind_rows(brazos_raw, perdido_raw)
data$Port <- as.factor(data$Port)
data$Date <- as.Date(data$Date)
data$Date_Time <- ymd_hms(paste(data$Date, data$Time))
data$Day <- factor(weekdays(data$Date), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
data$Month <- format(data$Date, "%B")
data$Direction <- factor(data$Direction, levels = c("Entering", "Exiting"))

# use inputs to filter data
data <- data %>%
  filter(
    Port %in% ports,
    Date >= start_date,
    Date <= end_date
  )

# ---------------------------- ggplot themes -----------------------------------

# custom ggplot theme based on Tator platform
theme_tator <- function() {
  background_col <- "#0B1421"
  strip_col <- "#0F1C2F"
  text_col <- "#9FA8B8"
  
  theme_classic() +
    theme(
      axis.text.x = element_text(angle=45, hjust=1, color=text_col),
      axis.title.x = element_blank(),
      axis.text.y = element_text(color=text_col),
      axis.title.y = element_text(color=text_col),
      axis.line = element_line(color=text_col),
      axis.ticks = element_line(color=text_col),
      legend.position = "bottom",
      legend.text = element_text(color=text_col),
      legend.title = element_text(color=text_col),
      legend.background = element_rect(fill=background_col),
      plot.background = element_rect(fill=background_col),
      panel.background = element_rect(fill=background_col),
      strip.background = element_rect(fill=strip_col, color=background_col),
      strip.text = element_text(color=text_col)
    )
}

theme_tator_light <- function() {
  theme_bw() +
  theme(
    axis.text.x = element_text(angle=45, hjust=1),
    axis.title.x = element_blank(),
    legend.position = "bottom",
    strip.background = element_rect(fill="#8F96A1"),
    strip.text = element_text(color=background_col)
  )
}

# ------------------ line graph of counts per day ------------------------------

# format data to show counts by port, day, and direction
day_data <- data %>%
  group_by(Port, Date) %>%
  summarise(
    Entering = sum(`Entering Fishing`),
    Exiting = sum(`Exiting Fishing`)
  ) %>%
  pivot_longer(c(Entering, Exiting), names_to = "Direction", values_to = "Count") 

# create data for rectangles to shade/highlight weekends in plot
weekend_rects <- day_data %>%
  distinct(Date) %>%
  mutate(Weekend = wday(Date) %in% c(1, 7)) %>%
  filter(Weekend) %>%
  arrange(Date) %>%
  mutate(Group = cumsum(c(TRUE, diff(Date) > 1))) %>%
  group_by(Group) %>%
  summarise(
    Start = min(Date),
    End   = max(Date),
    .groups = "drop"
  )

# plot line graph for both passes
count_per_day <- ggplot(data=day_data) +
  geom_rect(data=weekend_rects, aes(xmin=Start-0.5, xmax=End+0.5, ymin=-Inf, ymax=Inf), fill = highlight_col, alpha=0.2, inherit.aes = FALSE) +
  geom_point(aes(x=Date, y=Count, color=Direction), size=1) +
  geom_line(aes(x=Date, y=Count, color=Direction)) +
  scale_color_manual(values = c("Entering" = entering_col, "Exiting" = exiting_col)) +
  scale_y_continuous(breaks = seq(0,max(day_data$Count), 100)) +
  scale_x_date(date_breaks = "1 day") +
  theme_tator_light() +
  facet_wrap(~Port) +
  labs(
    title = paste0("Date Range: ", min(day_data$Date), " - ", max(day_data$Date))
  )

ggsave(filename = paste0("figures/count_per_day_", min(day_data$Date), ".png"), 
       plot = count_per_day, width = 8, height = 5, dpi = 300)

# --------------------- bar graph for one week of data -------------------------

bar_data <- data %>%
  filter(
    Date >= as.Date("2026-06-29"), 
    Date <= as.Date("2026-07-05")
  ) %>%
  group_by(Port, Day, Date) %>%
  summarise(
    Entering = sum(`Entering Fishing`),
    Exiting = sum(`Exiting Fishing`)
  ) %>%
  pivot_longer(c(Entering, Exiting), names_to = "Direction", values_to = "Count") 

# plot bars for each day of the week
count_one_week <- ggplot(data=bar_data) +
  geom_col(aes(x=Day, y=Count, fill=Direction), position = "dodge") +
  scale_fill_manual(values = c("Entering" = entering_col, "Exiting" = exiting_col)) +
  scale_y_continuous(breaks = seq(0, max(day_data$Count)+100, 50), expand =c(0,0)) +
  theme_tator_light() +
  facet_wrap(~Port) +
  labs(
    title = paste0("Week of ", min(bar_data$Date))
  )

ggsave(filename = paste0("figures/count_per_weekday_", min(bar_data$Date), ".png"), 
       plot = count_one_week, width = 8, height = 5, dpi = 300)

#  ------------- average count per weekday for one month -----------------------

avg_weekday <- data %>%
  filter(Month == "June") %>%
  group_by(Port, Month, Date, Day) %>%
  summarise(
    Entering = sum(`Entering Fishing`),
    Exiting = sum(`Exiting Fishing`)
  ) %>%
  pivot_longer(c(Entering, Exiting), names_to = "Direction", values_to = "Count") #%>%
  # group_by(Port, Month, Day, Direction) %>%
  # summarise(
  #   Mean_Count = mean(Count),
  #   SD_Count = sd(Count),
  #   N_Count = n_distinct(Date),
  #   t_crit = qt(0.975, df = N_Count - 1),
  #   Margin = t_crit * SD_Count / sqrt(N_Count),
  #   Lower = Mean_Count - Margin, 
  #   Upper = Mean_Count + Margin
  # )

avg_count_weekday <- ggplot(data=avg_weekday) +
  geom_point(aes(x=Day, y=Count, color=Direction), alpha=0.75) +
  scale_color_manual(values = c("Entering" = entering_col, "Exiting" = exiting_col)) +
  scale_y_continuous(breaks = seq(0, max(avg_weekday$Count)+100, 50), limits = c(0, max(avg_weekday$Count)+100), expand =c(0,0)) +
  theme_tator_light() +
  facet_wrap(~Port) +
  labs(
    title = paste(first(avg_weekday$Month), year(avg_weekday$Date))
  )

ggsave(filename = paste0("figures/avg_count_weekday_", min(avg_weekday$Date), ".png"), 
       plot = avg_count_weekday, width = 8, height = 5, dpi = 300)

# ----------------- difference in counts per month and weekday -----------------

diff_weekday <- data %>%
  filter(Month == "June") %>%
  group_by(Port, Month, Date, Day) %>%
  summarise(
    Entering = sum(`Entering Fishing`),
    Exiting = sum(`Exiting Fishing`),
    Difference = Entering - Exiting
  )

diff_count_weekday <- ggplot(data=diff_weekday) +
  geom_point(aes(x=Day, y=Difference), alpha=0.75, color="black") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha=0.5) +
  scale_y_continuous(
    breaks = seq(round(min(diff_weekday$Difference), digits=-1)-10, round(max(diff_weekday$Difference), digits=-1)+10, 10), 
    limits = c(round(min(diff_weekday$Difference), digits=-1)-10, round(max(diff_weekday$Difference), digits=-1)+10)
  ) +
  theme_tator_light() +
  facet_wrap(~Port) +
  labs(
    title = paste(first(diff_weekday$Month), year(diff_weekday$Date)),
    y = "Entering Count - Exiting Count"
  )

ggsave(filename = paste0("figures/diff_count_weekday_", min(diff_weekday$Date), ".png"), 
       plot = diff_count_weekday, width = 8, height = 5, dpi = 300)

# ----------------- mean count per hour of day for one week --------------------

count_per_hour <- data %>%
  filter(
    Date >= as.Date("2026-06-29"), 
    Date <= as.Date("2026-07-05")
  ) %>%
  mutate(
    Hour = factor(as.integer(str_sub(Time, 1, 2)), levels = 0:23)
  ) %>%
  group_by(Port, Date, Hour, Direction, .drop=FALSE) %>%
  summarise(
    Count = n()
  ) 
  
mean_per_hour <- count_per_hour %>%
  group_by(Port, Hour, Direction, .drop=FALSE) %>%
  summarise(
    Mean_Count = mean(Count),
    SD_Count = sd(Count),
    N_Count = n_distinct(Date),
    t_crit = qt(0.975, df = N_Count - 1),
    Margin = t_crit * SD_Count / sqrt(N_Count),
    Lower = Mean_Count - Margin,
    Upper = Mean_Count + Margin
  ) %>%
  mutate(
    Lower = case_when(
      Lower < 0 ~ 0,
      TRUE ~ Lower
    )
  )
  
mean_count_hour <- ggplot(data=mean_per_hour, aes(x=Hour)) +
  geom_ribbon(aes(ymin=Lower, ymax=Upper, fill = Direction, group = Direction), alpha=0.25) +
  geom_point(aes(y=Mean_Count, color = Direction), alpha=1, size=1) +
  geom_line(aes(y=Mean_Count, color = Direction, group = Direction), alpha=1) +
  scale_color_manual(values = c("Entering" = entering_col, "Exiting" = exiting_col)) +
  scale_fill_manual(values = c("Entering" = entering_col, "Exiting" = exiting_col)) +
  scale_y_continuous(breaks = seq(0, max(mean_per_hour$Upper)+10, 10)) +
  theme_tator_light() +
  facet_wrap(~Port) +
  labs(
    title = paste0("Week of ", min(count_per_hour$Date)),
    y = "Mean Count",
    x = "Hour"
  ) 

ggsave(filename = paste0("figures/mean_count_hour_", min(count_per_hour$Date), ".png"), 
       plot = mean_count_hour, width = 8, height = 5, dpi = 300)

# ----------------- count distribution per hour of day for one week ------------

count_hour_whiskers <- ggplot(data=count_per_hour, aes(x=Hour)) +
  geom_boxplot(aes(y=Count, color = Direction), outlier.size=1) +
  scale_color_manual(values = c("Entering" = entering_col, "Exiting" = exiting_col)) +
  scale_y_continuous(breaks = seq(0, max(count_per_hour$Count)+10, 10)) +
  theme_tator_light() +
  facet_wrap(~Port) +
  labs(
    title = paste0("Week of 2026-06-29"),
    y = "Count",
    x = "Hour"
  ) 

ggsave(filename = paste0("figures/count_hour_whiskers_", min(count_per_hour$Date), ".png"), 
       plot = count_hour_whiskers, width = 8, height = 5, dpi = 300)

count_hour_violin <- ggplot(data=count_per_hour, aes(x=Hour)) +
  geom_violin(aes(y=Count, color = Direction), scale="width") +
  scale_color_manual(values = c("Entering" = entering_col, "Exiting" = exiting_col)) +
  scale_y_continuous(breaks = seq(0, max(count_per_hour$Count)+10, 10)) +
  theme_tator_light() +
  facet_wrap(~Port) +
  labs(
    title = paste0("Week of 2026-06-29"),
    y = "Count",
    x = "Hour"
  )

ggsave(filename = paste0("figures/count_hour_violin_", min(count_per_hour$Date), ".png"), 
       plot = count_hour_violin, width = 8, height = 5, dpi = 300)
