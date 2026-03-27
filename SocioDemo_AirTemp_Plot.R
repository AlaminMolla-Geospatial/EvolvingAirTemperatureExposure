# -----------------------------
# Libraries
# -----------------------------
library(ggplot2)
library(tidyr)
library(dplyr)
library(broom)
# -----------------------------
# Read data
# -----------------------------
df <- read.csv("NationalExposureULTIMATE.csv")
df2 <- df %>% mutate(.rowid = row_number())

# -----------------------------
# 1. Socio-demographic variables (exclude *_prop and NDVI)
# -----------------------------
socio_long <- df2 %>%
  pivot_longer(
    cols = matches("^(WH|BAA|HL|OTHER|RENTER|OLDER|POVERTY)_\\d+$"), 
    names_to  = c("SocioVar", "Year"),
    names_pattern = "(.*)_(\\d+)",
    values_to = "SocioValue"
  ) %>%
  mutate(Year = as.character(Year))

# -----------------------------
# 2. DT_Min long
# -----------------------------
dt_min_long <- df2 %>%
  pivot_longer(
    cols = matches("^DT_Min_"),
    names_to = "Year",
    names_pattern = "DT_Min_(\\d+)",
    values_to = "DT_Value_min"
  ) %>%
  mutate(Year = as.character(Year))

df_min <- socio_long %>%
  left_join(dt_min_long %>% select(.rowid, Year, DT_Value_min), 
            by = c(".rowid", "Year")) %>%
  drop_na(DT_Value_min, SocioValue)

# -----------------------------
# 3. DT_Max long
# -----------------------------
dt_max_long <- df2 %>%
  pivot_longer(
    cols = matches("^DT_Max_"),
    names_to = "Year",
    names_pattern = "DT_Max_(\\d+)",
    values_to = "DT_Value_max"
  ) %>%
  mutate(Year = as.character(Year))

df_max <- socio_long %>%
  left_join(dt_max_long %>% select(.rowid, Year, DT_Value_max), 
            by = c(".rowid", "Year")) %>%
  drop_na(DT_Value_max, SocioValue)

# -----------------------------
# 4. Order years
# -----------------------------
year_levels <- c("1980","1990","2000","2010","2020")
df_min <- df_min %>% mutate(Year = factor(Year, levels = year_levels))
df_max <- df_max %>% mutate(Year = factor(Year, levels = year_levels))

# -----------------------------
# 5. Plot DT_Min
# -----------------------------
p_min <- ggplot(df_min, aes(x = SocioValue, y = DT_Value_min)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue", size = 0.6) +
  facet_grid(SocioVar ~ Year, scales = "free_x") +
  labs(title = "Nightime Tair Deviations vs. Socio-demographics (rows = variable, cols = year)",
       x = "Number of Populations", y = "Nightime Air Temperature Deviations") +
  theme_bw(base_size = 13) +
  theme(
    strip.text.y = element_text(angle = 0, size = 11, face = "bold"),
    strip.text.x = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title.y = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    panel.spacing = unit(0.6, "lines")
  )

# -----------------------------
# 6. Plot DT_Max
# -----------------------------
p_max <- ggplot(df_max, aes(x = SocioValue, y = DT_Value_max)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred", size = 0.6) +
  facet_grid(SocioVar ~ Year, scales = "free_x") +
  labs(title = "Daytime Tair Deviations vs. Socio-demographics (rows = variable, cols = year)",
       x = "Number of Populations", y = "Daytime Air Temperature Deviations") +
  theme_bw(base_size = 13) +
  theme(
    strip.text.y = element_text(angle = 0, size = 11, face = "bold"),
    strip.text.x = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    axis.title.y = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    panel.spacing = unit(0.6, "lines")
  )

# -----------------------------
# 7. Save figures (A4 size)
# -----------------------------
# Portrait A4
ggsave("DT_Min_facetNEW.jpg", p_min, width = 8.27, height = 11.69, units = "in")
ggsave("DT_Max_facetNEW.jpg", p_max, width = 8.27, height = 11.69, units = "in")

# Landscape A4
ggsave("DT_Min_facet_landscapeNEW.jpg", p_min, width = 11.69, height = 8.27, units = "in")
ggsave("DT_Max_facet_landscapeNEW.jpg", p_max, width = 11.69, height = 8.27, units = "in")




## Slopes, Intercepts, and R² ##

# ----- Slopes for DT_Min -----
slopes_min <- df_min %>%
  group_by(SocioVar, Year) %>%
  do({
    model <- lm(DT_Value_min ~ SocioValue, data = .)
    tidy_df <- tidy(model)
    glance_df <- glance(model)
    data.frame(
      Intercept = tidy_df$estimate[tidy_df$term == "(Intercept)"],
      Slope     = tidy_df$estimate[tidy_df$term == "SocioValue"],
      R2        = glance_df$r.squared
    )
  }) %>%
  ungroup() %>%
  mutate(Type = "DT_Min",
         Variable = paste0(SocioVar, "_", Year, "_DT_Min")) %>%
  select(Variable, Intercept, Slope, R2)

# ----- Slopes for DT_Max -----
slopes_max <- df_max %>%
  group_by(SocioVar, Year) %>%
  do({
    model <- lm(DT_Value_max ~ SocioValue, data = .)
    tidy_df <- tidy(model)
    glance_df <- glance(model)
    data.frame(
      Intercept = tidy_df$estimate[tidy_df$term == "(Intercept)"],
      Slope     = tidy_df$estimate[tidy_df$term == "SocioValue"],
      R2        = glance_df$r.squared
    )
  }) %>%
  ungroup() %>%
  mutate(Type = "DT_Max",
         Variable = paste0(SocioVar, "_", Year, "_DT_Max")) %>%
  select(Variable, Intercept, Slope, R2)

# ----- Combine -----
slopes_final <- bind_rows(slopes_min, slopes_max)

# ----- Save to CSV -----
write.csv(slopes_final, "slopes_intercepts_r2_tidy.csv", row.names = FALSE)
