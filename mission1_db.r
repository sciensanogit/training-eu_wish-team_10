library(ggplot2)
library(dplyr)
library(scales)

df_sc <- read.csv("https://data.geo.be/ws/sciensano/wfs?SERVICE=WFS&REQUEST=GetFeature&VERSION=2.0.0&TYPENAMES=sciensano:wastewatertreatmentplantscovid&outputFormat=csv")
df_pmmv <- read.csv("https://data.geo.be/ws/sciensano/wfs?SERVICE=WFS&REQUEST=GetFeature&VERSION=2.0.0&TYPENAMES=sciensano:wastewatertreatmentplantspmmv&outputFormat=csv")

df <- df_sc %>%
  rbind(df_pmmv)

df <- df %>%
  select(siteName, collDTStart, labName, labProtocolID, flowRate, popServ, measure, value)

df$date <- as.POSIXct(df$collDTStart, format = "%Y-%m-%dT%H:%M:%S")
df$date <- as.Date(df$date)

# Create the plot
plot <- df %>%
  filter(labProtocolID == "SC_COV_4.1") %>%
  filter(measure == "SARS-CoV-2 E gene") %>%
  filter(date > "2024-09-01" & date < "2025-09-01") %>%
  filter(siteName %in% c("Aalst", "Oostende")) %>%
  ggplot(aes(x = date, y = value, group = siteName, color = siteName)) +
  geom_point(size = 2, alpha = 0.6, na.rm = TRUE) +
  geom_line(linewidth = 1, na.rm = TRUE) +
  # Add horizontal reference lines
  geom_hline(yintercept = 2500, linetype = "dotted", color = "gray60", linewidth = 0.5) +
  geom_hline(yintercept = 5000, linetype = "dotted", color = "gray60", linewidth = 0.5) +
  geom_hline(yintercept = 9000, linetype = "dotted", color = "gray60", linewidth = 0.5) +
  # Add level labels
  annotate("text", x = max(df$date[df$date > "2024-09-01" & df$date < "2025-09-01"], na.rm = TRUE), 
           y = 12000, label = "Very high level", hjust = 1, vjust = -0.5, size = 3, color = "gray50") +
  annotate("text", x = max(df$date[df$date > "2024-09-01" & df$date < "2025-09-01"], na.rm = TRUE), 
           y = 5000, label = "High level", hjust = 1, vjust = -0.5, size = 3, color = "gray50") +
  annotate("text", x = max(df$date[df$date > "2024-09-01" & df$date < "2025-09-01"], na.rm = TRUE), 
           y = 2500, label = "Moderate level", hjust = 1, vjust = -0.5, size = 3, color = "gray50") +
  # Formatting
  scale_y_continuous(breaks = seq(0, 9000, 3000), limits = c(0, NA)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%d/%m/%y (W%V)") +
  scale_color_manual(values = c("Aalst" = "#C4D600", "Oostende" = "#90EE90")) +
  labs(
    title = "SARS-CoV-2",
    y = "SARS-CoV-2 viral to faecal ratio\n(10e-6 copies/copies)",
    x = NULL,
    color = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, color = "#5B9E7C", face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 10, angle = 90),
    panel.grid.major = element_line(color = "gray90", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

plot

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Your existing data preparation code
df_sc <- read.csv("https://data.geo.be/ws/sciensano/wfs?SERVICE=WFS&REQUEST=GetFeature&VERSION=2.0.0&TYPENAMES=sciensano:wastewatertreatmentplantscovid&outputFormat=csv")
df_pmmv <- read.csv("https://data.geo.be/ws/sciensano/wfs?SERVICE=WFS&REQUEST=GetFeature&VERSION=2.0.0&TYPENAMES=sciensano:wastewatertreatmentplantspmmv&outputFormat=csv")

df <- df_sc %>%
  rbind(df_pmmv)

df <- df %>%
  select(siteName, collDTStart, labName, labProtocolID, flowRate, popServ, measure, value)

df$date <- as.POSIXct(df$collDTStart, format = "%Y-%m-%dT%H:%M:%S")
df$date <- as.Date(df$date)

# Get SARS-CoV-2 data
df_sars <- df %>%
  filter(labProtocolID == "SC_COV_4.1") %>%
  filter(measure == "SARS-CoV-2 E gene") %>%
  filter(date > "2024-09-01" & date < "2025-09-01") %>%
  select(siteName, date, value) %>%
  rename(sars_value = value)

# Get PMMoV data - using SC_PMMV protocols (all versions)
df_pmmv_filtered <- df %>%
  filter(grepl("^SC_PMMV", labProtocolID)) %>%  # All SC_PMMV versions
  filter(measure == "Pepper mild mottle virus capsid protein gene region") %>%
  filter(date > "2024-09-01" & date < "2025-09-01") %>%
  select(siteName, date, value) %>%
  rename(pmmv_value = value)

# Join both datasets and calculate ratio
df_ratio <- df_sars %>%
  inner_join(df_pmmv_filtered, by = c("siteName", "date")) %>%
  mutate(viral_faecal_ratio = sars_value / pmmv_value) %>%
  filter(!is.na(viral_faecal_ratio))

# Create the plot
plot <- df_ratio %>%
  ggplot(aes(x = date, y = viral_faecal_ratio, group = siteName, color = siteName)) +
  geom_point(size = 2, alpha = 0.6, na.rm = TRUE) +
  geom_line(linewidth = 1, na.rm = TRUE) +
  geom_hline(yintercept = 2500, linetype = "dotted", color = "gray60", linewidth = 0.5) +
  geom_hline(yintercept = 5000, linetype = "dotted", color = "gray60", linewidth = 0.5) +
  geom_hline(yintercept = 9000, linetype = "dotted", color = "gray60", linewidth = 0.5) +
  annotate("text", x = max(df_ratio$date, na.rm = TRUE), 
           y = 9000, label = "Very high level", hjust = 1, vjust = -0.5, size = 3, color = "gray50") +
  annotate("text", x = max(df_ratio$date, na.rm = TRUE), 
           y = 5000, label = "High level", hjust = 1, vjust = -0.5, size = 3, color = "gray50") +
  annotate("text", x = max(df_ratio$date, na.rm = TRUE), 
           y = 2500, label = "Moderate level", hjust = 1, vjust = -0.5, size = 3, color = "gray50") +
  scale_y_continuous(breaks = seq(0, 9000, 3000), limits = c(0, NA)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%d/%m/%y (W%V)") +
  scale_color_viridis_d() +
  labs(
    title = "SARS-CoV-2",
    y = "SARS-CoV-2 viral to faecal ratio\n(10e-6 copies/copies)",
    x = NULL,
    color = "Treatment Plant"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, color = "#5B9E7C", face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 10, angle = 90),
    panel.grid.major = element_line(color = "gray90", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

plot

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# Your existing data preparation code
df_sc <- read.csv("https://data.geo.be/ws/sciensano/wfs?SERVICE=WFS&REQUEST=GetFeature&VERSION=2.0.0&TYPENAMES=sciensano:wastewatertreatmentplantscovid&outputFormat=csv")
df_pmmv <- read.csv("https://data.geo.be/ws/sciensano/wfs?SERVICE=WFS&REQUEST=GetFeature&VERSION=2.0.0&TYPENAMES=sciensano:wastewatertreatmentplantspmmv&outputFormat=csv")

df <- df_sc %>%
  rbind(df_pmmv)

df <- df %>%
  select(siteName, collDTStart, labName, labProtocolID, flowRate, popServ, measure, value)

df$date <- as.POSIXct(df$collDTStart, format = "%Y-%m-%dT%H:%M:%S")
df$date <- as.Date(df$date)

# Get SARS-CoV-2 data
df_sars <- df %>%
  filter(labProtocolID == "SC_COV_4.1") %>%
  filter(measure == "SARS-CoV-2 E gene") %>%
  filter(date > "2024-09-01" & date < "2025-09-01") %>%
  select(siteName, date, value) %>%
  rename(sars_value = value)

# Get PMMoV data - using SC_PMMV protocols (all versions)
df_pmmv_filtered <- df %>%
  filter(grepl("^SC_PMMV", labProtocolID)) %>%
  filter(measure == "Pepper mild mottle virus capsid protein gene region") %>%
  filter(date > "2024-09-01" & date < "2025-09-01") %>%
  select(siteName, date, value) %>%
  rename(pmmv_value = value)

# Join both datasets and calculate ratio
df_ratio <- df_sars %>%
  inner_join(df_pmmv_filtered, by = c("siteName", "date")) %>%
  mutate(viral_faecal_ratio = sars_value / pmmv_value) %>%
  filter(!is.na(viral_faecal_ratio))

# Create the plot
plot <- df_ratio %>%
  ggplot(aes(x = date, y = viral_faecal_ratio, group = siteName, color = siteName)) +
  geom_point(size = 2, alpha = 0.6, na.rm = TRUE) +
  geom_line(linewidth = 1, na.rm = TRUE) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  scale_x_date(date_breaks = "1 month", date_labels = "%d/%m/%y (W%V)") +
  scale_color_viridis_d() +
  labs(
    title = "SARS-CoV-2",
    y = "SARS-CoV-2 viral to faecal ratio\n(10e-6 copies/copies)",
    x = NULL,
    color = "Treatment Plant"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, color = "#5B9E7C", face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 10, angle = 90),
    panel.grid.major = element_line(color = "gray90", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

plot