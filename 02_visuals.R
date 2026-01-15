# Load packages ----
# select packages
pkgs <- c("dplyr", "tidyr", "zoo", "writexl", "ggplot2")
# install packages
install.packages(setdiff(pkgs, rownames(installed.packages())))
invisible(lapply(pkgs, FUN = library, character.only = TRUE))

# Load data ----
cat("- Loading data...\n")
# Read the Excel file with all sheets
excel_path <- "./data/Belgium_export.xlsx"
df_site_raw <- readxl::read_excel(excel_path, sheet = "site_raw")
df_site <- readxl::read_excel(excel_path, sheet = "site")
df_nation <- readxl::read_excel(excel_path, sheet = "nation")

cat("- Data loaded successfully\n")

# Create folder if not existing ----
cat("- Creating plot folder...\n")
if (!dir.exists("./plot")) {
  dir.create("./plot", recursive = TRUE)
}
cat("- Folder created\n")

# Prepare data for visualization ----
cat("- Preparing data for visualization...\n")

# Convert date column to Date type
df_nation <- df_nation %>%
  mutate(date = as.Date(date))

# Filter data with non-NA viral ratio values
df_viral <- df_nation %>%
  filter(!is.na(value_pmmv) | !is.na(value_pmmv_avg14d_past))

# Graph at national level ----
cat("- Creating viral ratio visualization...\n")

# Create viral ratio plot with 14-day moving average
p_viral <- ggplot(df_viral, aes(x = date)) +
  geom_point(aes(y = value_pmmv, color = "Viral Ratio"), 
             size = 2.5, alpha = 0.7, na.rm = TRUE) +
  geom_line(aes(y = value_pmmv_avg14d_past, color = "14-day Moving Average"), 
            linewidth = 1.2, linetype = "solid", na.rm = TRUE) +
  scale_color_manual(
    name = "",
    values = c(
      "Viral Ratio" = "#2E86AB",
      "14-day Moving Average" = "#E63946"
    )
  ) +
  scale_x_date(
    date_breaks = "2 weeks",
    date_labels = "%b %d",
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(
    labels = scales::comma,
    limits = NULL,
    expand = expansion(mult = c(0.05, 0.1))
  ) +
  labs(
    title = "Viral Ratio at National Level - Belgium",
    subtitle = "Daily viral ratio with 14-day moving average trend",
    x = "Date",
    y = "Viral Ratio (PMMV)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 5)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40", margin = margin(b = 15)),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.key.width = unit(1.5, "cm"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
    axis.text = element_text(size = 10, color = "gray30"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.title = element_text(size = 12, face = "bold", color = "gray20"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    plot.margin = margin(20, 20, 20, 20)
  )

# Save graph ----
cat("- Saving visualization...\n")

ggsave(
  filename = "./plot/graph-viral_ratio-nation.png",
  plot = p_viral,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)

# Display success message ----
cat("- Success : visuals saved \n")
cat("  * ./plot/graph-viral_ratio-nation.png\n")
cat("\n- Analysis complete!\n")