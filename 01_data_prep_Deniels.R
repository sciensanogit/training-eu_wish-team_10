############################################################################### #
# Aim ----
#| load, clean and save data
# NOTES:
#| git cheat: git status, git add -A, git commit -m "", git push, git pull, git restore
#| list of things to do...
############################################################################### #

# Load packages ----
# select packages
pkgs <- c("dplyr", "tidyr", "zoo", "writexl", "ggplot2")
# install packages
install.packages(setdiff(pkgs, rownames(installed.packages())))
invisible(lapply(pkgs, FUN = library, character.only = TRUE))

# load data ----
# Belgian data are available here https://www.geo.be/catalog/details/9eec5acf-a2df-11ed-9952-186571a04de2?l=en
#| Metadata
#| siteName is the name of the treatment plant
#| collDTStart is the date of sampling
#| labName is the name of the lab analysing the sample
#| labProtocolID is the protocol used to analyse the dample
#| flowRate is the flow rate measured at the inlet of the treatment plant during sampling
#| popServ is the population covered by the treatment plant
#| measure is the target measured
#| value is the result

# sars-cov-2 data
df_sc <- read.csv("https://data.geo.be/ws/sciensano/wfs?SERVICE=WFS&REQUEST=GetFeature&VERSION=2.0.0&TYPENAMES=sciensano:wastewatertreatmentplantscovid&outputFormat=csv")

# pmmv data
df_pmmv <- read.csv("https://data.geo.be/ws/sciensano/wfs?SERVICE=WFS&REQUEST=GetFeature&VERSION=2.0.0&TYPENAMES=sciensano:wastewatertreatmentplantspmmv&outputFormat=csv")

# join both
df <- df_sc %>%
  rbind(df_pmmv)

# clean data
df <- df %>%
  select(siteName, collDTStart, labName, labProtocolID, flowRate, popServ, measure, value, quality)

# format date
df$collDTStart <- as.Date(df$collDTStart)

# set and subset dates
date_reporting <- as.Date("2025-09-01", format = "%Y-%m-%d")
date_graph_start <- as.Date("2024-09-01", format = "%Y-%m-%d")
date_graph_end <- as.Date("2025-12-01", format = "%Y-%m-%d")

# subset sars and pmmv data based on labProtocolID used betwen date_start and date_end
# display existing labProtocolID
# unique(df$labProtocolID)
df_subset <- df %>%
  filter(collDTStart >= date_graph_start,
         collDTStart <= date_graph_end)

# rename measures
# diplay existing measure
# unique(df$measure)
df_filtered <- df_subset %>%
  mutate(measure = case_when(
    measure %in% c(
      "SARS-CoV-2 E gene",
      "SARS-CoV-2 nucleocapsid gene, allele 1",
      "SARS-CoV-2 nucleocapsid gene, allele 2"
    ) ~ "SARS",
    
    measure == "Pepper mild mottle virus capsid protein gene region" ~ "PMMV",
    
    TRUE ~ measure
  ))

# translate siteName to english
df_filtered1 <- df_filtered %>%
  mutate(siteName_en = recode(siteName,
                              "Aalst"                     = "Aalst",
                              "Antwerpen-Noord"           = "Antwerp North",
                              "Antwerpen-Zuid"            = "Antwerp South",
                              "Brugge"                    = "Bruges",
                              "Bruxelles-Sud"             = "Brussels South",
                              "Dendermonde"               = "Dendermonde",
                              "Genk"                      = "Genk",
                              "Gent"                      = "Ghent",
                              "Grimbergen"                = "Grimbergen",
                              "Harelbeke"                 = "Harelbeke",
                              "Hasselt"                   = "Hasselt",
                              "Leuven"                    = "Leuven",
                              "Mechelen-Noord"            = "Mechelen North",
                              "Mornimont"                 = "Mornimont",
                              "Namur-Brumagne"            = "Namur Brumagne",
                              "Oostende"                  = "Ostend",
                              "Tessenderlo"               = "Tessenderlo",
                              "Liège Oupeye"              = "Liege Oupeye",
                              "Liège Sclessin"            = "Liege Sclessin",
                              "Arlon"                     = "Arlon",
                              "Basse Wavre (Dyle)"        = "Basse-Wavre (Dyle)",
                              "Marche-en-Famenne"         = "Marche-en-Famenne",
                              "Marchienne-au-Pont"        = "Marchienne-au-Pont",
                              "Montignies-sur-Sambre"     = "Montignies-sur-Sambre",
                              "Roselies"                  = "Roselies",
                              "Vallée du Hain (l'Orchis)" = "Valley of the Hain (l'Orchis)",
                              "Wasmuel"                   = "Wasmuel",
                              "Bruxelles-Nord"            = "Brussels North",
                              "Deurne"                    = "Deurne",
                              "Liedekerke"                = "Liedekerke"
  ))


# apply LOQ provided by the lab
df_filtered2 <- df_filtered1 %>%
  mutate(value = case_when(
    measure == "PMMV" & value < 250 ~ NA_real_,
    measure == "SARS" & value < 8   ~ NA_real_,
    TRUE ~ value
  ))
# remove outliers
df_filtered3 <- df_filtered2 %>%
  group_by(measure) %>% 
  mutate(
    Q1 = quantile(value, 0.25, na.rm = TRUE),
    Q3 = quantile(value, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lower_bound = Q1 - 1.5 * IQR,
    upper_bound = Q3 + 1.5 * IQR,
    value = ifelse(value < lower_bound | value > upper_bound, NA, value)
  ) %>%
  select(-Q1, -Q3, -IQR, -lower_bound, -upper_bound) %>%
  ungroup()
df_filtered4 <- df_filtered3 %>%
  mutate(value = ifelse(quality == "Quality concerns", NA_real_, value))


# compute mean of replicated analysis of each measure
df_filtered5 <- df_filtered4 %>%
  group_by(measure) %>%
  mutate(mean_value = mean(value, na.rm = TRUE)) %>%
  ungroup()

# compute viral ratio
# unique(df$measure) ...


# compute moving average on past 14 days

# natinoal aggregation: compute weighted mean with factor being the population served by each site

# export data ----
# create folder if not existing

# export as csv

# export as xls

# export as rds


# display msg
cat("- Success : data prep \n")

