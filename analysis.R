library("tidyverse")
library("haven")

# Einlesen STATA-Datei Allbus 2021
# https://search.gesis.org/research_data/ZA5281
allbus <- read_dta(
  "./data/allbus_1980_2018.dta",
  encoding = NULL,
  col_select = NULL,
  skip = 0,
  n_max = Inf,
  .name_repair = "unique"
)

# Erstellung eines kleines Datensatz (Sample)
sample <-
  allbus[allbus$year == 1992, c("age", "di01a", "educy")]

# Interessante Variablen
# - Alter (age)
# - Nettoeinkommen (di01a)
# - Ausbildungsjahre (educy)

# Datensatz bereinigen
sample[sample < 0] <- NA

# Summary Statistiken
summary(sample$age)
summary(sample$di01a)

# Histogramm
hist(sample$age, xlab = "Alter (age)")
hist(sample$di01a, xlab = "Nettoeinkommen (di01a)")
hist(sample$educy, xlab = "Ausbildungsjahre (educy)")

# Korrelation
cor(sample$age, sample$di01a, use = "complete.obs")
cor(sample$educy, sample$di01a, use = "complete.obs")

# Regression berechnen
model <- lm(di01a ~ educy + age, data = sample)
summary(model)

# Daten anzeigen
plot(
  sample$educy,
  sample$di01a,
  ylim = c(0, 15000),
  xlab = "Bildungsjahre",
  ylab = "Einkommen pro Jahr",
  abline(lm(di01a ~ educy, data = sample))
)
