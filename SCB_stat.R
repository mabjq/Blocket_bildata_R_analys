# ----------------------------------------------------------------------------
# Hämta och analysera data över nyregistrerade personbilar och drivmedel
# i Sverige från SCB för perioden 2006-2022.
# ----------------------------------------------------------------------------

library(pxweb)
library(dplyr)
library(ggplot2)

# ----------------------------------------------------------------------------
# Hämtar grundläggande metadata och kikar på tillgängliga variabler och värden
# ----------------------------------------------------------------------------

meta <- pxweb_get("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/TK/TK1001/TK1001A/PersBilarDrivMedel")
meta$variables

# ----------------------------------------------------------------------------
# Definierar min tidsperiod och hämta data från SCB
# ----------------------------------------------------------------------------

tid <- paste0(rep(2006:2022, each = 12), "M", sprintf("%02d", 1:12))

scb_data <- pxweb_get_data(
  url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/TK/TK1001/TK1001A/PersBilarDrivMedel",
  query = list(
    Region = "*",  
    Drivmedel = c("100", "110", "120", "130", "140", "150", "160", "190"),  
    Tid = tid,  
    ContentsCode = "TK1001AA"  # Antal fordon
  )
)

scb_data <- as.data.frame(scb_data)

head(scb_data)

# ----------------------------------------------------------------------------
# Skapar dataframe med data per år och drivmedel
# Syfte: För att få en översiktlig bild av hur nyregistreringarna utvecklats
#        över tid för olika bränsletyper.
# ----------------------------------------------------------------------------

scb_data_yearly <- scb_data |> 
  mutate(År = as.numeric(substr(månad, 1, 4))) |> 
  group_by(drivmedel, År) |> 
  summarise(Antal = sum(Antal, na.rm = TRUE)) |> 
  ungroup()

head(scb_data_yearly)

# ----------------------------------------------------------------------------
# Beräknar det totala antalet nyregistreringar per år
# ----------------------------------------------------------------------------

scb_data_total <- scb_data_yearly |> 
  group_by(År) |> 
  summarise(Antal = sum(Antal, na.rm = TRUE)) |> 
  mutate(drivmedel = "Total")

# ----------------------------------------------------------------------------
# Kombinerar den årliga datan med totalsumman per år
# Syfte: För att ha alla relevanta data i en och samma dataframe för visualisering
# ----------------------------------------------------------------------------

scb_data_with_total <- bind_rows(scb_data_yearly, scb_data_total)

# ----------------------------------------------------------------------------
# Visualiserar utvecklingen av nyregistrerade bilar per bränsletyp över tid
# Syfte: För att förändringar i popularitet för olika drivmedel under perioden. 
# Inkluderar en trendlinje för totalen.
# ----------------------------------------------------------------------------

scb_data_with_total$drivmedel <- factor(scb_data_with_total$drivmedel, 
                                        levels = c("bensin", "diesel", "el", "elhybrid", "laddhybrid", "etanol/etanol flexifuel", "gas/gas flexifuel", "övriga bränslen", "Total"),
                                        labels = c("Bensin", "Diesel", "El", "Elhybrid", "Laddhybrid", "Etanol", "Gas", "Övriga","Total"))

ggplot(scb_data_with_total, aes(x = År, y = Antal, color = drivmedel, linetype = drivmedel)) +
  geom_line(linewidth = 0.75) +
  geom_smooth(data = subset(scb_data_with_total, drivmedel == "Total"), 
              aes(x = År, y = Antal), 
              method = "lm", color = "grey10", linewidth = 0.5, linetype = "dotted", se = FALSE) +  # Trendlinje
  scale_color_manual(values = c("Bensin" = "blue", "Diesel" = "red", "El" = "green", 
                                "Elhybrid" = "purple", "Laddhybrid" = "orange", 
                                "Etanol" = "brown", "Gas" = "pink", "Övriga" = "gray", 
                                "Total" = "black")) +
  scale_linetype_manual(values = c("Bensin" = "solid", "Diesel" = "solid", "El" = "solid", 
                                   "Elhybrid" = "solid", "Laddhybrid" = "solid", 
                                   "Etanol" = "solid", "Gas" = "solid", "Övriga" = "solid", 
                                   "Total" = "dashed")) +
  labs(title = "Nyregistrerade personbilar per bränsletyp i Sverige (2006-2022)",
       x = "År", y = "Antal nyregistrerade bilar", color = "Bränsletyp", linetype = "Bränsletyp") +
  theme_minimal() +
  theme(legend.position = "bottom")

# ----------------------------------------------------------------------------
# Analys av nyregistreringar under år 2022
# Syfte: För att få inblick i el- och laddhybriders marknadsandelar det senaste året.
# ----------------------------------------------------------------------------

data_2022 <- scb_data_with_total |> 
  filter(År == 2022)

print(data_2022)

total_2022 <- data_2022 |> 
  filter(drivmedel == "Total") |> 
  pull(Antal)

el_laddhybrid_2022 <- data_2022 |> 
  filter(drivmedel %in% c("El", "Laddhybrid")) |> 
  summarise(Antal = sum(Antal, na.rm = TRUE)) |> 
  pull(Antal)

andel_el_laddhybrid_2022 <- (el_laddhybrid_2022 / total_2022) * 100

cat("Andelen nybilsregistreringar som el- och laddhybrider stod för 2022: ", 
    round(andel_el_laddhybrid_2022, 1), "%\n")

# ----------------------------------------------------------------------------
# Analys av den årliga förändringen i totala nyregistreringar
# Syfte: För att visa den genomsnittliga årliga förändringen av antalet 
#         nyregistrerade bilar under perioden.
# ----------------------------------------------------------------------------

total_data <- scb_data_with_total |> 
  filter(drivmedel == "Total")

# Skapar en linjär modell för att analysera trenden över tid
lm_model <- lm(Antal ~ År, data = total_data)

# Extrahera lutningen från modellen, vilket visar den genomsnittliga förändringen
yearly_change <- coef(lm_model)["År"]

cat("Genomsnittlig årlig förändring i totala nyregistreringar: ", 
    round(yearly_change, 0), " bilar per år\n")
