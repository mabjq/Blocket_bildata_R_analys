# ----------------------------------------------------------------------------
# Analys av Försäljningspris för Begagnade Bilar insamlade från Blocket data
# ----------------------------------------------------------------------------

library(dplyr)
library(readxl)
library(ggplot2)
library(scales)
library(corrplot)
library(tidyr)
library(forcats)
library(caret)
library(leaps)
library(lmtest)
library(car)

blocket_data <- read_excel("Datainsamling_Blocket.xlsx", col_types = "text")

head(blocket_data)
str(blocket_data)

# ----------------------------------------------------------------------------
# Rengöring och förberedelse av data
# Syfte: Säkerställa datakvalitet genom att hantera saknade värden, felaktiga format,
#        inkonsistenser och skapa lämpliga datatyper för analys.
# ----------------------------------------------------------------------------

colSums(is.na(blocket_data))
unique(blocket_data$Bränsle) 

# Funktion för att skriva ut NA-statistik 
print_na_stats <- function(data) {
  na_per_kolumn <- colSums(is.na(data))
  print(na_per_kolumn)
  return(data)
}

blocket_data <- blocket_data |> 
  select(-Index) |> 
  rename(Hästkrafter = "Hästkrafter (HK)")

colnames(blocket_data)

# Konvertera kolumner till rätt datatyper och hantera inledande formatproblem
blocket_data <- blocket_data |> 
  mutate(
    Försäljningspris = as.numeric(gsub("[[:space:],]", "", Försäljningspris)),  # Ta bort mellanslag och kommatecken
    Miltal = as.numeric(gsub("[[:space:],]", "", Miltal)),  # Ta bort mellanslag och kommatecken
    Modellår = as.numeric(gsub("[[:space:],]", "", Modellår)),  # Ta bort mellanslag och kommatecken
    Hästkrafter = as.numeric(gsub("[^0-9.]", "", Hästkrafter)),  # Ta bort allt som inte är siffror eller punkter
    Säljare = as.factor(Säljare),
    Bränsle = as.factor(Bränsle),
    Växellåda = as.factor(Växellåda),
    Biltyp = as.factor(Biltyp),
    Drivning = as.factor(Drivning),
    Färg = as.factor(Färg),
    Märke = as.factor(Märke),
    Modell = as.factor(Modell),
    Region = as.factor(Region),
    Datum_i_trafik = as.Date(as.numeric(Datum_i_trafik), origin = "1899-12-30")  # Excel använder 1899-12-30 som startdatum
  ) |> 
  mutate(
    Hästkrafter = replace(Hästkrafter, is.na(Hästkrafter), mean(Hästkrafter, na.rm = TRUE))
  ) |> 
  print_na_stats() |> 
  filter(
    !is.na(Försäljningspris),
    !is.na(Miltal),
    !is.na(Hästkrafter),
    !is.na(Modellår),
    !is.na(Datum_i_trafik),
    !is.na(Bränsle),
    !is.na(Drivning),
    !is.na(Färg)
  ) |> 
  mutate(
    Bränsle = as.factor(tolower(trimws(Bränsle))),
    Växellåda = as.factor(tolower(trimws(Växellåda))),
    Drivning = as.factor(tolower(trimws(Drivning))),
    Färg = as.factor(tolower(trimws(Färg))),
    Märke = as.factor(tolower(trimws(Märke))),
    Modell = as.factor(tolower(trimws(Modell))),
    Region = as.factor(tolower(trimws(Region)))
  ) |> 
  mutate(
    Datum_i_trafik = case_when(
      as.character(Datum_i_trafik) == "20013-04-15" ~ as.Date("2013-04-15"),
      as.character(Datum_i_trafik) == "2919-04-01" ~ as.Date("2019-04-01"),
      TRUE ~ Datum_i_trafik
    ),
    Datum_i_trafik = if_else(
      Datum_i_trafik < as.Date("2000-01-01") | Datum_i_trafik > as.Date("2025-04-11"), 
      as.Date(NA),
      Datum_i_trafik
    )
  ) |> 
  filter(!is.na(Datum_i_trafik)) |> 
  mutate(
    Bränsle = fct_recode(Bränsle,
                         "miljöbränsle/hybrid" = "miljöbränsle",
                         "diesel" = "disel")
  ) |> 
  mutate(
    Drivning = fct_recode(Drivning,
                          "2wd" = "tvåhjulsdriven",
                          "4wd" = "fyrhjulsdriven")
  ) |> 
  mutate(
    Modell = fct_recode(Modell,
                        "up!" = "up",
                        "passat" = "vit",
                        "touran" = "turan",
                        "golf" = "svart",
                        "passat" = "silver",
                        "golf" = "gti",
                        "golf" = "e-golf",
                        "beetle" = "new beetle",
                        "california" = "california")
  ) |> 
  mutate(
    Biltyp = fct_recode(Biltyp,
                        "Halvkombi" = "halvkombi",
                        "Halvkombi" = "Halvkomni",
                        "Familjebuss" = "familjebuss",
                        "Övriga" = "Cab", # Grupperas till Övriga (beslut taget vid modellering)
                        "Övriga" = "Coupé",
                        "Övriga" = "Sedan")
  ) |> 
  filter(Modellår <= 2022) |> 
  # Skapa Ålder baserat på datum i trafik och hanterar orimliga värden
  mutate(
    Ålder = as.numeric(difftime(Sys.Date(), Datum_i_trafik, units = "days")) / 365.25, 
    Ålder = if_else(Ålder < 0 | Ålder > 25, NA_real_, Ålder)
  ) |> 
  filter(!is.na(Ålder)) |> 
  mutate(
    Miltal = case_when(
      Miltal == 249764 ~ 24976,  
      TRUE ~ Miltal
    )
  ) |> 
  # Skapa nya variabler för analys
  mutate(
    Pris_per_Hästkraft = Försäljningspris / Hästkrafter,
    Mil_per_År = Miltal / Ålder
  )

nrow(blocket_data)
unique(blocket_data$Bränsle)
unique(blocket_data$Drivning)
unique(blocket_data$Modell)
unique(blocket_data$Växellåda)
summary(blocket_data$Hästkrafter)


# ----------------------------------------------------------------------------
# Explorativ Dataanalys (EDA) och Visualisering
# Syfte: Utforska datans struktur, identifiera mönster, relationer mellan variabler
#        och potentiella problem som kan påverka modelleringen.
# ----------------------------------------------------------------------------

# Fördelning av numeriska variabler för att förstå deras spridning och form
blocket_data_long <- blocket_data |> 
  select(Försäljningspris, Miltal, Hästkrafter, Modellår, Pris_per_Hästkraft, Mil_per_År) |> 
  pivot_longer(everything(), names_to = "Variable", values_to = "Value")

ggplot(blocket_data_long, aes(x = Value)) +
  geom_histogram(data = blocket_data_long |> filter(Variable == "Modellår"), 
                 bins = 23, fill = "lightblue", alpha = 0.7) +
  geom_histogram(data = blocket_data_long |> filter(Variable != "Modellår"), 
                 bins = 30, fill = "lightblue", alpha = 0.7) +
  facet_wrap(~ Variable, scales = "free", ncol = 2) +
  scale_x_continuous(labels = function(x) {
    ifelse(x >= 2000 & x <= 2022, as.integer(x), scales::label_number(big.mark = ",")(x))
  }) +
  labs(title = "Fördelning av Numeriska Variabler", x = "Värde", y = "Antal") +
  theme_minimal()

# Jämförelse av försäljningspris mellan olika säljartyper
ggplot(blocket_data, aes(x = Säljare, y = Försäljningspris)) +
  geom_boxplot(fill = "lightcoral") +
  coord_cartesian(ylim = c(0, 500000)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Försäljningspris per Säljare", x = "Säljare", y = "Pris (SEK)") +
  theme_minimal()

# Jämförelse av försäljningspris mellan olika växellådor
ggplot(blocket_data, aes(x = Växellåda, y = Försäljningspris)) +
  geom_boxplot(fill = "lightcyan") +
  coord_cartesian(ylim = c(0, 500000)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Försäljningspris per Växellåda", x = "Växellåda", y = "Pris (SEK)") +
  theme_minimal()

# Fördelning av försäljningspris över olika biltyper
ggplot(blocket_data, aes(x = Biltyp, y = Försäljningspris)) +
  geom_boxplot(fill = "lightyellow") +
  coord_cartesian(ylim = c(0, 500000)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Försäljningspris per Biltyp", x = "Biltyp", y = "Pris (SEK)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Interaktionseffekt av bränsletyp och region på försäljningspris
ggplot(blocket_data, aes(x = Bränsle, y = Försäljningspris, fill = Region)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 500000)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Försäljningspris per Bränsle och Region", x = "Bränsle", y = "Pris (SEK)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Relation mellan bilens ålder och försäljningspris
ggplot(blocket_data, aes(x = Ålder, y = Försäljningspris)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  scale_y_continuous(labels = comma) +
  coord_cartesian(ylim = c(0, 500000)) +
  labs(title = "Försäljningspris vs. Ålder", x = "Ålder (år)", y = "Pris (SEK)") +
  theme_minimal()

# Undersöker om transformation av priset (log) kan visa ett linjärt samband med ålder
ggplot(blocket_data, aes(x = Ålder, y = log(Försäljningspris))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(title = "Log(Försäljningspris) vs. Ålder", x = "Ålder (år)", y = "Log(Pris)") +
  theme_minimal()

# Korrelation mellan numeriska variabler
numeric_vars <- blocket_data |> 
  select(Försäljningspris, Miltal, Hästkrafter, Modellår, Ålder, Pris_per_Hästkraft, Mil_per_År)

cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")

# ----------------------------------------------------------------------------
# Skapar variabeln Modellgrupp
# Syfte: Hantera det stora antalet unika bilmodeller genom att gruppera dem
#        baserat på liknande segment. 
# ----------------------------------------------------------------------------

modell_counts <- blocket_data |> 
  group_by(Modell) |> 
  summarise(Count = n()) |> 
  arrange(desc(Count))

print(modell_counts)
summary(modell_counts$Count)

ggplot(modell_counts, aes(x = reorder(Modell, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Antal Observationer per Bilmodell", x = "Modell", y = "Antal Observationer") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(blocket_data, aes(x = Modell, y = Försäljningspris)) +
  geom_boxplot(fill = "lightblue") +
  coord_cartesian(ylim = c(0, 1000000)) +  # Zooma in för att exkludera rallybilen
  scale_y_continuous(labels = comma) +
  labs(title = "Försäljningspris per Modell", x = "Modell", y = "Pris (SEK)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Skapa den nya kategoriska variabeln Modellgrupp 
blocket_data <- blocket_data |> 
  mutate(
    Modellgrupp = case_when(
      Modell %in% c("golf", "polo", "up!", "gti", "t-roc", "scirocco", "polo cross", "lupo", "jetta", "id.3", "beetle") ~ "Kompaktbilar",
      Modell %in% c("passat", "touran", "crosstouran") ~ "Familjebilar",
      Modell %in% c("tiguan", "touareg", "tiguan allspace", "t-cross", "id.5", "id.4") ~ "SUV:ar",
      Modell %in% c("multivan", "caravelle", "sharan", "caddy") ~ "Minibussar",
      Modell %in% c("arteon", "california", "phaeton") ~ "Lyxmodeller",
      TRUE ~ "Övriga"
    ),
    Modellgrupp = as.factor(Modellgrupp)
  )

modellgrupp_counts <- blocket_data |> 
  group_by(Modellgrupp) |> 
  summarise(Count = n()) |> 
  arrange(desc(Count))

print(modellgrupp_counts)

ggplot(blocket_data, aes(x = Modellgrupp, y = Försäljningspris)) +
  geom_boxplot(fill = "lightblue") +
  coord_cartesian(ylim = c(0, 500000)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Försäljningspris per Modellgrupp", x = "Modellgrupp", y = "Pris (SEK)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Undersöker gruppen "Övriga" närmare 
ovriga_bilar <- blocket_data |> 
  filter(Modellgrupp == "Övriga")

table(ovriga_bilar$Biltyp)
table(ovriga_bilar$Modell)

# Statistisk test (ANOVA) för att undersöka om det finns signifikanta prisskillnader mellan modellgrupperna
anova_result <- aov(Försäljningspris ~ Modellgrupp, data = blocket_data)
summary(anova_result)



# ----------------------------------------------------------------------------
# Regression och Modellering av Försäljningspris
# Syfte: Utveckla en statistisk modell för att prediktera försäljningspris
#        baserat på olika bilattribut. Tre modeller utforskas:
#        1. En basmodell med alla relevanta prediktorer.
#        2. En modell baserad på variabelselektion (Best Subset Selection).
#        3. En modell som inkluderar interaktionseffekter.
# Utvärdering: Modellernas prestanda jämförs med hjälp av 10-faldig
#               korsvalidering (CV10) på träningsdatan.
# ----------------------------------------------------------------------------

set.seed(123)
train_index <- sample(1:nrow(blocket_data), 0.8 * nrow(blocket_data))
train_data <- blocket_data[train_index, ]
test_data <- blocket_data[-train_index, ]

# Log-transformera Försäljningspris för att hantera skevhet och icke-linjäritet
train_data <- train_data |> 
  mutate(Log_Försäljningspris = log(Försäljningspris))
test_data <- test_data |> 
  mutate(Log_Försäljningspris = log(Försäljningspris))

str(train_data)
str(test_data)

# ----------------------------------------------------------------------------
# Basmodell (Model 1)
# ----------------------------------------------------------------------------

model1 <- lm(Log_Försäljningspris ~ Ålder + Miltal + Hästkrafter + Bränsle + Säljare + Biltyp + Region + Växellåda + Modellgrupp, 
             data = train_data)

set.seed(123)
cv_model1 <- train(Log_Försäljningspris ~ Ålder + Miltal + Hästkrafter + Bränsle + Säljare + Biltyp + Region + Växellåda + Modellgrupp, 
                   data = train_data, 
                   method = "lm", 
                   trControl = trainControl(method = "cv", number = 10))

summary(model1)

# Beräkna och presentera genomsnittlig R^2 och RMSE
cv_r2_model1 <- mean(cv_model1$resample$Rsquared)
cv_r2_sd_model1 <- sd(cv_model1$resample$Rsquared)
cv_rmse_model1 <- mean(cv_model1$resample$RMSE)
cv_rmse_sd_model1 <- sd(cv_model1$resample$RMSE)
cat("CV10 Mean R^2 Model 1:", cv_r2_model1, "(SD:", cv_r2_sd_model1, ")\n")
cat("CV10 Mean RMSE Model 1:", cv_rmse_model1, "(SD:", cv_rmse_sd_model1, ")\n")

# ----------------------------------------------------------------------------
# Variabelselektion (Model 2 - Best Subset Selection)
# ----------------------------------------------------------------------------

regfit_best <- regsubsets(Log_Försäljningspris ~ Ålder + Miltal + Hästkrafter + Bränsle + Säljare + Biltyp + Region + Växellåda + Modellgrupp, 
                          data = train_data, nvmax = 20, method = "exhaustive")
reg_summary <- summary(regfit_best)
par(mfrow = c(1, 2))
plot(reg_summary$adjr2, xlab = "Antal prediktorer", ylab = "Justerad R^2", type = "b", main = "Justerad R^2 för Best Subset")
points(which.max(reg_summary$adjr2), max(reg_summary$adjr2), col = "red", pch = 20)
plot(reg_summary$bic, xlab = "Antal prediktorer", ylab = "BIC", type = "b", main = "BIC för Best Subset")
points(which.min(reg_summary$bic), min(reg_summary$bic), col = "red", pch = 20)
par(mfrow = c(1, 1))
best_predictors_13 <- names(coef(regfit_best, id = 13))[-1]
cat("Bästa prediktorer för 13 prediktorer:", best_predictors_13, "\n")

# Bygger Model 2 baserat på resultatet
formula_model2 <- as.formula("Log_Försäljningspris ~ Ålder + Miltal + Hästkrafter + Bränsle + Säljare + Biltyp + Växellåda + Modellgrupp")
model2 <- lm(formula_model2, data = train_data)

summary(model2)

set.seed(123)
cv_model2 <- train(Log_Försäljningspris ~ Ålder + Miltal + Hästkrafter + Bränsle + Säljare + Biltyp + Växellåda + Modellgrupp, 
                   data = train_data, method = "lm", trControl = trainControl(method = "cv", number = 10))

cv_r2_model2 <- mean(cv_model2$resample$Rsquared)
cv_r2_sd_model2 <- sd(cv_model2$resample$Rsquared)
cv_rmse_model2 <- mean(cv_model2$resample$RMSE)
cv_rmse_sd_model2 <- sd(cv_model2$resample$RMSE)
cat("CV10 Mean R^2 Model 2:", cv_r2_model2, "(SD:", cv_r2_sd_model2, ")\n")
cat("CV10 Mean RMSE Model 2:", cv_rmse_model2, "(SD:", cv_rmse_sd_model2, ")\n")

# ----------------------------------------------------------------------------
# Modellförbättring (Model 3 - Interaktioner)
# ----------------------------------------------------------------------------

# Inkluderar en interaktionsterm mellan Ålder och Modellgrupp
formula_model3 <- as.formula("Log_Försäljningspris ~ Ålder + Miltal + Hästkrafter + Bränsle + Säljare + Biltyp + Växellåda + Modellgrupp + Ålder:Modellgrupp")
model3 <- lm(formula_model3, data = train_data)

set.seed(123)
cv_model3 <- train(Log_Försäljningspris ~ Ålder + Miltal + Hästkrafter + Bränsle + Säljare + Biltyp + Växellåda + Modellgrupp + Ålder:Modellgrupp, 
                   data = train_data, method = "lm", trControl = trainControl(method = "cv", number = 10))

summary(model3)

cv_r2_model3 <- mean(cv_model3$resample$Rsquared)
cv_r2_sd_model3 <- sd(cv_model3$resample$Rsquared)
cv_rmse_model3 <- mean(cv_model3$resample$RMSE)
cv_rmse_sd_model3 <- sd(cv_model3$resample$RMSE)
cat("CV10 Mean R^2 Model 3:", cv_r2_model3, "(SD:", cv_r2_sd_model3, ")\n")
cat("CV10 Mean RMSE Model 3:", cv_rmse_model3, "(SD:", cv_rmse_sd_model3, ")\n")

# ----------------------------------------------------------------------------
# Utvärdering av modellerna
# ----------------------------------------------------------------------------

cat("Jämförelse av modeller:\n")
cat("Modell | CV10 Mean RMSE (log-skala) | CV10 Mean R^2 | SD RMSE | SD R^2\n")
cat("Model 1 | ", cv_rmse_model1, " | ", cv_r2_model1, " | ", cv_rmse_sd_model1, " | ", cv_r2_sd_model1, "\n")
cat("Model 2 | ", cv_rmse_model2, " | ", cv_r2_model2, " | ", cv_rmse_sd_model2, " | ", cv_r2_sd_model2, "\n")
cat("Model 3 | ", cv_rmse_model3, " | ", cv_r2_model3, " | ", cv_rmse_sd_model3, " | ", cv_r2_sd_model3, "\n")


# ----------------------------------------------------------------------------
# Undersökning av teoretiska antaganden
# Diagnostik av Model 3 initialt (bästa modell) och sedan Model 2 eftersom
# Model 3 förkastas pga höga värden för multikollinearitet i VIF.
# Utför tester på Model 2 för att kontrollera antaganden om residualer
# (icke-linjäritet, heteroskedasticitet, normalfördelning, autokorrelation)
# ----------------------------------------------------------------------------

# Bedöm multikollinearitet bland prediktorerna med Variance Inflation Factor (VIF)
vif_values <- vif(model3)
cat("VIF-värden för Model 3:\n")
print(vif_values)

# Bedöm VIF igen på Model 2
vif_values <- vif(model2)
cat("VIF-värden för Model 2:\n")
print(vif_values)

# Residuals vs Fitted, Q-Q plot, Scale-Location, Residuals vs Leverage
par(mfrow = c(2, 2))
plot(model2)  

# Identifiera inflytelserika observationer med Cook's distance
cooks_d <- cooks.distance(model2)
influential <- which(cooks_d > 0.5)
cat("Inflytelserika observationer (Cook's distance > 0.5):\n")
print(influential)

# Testa för korrellerade residualer (Durbin-Watson test)
dw_test <- dwtest(model2)
cat("Durbin-Watson-test:\nDW =", dw_test$statistic, "p-value =", dw_test$p.value, "\n")

# Identifiera potentiella outliers baserat på standardiserade residualer
std_res <- rstandard(model2)
outliers <- which(abs(std_res) > 3)
cat("Outliers (observationer med |std residual| > 3):", outliers, "\n")

# Identifiera observationer med hög leverage
leverage <- hatvalues(model2)
p <- length(coef(model2))
n <- nrow(train_data)
high_leverage <- which(leverage > 2 * p / n)
cat("High leverage-punkter:", high_leverage, "\n")


# ----------------------------------------------------------------------------
# Outlier-hantering 
# Undersöker hur borttagning av identifierade outliers påverkar modellens prestanda
# ----------------------------------------------------------------------------

outliers_to_check <- c(217, 263, 285, 316, 361, 378, 549, 594, 626, 734, 834, 847, 886, 909, 939)
cat("Outliers i träningsdata:\n")
print(train_data[outliers_to_check, ])

# Spara CV10-resultaten för den ursprungliga Model 2 (ingen borttagning)
results <- data.frame(
  Scenario = "Ingen borttagning",
  CV10_Mean_R2 = cv_r2_model2,
  CV10_SD_R2 = cv_r2_sd_model2,
  CV10_Mean_RMSE = cv_rmse_model2,
  CV10_SD_RMSE = cv_rmse_sd_model2
)

# Scenario 1: Utvärdera effekten av att ta bort outliers 285, 626, 734 och 847
outliers_to_remove <- c(285, 626, 734, 847)
train_data_4 <- train_data[-outliers_to_remove, ]
model2_2 <- lm(formula_model2, data = train_data_4)
set.seed(123)
cv_model2_2 <- train(formula_model2, 
                     data = train_data_4, 
                     method = "lm", 
                     trControl = trainControl(method = "cv", number = 10))

# Beräkna och spara korsvalideringsresultat för detta scenario
cv_r2_model2_2 <- mean(cv_model2_2$resample$Rsquared)
cv_r2_sd_model2_2 <- sd(cv_model2_2$resample$Rsquared)
cv_rmse_model2_2 <- mean(cv_model2_2$resample$RMSE)
cv_rmse_sd_model2_2 <- sd(cv_model2_2$resample$RMSE)
results <- rbind(results, data.frame(
  Scenario = "4 outliers borttagna (285, 626, 734, 847)",
  CV10_Mean_R2 = cv_r2_model2_2,
  CV10_SD_R2 = cv_r2_sd_model2_2,
  CV10_Mean_RMSE = cv_rmse_model2_2,
  CV10_SD_RMSE = cv_rmse_sd_model2_2
))

# Scenario 2: Utvärdera effekten av att ta bort alla identifierade outliers
all_outliers <- c(217, 263, 285, 316, 361, 378, 549, 594, 626, 734, 834, 847, 886, 909, 939)
train_data_15 <- train_data[-all_outliers, ]
model2_15 <- lm(formula_model2, data = train_data_15)
set.seed(123)
cv_model2_15 <- train(formula_model2, 
                      data = train_data_15, 
                      method = "lm", 
                      trControl = trainControl(method = "cv", number = 10))

cv_r2_model2_15 <- mean(cv_model2_15$resample$Rsquared)
cv_r2_sd_model2_15 <- sd(cv_model2_15$resample$Rsquared)
cv_rmse_model2_15 <- mean(cv_model2_15$resample$RMSE)
cv_rmse_sd_model2_15 <- sd(cv_model2_15$resample$RMSE)
results <- rbind(results, data.frame(
  Scenario = "15 outliers borttagna",
  CV10_Mean_R2 = cv_r2_model2_15,
  CV10_SD_R2 = cv_r2_sd_model2_15,
  CV10_Mean_RMSE = cv_rmse_model2_15,
  CV10_SD_RMSE = cv_rmse_sd_model2_15
))

cat("\nJämförelse av Model 2 med olika outlier-borttagningar:\n")
print(results)

# ----------------------------------------------------------------------------
# Träna om den slutgiltiga model2 och testa på testdata
# ----------------------------------------------------------------------------

final_model <- lm(formula_model2, data = train_data_4)

predictions_final <- predict(final_model, newdata = test_data)
test_rmse_final <- sqrt(mean((test_data$Log_Försäljningspris - predictions_final)^2))
test_r2_final <- 1 - sum((test_data$Log_Försäljningspris - predictions_final)^2) / sum((test_data$Log_Försäljningspris - mean(test_data$Log_Försäljningspris))^2)
cat("Test RMSE (log-skala):", test_rmse_final, "\n")
cat("Test R^2:", test_r2_final, "\n")

predictions_sek_final <- exp(predict(final_model, newdata = test_data))
actual_sek <- exp(test_data$Log_Försäljningspris)
test_rmse_sek_final <- sqrt(mean((actual_sek - predictions_sek_final)^2))
cat("Test RMSE (SEK):", test_rmse_sek_final, "\n")

cat("\nSammanfattning av den slutgiltiga modellen:\n")
summary(final_model)


# ----------------------------------------------------------------------------
# Hypotesprövning, konfidens- och prediktionsintervall
# ----------------------------------------------------------------------------

# Utför hypotesprövning för att bedöma signifikansen av prediktorerna
cat("\nHypotesprövning baserat på p-värden från den slutgiltiga modellen:\n")
summary_final <- summary(final_model)
p_values <- summary_final$coefficients[, 4]
significant_predictors <- names(p_values[p_values < 0.05])
cat("Signifikanta prediktorer (p-värde < 0.05):\n")
print(significant_predictors)

# Beräkna och presentera konfidensintervall för modellens koefficienter
cat("\nKonfidensintervall (95%) för koefficienterna:\n")
confint_final <- confint(final_model, level = 0.95)
print(confint_final)

# Beräkna och presentera konfidens- och prediktionsintervall för några observationer i testdatan
new_data <- test_data[1:5, ]
confidence_intervals <- predict(final_model, newdata = new_data, interval = "confidence", level = 0.95)
prediction_intervals <- predict(final_model, newdata = new_data, interval = "prediction", level = 0.95)

cat("\nKonfidensintervall (95%) för medelvärdet av predikterade log-priser:\n")
print(confidence_intervals)

cat("\nPrediktionsintervall (95%) för individuella predikterade log-priser:\n")
print(prediction_intervals)

# Exponentiera intervallen för att få dem i SEK
confidence_intervals_sek <- exp(confidence_intervals)
prediction_intervals_sek <- exp(prediction_intervals)

cat("\nKonfidensintervall (95%) för medelvärdet av predikterade priser i SEK:\n")
print(confidence_intervals_sek)

cat("\nPrediktionsintervall (95%) för individuella predikterade priser i SEK:\n")
print(prediction_intervals_sek)


# ----------------------------------------------------------------------------
# Undersökning av diskrepans: Elbilar i modellen vs. rådatan
# Syfte: Modellen visar att elbilar är 25 % billigare än bensinbilar, men rådatan
#        och plotten (Figur A5) visar att elbilar har ett högre medelpris.
#        Vi undersöker systematiskt orsaken till denna skillnad.
# ----------------------------------------------------------------------------

# Steg 1: Ladda och granska rådatan
cat("\nSteg 1: Medelvärden och fördelningar i rådatan (blocket_data)\n")

blocket_data |> 
  group_by(Bränsle) |> 
  summarise(
    n = n(),
    mean_price = mean(Försäljningspris, na.rm = TRUE),
    median_price = median(Försäljningspris, na.rm = TRUE),
    sd_price = sd(Försäljningspris, na.rm = TRUE)
  ) |> 
  print()

blocket_data |> 
  group_by(Bränsle) |> 
  summarise(
    mean_age = mean(Ålder, na.rm = TRUE),
    median_age = median(Ålder, na.rm = TRUE)
  ) |> 
  print()

blocket_data |> 
  group_by(Bränsle, Modellgrupp) |> 
  summarise(n = n()) |> 
  mutate(prop = n / sum(n)) |> 
  print()

# Steg 2: Jämför medelpriser efter EDA och databehandling
cat("\nSteg 2: Medelpriser i den bearbetade datan (train_data_4)\n")

train_data_4 |> 
  group_by(Bränsle) |> 
  summarise(
    n = n(),
    mean_price = mean(exp(Log_Försäljningspris), na.rm = TRUE),  # Omvandla tillbaka till SEK
    median_price = median(exp(Log_Försäljningspris), na.rm = TRUE)
  ) |> 
  print()

train_data_4 |> 
  group_by(Bränsle) |> 
  summarise(
    mean_age = mean(Ålder, na.rm = TRUE),
    mean_miles = mean(Miltal, na.rm = TRUE),
    mean_hp = mean(Hästkrafter, na.rm = TRUE)
  ) |> 
  print()

# Steg 3: Undersök modellens prediktioner
cat("\nSteg 3: Modellens predikterade priser per bränsletyp\n")

train_data_4$predicted_log_price <- predict(final_model, newdata = train_data_4)
train_data_4$predicted_price <- exp(train_data_4$predicted_log_price)
train_data_4 |> 
  group_by(Bränsle) |> 
  summarise(
    n = n(),
    mean_predicted_price = mean(predicted_price, na.rm = TRUE),
    mean_actual_price = mean(exp(Log_Försäljningspris), na.rm = TRUE)
  ) |> 
  print()

# Steg 4: Skapa en "typisk" bil och predicera priset för att se den isolerade effekten av bränsletyp.
cat("\nSteg 4: Predikterat pris för en typisk bil (el vs. bensin)\n")

typical_car <- data.frame(
  Ålder = mean(train_data_4$Ålder, na.rm = TRUE),
  Miltal = mean(train_data_4$Miltal, na.rm = TRUE),
  Hästkrafter = mean(train_data_4$Hästkrafter, na.rm = TRUE),
  Bränsle = factor(c("bensin", "el"), levels = levels(train_data_4$Bränsle)),  # Testar både bensin och el
  Säljare = factor("Privat", levels = levels(train_data_4$Säljare)),
  Biltyp = factor("Kombi", levels = levels(train_data_4$Biltyp)),
  Växellåda = factor("manuell", levels = levels(train_data_4$Växellåda)),
  Modellgrupp = factor("Kompaktbilar", levels = levels(train_data_4$Modellgrupp))
)

typical_car$predicted_log_price <- predict(final_model, newdata = typical_car)
typical_car$predicted_price <- exp(typical_car$predicted_log_price)
typical_car[, c("Bränsle", "predicted_price")] |> 
  print()

# Steg 5: Jämför medelpriser med justering för ålder
cat("\nSteg 5: Medelpriser per bränsle och åldersgrupp\n")

blocket_data <- blocket_data |> 
  mutate(Age_Group = cut(Ålder, breaks = c(0, 5, 10, 15, 20, Inf),
                         labels = c("0-5", "6-10", "11-15", "16-20", "20+")))

blocket_data |> 
  group_by(Bränsle, Age_Group) |> 
  summarise(
    n = n(),
    mean_price = mean(Försäljningspris, na.rm = TRUE)
  ) |> 
  filter(Bränsle %in% c("bensin", "el")) |> 
  print()

# Steg 6: Granska plotten och regionens effekt
cat("\nSteg 6: Medelpriser per bränsle och region\n")

blocket_data |> 
  group_by(Bränsle, Region) |> 
  summarise(
    n = n(),
    mean_price = mean(Försäljningspris, na.rm = TRUE)
  ) |> 
  filter(Bränsle %in% c("bensin", "el")) |> 
  print()

blocket_data |> 
  group_by(Region, Bränsle) |> 
  summarise(n = n()) |> 
  filter(Bränsle %in% c("bensin", "el")) |> 
  group_by(Region) |> 
  mutate(prop = n / sum(n)) |> 
  print()
