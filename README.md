# Analys av begagnatpriser för Volkswagen-bilar

## Projektöverblick

Detta projekt analyserar begagnatpriser för Volkswagen-bilar med linjär regression, baserat på data från Blocket och SCB. Syftet är att besvara tre frågor:
1. Hur påverkar faktorer som bilens ålder, bränsletyp och modellgrupp försäljningspriset?
2. Kan en linjär regressionsmodell förklara prisvariation och predicera priser på osedd data med hög noggrannhet?
3. Hur hanteras extremvärden (outliers) för att förbättra modellens prestanda utan överanpassning?

Projektet utvecklade tre linjära regressionsmodeller i R, där **Model 2** valdes för sin balanserade prestanda (R²=0.9206, RMSE=0.2234 på träningsdatan; testset: R²=0.9378, RMSE=0.1948). Viktiga resultat inkluderar att ålder minskar priset med 8.9 % per år, elbilar är 25.0 % billigare än bensinbilar, och dieselbilar är 13.8 % dyrare. Extremvärden hanterades med Cook’s distance och okulär kontroll, och korsvalidering (CV10) säkerställde generaliserbarhet. För detaljer, se rapporten `Analys av begagnatpriser.pdf`.

**Tekniker och verktyg:**
- **R**: Databehandling (`dplyr`, `tidyr`), modellering (`lm`), visualisering (`ggplot2`), och korsvalidering.
- **Excel**: Datainsamling från Blocket.
- **Statistiska metoder**: Hypotesprövning, konfidensintervall, extremvärdesanalys (James et al., 2021).
- **Datakällor**: Blocket (begagnatpriser), SCB (nyregistreringar 2000–2022).

## Filer

- **`Blocket_lm.R`**:  
  R-skript för linjär regressionsanalys. Inkluderar databehandling, modellering (Model 1–3), extremvärdeshantering (Cook’s distance, standardiserade residualer), och prestandautvärdering (R², RMSE, CV10). Kräver `Datainsamling_Blocket.xlsx` som indata.

- **`SCB_stat.R`**:  
  R-skript för att bearbeta SCB-data om nyregistrerade bilar efter bränsletyp. Kräver tillgång till SCB:s dataset.

- **`Datainsamling_Blocket.xlsx`**:  
  Excel-fil med rådata från Blocket, inklusive försäljningspris, ålder, bränsletyp, modellgrupp, miltal och hästkrafter för Volkswagen-bilar.

- **`Analys av begagnatpriser.pdf`**:  
  Rapport som dokumenterar projektets metod, analys, resultat och slutsatser. Inkluderar visualiseringar (t.ex. boxplot, korrelationsmatris) och tabeller (t.ex. modelljämförelse).

## Krav och körning

För att köra skripten (`Blocket_lm.R`, `SCB_stat.R`) behöver du:
- **R** (version 4.0 eller senare).
