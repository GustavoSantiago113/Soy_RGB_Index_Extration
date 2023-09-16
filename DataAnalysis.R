# Analyzing the Data ----

## Libraries ----

library(dplyr)
library(tidyverse)
library(stringr)
library(ggcorrplot)
library(xgboost)
library(caTools)
library(Metrics)
library(ggplot2)
library(openxlsx)
library(agricolae)

## Calculating Anova for the whole experiment ----

data <- read.csv("data.csv")
data$Bloco <- as.factor(data$Bloco)
data$Tratamento <- as.factor(data$Tratamento)
data$Inoculacao <- as.factor(data$Inoculacao)
data$DAS <- as.factor(data$DAS)

variables <- c("Rn", "Gn", "Bn", "H", "S", "V", "MPRI", "ICVE", "Clof.A", "Clof.B", "Clorof.Total", "NDVI")

model_list <- list()

xlsx_file <- "anova_summary.xlsx"
wb <- createWorkbook()

for (variable in variables) {
  
  formula <- as.formula(paste(variable, "~ Bloco + Tratamento + Inoculacao + DAS + Tratamento*Inoculacao*DAS"))
  
  model <- aov(formula,
               data = data)
  
  model_name <- paste(variable, "Anova", sep = "_")
  model_list[[model_name]] <- model
  
  summary_table <- data.frame(unclass(summary(model_list[[model_name]])), check.names = FALSE, stringsAsFactors = FALSE) %>%
    rownames_to_column()
  
  addWorksheet(wb, sheetName = model_name)
  writeData(wb, sheet = model_name, x = summary_table, startCol = 1, startRow = 1)
  
}

saveWorkbook(wb, xlsx_file)


## Calculating Anova for specific periods ----

### Only DAS ----

data <- read.csv("data.csv")
data$Bloco <- as.factor(data$Bloco)
data$Tratamento <- as.factor(data$Tratamento)
data$Inoculacao <- as.factor(data$Inoculacao)
data$DAS <- as.factor(data$DAS)

variablesDAS <- c("Rn", "H", "S","MPRI", "ICVE", "Bn")

modelDAS_list <- list()

xlsx_file <- "TukeyDAS_summary.xlsx"
wb <- createWorkbook()

for (variable in variablesDAS) {
  
  formula <- as.formula(paste(variable, "~ Bloco + Tratamento + Inoculacao + DAS + Tratamento*Inoculacao*DAS"))
  
  model <- aov(formula,
               data = data)
  
  model_name <- paste(variable, "Tukey", sep = "_")
  modelDAS_list[[model_name]] <- model
  
  tukey <- HSD.test(modelDAS_list[[model_name]], 'DAS')
  
  summaryTukeyDAS_table <- tukey[["groups"]] %>%
    rownames_to_column()
  
  addWorksheet(wb, sheetName = model_name)
  writeData(wb, sheet = model_name, x = summaryTukeyDAS_table, startCol = 1, startRow = 1)
  
}

saveWorkbook(wb, xlsx_file)


### Tratamento:DAS -----

data <- read.csv("data.csv")
data$Bloco <- as.factor(data$Bloco)
data$Tratamento <- as.factor(data$Tratamento)
data$Inoculacao <- as.factor(data$Inoculacao)

variablesTDAS <- c("Gn", "Clof.B", "Clorof.Total", "Clof.A")
DASList <- c(31, 42, 53, 65, 87, 101, 109)

modelTDAS_list <- list()

xlsx_file <- "TukeyTDAS_summary.xlsx"
wb <- createWorkbook()

for (variable in variablesTDAS) {
  
  for (day in DASList){
    dataUsed <- data %>%
      filter(DAS == day)
    
    dataUsed$DAS <- as.factor(dataUsed$DAS)
    
    formula <- as.formula(paste(variable, "~ Bloco + Tratamento"))
    
    model <- aov(formula,
                 data = dataUsed)
    
    model_name <- paste(variable, day, "Tukey", sep = "_")
    modelTDAS_list[[model_name]] <- model
    
    tukey <- HSD.test(modelTDAS_list[[model_name]], 'Tratamento')
    
    summaryTukeyDAS_table <- tukey[["groups"]] %>%
      rownames_to_column()
    
    addWorksheet(wb, sheetName = model_name)
    writeData(wb, sheet = model_name, x = summaryTukeyDAS_table, startCol = 1, startRow = 1)
    
  }
  
}

saveWorkbook(wb, xlsx_file)

### Inoculacao:DAS ---- 

data <- read.csv("data.csv")
data$Bloco <- as.factor(data$Bloco)
data$Tratamento <- as.factor(data$Tratamento)
data$Inoculacao <- as.factor(data$Inoculacao)

variablesIDAS <- c("Clorof.Total", "Clof.A", "V", "NDVI")
DASList <- c(31, 42, 53, 65, 87, 101, 109)

modelIDAS_list <- list()

xlsx_file <- "TukeyIDAS_summary.xlsx"
wb <- createWorkbook()

for (variable in variablesIDAS) {
  
  for (day in DASList){
    dataUsed <- data %>%
      filter(DAS == day)
    
    formula <- as.formula(paste(variable, "~ Bloco + Inoculacao"))
    
    model <- aov(formula,
                 data = dataUsed)
    
    model_name <- paste(variable, day, "Tukey", sep = "_")
    modelIDAS_list[[model_name]] <- model
    
    tukey <- HSD.test(modelIDAS_list[[model_name]], 'Inoculacao')
    
    summaryTukeyDAS_table <- tukey[["groups"]] %>%
      rownames_to_column()
    
    addWorksheet(wb, sheetName = model_name)
    writeData(wb, sheet = model_name, x = summaryTukeyDAS_table, startCol = 1, startRow = 1)
    
  }
  
}

saveWorkbook(wb, xlsx_file)

### Inoculacao -----

data <- read.csv("data.csv")
data$Bloco <- as.factor(data$Bloco)
data$Tratamento <- as.factor(data$Tratamento)
data$Inoculacao <- as.factor(data$Inoculacao)
data$DAS <- as.factor(data$DAS)

xlsx_file <- "TukeyDASINO_summary.xlsx"
wb <- createWorkbook()

formula <- as.formula(paste("Bn", "~ Bloco + Tratamento + Inoculacao + DAS + Tratamento*Inoculacao*DAS"))

model <- aov(formula,
             data = data)

tukey <- HSD.test(model, 'Inoculacao')

summaryTukeyInoculacao_table <- tukey[["groups"]] %>%
  rownames_to_column()

addWorksheet(wb, sheetName = "Inoculacao")
writeData(wb, sheet = "Inoculacao", x = summaryTukeyInoculacao_table, startCol = 1, startRow = 1)

saveWorkbook(wb, xlsx_file)

### Tratamento ----

data <- read.csv("data.csv")
data$Bloco <- as.factor(data$Bloco)
data$Tratamento <- as.factor(data$Tratamento)
data$Inoculacao <- as.factor(data$Inoculacao)
data$DAS <- as.factor(data$DAS)

xlsx_file <- "TukeyTrat_summary.xlsx"
wb <- createWorkbook()

formula <- as.formula(paste("NDVI", "~ Bloco + Tratamento + Inoculacao + DAS + Tratamento*Inoculacao*DAS"))

model <- aov(formula,
             data = data)

tukey <- HSD.test(model, 'Tratamento')

summaryTukeyInoculacao_table <- tukey[["groups"]] %>%
  rownames_to_column()

addWorksheet(wb, sheetName = "Tratamento")
writeData(wb, sheet = "Tratamento", x = summaryTukeyInoculacao_table, startCol = 1, startRow = 1)

saveWorkbook(wb, xlsx_file)

## Generating graphs ----

data <- read.csv("data.csv")
data$Bloco <- as.factor(data$Bloco)
data$Tratamento <- as.factor(data$Tratamento)
data$Inoculacao <- as.factor(data$Inoculacao)

### Only DAS ----
# Rn, H, S, MPRI, ICVE, Bn - Line graph	
datafgraph <- data %>%
  dplyr::select(c(DAS, Bn)) %>%
  group_by(DAS) %>%
  mutate(SD = sd(Bn)) %>%
  mutate(Bn = mean(Bn, na.rm = TRUE)) %>%
  unique()

p <- ggplot(datafgraph, aes(x = DAS, y = Bn)) +
  geom_line(color = "black") +
  geom_errorbar(aes(ymin = Bn - SD, ymax = Bn + SD), width = 0.2, color = "red") +
  labs(x = "DAS", y = "Bn")

print(p)

### Treatment:DAS ----
# Gn, Clof.A, Clof.B, Clorof.Total - Barras

datafgraph <- data %>%
  dplyr::select(c(DAS, Tratamento, Clof.B)) %>%
  group_by(DAS, Tratamento) %>%
  mutate(SD = sd(Clof.B, na.rm = TRUE)) %>%
  mutate(Clof.B = mean(Clof.B, na.rm = TRUE)) %>%
  unique()

datafgraph$DAS <- as.factor(datafgraph$DAS)

p <- ggplot(datafgraph, aes(x = DAS, y = Clof.B, fill = Tratamento)) +
  geom_bar(position="dodge", stat="identity", width = 0.7) +
  geom_errorbar(aes(ymin = Clof.B - SD, ymax = Clof.B + SD), width = 0.25,  # Adjust the width of error bars as needed
                position = position_dodge(width = 0.7), color = "red") +
  labs(x = "DAS", y = "Chlorophyll B", fill = "Treatment")

print(p)

### Inoculation:DAS ----
# Clorof.Total, Clof.A, NDVI, V - Linear Graph

datafgraph <- data %>%
  dplyr::select(c(DAS, Inoculacao, NDVI)) %>%
  group_by(DAS, Inoculacao) %>%
  mutate(SD = sd(NDVI, na.rm = TRUE)) %>%
  mutate(NDVI = mean(NDVI, na.rm = TRUE)) %>%
  mutate(Inoculacao = ifelse(Inoculacao == "com", "yes", 
                              ifelse(Inoculacao == "sem", "no", Inoculacao)))%>%
  unique() %>%
  drop_na()

p <- ggplot(datafgraph, aes(x = DAS, y = NDVI, color = Inoculacao)) +
  geom_line() +
  labs(x = "DAS", y = "NDVI") +
  guides(color = guide_legend(title = "Inoculation")) +
  scale_color_manual(values = c("yes" = "blue", "no" = "red"))

print(p)

### Treatment ----
# NDVI - Box Plot
datafgraph <- data %>%
  dplyr::select(c(Tratamento, NDVI)) %>%
  group_by(Tratamento) %>%
  mutate(SD = sd(NDVI, na.rm = TRUE)) %>%
  drop_na()

p <- ggplot(datafgraph, aes(x = Tratamento, y = NDVI)) +
  geom_boxplot() +
  labs(x = "Treatment", y = "NDVI")

print(p)

## Generating correlation matrix ----

### With NDVI ----

data <- read.csv("data.csv")

dataForCorrelationWNDVI <- data %>%
  dplyr::select(-c(Bloco, Tratamento, Inoculacao, DAS)) %>%
  tidyr::drop_na()

ggcorrplot(cor(dataForCorrelationWNDVI),
           type = "lower",
           lab = TRUE)
  
### Without NDVI ----
dataForCorrelationWONDVI <- data %>%
  dplyr::select(-c(Bloco, Tratamento, Inoculacao, DAS, NDVI)) %>%
  tidyr::drop_na()

ggcorrplot(cor(dataForCorrelationWONDVI),
           type = "lower",
           lab = TRUE)

## Linear model ----

data <- read.csv("data.csv")

dataForCorrelationWNDVI <- data %>%
  dplyr::select(-c(Bloco, Tratamento, Inoculacao, DAS, Clof.A, Clof.B, Clorof.Total)) %>%
  tidyr::drop_na()

model <- lm(NDVI ~ Rn + Gn + H + V + S + Bn,
             data = dataForCorrelationWNDVI)

summary(model)

ggplot(dataForCorrelationWNDVI, aes(x=predict(model), y= NDVI)) +
  geom_point() +
  geom_abline() +
  labs(x='Predicted Values', y='Observed Values', title='Predicted vs. Observed Values')
