# Analyzing the Data ----

## Libraries ----

library(tidyverse)
library(stringr)
library(ggcorrplot)
library(caTools)
library(Metrics)
library(ggplot2)
library(openxlsx)
library(multcompView)

## Calculating Anova for the whole experiment ----

data <- read.csv("data.csv")
data$Block <- as.factor(data$Block)
data$Treatment <- as.factor(data$Treatment)
data$Inoculation <- as.factor(data$Inoculation)
data$DAS <- as.factor(data$DAS)

variables <- c("Rn", "Gn", "Bn", "H", "S", "V", "MPRI", "ICVE", "Chlorophyll.A", "Chlorophyll.B", "Total.Chlorophyll", "NDVI")

model_list <- list()

xlsx_file <- "anova_summary.xlsx"
wb <- createWorkbook()

for (variable in variables) {
  
  dataUsed <- data %>%
    select(DAS, Block, Treatment, Inoculation, {{ variable }}) %>%
    group_by(DAS, Block, Treatment, Inoculation) %>%
    summarise(Variable = mean(.data[[variable]], na.rm = TRUE))
  
  model <- aov(Variable ~ Block + Treatment*Inoculation*DAS,
               data = dataUsed)
  
  model_name <- paste(variable, "Anova", sep = "_")
  model_list[[model_name]] <- model
  
  summary_table <- data.frame(unclass(summary(model_list[[model_name]])), check.names = FALSE, stringsAsFactors = FALSE) %>%
    rownames_to_column()
  
  addWorksheet(wb, sheetName = model_name)
  writeData(wb, sheet = model_name, x = summary_table, startCol = 1, startRow = 1)
  
}

saveWorkbook(wb, xlsx_file)

## Tukey test for difference in treatment (Gn, S, MPRI, NDVI) ----

data <- read.csv("data.csv")
data$Block <- as.factor(data$Block)
data$Treatment <- as.factor(data$Treatment)
data$Inoculation <- as.factor(data$Inoculation)
data$DAS <- as.factor(data$DAS)

variables <- c("Gn", "S", "MPRI", "NDVI")

xlsx_file <- "Treatment.xlsx"
wb <- createWorkbook()

for (variable in variables) {
  
  data_summary <- data %>%
    select(Block, Treatment, {{ variable }}) %>%
    group_by(Treatment) %>%
    summarise(mean = mean(.data[[variable]], na.rm = TRUE),
              sd = sd(.data[[variable]], na.rm = TRUE)) %>%
    arrange(desc(mean))
  
  dataUsed <- data %>%
    select(DAS, Block, Treatment, Inoculation, {{ variable }}) %>%
    group_by(DAS, Block, Treatment, Inoculation) %>%
    summarise(Variable = mean(.data[[variable]], na.rm = TRUE))
  
  model <- aov(Variable~ Block + Treatment*Inoculation*DAS,
               data = dataUsed)
  
  model_name <- paste(variable, "Tukey", sep = "_")
  
  tukey <- TukeyHSD(model)
  
  tukey.cld <- multcompLetters4(model, tukey)
  
  cld <- as.data.frame.list(tukey.cld$`Treatment`)
  data_summary$Tukey <- cld$Letters
  
  addWorksheet(wb, sheetName = model_name)
  writeData(wb, sheet = model_name, x = data_summary, startCol = 1, startRow = 1)
  
}

saveWorkbook(wb, xlsx_file)

## Tukey test for difference in inoculation (NDVI) ----

### Tukey ----
data <- read.csv("data.csv")
data$Block <- as.factor(data$Block)
data$Treatment <- as.factor(data$Treatment)
data$Inoculation <- as.factor(data$Inoculation)
data$DAS <- as.factor(data$DAS)

variables <- "NDVI"

xlsx_file <- "Inoculation.xlsx"
wb <- createWorkbook()

for (variable in variables) {
  
  data_summary <- data %>%
    select(Block, Inoculation, {{ variable }}) %>%
    group_by(Inoculation) %>%
    summarise(mean = mean(.data[[variable]], na.rm = TRUE),
              sd = sd(.data[[variable]], na.rm = TRUE)) %>%
    arrange(desc(mean))
  
  dataUsed <- data %>%
    select(DAS, Block, Treatment, Inoculation, {{ variable }}) %>%
    group_by(DAS, Block, Treatment, Inoculation) %>%
    summarise(Variable = mean(.data[[variable]], na.rm = TRUE))
  
  model <- aov(Variable~ Block + Treatment*Inoculation*DAS,
               data = dataUsed)
  
  model_name <- paste(variable, "Tukey", sep = "_")
  
  tukey <- TukeyHSD(model)
  
  tukey.cld <- multcompLetters4(model, tukey)
  
  cld <- as.data.frame.list(tukey.cld$`Inoculation`)
  data_summary$Tukey <- cld$Letters
  
  addWorksheet(wb, sheetName = model_name)
  writeData(wb, sheet = model_name, x = data_summary, startCol = 1, startRow = 1)
  
}

saveWorkbook(wb, xlsx_file)

### Graph ----
datafgraph <- data %>%
  dplyr::select(Inoculation, Treatment, NDVI) %>%
  group_by(Inoculation) %>%
  na.omit() %>%
  mutate(Inoculation = ifelse(Inoculation == "sim", "yes", 
                             ifelse(Inoculation == "não", "no", Inoculation)))%>%
  select(Inoculation, NDVI) %>%
  unique()

p <- ggplot(datafgraph, aes(x = Inoculation, y = NDVI, color = Inoculation)) +
  geom_boxplot()+
  labs(x = "Inoculation", y = variable, group = "Inoculation")+
  theme_minimal()

print(p)

ggsave(filename = "NDVI_Plot.jpg",
       dpi  = 500,
       bg="white")


## Line graph for DAS (Rn, Gn, Bn. S, V, MPRI, NDVI) ----

data <- read.csv("data.csv")

variables <- c("Rn", "Bn", "Gn", "V", "S", "MPRI", "NDVI")

### Graph ----

for (variable in variables){
  
  datafgraph <- data %>%
    dplyr::select(DAS, {{ variable }}) %>%
    group_by(DAS) %>%
    na.omit() %>%
    mutate(SD = sd( .data[[variable]]), na.rm = TRUE) %>%
    mutate(Variable = mean(.data[[variable]]), na.rm = TRUE) %>%
    unique()
  
  p <- ggplot(datafgraph, aes(x = DAS, y = Variable)) +
    geom_line(color = "black", linewidth = 0.7) +
    geom_errorbar(aes(ymin = Variable - SD, ymax = Variable + SD), width = 0.2, color = "red") +
    labs(x = "DAS", y = variable) +
    theme_minimal()
  
  print(p)
  
  ggsave(filename = paste(variable, "Line_Plot.jpg", sep = "_"),
         dpi  = 500,
         bg="white")
  
}

### Tukey ----

data$Block <- as.factor(data$Block)
data$Treatment <- as.factor(data$Treatment)
data$Inoculation <- as.factor(data$Inoculation)
data$DAS <- as.factor(data$DAS)

xlsx_file <- "DAS1.xlsx"
wb <- createWorkbook()

for (variable in variables) {
  
  if(variable == "NDVI"){
    data_summary <- data %>%
      select(DAS, {{ variable }}) %>%
      filter(DAS %in% c(31, 42, 53, 65)) %>%
      group_by(DAS) %>%
      summarise(mean = mean(.data[[variable]], na.rm = TRUE),
                sd = sd(.data[[variable]], na.rm = TRUE)) %>%
      arrange(desc(mean))
  }else{
    data_summary <- data %>%
      select(DAS, {{ variable }}) %>%
      group_by(DAS) %>%
      summarise(mean = mean(.data[[variable]], na.rm = TRUE),
                sd = sd(.data[[variable]], na.rm = TRUE)) %>%
      arrange(desc(mean))
  }
  
  dataUsed <- data %>%
    select(DAS, Block, Treatment, Inoculation, {{ variable }}) %>%
    group_by(DAS, Block, Treatment, Inoculation) %>%
    summarise(Variable = mean(.data[[variable]], na.rm = TRUE))
  
  model <- aov(Variable~ Block + Treatment*Inoculation*DAS,
               data = dataUsed)
  
  model_name <- paste(variable, "Tukey", sep = "_")
  
  tukey <- TukeyHSD(model)
  
  tukey.cld <- multcompLetters4(model, tukey)
  
  cld <- as.data.frame.list(tukey.cld$`DAS`)
  data_summary$Tukey <- cld$Letters
  
  addWorksheet(wb, sheetName = model_name)
  writeData(wb, sheet = model_name, x = data_summary, startCol = 1, startRow = 1)
  
}

saveWorkbook(wb, xlsx_file)

## Interaction Inoculation x DAS (Chlorophyll A, Chlorophyll B, Total Chlorophyll) ----

data <- read.csv("data.csv")
data$Block <- as.factor(data$Block)
data$Treatment <- as.factor(data$Treatment)
data$Inoculation <- as.factor(data$Inoculation)
data$DAS <- as.factor(data$DAS)

### Tukey ----

variablesIDAS <- c("Chlorophyll.A", "Chlorophyll.B", "Total.Chlorophyll")

modelIDAS_list <- list()

xlsx_file <- "TukeyIDAS_summary.xlsx"
wb <- createWorkbook()

for (variable in variablesIDAS) {
  
  dataUsed <- data %>%
    select(DAS, Block, Treatment, Inoculation, {{ variable }}) %>%
    group_by(DAS, Block, Treatment, Inoculation) %>%
    summarise(Variable = mean(.data[[variable]], na.rm = TRUE))
  
  data_summary <- dataUsed %>%
    select(Block, Inoculation, DAS, Variable) %>%
    group_by(DAS, Inoculation) %>%
    na.omit() %>%
    summarise(mean = mean(Variable, na.rm = TRUE),
              sd = sd(Variable, na.rm = TRUE)) %>%
    arrange(desc(mean))
  
  model <- aov(Variable ~ Block + DAS*Treatment*Inoculation,
               data = dataUsed)
  
  model_name <- paste(variable, "Tukey", sep = "_")
  
  tukey <- TukeyHSD(model)
  
  tukey.cld <- multcompLetters4(model, tukey)
  
  cld <- as.data.frame.list(tukey.cld$`DAS:Inoculation`)
  
  data_summary$Tukey <- cld$Letters
  
  data_summary <- data_summary %>% arrange(Inoculation)
  
  addWorksheet(wb, sheetName = model_name)
  writeData(wb, sheet = model_name, x = data_summary, startCol = 1, startRow = 1)
    
  
}

saveWorkbook(wb, xlsx_file)

### Graph ----

for (variable in variablesIDAS){
  
  datafgraph <- data %>%
    dplyr::select(DAS, Inoculation, {{ variable }}) %>%
    group_by(DAS, Inoculation) %>%
    na.omit() %>%
    mutate(SD = sd( .data[[variable]]), na.rm = TRUE) %>%
    mutate(Variable = mean(.data[[variable]]), na.rm = TRUE) %>%
    mutate(Inoculation = case_when(Inoculation == "sim" ~ "yes",
                                   Inoculation == "não" ~ "no")) %>%
    select(DAS, Variable, SD) %>%
    unique()
  
  datafgraph$DAS <- as.numeric(as.character(datafgraph$DAS))
  
  p <- ggplot(datafgraph, aes(x = DAS, y = Variable, color = Inoculation)) +
    geom_line() +
    labs(x = "DAS", y = variable, color = "Inoculation") +
    theme_minimal()
  
  print(p)
  
  ggsave(filename = paste(variable, "Line_Plot.jpg", sep = "_"),
         dpi  = 500,
         bg="white")
  
}


## Triple Interaction (H, ICVE) ----

data <- read.csv("data.csv")
data$Block <- as.factor(data$Block)
data$Treatment <- as.factor(data$Treatment)
data$Inoculation <- as.factor(data$Inoculation)
data$DAS <- as.factor(data$DAS)

variablesTIDAS <- c("H", "ICVE")

### Graph ----

for (variable in variablesTIDAS){
  
  datafgraph <- data %>%
    dplyr::select(DAS, Treatment, Inoculation, {{ variable }}) %>%
    group_by(DAS, Treatment, Inoculation) %>%
    na.omit() %>%
    mutate(SD = sd( .data[[variable]]), na.rm = TRUE) %>%
    mutate(Variable = mean(.data[[variable]]), na.rm = TRUE) %>%
    mutate(Inoculation = case_when(Inoculation == "sim" ~ "yes",
                                   Inoculation == "não" ~ "no")) %>%
    select(DAS, Variable, SD) %>%
    unique()
  
  datafgraph$DAS <- as.numeric(as.character(datafgraph$DAS))
  datafgraph$Inoculation <- as.factor(datafgraph$Inoculation)
  
  p <- ggplot(datafgraph, aes(x = DAS, y = Variable, color = Treatment, linetype = Inoculation)) +
    geom_line() +
    labs(x = "DAS", y = variable, color = "Inoculation") +
    theme_minimal()
  
  print(p)
  
  ggsave(filename = paste(variable, "Line_Plot.jpg", sep = "_"),
         dpi  = 500,
         bg="white")
  
}

### Tukey ----

modelTIDAS_list <- list()

xlsx_file <- "TukeyTIDAS_summary.xlsx"
wb <- createWorkbook()

for (variable in variablesTIDAS) {
  
  dataUsed <- data %>%
    select(DAS, Block, Treatment, Inoculation, {{ variable }}) %>%
    group_by(DAS, Block, Treatment, Inoculation) %>%
    summarise(Variable = mean(.data[[variable]], na.rm = TRUE))
  
  data_summary <- dataUsed %>%
    select(Block, Treatment, Inoculation, DAS, Variable) %>%
    group_by(DAS, Treatment, Inoculation) %>%
    na.omit() %>%
    summarise(mean = mean(Variable, na.rm = TRUE),
              sd = sd(Variable, na.rm = TRUE)) %>%
    arrange(desc(mean))
  
  model <- aov(Variable ~ Block + DAS*Treatment*Inoculation,
               data = dataUsed)
  
  model_name <- paste(variable, "Tukey", sep = "_")
  
  tukey <- TukeyHSD(model)
  
  tukey.cld <- multcompLetters4(model, tukey)
  
  cld <- as.data.frame.list(tukey.cld$`DAS:Treatment:Inoculation`)
  
  data_summary$Tukey <- cld$Letters
  
  data_summary <- data_summary %>% arrange(Inoculation)
  
  addWorksheet(wb, sheetName = model_name)
  writeData(wb, sheet = model_name, x = data_summary, startCol = 1, startRow = 1)
  
  
}

saveWorkbook(wb, xlsx_file)


## Generating correlation matrix ----

### With NDVI ----

data <- read.csv("dataNew.csv")

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

data <- read.csv("dataNew.csv")

variables <- c("NDVI", "Clof.A", "Clof.B", "Clorof.Total")
coefficients_df <- data.frame(Variable = character(0), Intercept = numeric(0))

for (variable in variables){
  
  datanew <- data %>%
    dplyr::select(-c(Bloco, Tratamento, Inoculacao, DAS)) %>%
    mutate(Variable = .data[[variable]]) %>%
    na.omit()
  
  formula <- as.formula(paste(variable, "~ Rn + Gn + H + V + S + Bn"))
  
  model <- lm(formula,
              data = datanew)
  
  summary(model)
  
  coefficients_df <- rbind(coefficients_df, data.frame(Variable = variable, Intercept = coef(model)[1], Coefficients = coef(model)[-1]))
  
  p <- ggplot(datanew, aes(x=predict(model), y = Variable)) +
      geom_point(na.rm = TRUE, color = "blue") +
      geom_abline(linewidth = 2)
  
  print(p)
  
  ggsave(filename = paste(variable, "Predictions_Plot.jpg", sep = "_"),
         dpi  = 500,
         bg="white")
  
}

write.csv(coefficients_df, "coefficients.csv", row.names = FALSE)

