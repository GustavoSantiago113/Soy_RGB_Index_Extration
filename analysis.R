# Analyzing the Data ----

## Libraries ----

library(tidyverse)
library(ggplot2)
library(lme4)
library(car)
library(emmeans)
library(multcomp)
library(RColorBrewer)
library(ggpubr)
library(corrplot)


## Data preparation ----

data <- read.csv("data.csv")
data$Block <- as.factor(data$Block)
data$Treatment <- as.factor(data$Treatment)
data$Inoculation <- as.factor(data$Inoculation)
data$DAS <- as.factor(data$DAS)

variables <- c("ICVE", "ExG", "NGRDI", "NGBDI", "RGBVI", "MGRVI", "Chlorophyll.A", "Chlorophyll.B", "Total.Chlorophyll", "NDVI")

dataUsed <- data %>%
  group_by(DAS, Block, Treatment, Inoculation) %>%
  summarise(across(all_of(variables), mean, na.rm = TRUE, .names = "{.col}"))

## Calculating ANOVA for whole experiment ----

variables <- c("ICVE", "ExG", "NGRDI", "NGBDI", "RGBVI", "MGRVI", "Chlorophyll.A", "Chlorophyll.B", "Total.Chlorophyll", "NDVI")

lines <- c("Intercept","Treatment", "Inoculation", "DAS", "Treatment:Inoculation", "Treatment:DAS", "Inoculation:DAS", "Treatment:Inoculation:DAS")
general_anova_table <- data.frame(lines)

for (variable in variables) {
  
  formula <- as.formula(paste(variable, "~ Treatment*Inoculation*DAS + (1|Block)"))
  
  model <- lmer(formula, data = dataUsed)
  anova_results <- Anova(model, type = "III")
  
  general_anova_table[[variable]] <- unlist(anova_results$`Pr(>Chisq)`)
  
}

write.csv(general_anova_table, "./Outputs/general_anova.csv", row.names = FALSE)

# I observed that ICVE and the NGBDI presented p<0.05 in the triple interaction
# and CHlo.A presented p<0.05 in the interaction Inoculation:DAS
# ExG, NGRDI, RGBVI, MGRVI, Chlo.B Total.Chlo and NDVI presented p<0.05 in DAS
# Lastly, ExG and NDVI presented p<0.05 in Inoculation

## Triple interaction ----

variables <- c("ICVE", "NGBDI")

triple_interaction_table <- data.frame()

for(variable in variables){
  
  formula <- as.formula(paste(variable, "~ Treatment*Inoculation*DAS + (1|Block)"))
  
  model <- lmer(formula, data = dataUsed)
  anova_results <- Anova(model, type = "III")
  
  emmeans_result <- emmeans(model, ~ Treatment*Inoculation*DAS)
  tukey_results <- cld(emmeans_result, Letters = letters, adjust = "tukey")
  tukey_results$`.group` <- rev(tukey_results$`.group`)
  
  if (nrow(triple_interaction_table) == 0) {
    triple_interaction_table <- tukey_results
  }else{
    triple_interaction_table <- rbind(triple_interaction_table, tukey_results)
  }
  
}

write.csv(triple_interaction_table, "./Outputs/triple_interaction_table.csv", row.names = FALSE)

## Inoculation:DAS interaction ----

model <- lmer(Chlorophyll.A~ Treatment*Inoculation*DAS + (1|Block), data = dataUsed)
anova_results <- Anova(model, type = "III")
emmeans_result <- emmeans(model, ~ Inoculation*DAS)
tukey_results <- cld(emmeans_result, Letters = letters, adjust = "tukey")
tukey_results$`.group` <- rev(tukey_results$`.group`)
tukey_results$`.group`<-gsub("\\s", "", tukey_results$`.group`)

ID_interaction_graph <- ggplot(data = tukey_results, aes(x = DAS, y = emmean, fill = Inoculation))+
  geom_bar(stat="identity", position = "dodge")+
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), position=position_dodge(width = 1), width = 0.7)+
  geom_text(aes(label = .group), vjust = -2, size = 5, position=position_dodge(width = 0.9))+
  theme_minimal()+
  ylab('Chlorophyll A')

## DAS ----

model <- lmer(ExG~ Treatment*Inoculation*DAS + (1|Block), data = dataUsed)
anova_results <- Anova(model, type = "III")
emmeans_result <- emmeans(model, ~ DAS)
tukey_results <- cld(emmeans_result, Letters = letters, adjust = "tukey")
tukey_results$`.group` <- rev(tukey_results$`.group`)
tukey_results$`.group`<-gsub("\\s", "", tukey_results$`.group`)

ExG_DAS <- ggplot(data = tukey_results, aes(x = DAS, y=emmean, group = 1))+
  geom_line(linewidth=1, color="blue")+
  geom_text(aes(label = .group), vjust = -.7, size = 4, position=dodger)+
  theme_minimal()+
  ylab('ExG')+
  coord_cartesian(clip = 'off')

#---

model <- lmer(NGRDI ~ Treatment*Inoculation*DAS + (1|Block), data = dataUsed)
anova_results <- Anova(model, type = "III")
emmeans_result <- emmeans(model, ~ DAS)
tukey_results <- cld(emmeans_result, Letters = letters, adjust = "tukey")
tukey_results$`.group` <- rev(tukey_results$`.group`)
tukey_results$`.group`<-gsub("\\s", "", tukey_results$`.group`)

NGRDI_DAS <- ggplot(data = tukey_results, aes(x = DAS, y=emmean, group = 1))+
  geom_line(linewidth=1, color="blue")+
  geom_text(aes(label = .group), vjust = -.7, size = 4, position=dodger)+
  theme_minimal()+
  ylab('NGRDI')+
  coord_cartesian(clip = 'off')

#---

model <- lmer(RGBVI ~ Treatment*Inoculation*DAS + (1|Block), data = dataUsed)
anova_results <- Anova(model, type = "III")
emmeans_result <- emmeans(model, ~ DAS)
tukey_results <- cld(emmeans_result, Letters = letters, adjust = "tukey")
tukey_results$`.group` <- rev(tukey_results$`.group`)
tukey_results$`.group`<-gsub("\\s", "", tukey_results$`.group`)

RGBVI_DAS <- ggplot(data = tukey_results, aes(x = DAS, y=emmean, group = 1))+
  geom_line(linewidth=1, color="blue")+
  geom_text(aes(label = .group), vjust = -.7, size = 4, position=dodger)+
  theme_minimal()+
  ylab('RGBVI')+
  coord_cartesian(clip = 'off')

#---

model <- lmer(MGRVI ~ Treatment*Inoculation*DAS + (1|Block), data = dataUsed)
anova_results <- Anova(model, type = "III")
emmeans_result <- emmeans(model, ~ DAS)
tukey_results <- cld(emmeans_result, Letters = letters, adjust = "tukey")
tukey_results$`.group` <- rev(tukey_results$`.group`)
tukey_results$`.group`<-gsub("\\s", "", tukey_results$`.group`)

MGRVI_DAS <- ggplot(data = tukey_results, aes(x = DAS, y=emmean, group = 1))+
  geom_line(linewidth=1, color="blue")+
  geom_text(aes(label = .group), vjust = -.7, size = 4, position=dodger)+
  theme_minimal()+
  ylab('MGRVI')+
  coord_cartesian(clip = 'off')

#---

model <- lmer(Total.Chlorophyll ~ Treatment*Inoculation*DAS + (1|Block), data = dataUsed)
anova_results <- Anova(model, type = "III")
emmeans_result <- emmeans(model, ~ DAS)
tukey_results <- cld(emmeans_result, Letters = letters, adjust = "tukey")
tukey_results$`.group` <- rev(tukey_results$`.group`)
tukey_results$`.group`<-gsub("\\s", "", tukey_results$`.group`)

Total.Chlorophyll_DAS <- ggplot(data = tukey_results, aes(x = DAS, y=emmean, group = 1))+
  geom_line(linewidth=1, color="blue")+
  geom_text(aes(label = .group), vjust = -0.7, size = 4, position=dodger)+
  theme_minimal()+
  ylab('Total Chlorophyll')+
  coord_cartesian(clip = 'off')

#---

model <- lmer(NDVI ~ Treatment*Inoculation*DAS + (1|Block), data = dataUsed)
anova_results <- Anova(model, type = "III")
emmeans_result <- emmeans(model, ~ DAS)
tukey_results <- cld(emmeans_result, Letters = letters, adjust = "tukey")
tukey_results$`.group` <- rev(tukey_results$`.group`)
tukey_results$`.group`<-gsub("\\s", "", tukey_results$`.group`)

NDVI_DAS <- ggplot(data = tukey_results, aes(x = DAS, y=emmean, group = 1))+
  geom_line(linewidth=1, color="blue")+
  geom_text(aes(label = .group), vjust = -0.7, size = 4, position=dodger)+
  theme_minimal()+
  ylab('NDVI')+
  coord_cartesian(clip = 'off')

#---

model <- lmer(Chlorophyll.B ~ Treatment*Inoculation*DAS + (1|Block), data = dataUsed)
anova_results <- Anova(model, type = "III")
emmeans_result <- emmeans(model, ~ DAS)
tukey_results <- cld(emmeans_result, Letters = letters, adjust = "tukey")
tukey_results$`.group` <- rev(tukey_results$`.group`)
tukey_results$`.group`<-gsub("\\s", "", tukey_results$`.group`)

ChloB_DAS <- ggplot(data = tukey_results, aes(x = DAS, y=emmean, group = 1))+
  geom_line(linewidth=1, color="blue")+
  geom_text(aes(label = .group), vjust = -0.7, size = 4, position=dodger)+
  theme_minimal()+
  ylab('Chlorophyll B')+
  coord_cartesian(clip = 'off')

#---

ggarrange(ExG_DAS, MGRVI_DAS, NGRDI_DAS, RGBVI_DAS, ChloB_DAS, Total.Chlorophyll_DAS, NDVI_DAS,
          labels = c("A", "B", "C", "D", "E", "F", "G"),
          ncol = 2, nrow = 4)

## Inoculation ----
variables <- c("ExG", "NDVI")

inoculation_table <- data.frame()

for(variable in variables){
  
  formula <- as.formula(paste(variable, "~ Treatment*Inoculation*DAS + (1|Block)"))
  
  model <- lmer(formula, data = dataUsed)
  anova_results <- Anova(model, type = "III")
  
  emmeans_result <- emmeans(model, ~ Inoculation)
  tukey_results <- cld(emmeans_result, Letters = letters, adjust = "tukey")
  tukey_results$`.group` <- rev(tukey_results$`.group`)
  
  if (nrow(inoculation_table) == 0) {
    inoculation_table <- tukey_results
  }else{
    inoculation_table <- rbind(inoculation_table, tukey_results)
  }
  
}

write.csv(inoculation_table, "./Outputs/inoculation_table.csv", row.names = FALSE)

## Correlation ----

df_correlation <- data %>%
  dplyr::select(-c(Block, DAS, Inoculation, Line, Treatment))

M <-cor(df_correlation, use="complete.obs")

corrplot(M, method="number")
