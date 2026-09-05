install.packages('ggplot2')
install.packages("ggsignif")
install.packages('ggpubr')
install.packages('emmeans')
library(ggsignif)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(emmeans)
##########################################macros################################################

macro_light_CB.df <- data.frame(endoplasm = c(13.10,	12.35,	13.77),
                                ectoplasm = c(43.25,	40.52,	51.21),
                                pores = c(40.55, 35.87, 29.02),
                                slime = c(3.10, 11.25, 6.10))

macro_vein_CB.df <- data.frame(endoplasm = c(9.73, 20.91, 11.80),
                               ectoplasm = c(31.76, 31.32, 45.40),
                               pores = c(44.19, 31.22, 30.67),
                               slime = c(11.35, 14.87, 9.89),
                               dark_dots = c(0.68, 0.79, 0.34),
                               bright_dots = c(0.14, 0.10, 0.01))


macro_ff_CB.df <- data.frame(endoplasm = c(0.72, 4.54, 12.27, 8.43),
                             ectoplasm = c(56.10, 49.37, 43.19, 37.65),
                             pores = c(39.99, 40.15, 39.71, 43.07),
                             slime = c(2.75, 4.85, 3.04, 7.53),
                             dark_dots = c(0.43, 2.61, 0.34, 0.70),
                             bright_dots = c(0.01, 0.01, 0.00, 0.02))

macro_vein_LU.df <- data.frame(endoplasm = c(22.75, 27.02, 26.58, 36.69, 32.09),
                               ectoplasm = c(28.46, 23.91, 29.81, 30.51, 24.53),
                               pores = c(36.33, 26.16, 33.62, 28.05, 32.51),
                               slime = c(9.70, 19.33, 8.25, 3.88, 9.73),
                               dark_dots = c(0.26, 2.80, 0.93, 0.24, 0.41),
                               bright_dots = c(0.59, 0.78, 0.86, 0.40, 0.72))

macro_ff_LU.df <- data.frame(endoplasm = c(0, 4.41, 4.19, 4.21),
                             ectoplasm = c(60.75, 48.87, 68.25, 53.01),
                             pores = c(31.34, 37.44, 19.74, 31.87),
                             slime = c(6.89, 8.73, 7.01, 7.84),
                             dark_dots = c(0.34, 0.09, 2.42, 0.25),
                             bright_dots = c(0.24, 0.09, 0.16, 0.33))



macro_light_CB_long <- data.frame(
  Dataset = "macro_light_CB",
  Variable = rep(c("endoplasm", "ectoplasm"), each = 3),
  Value = c(macro_light_CB.df$endoplasm, macro_light_CB.df$ectoplasm)
)

macro_vein_CB_long <- data.frame(
  Dataset = "macro_vein_CB", 
  Variable = rep(c("endoplasm", "ectoplasm"), each = 3), 
Value = c(macro_vein_CB.df$endoplasm, macro_vein_CB.df$ectoplasm)
)

macro_ff_CB_long <- data.frame(
  Dataset = "macro_ff_CB",
  Variable = rep(c("endoplasm", "ectoplasm"), each = 4),
  Value = c(macro_ff_CB.df$endoplasm, macro_ff_CB.df$ectoplasm)
)

macro_vein_LU_long <- data.frame(
  Dataset = "macro_vein_LU",
  Variable = rep(c("endoplasm", "ectoplasm"), each = 5),
  Value = c(macro_vein_LU.df$endoplasm, macro_vein_LU.df$ectoplasm)
)

macro_ff_LU_long <- data.frame(
  Dataset = "macro_ff_LU",
  Variable = rep(c("endoplasm", "ectoplasm"), each = 4),
  Value = c(macro_ff_LU.df$endoplasm, macro_ff_LU.df$ectoplasm)
)

data_all <- rbind(macro_light_CB_long, macro_vein_CB_long, macro_ff_CB_long, 
                  macro_vein_LU_long, macro_ff_LU_long)

summary_data <- data_all %>%
  group_by(Dataset, Variable) %>%
  summarise(
    mean_value = mean(Value),
    sd_value = sd(Value),
    .groups = "drop"
  )


ggplot(summary_data, aes(x = Variable, y = mean_value, fill = Dataset)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
                width = 0.2, position = position_dodge(width = 0.8)) +
  geom_jitter(
    data = data_all,
    aes(x = Variable, y = Value, color = Dataset),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
    alpha = 0.6, size = 2, color = "grey40"
  ) +
  theme_minimal() +
  labs(x = '', y = 'mean value in %')


##feeding fronts
data_ff <- rbind(macro_ff_CB_long, macro_ff_LU_long)

summary_ff <- data_ff %>%
  group_by(Dataset, Variable) %>%
  summarise(
    mean_value = mean(Value),
    sd_value = sd(Value), 
    .groups = "drop"
  )

#colour = c("#9FD5B1", "#A1B3D7")
ggplot(summary_ff, aes(x = Variable, y = mean_value, fill = Dataset)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.65), 
           width = 0.6, color = "grey40") +
  geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
                width = 0.2, position = position_dodge(width = 0.65)) +
  scale_fill_manual(values = c(macro_ff_CB = "#9FD5B1",
                               macro_ff_LU = "#A1B3D7"))+
  geom_jitter(
    data = data_ff,
    aes(x = Variable, y = Value, color = Dataset),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
    alpha = 0.6, size = 2, color = "grey40"
  ) +
  stat_compare_means(data = data_ff,
                     aes(x = Variable, y = Value),
                     method = "t.test",
                     label = "p.signif")+
  theme_classic() +
  labs(x = '', y = 'mean Volume in %')


##CB after light induction
data_light <- rbind(macro_vein_CB_long, macro_light_CB_long)

summary_light <- data_light %>%
  group_by(Dataset, Variable) %>%
  summarise(
    mean_value = mean(Value),
    sd_value = sd(Value), 
    .groups = "drop"
  )

#colour = c("#9FD5B1", "#A1B3D7")
ggplot(summary_light, aes(x = Variable, y = mean_value, fill = Dataset)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.65), 
           width = 0.6, color = "grey40") +
  geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
                width = 0.2, position = position_dodge(width = 0.65)) +
  scale_fill_manual(values = c(macro_vein_CB = "#9FD5B1",
                               macro_light_CB = "#A1B3D7"))+
  geom_jitter(
    data = data_light,
    aes(x = Variable, y = Value, color = Dataset),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
    alpha = 0.6, size = 2, color = "grey40"
  ) +
  stat_compare_means(data = data_light,
                     aes(x = Variable, y = Value),
                     method = "t.test",
                     label = "p.signif")+
  theme_classic() +
  labs(x = '', y = 'mean Volume in %')

##vein
data_vein <- rbind(macro_vein_CB_long, macro_vein_LU_long)

summary_vein <- data_vein %>%
  group_by(Dataset, Variable) %>%
  summarise(
    mean_value = mean(Value),
    sd_value = sd(Value), 
    .groups = "drop"
  )

#colour = c("#9FD5B1", "#A1B3D7")
ggplot(summary_vein, aes(x = Variable, y = mean_value, fill = Dataset)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.65), 
           width = 0.6, color = "grey40") +
  geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
                width = 0.2, position = position_dodge(width = 0.65)) +
  scale_fill_manual(values = c(macro_vein_CB = "#9FD5B1",
                               macro_vein_LU = "#A1B3D7"))+
  geom_jitter(
    data = data_vein,
    aes(x = Variable, y = Value, color = Dataset),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
    alpha = 0.6, size = 2, color = "grey40"
  ) +
  stat_compare_means(data = data_vein, 
                     aes(x = Variable, y = Value),
                     method = "t.test",
                     label = "p.signif",
                     )+
  theme_classic() +
  labs(x = '', y = 'mean Volume in %')




################### repeat analysis with two-way ANOVA test ####################
#combine all endoplasm sets 
endo_all <- rbind(macro_vein_CB_long[c(1:3),], macro_light_CB_long[c(1:3),],
                  macro_ff_CB_long[c(1:4),], macro_vein_LU_long[c(1:5),], 
                  macro_ff_LU_long[c(1:4),])

ecto_all <- rbind(macro_vein_CB_long[c(4:6),], macro_light_CB_long[c(1:3),],
                  macro_ff_CB_long[c(5:8),], macro_vein_LU_long[c(6:10),], 
                  macro_ff_LU_long[c(5:8),])

endo_ecto_ratio2 <- data.frame(group = endo_all[, 1],
                               ratio = endo_all[,3]/ecto_all[,3])

endo_ecto_final <- data.frame(strain = substr(as.character(endo_ecto_ratio2[,1]),
                                              as.character(nchar(endo_ecto_ratio2[,1])-1), 
                                              as.character(nchar(endo_ecto_ratio2[,1]))),
                              feature = substr(as.character(endo_ecto_ratio2[,1]),
                                               1, 
                                               as.character(nchar(endo_ecto_ratio2[,1])-3)),
                              endo_ecto_ratio = endo_all[,3]/ecto_all[,3])

endo_ecto_final$strain <- as.factor(endo_ecto_final$strain)
endo_ecto_final$feature <- as.factor(endo_ecto_final$feature)


#do anova, two-sided test
model_ratio <- aov(endo_ecto_ratio ~ strain * feature, data = endo_ecto_final)
summary(model_ratio)

# turky test
tukey_ratio <- emmeans(model_ratio, pairwise ~ strain*feature, adjust ="tukey")
tukey_ratio



#plotting
ggplot(endo_ecto_final, aes(x = feature, y = endo_ecto_ratio, fill = strain)) +
  stat_summary(fun = mean,
               geom = "bar",
               position = position_dodge(width = 0.65),
               width = 0.6,
               color = "grey")+
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               position = position_dodge(width = 0.65),
               width = 0.2)+
  geom_jitter(
    aes(color = strain),
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.65),
    alpha = 0.6, size = 2
  )+
  stat_compare_means(method = "anova", label = "p.signif")+
  theme_classic() +
  labs(x = '', y = 'V(Endoplasm)/ V(Ectoplasm)')




##anova one-way test
model_1way <- aov(ratio ~ group, data = endo_ecto_ratio2 )
summary(model_1way)

TukeyHSD(model_1way)
comparisons <- list(c("macro_vein_CB", "macro_vein_LU"),
                   # c("macro_vein_CB", "macro_ff_CB"),    insignificant
                   # c("macro_vein_CB", "macro_ff_LU"),    insignificant
                   # c("macro_ff_CB", "macro_ff_LU"),      insignificant
                    c("macro_ff_CB", "macro_vein_LU"),
                    c("macro_ff_LU", "macro_vein_LU"),
                   # c("macro_light_CB", "macro_vein_LU"), insignificant
                    c("macro_light_CB", "macro_vein_CB"),
                    c("macro_light_CB", "macro_ff_LU"),
                    c("macro_light_CB", "macro_ff_CB"))



ggplot(endo_ecto_ratio2, aes(x = group, y = ratio, fill = group)) +
  stat_summary(fun = mean,
               geom = "bar",
               position = position_dodge(width = 0.65),
               width = 0.6)+
  stat_summary(fun.data = mean_se,
               geom = "errorbar",
               position = position_dodge(width = 0.65),
               width = 0.2)+
  geom_jitter(
    position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.65),
    alpha = 0.6, size = 2
  )+
  scale_fill_manual(values = c(
    "macro_ff_CB" = "#9FD5B1", 
    "macro_ff_LU" = "#A1B3D7",
    "macro_light_CB" = "#9FD5B1", 
    "macro_vein_CB"= "#9FD5B1", 
    "macro_vein_LU"= "#A1B3D7"))+
  stat_compare_means(comparisons = comparisons, 
                     method = "t.test",
                     p.adjust.method = "tukey",
                     label = "p.signif")+
  theme_classic() +
  labs(x = '', y = 'V(Endoplasm)/ V(Ectoplasm)')
