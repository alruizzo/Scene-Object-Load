####========================================= A.L.R.R.2019-2020
### DESCRIPTION
## This script is used for analyzing the effect of "scene
## ...complexity" or "scene object load" on behavioral
## ...performance during retrieval


####==========================================================
### REQUIRED PACKAGES
## Install possibly relevant packages

# install.packages("pacman")
# require(pacman)
# pacman::p_load(ggplot2, dplyr, devtools, ggthemes, ggvis,
# plotly, rio, rmarkdown, shiny, stringr,
# tidyr, readxl, ggpubr, pastecs, psych,
# car, tidyverse, rstatix, BayesFactor,
# sjstats, ggsci, scales)


####==========================================================
### WORKING DIRECTORY
setwd('/work/dir/')


####==========================================================
### FILES TO WORK WITH

  # Create total
if (exists('total_div')) {
  total <- total_div
} else {
  total <- read.csv("./data/total_div.csv")
}
  
  # Make sure SCD is factor
total$is_SCD <- factor(total$is_SCD, levels = c("SCD","CON"))


####==========================================================
### DATA EXTENSION
## Prepare data frames for statistical analyses

# Extend data frames
# Hit Rate All
HR_all <- total %>% pivot_longer(cols = c(
  "hit_rate_hi_all", "hit_rate_lo_all"),
  names_to = "HR_all_complex",
  values_to = "value_HR_all_complex")
HR_all$subject <- factor(HR_all$subject,
                         levels=unique(HR_all$subject))
HR_all$HR_all_complex <- factor(HR_all$HR_all_complex,
                                levels=unique(
                                  HR_all$HR_all_complex))
HR_all <- as.data.frame(HR_all)

# FA Rate All
FA_all <- total %>% pivot_longer(cols = c(
  "FA_rate_hi_all", "FA_rate_lo_all"),
  names_to = "FA_all_complex",
  values_to = "value_FA_all_complex")
FA_all$subject <- factor(FA_all$subject,
                         levels=unique(FA_all$subject))
FA_all$FA_all_complex <- factor(FA_all$FA_all_complex,
                                levels=unique(
                                  FA_all$FA_all_complex))
FA_all <- as.data.frame(FA_all)

# Recognition Accuracy All
Rec_acc_all <- total %>% pivot_longer(cols = c(
  "rec_accuracy_hi_all", "rec_accuracy_lo_all"),
  names_to = "rec_acc_all_complex",
  values_to = "value_rec_acc_all_complex")
Rec_acc_all$subject <- factor(Rec_acc_all$subject,
                              levels=unique(
                                Rec_acc_all$subject))
Rec_acc_all$rec_acc_all_complex <-
  factor(Rec_acc_all$rec_acc_all_complex,
         levels=unique(
           Rec_acc_all$rec_acc_all_complex))
Rec_acc_all <- as.data.frame(Rec_acc_all)


####==========================================================
### DATA INSPECTION

# Identify outliers
HR_all %>% group_by(HR_all_complex, is_SCD) %>%
  identify_outliers(value_HR_all_complex)
FA_all %>% group_by(FA_all_complex, is_SCD) %>%
  identify_outliers(value_FA_all_complex)
Rec_acc_all %>% group_by(rec_acc_all_complex, is_SCD) %>%
  identify_outliers(value_rec_acc_all_complex)

# Test for normality
HR_all %>% group_by(HR_all_complex, is_SCD) %>%
  shapiro_test(value_HR_all_complex)
FA_all %>% group_by(FA_all_complex, is_SCD) %>%
  shapiro_test(value_FA_all_complex)
Rec_acc_all %>% group_by(rec_acc_all_complex, is_SCD) %>%
  shapiro_test(value_rec_acc_all_complex)

# Homogeneity of variance
HR_all %>% group_by(HR_all_complex) %>%
  levene_test(value_HR_all_complex ~ is_SCD)
FA_all %>% group_by(FA_all_complex) %>%
  levene_test(value_FA_all_complex ~ is_SCD)
Rec_acc_all %>% group_by(rec_acc_all_complex) %>%
  levene_test(value_rec_acc_all_complex ~ is_SCD)

# Homogeneity of covariances
box_m(HR_all[, "value_HR_all_complex", drop = F],
      HR_all$is_SCD)
box_m(FA_all[, "value_FA_all_complex", drop = F],
      FA_all$is_SCD)
box_m(Rec_acc_all[, "value_rec_acc_all_complex",
                  drop = F],
      Rec_acc_all$is_SCD)


####==========================================================
### Mixed ANOVA Hit Rate
  ## 2x2 mixed: IV between: is_SCD; IV within: complexity;
  ## ...DV: recognition accuracy/hit rate/FA rate
  ## Mixed ANOVA: y ~ b1*b2*w1 + Error(id/w1) -> [?anova_test]

res.aov.HR_all <- anova_test(data = HR_all,
                             dv = value_HR_all_complex,
                             wid = subject,
                             between = is_SCD,
                             within = HR_all_complex,
                             effect.size = "pes")
get_anova_table(res.aov.HR_all, correction = "auto")


####==========================================================
### Mixed ANOVA FA Rate

res.aov.FA_all <- anova_test(data = FA_all,
                             dv = value_FA_all_complex,
                             wid = subject,
                             between = is_SCD,
                             within = FA_all_complex,
                             detailed = F,
                             effect.size = "pes")
get_anova_table(res.aov.FA_all)

# Obtain means across load level
describeBy(FA_all$value_FA_all_complex,
           FA_all$FA_all_complex)


####==========================================================
### Mixed ANOVA Recognition Accuracy

res.aov.rec_all <- anova_test(data = Rec_acc_all,
                              dv = value_rec_acc_all_complex,
                              wid = subject,
                              between = is_SCD,
                              within = rec_acc_all_complex,
                              effect.size = "pes")
get_anova_table(res.aov.rec_all)

# Obtain means across load levels
describeBy(Rec_acc_all$value_rec_acc_all_complex,
           Rec_acc_all$rec_acc_all_complex)


####==========================================================
### One-way ANOVA to study significant interaction effects

## Within each WS group, comparing between-subject variable
bs <- HR_all %>%
  group_by(HR_all_complex) %>%
  anova_test(dv = value_HR_all_complex,
             wid = subject,
             between = is_SCD) %>%
  get_anova_table() %>% adjust_pvalue(method = "bonferroni")
    # Save P value for plotting later on
sig_pval_bs_HR_all <- bs$p[which(
  bs$HR_all_complex=='hit_rate_hi_all')]
pvalhi_hr_all <- paste("~italic(P) ==",
                       sig_pval_bs_HR_all)
sig_pval_bs_HR_all <- bs$p[which(
  bs$HR_all_complex=='hit_rate_lo_all')]
pvallo_hr_all <- paste("~italic(P) ==",
                       sig_pval_bs_HR_all)

## Within each BS group, comparing within-subject variable
ws <- FA_all %>%
  group_by(is_SCD) %>%
  anova_test(dv = value_FA_all_complex, wid = subject,
             within = FA_all_complex,
             effect.size = "pes") %>%
  get_anova_table() %>% adjust_pvalue(method = "bonferroni")

# Save P-value for plotting later on
sig_pval_ws_FA_all <- ws$p[which(ws$is_SCD=='CON')]
pvalcon_fa_all <- paste("~italic(P) ==",
                        sig_pval_ws_FA_all)
sig_pval_ws_FA_all <- ws$p[which(ws$is_SCD=='SCD')]
pvalscd_fa_all <- paste("~italic(P) ==",
                        sig_pval_ws_FA_all)


####==========================================================
### PLOTTING

# Hit rate
bxp_HR_all <- ggboxplot(HR_all, x = "HR_all_complex",
                        y = "value_HR_all_complex",
                        fill = "is_SCD", palette = "npg",
                        #ylab = "Hit Rate",
                        xlab = "Scene object load",
                        notch = F,
                        add = "jitter",
                        panel.labs = list(
                          "is_SCD" = c("SCD", "CON")))
ggpar(bxp_HR_all, ylim = c(0, 1), font.x = 18, font.y = 18,
      font.tickslab = 14, legend.title = "Group",
      font.legend = 18,
      legend = "top", font.title = c(20,"bold")
      ) + scale_x_discrete(labels=c("High", "Low")
                                         ) + grids(axis = "y") +
  #geom_segment(aes(x = 1.8, xend = 2.2, y = 1, yend = 1),
               #size=0.5, show.legend=F, lineend = "square",
               #color = "black") + #"#4DBBD5FF" for blue
  #geom_segment(aes(x = 0.8, xend = 1.2, y = 1, yend = 1),
               #size=0.5, show.legend=F, lineend = "square",
               #color = "black") + #"#E64B35FF" for red
  #annotate(geom="text", x=c(1, 2), y = c(1.03, 1.03),
           #label = c(pvalhi_hr_all, pvallo_hr_all),
           #color="black", size = 5, parse = T) +
  ggtitle("Hit Rate") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y=element_blank())
ggsave('/work/dir/figures/bxp_HR_all_no_pval.png',
       dpi = 500, width = 20,
       height = 15, units = "cm")

# FA rate
bxp_FA_all <- ggboxplot(FA_all, x = "FA_all_complex",
                        y = "value_FA_all_complex",
                        fill = "is_SCD", palette = "npg",
                        #ylab = "False Alarm Rate",
                        xlab = "Scene object load",
                        notch = F,
                        add = "jitter",
                        panel.labs = list(
                          "is_SCD" = c("SCD", "CON")))
ggpar(bxp_FA_all, ylim = c(0, 1), font.x = 18,
      font.y = 18,
      font.tickslab = 14, legend.title = "Group",
      font.legend = 18,
      legend = "top", font.title = c(20,"bold")
      ) + scale_x_discrete(labels=c("High", "Low")
                                         ) + grids(axis = "y") +
  geom_segment(aes(x = 1.2, xend = 2.2, y = 0.83,
                   yend = 0.83),
               size=0.5, show.legend=F, lineend = "square",
               color = "black") + #"#4DBBD5FF" for blue
  geom_segment(aes(x = 0.8, xend = 1.8, y = 0.97,
                   yend = 0.97),
               size=0.5, show.legend=F, lineend = "square",
               color = "black") + #"#E64B35FF" for red
  annotate(geom="text", x=c(1.69, 1.3), y = c(0.86, 1),
           label = c(pvalcon_fa_all, pvalscd_fa_all),
           color="black", size = 5, parse = T) +
  ggtitle("False Alarm Rate") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y=element_blank())
ggsave("/work/dir/figures/bxp_FA_all.png",
       dpi = 500, width = 20,
       height = 15, units = "cm")

# Recognition Accuracy
bxp_rec_acc_all <- ggboxplot(Rec_acc_all,
                             x = "rec_acc_all_complex",
                             y = "value_rec_acc_all_complex",
                             fill = "is_SCD",
                             palette = "npg",
                             #ylab = "Recognition Accuracy",
                             xlab = "Scene object load",
                             notch = F,
                             add = "jitter",
                             panel.labs = list(
                               "is_SCD" = c("SCD", "CON"))) +
  geom_hline(yintercept=0, size=0.3)
pval_sig_RA_all_mixed <-
  as.character(res.aov.rec_all$p[which(
    res.aov.rec_all$Effect=='rec_acc_all_complex')])
pvalraallcomplex <- paste("~italic(P) ==",
                          pval_sig_RA_all_mixed)
ggpar(bxp_rec_acc_all, ylim = c(-0.25, 0.75),
      font.x = 18, font.y = 18,
      font.tickslab = 14, legend.title = "Group",
      font.legend = 18,
      legend = "top", font.title = c(20,"bold")
      ) + scale_x_discrete(labels=c("High", "Low")
                                         ) + grids(axis = "y") +
geom_segment(aes(x = 1, xend = 2, y = 0.66, yend = 0.66),
             size=0.4, show.legend=F, lineend = "square",
             color = "black") +
  annotate(geom="text", x=1.5, y=0.69,
           label=pvalraallcomplex,
           color="black", size = 5, parse = T) +
  ggtitle("Recognition Accuracy") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y=element_blank())
ggsave("/work/dir/figures/bxp_rec_acc_all.png",
       dpi = 500, width = 20,
       height = 15, units = "cm")
