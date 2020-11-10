####========================================= A.L.R.R.2019-2020
### DESCRIPTION
## This script deals with the correlations between ROI
## ...extracted BOLD signal (PEs and COPEs) and behavior.
## ...It also plots the results.


####==========================================================
### INSTALL PACKAGES
# install.packages("pacman")
# require(pacman)
# pacman::p_load(ggplot2, dplyr, devtools, ggplot2, ggthemes,
# ggvis, plotly, rio, rmarkdown, shiny,
# stringr, tidyr, readxl,ggpubr, pastecs,
# psych, car, tidyverse, rstatix, cocor, ppcor,
# RColorBrewer, multcomp)


####==========================================================
### WORKING DIRECTORY
setwd('/work/dir/')


####==========================================================
### REQUIRED FILES
## Create the necessary data frames

  # Take them from the workspace if present or from "data"
if(!exists('total_no_div')){
  total_no_div <- read.csv(
    './data/total_no_div.csv')
}
if(!exists('total_div')){
  total_div <- read.csv(
    './data/total_div.csv')
}

  # Merge the two data frames (divided by load or not
  #...divided to have all information in one data frame)
total <- cbind(total_no_div, total_div)
total <- total[,!duplicated(colnames(total))]

  # Make sure SCD is factor
total$is_SCD <- factor(total$is_SCD, levels = c("SCD","CON"))


####==========================================================
### DESCRIPTIVES OVERALL MEMORY PERFORMANCE - SCENES

## OVERALL HIT RATE
describeBy(total$hit_rate_all,total$is_SCD)
t.test(total$hit_rate_all~total$is_SCD)

## OVERALL FA RATE
describeBy(total$FA_rate_all,total$is_SCD)
t.test(total$FA_rate_all~total$is_SCD)

## OVERALL RECOGNITION ACCURACY
describeBy(total$rec_accuracy_all,total$is_SCD)
t.test(total$rec_accuracy_all~total$is_SCD)


####==========================================================
### ANALYSIS OF VARIANCE ROIs

  ## Mixed ANOVA:
  ## Comparing whether participants' responses differ for...
  ## ... *PPA* and *LOC* in response to high-load and...
  ## ...low-load scenes (within-subjects) and/or for SCD...
  ## ...(between-subjects)

# Extend data frame from wide to long
long <- total %>%pivot_longer(cols = c("pe1_place",
                                        "pe3_place",
                                        "pe1_obj",
                                        "pe3_obj"),
                               names_to = c("pe_complex",
                                            "pe_structure"),
                               names_pattern = "(.*)_(.*)",
                               values_to = "value_pe_complex")
long$subject <- factor(long$subject,
                       levels=unique(long$subject))
long$pe_complex <- factor(long$pe_complex,
                          levels=unique(
                            long$pe_complex))
long$pe_structure <- factor(long$pe_structure,
                            levels=unique(
                              long$pe_structure))

# Mixed ANOVA test
res.aov.pe <- anova_test(data = long,
                         dv = value_pe_complex,
                         wid = subject,
                         between = is_SCD,
                         within = c(pe_complex,
                                    pe_structure),
                         effect.size = "pes")
get_anova_table(res.aov.pe)

# One-way ANOVA PPA:
    # Data frame extension
pe <- total %>% pivot_longer(cols = c("pe1_place",
                                      "pe3_place"),
                             names_to = "pe_complex",
                             values_to = "value_pe_complex")
pe$subject <- factor(pe$subject,
                     levels=unique(pe$subject))
pe$pe_complex <- factor(pe$pe_complex,
                        levels=unique(pe$pe_complex))
pe %>% group_by(pe_complex, is_SCD) %>%
  identify_outliers(value_pe_complex)

    # ANOVA test PPA
res.aov.pe <- anova_test(data = pe,
                         dv = value_pe_complex,
                         wid = subject,
                         between = is_SCD,
                         within = pe_complex,
                         effect.size = "pes")
get_anova_table(res.aov.pe)
pval_sig_PPA <-
  as.character(res.aov.pe$p[which(
    res.aov.pe$Effect=='pe_complex')])

# One-way ANOVA LOC:
    # Data frame extension
pel <- total %>% pivot_longer(cols = c("pe1_obj",
                                      "pe3_obj"),
                             names_to = "pe_complex",
                             values_to = "value_pe_complex")
pel$subject <- factor(pel$subject,
                     levels=unique(pel$subject))
pel$pe_complex <- factor(pel$pe_complex,
                        levels=unique(pel$pe_complex))
pel %>% group_by(pe_complex, is_SCD) %>%
  identify_outliers(value_pe_complex)

    # ANOVA test LOC
res.aov.pe <- anova_test(data = pel,
                         dv = value_pe_complex,
                         wid = subject,
                         between = is_SCD,
                         within = pe_complex)
get_anova_table(res.aov.pe)
pval_sig_LOC <-
  as.character(res.aov.pe$p[which(
    res.aov.pe$Effect=='pe_complex')])


####==========================================================
### General Linear Models

## HR
glmhr <- glm(
  total_no_div$hit_rate_all ~
    total_no_div$is_SCD +
    total_no_div$cope1_place +
    total_no_div$cope1_obj +
    total_no_div$is_SCD:total_no_div$cope1_place +
    total_no_div$is_SCD:total_no_div$cope1_obj)
summary.glm(glmhr)

## FA
glmfa <- glm(
  total_no_div$FA_rate_all ~
    total_no_div$is_SCD +
    total_no_div$cope1_place +
    total_no_div$cope1_obj +
    total_no_div$is_SCD:total_no_div$cope1_place +
    total_no_div$is_SCD:total_no_div$cope1_obj)
summary.glm(glmfa)

## REC ACC
glmrecacc <- glm(total_no_div$rec_accuracy_all ~
    total_no_div$is_SCD +
    total_no_div$cope1_place +
    total_no_div$cope1_obj +
    total_no_div$is_SCD:total_no_div$cope1_place +
    total_no_div$is_SCD:total_no_div$cope1_obj)
summary.glm(glmrecacc)

## Hit rate vs FA rate
glmrecacchrfa <- glm(total_no_div$cope1_place ~
                       total_no_div$hit_rate_all +
                       total_no_div$FA_rate_all +
                       total_no_div$is_SCD +
                       total_no_div$hit_rate_all*total_no_div$is_SCD +
                       total_no_div$FA_rate_all*total_no_div$is_SCD)
summary.glm(glmrecacchrfa)

## HR vs FR in glmrecacchrfa
  # Create contrast vector in a row
c <- matrix(c(0, -1, 1, 0, 0, 0), 1)

  # Test the linear hypotheses
contrast_hr_fa <- glht(glmrecacchrfa, linfct = c)
summary(contrast_hr_fa)

## PPA vs LOC in glmrecacc
  # Create contrast vector in a row
c <- matrix(c(0, 0, 1, -1, 0, 0), 1)

  # Test the linear hypotheses
contrast_ppa_loc <- glht(glmrecacc, linfct = c)
summary(contrast_ppa_loc)


####==========================================================
### PLOTTING
## ROIs comparison (effect of object load)

# PPA
bxp_pe <- ggboxplot(pe, x = "pe_complex",
                    y = "value_pe_complex",
                    fill = "is_SCD",
                    palette = "npg",
                    ylab = "% BOLD \u2206",
                    xlab = "Scene object load",
                    notch = F,
                    add = "jitter", #posing problems in newer R
                    add.params = list(#fill = "is_SCD",
                                      color = "black",
                                      alpha = 1),
                    panel.labs = list("is_SCD" = c(
                      "SCD", "CON")))
#geom_boxplot(outlier.shape = NA)
#geom_point(position = "jitter")
pvalPPA <- paste("~italic(P) ==", pval_sig_PPA)
ggpar(bxp_pe, ylim = c(0, 3), font.x = 18,
      font.y = 18, font.tickslab = 16,
      legend.title = "Group", font.legend = 18,
      legend = "top", font.title = c(20,"bold")) +
  scale_x_discrete(labels=c("High", "Low")) +
  grids(axis = "y") +
  geom_segment(aes(x = 1, xend = 2, y = 2.6,
                   yend = 2.6),
               size=0.4, show.legend=F,
               lineend = "square",
               color = "black") +
  annotate(geom="text", x=1.5, y=2.69, label=pvalPPA,
           color="black", size = 7, parse = T) +
  ggtitle("PPA") + theme(plot.title = element_text(hjust = 0.5))
ggsave("/work/dir/figures/bxp_PPA.png",
       dpi = 500, width = 20,
       height = 15, units = "cm")
rm(pvalPPA, pval_sig_PPA, bxp_pe)

# LOC
bxp_pel <- ggboxplot(pel, x = "pe_complex",
                    y = "value_pe_complex",
                    fill = "is_SCD",
                    palette = "npg",
                    ylab = "% BOLD \u2206",
                    xlab = "Scene object load",
                    notch = F,
                    add = "jitter",
                    panel.labs = list("is_SCD" = c("SCD", "CON")))
pvalLOC <- paste("~italic(P) ==", pval_sig_LOC)
ggpar(bxp_pel, ylim = c(0, 3), font.x = 18,
      font.y = 18, font.tickslab = 16,
      legend.title = "Group", font.legend = 18,
      legend = "top", font.title = c(20,"bold")) +
  scale_x_discrete(labels=c("High", "Low")) +
  grids(axis = "y") +
  geom_segment(aes(x = 1, xend = 2, y = 2.6, yend = 2.6),
               size=0.4, show.legend=F,
               lineend = "square",
               color = "black") +
  annotate(geom="text", x=1.5, y=2.69, label=pvalLOC,
           color="black", size = 7, parse = T) +
  ggtitle("LOC") +
  theme(plot.title = element_text(hjust = 0.5))
ggsave("/work/dir/figures/bxp_LOC.png",
       dpi = 500, width = 20,
       height = 15, units = "cm")
rm(list = ls(pattern = '^pe'))
rm(list = ls(pattern = '^pval'))

## ROIs correlation with memory performance
# PPA
p <- ggscatter(total, x = "cope1_place",
               y = "rec_accuracy_all",
               shape = "is_SCD",
               color = "is_SCD",
               size = 4,
               palette = "npg",
               rug = T,
               add = "reg.line",
               add.params = list(color = "darkgray",
                                 fill = "gray90"),
               conf.int = T,
               xlab = "High-load vs Low-load (PPA)",
               ylab = "Recognition Accuracy") +
  stat_cor(label.x = -0.25, label.y = 0.7, size = 5)
ggpar(p, xlim = c(-0.25,0.75), ylim= c(-0.25,0.75),
      legend.title = "Group",
      font.x = 18, font.y = 18, font.tickslab = 16,
      font.legend = 18)
ggsave("/work/dir/figures/corPPArec_acc.png",
       dpi = 500, width = 20,
       height = 15, units = "cm")

# LOC
q <- ggscatter(total, x = "cope1_obj",
               y = "rec_accuracy_all",
               shape = "is_SCD",
               color = "is_SCD",
               size = 4,
               palette = "npg",
               rug = T,
               add = "reg.line",
               add.params = list(color = "darkgray",
                                 fill = "gray90"),
               conf.int = T,
               xlab = "High-load vs Low-load (LOC)",
               ylab = "Recognition Accuracy") +
  stat_cor(label.x = -0.5, label.y = 0.7, size = 5)
ggpar(q, xlim = c(-0.5,1), ylim= c(-0.25,0.75),
      legend.title = "Group",
      font.x = 18, font.y = 18, font.tickslab = 16,
      font.legend = 18)
ggsave("/work/dir/figures/corLOCrec_acc.png",
       dpi = 500, width = 20,
       height = 15, units = "cm")
