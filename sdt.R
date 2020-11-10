####========================================= A.L.R.R.2019-2020
### DESCRIPTION
## This script calculates the number of hits, FAs, CRs, and...
## ...misses for SDT (d' and C) as well as the bias index


####==========================================================
### INSTALL PACKAGES
# install.packages("pacman")
# require(pacman)
# pacman::p_load(psycho, dplyr, tidyr, rstatix)


####==========================================================
### WORKING DIRECTORY
setwd('/work/dir/')


####==========================================================
### REQUIRED FILES

if(!exists('total_sdt')){
  total_sdt <- read.csv(
    './data/response_counts.csv')
}

## Adjust levels of is_SCD
total_sdt$is_SCD <- factor(total_sdt$is_SCD,
                           levels = c("SCD","CON"))

## Create copy of total sdt (to make it easier to name)
total2 <- total_sdt


####==========================================================
### CALCULATE D-PRIME INDICES
# dprime(psycho package)

# Adjust data frame to have separate indices...
# ...according to Load (as I had it separately per...
# ...confidence level):
total2$nr_hit_hi <-
  total2$nrhits_hc_hi + total2$nrhits_lc_hi
total2$nr_hit_lo <-
  total2$nrhits_hc_lo + total2$nrhits_lc_lo
total2$nr_FA_hi <-
  total2$nrFA_hc_hi + total2$nrFA_lc_hi
total2$nr_FA_lo <-
  total2$nrFA_hc_lo + total2$nrFA_lc_lo
total2$nr_miss_hi <-
  total2$nrmisses_hc_hi + total2$nrmisses_lc_hi
total2$nr_miss_lo <-
  total2$nrmisses_hc_lo + total2$nrmisses_lc_lo
total2$nr_CR_hi <-
  total2$nrCR_hc_hi + total2$nrCR_lc_hi
total2$nr_CR_lo <-
  total2$nrCR_hc_lo + total2$nrCR_lc_lo

# Obtain SDT indices:
  # High
indices_hi <- psycho::dprime(
  n_hit = total2$nr_hit_hi,
  n_fa = total2$nr_FA_hi,
  n_miss = total2$nr_miss_hi,
  n_cr = total2$nr_CR_hi)

  # Low
indices_lo <- psycho::dprime(
  n_hit = total2$nr_hit_lo,
  n_fa = total2$nr_FA_lo,
  n_miss = total2$nr_miss_lo,
  n_cr = total2$nr_CR_lo)

# Add SDT indices to the data frame
total2$dprime_hi <- round(indices_hi$dprime, 2)
total2$dprime_lo <- round(indices_lo$dprime, 2)
total2$C_hi <- round(indices_hi$c, 2)
total2$C_lo <- round(indices_lo$c, 2)


####==========================================================
### BIAS INDEX CALCULATION
## Bias across load levels (total required: total_div)
## B = FA / [1 - (H - FA)]

# Access data
if(!exists('total_div')){
  total <- read.csv(
    './data/total_div.csv')
}

# Calculate Bias for:
# High-Load
total$bias_hi <- round(total$FA_rate_hi_all /
  (1 - (total$hit_rate_hi_all - total$FA_rate_hi_all)), 2)

# Low-Load
total$bias_lo <- round(total$FA_rate_lo_all /
  (1 - (total$hit_rate_lo_all - total$FA_rate_lo_all)), 2)

# Preparation for ANOVA
  # Data extension
bias <- total %>% pivot_longer(cols = c(
  "bias_hi", "bias_lo"),
  names_to = "bias_level",
  values_to = "value_bias")
bias$subject <- factor(bias$subject,
                         levels=unique(
                           bias$subject))
bias <- as.data.frame(bias)
bias$bias_level <- substr(bias$bias_level, 6, 8)
bias$bias_level <- factor(bias$bias_level,
                             levels=unique(
                               bias$bias_level))

  # ANOVA bias
res.aov.bias_level <- anova_test(data = bias,
                              dv = value_bias,
                              wid = subject,
                              between = is_SCD,
                              within = bias_level,
                              effect.size = "pes")
get_anova_table(res.aov.bias_level)

  # Between groups
bias %>%
  group_by(bias_level) %>%
  anova_test(dv = value_bias, wid = subject,
             between = is_SCD,
             effect.size = "pes") %>%
  get_anova_table() %>% adjust_pvalue(method = "holm")

  # Within groups
bias %>%
  group_by(is_SCD) %>%
  anova_test(dv = value_bias, wid = subject,
             within = bias_level,
             effect.size = "pes") %>%
  get_anova_table() %>% adjust_pvalue(method = "holm")
