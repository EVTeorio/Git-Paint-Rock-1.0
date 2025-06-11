

# Required libraries
library(dplyr)
library(tidyr)
library(ggpubr)
library(rstatix)  # For repeated measures stats


# Prepare wide format data for Accuracy and F1_Macro
acc_wide <- summary_df %>%
  select(Sample, Model, Accuracy) %>%
  pivot_wider(names_from = Model, values_from = Accuracy)

f1_wide <- summary_df %>%
  select(Sample, Model, F1_Macro) %>%
  pivot_wider(names_from = Model, values_from = F1_Macro)

model_names <- names(acc_wide)[-1]
model_pairs <- combn(model_names, 2, simplify = FALSE)

check_normality <- function(wide_data, measure_name) {
  cat("\nChecking normality of pairwise differences for", measure_name, ":\n")
  normality_results <- sapply(model_pairs, function(pair) {
    diff_col <- wide_data[[pair[1]]] - wide_data[[pair[2]]]
    test <- shapiro.test(diff_col)
    cat(paste0("  ", pair[1], " - ", pair[2], ": p = ", round(test$p.value, 4), "\n"))
    return(test$p.value > 0.05)
  })
  all(normality_results)  # TRUE if all pairs are normal
}

run_analysis <- function(df, dv_name) {
  cat("\n### Analyzing", dv_name, "###\n")
  
  # Check normality of differences
  wide_df <- df %>%
    select(Sample, Model, all_of(dv_name)) %>%
    pivot_wider(names_from = Model, values_from = all_of(dv_name))
  
  is_normal <- check_normality(wide_df, dv_name)
  
  if (is_normal) {
    cat("All pairwise differences pass normality. Using repeated-measures ANOVA.\n")
    
    # Run RM-ANOVA with sphericity check
    aov_res <- df %>%
      anova_test(dv = !!sym(dv_name), wid = Sample, within = Model, detailed = TRUE)
    
    print(aov_res)
    
    # Check Mauchly's Test for sphericity
    mauchly_p <- aov_res$`Mauchly's Test for Sphericity`$p
    
    if (!is.na(mauchly_p) && mauchly_p < 0.05) {
      cat("Sphericity violated, using Greenhouse-Geisser correction.\n")
      # Print corrected ANOVA p-values
      print(aov_res$`Sphericity Corrections`)
    } else {
      cat("Sphericity assumed.\n")
    }
    
    # Post-hoc pairwise comparisons (paired t-tests with Holm correction)
    posthoc <- df %>%
      pairwise_t_test(
        formula = as.formula(paste(dv_name, "~ Model")),
        paired = TRUE,
        p.adjust.method = "holm"
      )
    print(posthoc)
    
  } else {
    cat("Not all pairwise differences are normal. Using Friedman test.\n")
    
    # Friedman test (non-parametric RM ANOVA alternative)
    friedman_res <- df %>%
      friedman_test(
        formula = as.formula(paste(dv_name, "~ Model | Sample"))
      )
    print(friedman_res)
    
    # Post-hoc Nemenyi test for pairwise differences
    posthoc <- df %>%
      wilcox_test(
        formula = as.formula(paste(dv_name, "~ Model")),
        paired = TRUE,
        p.adjust.method = "holm"
      )
    print(posthoc)
  }
}

# Run the analysis on Accuracy and F1_Macro
run_analysis(summary_df, "Accuracy")
run_analysis(summary_df, "F1_Macro")
