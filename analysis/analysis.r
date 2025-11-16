# ==========================================
# BK_XSTK_Group07_ConcreteStrength_Analysis_Upgraded.R
# FULL STATISTICS + ANOVA + MULTIPLE REGRESSION
# (NO LEAKAGE, WITH VIF + MODEL DIAGNOSTICS + STEPWISE AIC)
# ==========================================

# 0. --- Xóa môi trường ---
rm(list=ls())
cat("\014")

# 1. --- Packages cần dùng ---
packages <- c("dplyr", "ggplot2", "readr", "patchwork", "car", "MASS", "gridExtra")
for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos="https://cloud.r-project.org")
  library(p, character.only = TRUE)
}
options(bitmapType = "cairo")

# 2. --- Thư mục output ---
out_dir <- "D:/HCMUT/Probability and Stastistics/Assignment/Output"
tables_dir <- file.path(out_dir, "tables")
plots_dir  <- file.path(out_dir, "plots")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

# 3. --- Đọc dữ liệu ---
data_cleaned <- read_csv(file.path(out_dir,"data_cleaned.csv"))
data_scaled  <- read_csv(file.path(out_dir,"data_scaled.csv"))

# 4. --- Thống kê mô tả (Descriptive Statistics) ---
numeric_vars <- names(data_cleaned)[sapply(data_cleaned, is.numeric)]

desc_stats <- data_cleaned %>%
  summarise(across(all_of(numeric_vars),
                   list(mean=mean, sd=sd, min=min, max=max, median=median)))

desc_stats_tidy <- desc_stats %>% t() %>% as.data.frame()
colnames(desc_stats_tidy) <- "value"
desc_stats_tidy$stat <- rownames(desc_stats_tidy)
desc_stats_tidy <- desc_stats_tidy %>% dplyr::select(stat, value)

cat("\n📊 Descriptive Statistics:\n")
print(desc_stats_tidy)
write.csv(desc_stats_tidy, file.path(tables_dir, "descriptive_stats.csv"), row.names=FALSE)

# 5. --- Histogram (visual exploration, không ảnh hưởng phân tích chính) ---
hist_strength <- ggplot(data_cleaned, aes(x=strength)) +
  geom_histogram(binwidth=2, fill="red2", color="black") +
  geom_vline(aes(xintercept=mean(strength)), color="darkred", linetype="dashed", linewidth=1) +
  geom_vline(aes(xintercept=median(strength)), color="firebrick4", linetype="dotted", linewidth=1) +
  labs(title="Distribution of Concrete Strength",
       x="Strength (MPa)", y="Frequency") +
  theme_minimal(base_size=14) +
  theme(plot.title = element_text(hjust=0.5, face="bold", size=16),
        axis.title = element_text(face="bold", size=14),
        axis.text = element_text(color="black", size=12),
        panel.grid.minor = element_blank())

hist_log_strength <- ggplot(data_cleaned, aes(x=log_strength)) +
  geom_histogram(binwidth=0.1, fill="steelblue", color="black") +
  geom_vline(aes(xintercept=mean(log_strength)), color="navy", linetype="dashed", linewidth=1) +
  geom_vline(aes(xintercept=median(log_strength)), color="darkblue", linetype="dotted", linewidth=1) +
  labs(title="Distribution of Log(Concrete Strength)",
       x="Log(Strength)", y="Frequency") +
  theme_minimal(base_size=14) +
  theme(plot.title = element_text(hjust=0.5, face="bold", size=16),
        axis.title = element_text(face="bold", size=14),
        axis.text = element_text(color="black", size=12),
        panel.grid.minor = element_blank())

# Lưu biểu đồ
ggsave(file.path(plots_dir,"hist_strength.png"), hist_strength, width=6, height=4)
ggsave(file.path(plots_dir,"hist_log_strength.png"), hist_log_strength, width=6, height=4)

# --- 6. Scatter plots (visual exploration) ---
numeric_vars_no_strength <- setdiff(names(data_cleaned)[sapply(data_cleaned, is.numeric)],
                                    c("strength", "log_strength"))
plots <- list()
for (var in numeric_vars_no_strength) {
  p <- ggplot(data_cleaned, aes_string(x=var, y="strength")) +
    geom_point(color="#1E3A8A", alpha=0.6, size=2.5) +
    geom_smooth(method="lm", se=TRUE, color="red", fill="red", alpha=0.2, linewidth=1.2) +
    labs(title=paste0(var, " vs Concrete Strength"),
         x=var, y="Strength (MPa)") +
    theme_minimal(base_size=14) +
    theme(plot.title = element_text(hjust=0.5, face="bold", size=14),
          axis.title = element_text(face="bold", size=12),
          axis.text = element_text(color="black", size=10),
          panel.grid.minor = element_blank())
  ggsave(file.path(plots_dir, paste0("scatter_", var, ".png")), p, width=6, height=4)
  plots[[var]] <- p
}
wrap_plot_all <- wrap_plots(plots, ncol=4) +
  plot_annotation(title = "Scatter Plots: Predictors vs Concrete Strength",
                  theme = theme(plot.title = element_text(hjust=0.5, face="bold", size=16)))
ggsave(file.path(plots_dir,"scatter_all_upgraded.png"), wrap_plot_all,
       width=16, height=14)

# 7. --- Grouping for ANOVA ---
make_group <- function(x, probs=seq(0,1,length.out=4), labels=NULL) {
  cut(x, breaks=quantile(x, probs=probs), labels=labels, include.lowest=TRUE)
}
data_cleaned <- data_cleaned %>%
  mutate(
    cement_group = make_group(cement, labels=c("Low","Med","High")),
    age_group    = cut(age, breaks=c(0,7,28,90,365),
                       labels=c("1-7","8-28","29-90","91-365"), include.lowest=TRUE)
  )

# 8. --- One-way ANOVA (nâng cấp: kiểm tra giả thiết) ---
aov1 <- aov(strength ~ cement_group, data=data_cleaned)

# --- 8a. Kết quả ANOVA ---
cat("\n--- ONE-WAY ANOVA ---\n")
anova_summary <- summary(aov1)
print(anova_summary)
write.csv(as.data.frame(anova_summary[[1]]),
          file.path(tables_dir,"anova_summary.csv"))

# --- 8b. Kiểm tra giả thiết --- 
# 1. Normality (Shapiro-Wilk)
resid_aov <- residuals(aov1)
shapiro_res <- shapiro.test(resid_aov)
cat("\nShapiro-Wilk test for residuals:\n")
print(shapiro_res)
write.csv(data.frame(statistic=shapiro_res$statistic, p.value=shapiro_res$p.value),
          file.path(tables_dir,"shapiro_residuals.csv"))

# 2. Homogeneity of variance (Levene's Test)
library(car)
levene_res <- leveneTest(strength ~ cement_group, data=data_cleaned)
cat("\nLevene's Test for homogeneity of variance:\n")
print(levene_res)
write.csv(as.data.frame(levene_res),
          file.path(tables_dir,"levene_test.csv"))

# --- 8c. Tukey post-hoc test ---
tukey_res <- as.data.frame(TukeyHSD(aov1)$cement_group)
write.csv(tukey_res, file.path(tables_dir,"tukey_cement.csv"))

# --- 8d. Boxplot có mean và annotation ---
box_cement_clean <- ggplot(data_cleaned, aes(x=cement_group, y=strength)) +
  geom_boxplot(fill="#87CEFA", color="black", alpha=0.7, width=0.6, outlier.shape=NA) +
  geom_jitter(width=0.15, alpha=0.5, color="#1E3A8A", size=1.5) +
  stat_summary(fun=mean, geom="point", shape=18, size=3, color="red") +
  labs(title="Concrete Strength by Cement Group",
       x="Cement Group", y="Strength (MPa)") +
  theme_minimal(base_size=14) +
  theme(plot.title = element_text(hjust=0.5, face="bold", size=16),
        axis.title = element_text(face="bold", size=14),
        axis.text = element_text(color="black", size=12),
        panel.grid.minor = element_blank())
ggsave(file.path(plots_dir,"boxplot_cement_strength.png"), box_cement_clean, width=6, height=5)


# 9. --- Two-way ANOVA (cốt lõi) ---
aov2 <- aov(strength ~ cement_group * age_group, data=data_cleaned)
write.csv(as.data.frame(summary(aov2)[[1]]),
          file.path(tables_dir,"two_way_anova.csv"))

interaction_plot <- ggplot(data_cleaned, aes(x=age_group, y=strength, color=cement_group, group=cement_group)) +
  stat_summary(fun=mean, geom="line", linewidth=1.2) +
  stat_summary(fun=mean, geom="point", size=3) +
  stat_summary(fun.data=mean_se, geom="errorbar", width=0.2, linewidth=0.8) +
  scale_color_manual(values=c("Low"="#1b9e77", "Med"="#d95f02", "High"="#7570b3")) +
  labs(title="Interaction Plot: Concrete Strength ~ Cement × Age",
       x="Age Group (days)", y="Mean Strength (MPa)", color="Cement Group") +
  theme_minimal(base_size=14) +
  theme(plot.title=element_text(hjust=0.5, face="bold", size=16),
        axis.title=element_text(face="bold", size=14),
        axis.text=element_text(size=12),
        legend.title=element_text(face="bold", size=13),
        legend.text=element_text(size=12),
        panel.grid.major=element_line(color="grey90"),
        panel.grid.minor=element_blank())

ggsave(file.path(plots_dir, "interaction_plot.png"), interaction_plot, width=6, height=5)


# 10. --- Multiple Linear Regression (cốt lõi) ---
predictors <- setdiff(names(data_scaled),
                      c("strength", "log_strength", "ratio_water_cement", "total_aggregates"))
predictors <- c(predictors, "cement_fraction", "water_fraction")

formula <- as.formula(paste("strength ~", paste(predictors, collapse=" + ")))
lm_model <- lm(formula, data=data_scaled)
cat("\n--- MULTIPLE LINEAR REGRESSION (NO LEAKAGE) ---\n")
print(summary(lm_model))
write.csv(as.data.frame(summary(lm_model)$coefficients),
          file.path(tables_dir,"lm_coefficients.csv"))

# VIF CHECK (multicollinearity)
vif_values <- car::vif(lm_model)
write.csv(as.data.frame(vif_values), file.path(tables_dir,"vif.csv"))

# STEPWISE AIC (variable selection)
step_model <- stepAIC(lm_model, direction="both", trace=FALSE)
print(summary(step_model))
write.csv(as.data.frame(summary(step_model)$coefficients),
          file.path(tables_dir,"lm_stepwise_coeff.csv"))

# MODEL DIAGNOSTICS (residual plots)
residuals_df <- data.frame(Fitted = fitted(lm_model), Residuals = resid(lm_model))
resid_plot_up <- ggplot(residuals_df, aes(x=Fitted, y=Residuals)) +
  geom_point(alpha=0.6, color="#1E3A8A", size=2.5) +
  geom_hline(yintercept=0, linetype="dashed", color="red", linewidth=1) +
  geom_smooth(method="loess", se=TRUE, color="#D55E00", alpha=0.3, linewidth=1) +
  labs(title="Residuals vs Fitted Values", x="Fitted Values", y="Residuals") +
  theme_minimal(base_size=14) +
  theme(plot.title=element_text(hjust=0.5, face="bold", size=16),
        axis.title=element_text(face="bold", size=14),
        axis.text=element_text(size=12, color="black"),
        panel.grid.minor = element_blank())

qq_df <- data.frame(Residuals = resid(lm_model))
qq_plot_up <- ggplot(qq_df, aes(sample=Residuals)) +
  stat_qq(size=2.5, alpha=0.6, color="#1E3A8A") +
  stat_qq_line(color="#D55E00", linewidth=1) +
  labs(title="Normal Q-Q Plot of Residuals", x="Theoretical Quantiles", y="Sample Quantiles") +
  theme_minimal(base_size=14) +
  theme(plot.title=element_text(hjust=0.5, face="bold", size=16),
        axis.title=element_text(face="bold", size=14),
        axis.text=element_text(size=12, color="black"),
        panel.grid.minor = element_blank())

# Lưu plot diagnostics
ggsave(file.path(plots_dir, "residuals_fitted.png"), resid_plot_up, width=6, height=5)
ggsave(file.path(plots_dir, "qq_norm.png"), qq_plot_up, width=6, height=5)
combined_plot_up <- grid.arrange(resid_plot_up, qq_plot_up, ncol=2)
ggsave(file.path(plots_dir, "diagnostic_plots_combined.png"), combined_plot_up, width=12, height=6)

cat("\n✅ DONE! FULL STATISTICS, ANOVA & MULTIPLE REGRESSION.\n")
