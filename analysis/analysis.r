# ==========================================
# BK_XSTK_Group07_ConcreteStrength_Analysis_Upgraded.R
# FULL STATISTICS + ANOVA + MULTIPLE REGRESSION
# (SỬ DỤNG DUY NHẤT data_cleaned)
# ==========================================

# 0. --- Xóa môi trường ---
rm(list=ls())
cat("\014")

# 1. --- Packages cần dùng ---
packages <- c("dplyr","ggplot2","readr","patchwork","car","MASS","gridExtra")
for(p in packages){
  if(!requireNamespace(p, quietly=TRUE)) install.packages(p, repos="https://cloud.r-project.org")
  library(p, character.only = TRUE)
}
options(bitmapType="cairo")

# 2. --- Thư mục output ---
out_dir <- "D:/HCMUT/Probability and Stastistics/Assignment/Output"
tables_dir <- file.path(out_dir,"tables")
plots_dir  <- file.path(out_dir,"plots")
dir.create(tables_dir, recursive=TRUE, showWarnings=FALSE)
dir.create(plots_dir, recursive=TRUE, showWarnings=FALSE)

# 3. --- Đọc dữ liệu (tự chọn file CSV) ---
cat("📂 Chọn file dữ liệu CSV (data_cleaned.csv):\n")
data_file <- file.choose()  # Hộp thoại chọn file
data_cleaned <- read_csv(data_file)


# 4. --- Descriptive Statistics ---
numeric_vars <- names(data_cleaned)[sapply(data_cleaned,is.numeric)]
desc_stats <- data_cleaned %>%
  summarise(across(all_of(numeric_vars),
                   list(mean=mean, sd=sd, min=min, max=max, median=median)))
desc_stats_tidy <- desc_stats %>% t() %>% as.data.frame()
colnames(desc_stats_tidy) <- "value"
desc_stats_tidy$stat <- rownames(desc_stats_tidy)
desc_stats_tidy <- desc_stats_tidy %>% dplyr::select(stat, value)
write.csv(desc_stats_tidy, file.path(tables_dir,"descriptive_stats.csv"), row.names=FALSE)

# 4a. --- Ma trận tương quan numeric predictors + strength ---
library(corrplot)

# Chọn các biến numeric dùng cho hồi quy
numeric_corr_vars <- c("strength","cement","slag","ash","water",
                       "superplastic","coarseagg","fineagg","age",
                       "cement_fraction","water_fraction")
corr_mat <- cor(data_cleaned[, numeric_corr_vars])

# Lưu ma trận tương quan ra csv
write.csv(corr_mat, file.path(tables_dir,"correlation_matrix.csv"), row.names=TRUE)

# Vẽ heatmap tương quan
png(file.path(plots_dir,"correlation_heatmap.png"), width=800, height=800)
corrplot(corr_mat, method="color", addCoef.col="black", number.cex=0.7,
         tl.col="black", tl.srt=45, cl.cex=0.8, title="Correlation Matrix")
dev.off()


# 5. --- Histogram Strength & Log(Strength) ---
hist_strength <- ggplot(data_cleaned, aes(x=strength)) +
  geom_histogram(binwidth=2, fill="red2", color="black") +
  geom_vline(aes(xintercept=mean(strength)), color="darkred", linetype="dashed") +
  geom_vline(aes(xintercept=median(strength)), color="firebrick4", linetype="dotted") +
  labs(title="Distribution of Concrete Strength", x="Strength (MPa)", y="Frequency") +
  theme_minimal(base_size=14)
ggsave(file.path(plots_dir,"hist_strength.png"), hist_strength, width=6, height=4)

hist_log_strength <- ggplot(data_cleaned, aes(x=log_strength)) +
  geom_histogram(binwidth=0.1, fill="steelblue", color="black") +
  geom_vline(aes(xintercept=mean(log_strength)), color="navy", linetype="dashed") +
  geom_vline(aes(xintercept=median(log_strength)), color="darkblue", linetype="dotted") +
  labs(title="Distribution of Log(Concrete Strength)", x="Log(Strength)", y="Frequency") +
  theme_minimal(base_size=14)
ggsave(file.path(plots_dir,"hist_log_strength.png"), hist_log_strength, width=6, height=4)

# 6. --- Scatter plots predictor numeric vs strength ---
numeric_vars_no_strength <- setdiff(numeric_vars, c("strength","log_strength"))
plots <- list()
for(var in numeric_vars_no_strength){
  p <- ggplot(data_cleaned, aes_string(x=var, y="strength")) +
    geom_point(color="#1E3A8A", alpha=0.6, size=2.5) +
    geom_smooth(method="lm", se=TRUE, color="red", fill="red", alpha=0.2) +
    labs(title=paste0(var," vs Concrete Strength"), x=var, y="Strength (MPa)") +
    theme_minimal(base_size=14)
  ggsave(file.path(plots_dir,paste0("scatter_",var,".png")), p, width=6, height=4)
  plots[[var]] <- p
}
wrap_plot_all <- wrap_plots(plots, ncol=4) +
  plot_annotation(title="Scatter Plots: Predictors vs Concrete Strength")
ggsave(file.path(plots_dir,"scatter_all_upgraded.png"), wrap_plot_all, width=16, height=14)

# 7. --- Tạo nhóm cho ANOVA ---
make_group <- function(x, probs=seq(0,1,length.out=4), labels=NULL){
  cut(x, breaks=quantile(x, probs=probs), labels=labels, include.lowest=TRUE)
}
data_cleaned <- data_cleaned %>%
  mutate(
    cement_group = make_group(cement, labels=c("Low","Med","High")),
    age_group    = cut(age, breaks=c(0,7,28,90,365),
                       labels=c("1-7","8-28","29-90","91-365"), include.lowest=TRUE)
  )

# 8. --- One-way ANOVA: strength ~ cement_group ---
aov1 <- aov(strength ~ cement_group, data=data_cleaned)
write.csv(as.data.frame(summary(aov1)[[1]]), file.path(tables_dir,"anova_summary.csv"))

# Normality test
shapiro_res <- shapiro.test(residuals(aov1))
write.csv(data.frame(statistic=shapiro_res$statistic, p.value=shapiro_res$p.value),
          file.path(tables_dir,"shapiro_residuals.csv"))

# Homogeneity of variance (Levene)
levene_res <- car::leveneTest(strength ~ cement_group, data=data_cleaned)
write.csv(as.data.frame(levene_res), file.path(tables_dir,"levene_test.csv"))

# Tukey post-hoc
tukey_res <- as.data.frame(TukeyHSD(aov1)$cement_group)
write.csv(tukey_res, file.path(tables_dir,"tukey_cement.csv"))

# Boxplot cement_group vs strength
box_cement_clean <- ggplot(data_cleaned, aes(x=cement_group, y=strength)) +
  geom_boxplot(fill="#87CEFA", color="black", alpha=0.7, outlier.shape=NA) +
  geom_jitter(width=0.15, alpha=0.5, color="#1E3A8A", size=1.5) +
  stat_summary(fun=mean, geom="point", shape=18, size=3, color="red") +
  labs(title="Concrete Strength by Cement Group", x="Cement Group", y="Strength (MPa)") +
  theme_minimal(base_size=14)
ggsave(file.path(plots_dir,"boxplot_cement_strength.png"), box_cement_clean, width=6, height=5)

# 9. --- Two-way ANOVA: cement_group × age_group ---
aov2 <- aov(strength ~ cement_group * age_group, data=data_cleaned)
write.csv(as.data.frame(summary(aov2)[[1]]), file.path(tables_dir,"two_way_anova.csv"))

interaction_plot <- ggplot(data_cleaned, aes(x=age_group, y=strength, color=cement_group, group=cement_group)) +
  stat_summary(fun=mean, geom="line") +
  stat_summary(fun=mean, geom="point", size=3) +
  stat_summary(fun.data=mean_se, geom="errorbar", width=0.2) +
  scale_color_manual(values=c("Low"="#1b9e77","Med"="#d95f02","High"="#7570b3")) +
  labs(title="Interaction Plot: Concrete Strength ~ Cement × Age", x="Age Group (days)", y="Mean Strength (MPa)") +
  theme_minimal(base_size=14)
ggsave(file.path(plots_dir,"interaction_plot.png"), interaction_plot, width=6, height=5)

# 10. --- Multiple Linear Regression (Cập nhật: loại bỏ biến trùng lặp) ---
# Chỉ giữ các biến numeric cần thiết (loại bỏ cement_fraction, water_fraction)
predictors <- c("cement", "slag", "ash", "water", "superplastic",
                "coarseagg", "fineagg", "age")  

# Scale numeric predictors
data_cleaned[, predictors] <- scale(data_cleaned[, predictors])

# Fit model
formula <- as.formula(paste("strength ~", paste(predictors, collapse=" + ")))
lm_model <- lm(formula, data=data_cleaned)

# Lưu hệ số hồi quy
write.csv(as.data.frame(summary(lm_model)$coefficients), file.path(tables_dir,"lm_coefficients.csv"))

# VIF check (kiểm tra đa cộng tuyến còn sót)
vif_values <- car::vif(lm_model)
write.csv(as.data.frame(vif_values), file.path(tables_dir,"vif_updated.csv"))

# Stepwise AIC (nếu muốn tối ưu mô hình)
step_model <- stepAIC(lm_model, direction="both", trace=FALSE)
write.csv(as.data.frame(summary(step_model)$coefficients), file.path(tables_dir,"lm_stepwise_coeff.csv"))

# Diagnostics plots
residuals_df <- data.frame(Fitted=fitted(lm_model), Residuals=resid(lm_model))
resid_plot <- ggplot(residuals_df, aes(Fitted, Residuals)) +
  geom_point(alpha=0.6, size=2.5) +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  geom_smooth(method="loess", se=TRUE) +
  labs(title="Residuals vs Fitted (Updated Model)")
ggsave(file.path(plots_dir,"residuals_fitted.png"), resid_plot, width=6, height=5)

qq_plot <- ggplot(residuals_df, aes(sample=Residuals)) +
  stat_qq() + stat_qq_line() +
  labs(title="Normal Q-Q Plot (Updated Model)")
ggsave(file.path(plots_dir,"qq_norm.png"), qq_plot, width=6, height=5)




cat("\n✅ DONE! FULL STATISTICS, ANOVA & MULTIPLE REGRESSION.\n")
