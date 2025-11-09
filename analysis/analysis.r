# ==========================================
# BK_XSTK_Group07_ConcreteStrength_Analysis_Full.R
# Phần 2: THỐNG KÊ MÔ TẢ & SUY DIỄN (tận dụng data_cleaned & data_scaled)
# Thêm tự động lưu bảng và đồ thị
# ==========================================

# 0. --- Xóa môi trường ---
rm(list=ls())
cat("\014")

# 1. --- Packages cần dùng ---
packages <- c("dplyr", "ggplot2", "readr", "patchwork")

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos="https://cloud.r-project.org")
  }
  library(p, character.only = TRUE)
}

options(bitmapType = "cairo")  # đồ thị đẹp trên Windows

# 2. --- Thư mục output ---
out_dir <- "D:/HCMUT/Probability and Stastistics/Assignment/Output"
tables_dir <- file.path(out_dir, "tables")
plots_dir  <- file.path(out_dir, "plots")

dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

# 3. --- Đọc dữ liệu tiền xử lý ---
data_cleaned <- read_csv(file.path(out_dir,"data_cleaned.csv"))
data_scaled  <- read_csv(file.path(out_dir,"data_scaled.csv"))

# 4. --- Thống kê mô tả ---
numeric_vars <- names(data_cleaned)[sapply(data_cleaned, is.numeric)]

desc_stats <- data_cleaned %>%
  summarise(across(all_of(numeric_vars),
                   list(mean=mean, sd=sd, min=min, max=max, median=median)))

# Tidy bảng
desc_stats_tidy <- desc_stats %>% t() %>% as.data.frame()
colnames(desc_stats_tidy) <- "value"
desc_stats_tidy$stat <- rownames(desc_stats_tidy)
desc_stats_tidy <- desc_stats_tidy %>% select(stat, value)

# In ra console
cat("\n📊 Thống kê mô tả:\n")
print(desc_stats_tidy)

# --- Lưu bảng thống kê ---
write.csv(desc_stats_tidy, file.path(tables_dir, "descriptive_stats.csv"), row.names=FALSE)

# 5. --- Histogram strength & log_strength ---
hist_strength <- ggplot(data_cleaned, aes(x=strength)) +
  geom_histogram(binwidth=2, fill="red2", color="black") +
  labs(title="Histogram of Strength", x="Strength (MPa)", y="Frequency") +
  theme_minimal()

hist_log_strength <- ggplot(data_cleaned, aes(x=log_strength)) +
  geom_histogram(binwidth=0.1, fill="steelblue", color="black") +
  labs(title="Histogram of Log(Strength)", x="Log(Strength)", y="Frequency") +
  theme_minimal()

print(hist_strength)
print(hist_log_strength)

# --- Lưu histogram ---
ggsave(file.path(plots_dir,"hist_strength.png"), plot=hist_strength, width=6, height=4)
ggsave(file.path(plots_dir,"hist_log_strength.png"), plot=hist_log_strength, width=6, height=4)

# 6. --- Scatter plots: strength vs numeric variables ---
plots <- lapply(numeric_vars[numeric_vars != "strength"], function(var) {
  ggplot(data_cleaned, aes_string(x=var, y="strength")) +
    geom_point(color="blue", alpha=0.6) +
    labs(title=paste(var, "vs Strength"), x=var, y="Strength") +
    theme_minimal()
})
wrap_plot_all <- patchwork::wrap_plots(plots, ncol=4)
print(wrap_plot_all)

# --- Lưu scatter plots ---
ggsave(file.path(plots_dir,"scatter_all.png"), plot=wrap_plot_all, width=16, height=12)
for (i in 1:length(plots)) {
  ggsave(file.path(plots_dir, paste0("scatter_", numeric_vars[i], "_vs_strength.png")),
         plot=plots[[i]], width=6, height=4)
}

# 7. --- Tạo nhóm numeric (cement_group, age_group) ---
make_group <- function(x, probs=seq(0,1,length.out=4), labels=NULL) {
  cut(x, breaks=quantile(x, probs=probs), labels=labels, include.lowest=TRUE)
}

data_cleaned <- data_cleaned %>%
  mutate(
    cement_group = make_group(cement, labels=c("Low","Med","High")),
    age_group    = cut(age, breaks=c(0,7,28,90,365),
                       labels=c("1-7","8-28","29-90","91-365"), include.lowest=TRUE)
  )

# 8. --- One-way ANOVA ---
aov1 <- aov(strength ~ cement_group, data=data_cleaned)
cat("\n--- One-way ANOVA ---\n")
print(summary(aov1))

# --- Lưu kết quả TukeyHSD ---
tukey_res <- as.data.frame(TukeyHSD(aov1)$cement_group)
write.csv(tukey_res, file.path(tables_dir,"tukey_cement.csv"), row.names=TRUE)

# 9. --- Two-way ANOVA ---
aov2 <- aov(strength ~ cement_group * age_group, data=data_cleaned)
cat("\n--- Two-way ANOVA ---\n")
print(summary(aov2))

# --- Lưu kết quả two-way ANOVA ---
twoway_anova_res <- as.data.frame(summary(aov2)[[1]])
write.csv(twoway_anova_res, file.path(tables_dir,"two_way_anova.csv"), row.names=TRUE)

# 10. --- Hồi quy tuyến tính đa biến ---
predictors <- setdiff(names(data_scaled), "strength")
formula <- as.formula(paste("strength ~", paste(predictors, collapse=" + ")))

lm_model <- lm(formula, data=data_scaled)
cat("\n--- Hồi quy tuyến tính đa biến ---\n")
print(summary(lm_model))

# --- Lưu hệ số hồi quy ---
lm_coef <- as.data.frame(summary(lm_model)$coefficients)
write.csv(lm_coef, file.path(tables_dir,"lm_coefficients.csv"), row.names=TRUE)

# 11. --- Hoàn tất ---
cat("\n✅ Hoàn tất thống kê mô tả, ANOVA và hồi quy đa biến.\n")
cat("📂 Tất cả bảng và hình đã lưu trong thư mục Output/tables và Output/plots.\n")
