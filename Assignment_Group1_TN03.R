# ==========================================
# BK_XSTK_Group07_ConcreteStrength_DataPrep.R
# Phần: Xử lý & Thống kê mô tả dữ liệu
# Thành viên 1: (Ghi tên bạn ở đây)
# Hướng dẫn: Mở file này trong RStudio và bấm "Source" hoặc Ctrl+Shift+Enter
# ==========================================

# 0. --- Clear workspace & console (optional) ---
rm(list = ls())
cat("\014")

# 1. --- Install necessary packages (chỉ chạy 1 lần) ---
needed <- c("psych", "ggplot2", "Hmisc", "car")
install_if_missing <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}
invisible(lapply(needed, install_if_missing))

# 2. --- Load libraries ---
library(psych)    # describe()
library(ggplot2)  # plotting (optional)
library(Hmisc)    # rcorr for correlation p-values
library(car)      # vif

# 3. --- Import dataset ---
# Option A: chọn file bằng hộp thoại (an toàn cho mọi máy)
data_path <- file.choose()  # Chọn file Concrete_Data.csv
data <- read.csv(data_path, header = TRUE)

# Option B: hoặc sửa đường dẫn tuyệt đối nếu muốn:
# data <- read.csv("D:/HCMUT/Probability and Statistics/Assignment/Data/Concrete_Data.csv")

# 4. --- Quick checks ---
cat("Data dimensions (rows, cols):\n")
print(dim(data))
cat("\nStructure:\n")
str(data)
cat("\nAny NA?\n")
print(anyNA(data))

# 5. --- Standardize column names (nhỏ, dễ đọc) ---
names(data) <- tolower(names(data))
print(names(data))

# 6. --- Descriptive statistics ---
desc <- describe(data)   # psych::describe
print(desc)

# Save descriptive table
out_dir <- "BK_Output"
if (!dir.exists(out_dir)) dir.create(out_dir)
write.csv(desc, file = file.path(out_dir, "summary_stats_psych.csv"), row.names = TRUE)
write.csv(as.data.frame(summary(data)), file = file.path(out_dir, "summary_base.csv"))

# 7. --- Correlation matrix + p-values ---
cor_mat <- cor(data)
rc <- rcorr(as.matrix(data))
write.csv(rc$r, file = file.path(out_dir, "correlation_matrix.csv"))
write.csv(rc$P, file = file.path(out_dir, "correlation_pvalues.csv"))

# 8. --- Simple plots (saved as PNG) ---
png(filename = file.path(out_dir, "hist_strength.png"), width = 800, height = 600)
hist(data$strength, main = "Histogram of Strength", xlab = "Strength (MPa)")
dev.off()

png(filename = file.path(out_dir, "boxplots_all_vars.png"), width = 1200, height = 800)
boxplot(data, main = "Boxplot of all variables (check outliers)", las = 2)
dev.off()

png(filename = file.path(out_dir, "scatter_cement_strength.png"), width = 800, height = 600)
plot(data$cement, data$strength, main="Cement vs Strength",
     xlab="Cement (kg/m3)", ylab="Strength (MPa)", pch=19)
abline(lm(data$strength ~ data$cement), col = "red")
dev.off()

# 9. --- Save a pairs plot (may be heavy) ---
png(filename = file.path(out_dir, "pairs_plot.png"), width = 1400, height = 1400)
pairs(data, main = "Pairs plot (all variables)")
dev.off()

# 10. --- Quick regression for exploratory purposes (not final) ---
lm_full <- lm(strength ~ ., data = data)
summary_lm <- summary(lm_full)
print(summary_lm)

vif_values <- tryCatch({
  vif(lm_full)
}, error = function(e) {
  NA
})
print(vif_values)
capture.output(summary_lm, file = file.path(out_dir, "lm_full_summary.txt"))
capture.output(vif_values, file = file.path(out_dir, "lm_vif.txt"))

# 11. --- Export cleaned data copy (no change made currently) ---
write.csv(data, file = file.path(out_dir, "data_cleaned_copy.csv"), row.names = FALSE)

# 12. --- End message ---
cat("\nAll outputs saved in folder: ", normalizePath(out_dir), "\n")
