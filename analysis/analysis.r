# =========================================================================================
# FILE: BK_XSTK_Group07_ConcreteStrength_Analysis_Optimized.R (BẢN CUỐI CÙNG - SỬ DỤNG 'strength')
# MỤC TIÊU: Phân tích Thống kê Mô tả, ANOVA, và Hồi quy Đa biến (MLR)
# =========================================================================================

# 0. --- THIẾT LẬP MÔI TRƯỜNG ---
rm(list=ls())
cat("\014")

# 1. --- QUẢN LÝ GÓI & THIẾT LẬP CÁC PHỤ THUỘC (DEPENDENCIES) ---
packages <- c("dplyr","ggplot2","readr","patchwork","car","MASS","gridExtra","corrplot", "tidyr")
for(p in packages){
  if(!requireNamespace(p, quietly=TRUE)) install.packages(p, repos="https://cloud.r-project.org")
  library(p, character.only = TRUE)
}
options(bitmapType="cairo")

# 2. --- THƯ MỤC OUTPUT ---
out_dir <- "D:/HCMUT/Probability and Stastistics/Assignment/Output" 
tables_dir <- file.path(out_dir,"tables")
plots_dir <- file.path(out_dir,"plots")
dir.create(tables_dir, recursive=TRUE, showWarnings=FALSE)
dir.create(plots_dir, recursive=TRUE, showWarnings=FALSE)

# 3. --- ĐỌC DỮ LIỆU ĐÃ TIỀN XỬ LÝ ---
cat("📂 Vui lòng chọn file dữ liệu CSV (data_cleaned.csv):\n")
data_file <- file.choose() 
data_cleaned <- read_csv(data_file) 
cat(paste0("\n✅ Đã nạp dữ liệu từ: ", data_file, "\n"))

# LƯU Ý: Không cần bước đổi tên cột vì toàn bộ code đã sử dụng tên biến 'strength'

# =========================================================================================
# I. THỐNG KÊ MÔ TẢ VÀ TƯƠNG QUAN
# =========================================================================================

# Danh sách các biến số
all_predictors <- c("cement", "slag", "ash", "water", "superplastic", 
                    "coarseagg", "fineagg", "age",
                    "ratio_water_cement", "cement_fraction", "water_fraction")

# 4. --- THỐNG KÊ MÔ TẢ (DESCRIPTIVE STATISTICS) ---
numeric_vars <- names(data_cleaned)[sapply(data_cleaned,is.numeric)]

desc_stats_tidy <- data_cleaned %>%
  summarise(across(all_of(numeric_vars),
                   list(mean=mean, sd=sd, min=min, max=max, median=median))) %>%
  tidyr::pivot_longer(cols = everything(), names_to = "stat_var", values_to = "value") %>%
  separate(stat_var, into = c("variable", "stat"), sep = "_", extra = "merge") %>% 
  dplyr::select(variable, stat, value) 

write.csv(desc_stats_tidy, file.path(tables_dir,"descriptive_stats.csv"), row.names=FALSE)
cat("✅ Đã hoàn tất Thống kê Mô tả.\n")

# 4a. --- MA TRẬN TƯƠNG QUAN (CORRELATION MATRIX) ---

# Gộp biến mục tiêu (strength) và tất cả các biến dự đoán
corr_vars <- c("strength", "log_strength", all_predictors)
corr_mat <- cor(data_cleaned[, corr_vars]) 

write.csv(corr_mat, file.path(tables_dir,"correlation_matrix_full.csv"), row.names=TRUE)

# Vẽ heatmap tương quan
png(file.path(plots_dir,"correlation_heatmap_full.png"), width=900, height=900)
corrplot(corr_mat, method="color", addCoef.col="black", number.cex=0.6,
         tl.col="black", tl.srt=45, cl.cex=0.8, title="Correlation Matrix (Including Engineered Features)")
dev.off()

# =========================================================================================
# 5. --- HISTOGRAM PHÂN PHỐI (ĐÃ THÊM MEAN, MEDIAN & LEGEND) ---
# =========================================================================================

# SỬ DỤNG stat_summary để tính Mean/Median ngay trong ggplot

# =========================================================================================
# 5a. HISTOGRAM CỦA STRENGTH (Cường độ Gốc)
# =========================================================================================

# 1. HISTOGRAM CỦA STRENGTH
hist_strength <- ggplot(data_cleaned, aes(x=strength)) +
  geom_histogram(binwidth=2, fill="red2", color="black") +
  
  # Đường Trung bình (Kỳ vọng) - Đứt nét, Đỏ
  geom_vline(aes(xintercept = mean(strength, na.rm=TRUE), color = "Trung bình (Kỳ vọng)"), 
             linetype="dashed", linewidth=1) +
  
  # Đường Trung vị (Median) - Liền nét, Xanh
  geom_vline(aes(xintercept = median(strength, na.rm=TRUE), color = "Trung vị (Median)"), 
             linetype="solid", linewidth=1) +
  
  # Thiết lập màu sắc và BỎ tiêu đề Legend (name = "")
  scale_color_manual(name = "",
                     values = c("Trung bình (Kỳ vọng)" = "darkred", 
                                "Trung vị (Median)" = "darkred")) +
  
  labs(title="Phân phối Cường độ Bê tông", x="Cường độ (MPa)", y="Tần suất") +
  theme_minimal(base_size=14) +
  theme(legend.position = "bottom")

# Lưu biểu đồ Strength
ggsave(file.path(plots_dir,"hist_strength_final.png"), hist_strength, width=6, height=5)
cat("✅ Đã lưu Biểu đồ Histogram Strength vào file 'hist_strength_final.png'.\n")

# =========================================================================================
# 5b. HISTOGRAM CỦA LOG_STRENGTH (Cường độ Log)
# =========================================================================================

# 2. HISTOGRAM CỦA LOG_STRENGTH
hist_log_strength <- ggplot(data_cleaned, aes(x=log_strength)) +
  geom_histogram(binwidth=0.1, fill="steelblue", color="black") +
  
  # Đường Trung bình (Kỳ vọng) - Đứt nét, Đỏ
  geom_vline(aes(xintercept = mean(log_strength, na.rm=TRUE), color = "Trung bình (Kỳ vọng)"), 
             linetype="dashed", linewidth=1) +
  
  # Đường Trung vị (Median) - Liền nét, Xanh
  geom_vline(aes(xintercept = median(log_strength, na.rm=TRUE), color = "Trung vị (Median)"), 
             linetype="solid", linewidth=1) +
  
  # Thiết lập màu sắc và BỎ tiêu đề Legend (name = "")
  scale_color_manual(name = "",
                     values = c("Trung bình (Kỳ vọng)" = "blue", 
                                "Trung vị (Median)" = "blue")) +
  
  labs(title="Phân phối Log(Cường độ Bê tông)", x="Log(Cường độ)", y="Tần suất") +
  theme_minimal(base_size=14) +
  theme(legend.position = "bottom")

# Lưu biểu đồ Log(Strength)
ggsave(file.path(plots_dir,"hist_log_strength_final.png"), hist_log_strength, width=6, height=5)
cat("✅ Đã lưu Biểu đồ Histogram Log(Strength) vào file 'hist_log_strength_final.png'.\n")

# =========================================================================================
# 6. --- SCATTER PLOTS (Predictor vs Strength) ---
# TẠO VÀ LƯU RIÊNG TỪNG BIỂU ĐỒ
# =========================================================================================

# Loại bỏ dòng plots <- list() vì chúng ta không cần gộp chúng.
for(var in all_predictors){
  p <- ggplot(data_cleaned, aes_string(x=var, y="strength")) +
    geom_point(color="#1E3A8A", alpha=0.6, size=2.5) +
    geom_smooth(method="lm", se=FALSE, color="red") + # Thêm đường hồi quy tuyến tính
    labs(title=paste0(var," vs Strength"), x=var, y="Strength (MPa)") +
    theme_minimal(base_size=12)
  
  # *** BƯỚC MỚI: LƯU TỪNG BIỂU ĐỒ ***
  # Tạo tên file động (ví dụ: scatter_cement_vs_strength.png)
  file_name <- paste0("scatter_", var, "_vs_strength.png")
  ggsave(file.path(plots_dir, file_name), p, width=6, height=5) # Kích thước nhỏ hơn cho biểu đồ đơn
}

cat("\n✅ Đã lưu thành công các biểu đồ Scatter Plot riêng lẻ vào thư mục plots.\n")

# REMOVE: Các dòng tạo và lưu biểu đồ gộp đã được loại bỏ.
# wrap_plot_all <- wrap_plots(plots, ncol=4) +
#     plot_annotation(title="Scatter Plots: Predictors vs Concrete Strength")
# ggsave(file.path(plots_dir,"scatter_all_predictors.png"), wrap_plot_all, width=16, height=14)


# =========================================================================================
# II. PHÂN TÍCH PHƯƠNG SAI (ANOVA)
# =========================================================================================

# 7. --- TẠO BIẾN NHÓM PHÂN LOẠI (FACTOR GROUPS) ---
make_group <- function(x, probs=seq(0,1,length.out=4), labels=NULL){
  cut(x, breaks=quantile(x, probs=probs, na.rm=TRUE), labels=labels, include.lowest=TRUE)
}

data_cleaned <- data_cleaned %>%
  mutate(
    cement_group = make_group(cement, labels=c("Low","Med","High")),
    age_group = cut(age, breaks=c(0,7,28,90,365),
                    labels=c("1-7","8-28","29-90","91-365"), include.lowest=TRUE)
  )

# 8. --- ONE-WAY ANOVA: strength ~ cement_group ---
aov1 <- aov(strength ~ cement_group, data=data_cleaned)
write.csv(as.data.frame(summary(aov1)[[1]]), file.path(tables_dir,"anova_summary_cement.csv"), row.names=TRUE)

# Diagnostics
shapiro_res <- shapiro.test(residuals(aov1))
write.csv(data.frame(statistic=shapiro_res$statistic, p.value=shapiro_res$p.value),
          file.path(tables_dir,"shapiro_residuals_cement.csv"), row.names=FALSE)

levene_res <- car::leveneTest(strength ~ cement_group, data=data_cleaned)
write.csv(as.data.frame(levene_res), file.path(tables_dir,"levene_test_cement.csv"), row.names=TRUE)

# Post-hoc Tukey HSD
tukey_res <- as.data.frame(TukeyHSD(aov1)$cement_group)
write.csv(tukey_res, file.path(tables_dir,"tukey_cement_posthoc.csv"), row.names=TRUE)

# Boxplot (ĐÃ SỬA LỖI: Thêm tất cả các điểm dữ liệu thô)
box_cement_clean <- ggplot(data_cleaned, aes(x=cement_group, y=strength)) +
  
  # *** BƯỚC MỚI: Thêm tất cả các điểm dữ liệu thô (Raw Data) ***
  # Sử dụng geom_jitter để làm các điểm lan tỏa, dễ nhìn hơn
  geom_jitter(color="gray60", size=1.5, alpha=0.4, width=0.1) +
  
  # Hộp Boxplot (Đặt sau jitter để nó nổi bật)
  geom_boxplot(fill="#87CEFA", color="black", alpha=0.7, outlier.shape=NA) + 
  # Lưu ý: Đặt outlier.shape=NA để Boxplot không vẽ Outliers (vì Jitter đã vẽ hết rồi)
  
  # Điểm Trung bình (Mean)
  stat_summary(fun=mean, geom="point", shape=18, size=4, color="darkred") + 
  
  labs(title="Concrete Strength by Cement Group (w/ Raw Data)", x="Cement Group", y="Strength (MPa)") +
  theme_minimal(base_size=14)

ggsave(file.path(plots_dir,"boxplot_cement_strength_with_all_points.png"), box_cement_clean, width=7, height=6)

cat("\n✅ Đã sửa mã Boxplot để hiển thị tất cả các điểm dữ liệu thô (dùng geom_jitter).\n")


# 9. --- TWO-WAY ANOVA: cement_group × age_group ---
aov2 <- aov(strength ~ cement_group * age_group, data=data_cleaned)
write.csv(as.data.frame(summary(aov2)[[1]]), file.path(tables_dir,"two_way_anova_summary.csv"), row.names=TRUE)

# Interaction Plot
interaction_plot <- ggplot(data_cleaned, aes(x=age_group, y=strength, color=cement_group, group=cement_group)) +
  stat_summary(fun=mean, geom="line") +
  stat_summary(fun=mean, geom="point", size=3) +
  scale_color_manual(values=c("Low"="#1b9e77","Med"="#d95f02","High"="#7570b3")) +
  labs(title="Interaction Plot: Concrete Strength ~ Cement × Age", x="Age Group (days)", y="Mean Strength (MPa)") +
  theme_minimal(base_size=14)
ggsave(file.path(plots_dir,"interaction_plot.png"), interaction_plot, width=7, height=5)


# =========================================================================================
# III. HỒI QUY TUYẾN TÍNH ĐA BIẾN (MULTIPLE LINEAR REGRESSION)
# =========================================================================================

predictors_mlr <- all_predictors 

# 10. --- TIỀN XỬ LÝ CHO MLR ---
# Chuẩn hóa (Scaling) các biến dự đoán (để so sánh hệ số dễ dàng hơn)
data_cleaned[, predictors_mlr] <- scale(data_cleaned[, predictors_mlr])

# 11. --- PHÂN TÍCH ĐA CỘNG TUYẾN (VIF CHECK) ---
cat("\n🔬 CHẨN ĐOÁN ĐA CỘNG TUYẾN (VIF CHECK - FULL MODEL):\n")
formula_vif <- as.formula(paste("strength ~", paste(predictors_mlr, collapse=" + ")))
lm_model_full_pre_split <- lm(formula_vif, data=data_cleaned)
vif_values_full <- car::vif(lm_model_full_pre_split)
write.csv(as.data.frame(vif_values_full), file.path(tables_dir,"vif_full_model.csv"), row.names=TRUE)
print(vif_values_full)

# --- 11b. CHIA TẬP DỮ LIỆU (TRAIN-TEST SPLIT) ---
cat("\n🔪 CHIA TẬP DỮ LIỆU (80% Train, 20% Test):\n")
set.seed(42) # Đảm bảo tính tái lập
train_indices <- sample(seq_len(nrow(data_cleaned)), size = floor(0.8 * nrow(data_cleaned)))

data_train_scaled <- data_cleaned[train_indices, ] # Dữ liệu TRAIN (đã scale)
data_test_scaled <- data_cleaned[-train_indices, ]  # Dữ liệu TEST (đã scale)

# 12. --- MÔ HÌNH HỒI QUY TỐI ƯU (STEPWISE AIC) SỬ DỤNG TẬP TRAIN ---
# Xây dựng mô hình Full trên tập TRAIN
lm_model_train_full <- lm(formula_vif, data = data_train_scaled)

# Thực hiện Stepwise Regression chỉ trên tập TRAIN
step_model <- stepAIC(lm_model_train_full, direction="both", trace=FALSE)

# Lưu kết quả mô hình Stepwise (tối ưu hóa)
write.csv(as.data.frame(summary(step_model)$coefficients), 
          file.path(tables_dir,"lm_stepwise_coeff_optimized.csv"), row.names=TRUE)
# R-squared cần được lấy từ summary(step_model) và lưu vào data frame
write.csv(data.frame(R.squared = summary(step_model)$r.squared), 
          file.path(tables_dir,"lm_stepwise_R2.csv"), row.names=TRUE)
cat("✅ Đã hoàn tất Stepwise Model trên tập TRAIN.\n")


# 13. --- DIAGNOSTICS PLOTS (Sử dụng mô hình tối ưu - step_model) ---

# Residuals vs Fitted
residuals_df <- data.frame(Fitted=fitted(step_model), Residuals=resid(step_model))
resid_plot <- ggplot(residuals_df, aes(Fitted, Residuals)) +
  geom_point(alpha=0.6, size=2.5) +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  geom_smooth(method="loess", se=TRUE) +
  labs(title="Residuals vs Fitted (Stepwise Model)")
ggsave(file.path(plots_dir,"residuals_fitted_stepwise.png"), resid_plot, width=6, height=5)

# Normal Q-Q Plot
qq_plot <- ggplot(residuals_df, aes(sample=Residuals)) +
  stat_qq() + stat_qq_line() +
  labs(title="Normal Q-Q Plot (Stepwise Model)")
ggsave(file.path(plots_dir,"qq_norm_stepwise.png"), qq_plot, width=6, height=5)


# 14. --- ĐÁNH GIÁ TỔNG QUÁT HÓA TRÊN TẬP TEST (OUT-OF-SAMPLE EVALUATION) ---

# Dự đoán trên tập TEST (data_test_scaled đã được tạo ở bước 11b)
data_test_scaled$predicted_strength <- predict(step_model, newdata = data_test_scaled)

# Tính toán các chỉ số đánh giá
actual_values <- data_test_scaled$strength
predicted_values <- data_test_scaled$predicted_strength
errors <- actual_values - predicted_values

# Tính toán các chỉ số: R-Squared, RMSE, MSE
ss_total <- sum((actual_values - mean(actual_values))^2)
ss_residual <- sum(errors^2)

r2_test <- 1 - (ss_residual / ss_total)
mse_test <- mean(errors^2)
rmse_test <- sqrt(mse_test)
mae_test <- mean(abs(errors))

# Lưu kết quả đánh giá cuối cùng
evaluation_results <- data.frame(
  Metric = c("R2_Test", "RMSE_Test", "MSE_Test", "MAE_Test"),
  Value = c(r2_test, rmse_test, mse_test, mae_test)
)
write.csv(evaluation_results, file.path(tables_dir, "evaluation_test_set.csv"), row.names=FALSE)

cat("\n✅ HOÀN TẤT PHÂN TÍCH: Toàn bộ thống kê, ANOVA, và Hồi quy đã được lưu vào thư mục Output.\n")
cat("\n--- KẾT QUẢ ĐÁNH GIÁ TỔNG QUÁT HÓA (TẬP TEST) ---\n")
cat(paste0("   - R-squared (Tập TEST): ", round(r2_test, 4), "\n"))
cat(paste0("   - RMSE (Tập TEST): ", round(rmse_test, 4), "\n"))