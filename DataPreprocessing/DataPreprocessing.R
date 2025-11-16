# ==========================================
# BK_XSTK_Group07_ConcreteStrength_DataPrep_Upgraded.R
# Phần 1: TIỀN XỬ LÝ DỮ LIỆU (UPGRADED)
# Thêm 2 biến chiến lược trọng tâm
# ==========================================

# 0. --- Xóa môi trường & console ---
rm(list = ls())
cat("\014")

# 1. --- Cài đặt gói ---
goi_can <- c("dplyr")
invisible(lapply(goi_can, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}))

# 2. --- Nạp thư viện ---
library(dplyr)

# 3. --- Đọc dữ liệu ---
cat("📂 Chọn file dữ liệu CSV (concrete.csv):\n")
duong_dan <- file.choose()
data <- read.csv(duong_dan, header = TRUE)

# 4. --- Kiểm tra sơ bộ ---
cat("\n📏 Kích thước dữ liệu:\n"); print(dim(data))
cat("\n🔍 Cấu trúc dữ liệu:\n"); str(data)
cat("\n❓ Có giá trị khuyết (NA) không?\n"); print(anyNA(data))

# 5. --- Chuẩn hóa tên cột ---
names(data) <- tolower(trimws(names(data)))

# 6. --- Xử lý NA ---
if (anyNA(data)) {
  for (cot in names(data)) {
    if (is.numeric(data[[cot]])) {
      data[[cot]][is.na(data[[cot]])] <- mean(data[[cot]], na.rm = TRUE)
    }
  }
}

# 7. --- Chuyển kiểu dữ liệu ---
for (cot in names(data)) {
  if (is.character(data[[cot]])) data[[cot]] <- as.factor(data[[cot]])
}

# 8. --- Feature Engineering ---
if (all(c("water", "cement") %in% names(data))) {
  data$ratio_water_cement <- round(data$water / data$cement, 3)
}

if (all(c("cement", "slag", "ash", "fineagg", "coarseagg") %in% names(data))) {
  data$total_aggregates <- data$cement + data$slag + data$ash +
    data$fineagg + data$coarseagg
}

# ✅ **Các biến chiến lược mới**
if ("total_aggregates" %in% names(data)) {
  data$cement_fraction <- round(data$cement / data$total_aggregates, 3)
  data$water_fraction  <- round(data$water / data$total_aggregates, 3)
}

# 9. --- Loại bỏ cột không cần thiết ---
if ("id" %in% names(data)) data$id <- NULL

# 10. --- Biến log-strength ---
if ("strength" %in% names(data)) data$log_strength <- log1p(data$strength)

# 11. --- Chuẩn hóa dữ liệu số ---
data_scaled <- as.data.frame(scale(dplyr::select_if(data, is.numeric)))

# 12. --- Xuất file ---
write.csv(data, "D:\\HCMUT\\Probability and Stastistics\\Assignment\\Output\\data_cleaned.csv", row.names = FALSE)
write.csv(data_scaled, "D:\\HCMUT\\Probability and Stastistics\\Assignment\\Output\\data_scaled.csv", row.names = FALSE)

cat("\n✅ Hoàn tất tiền xử lý dữ liệu. 2 biến chiến lược đã thêm vào.\n")
