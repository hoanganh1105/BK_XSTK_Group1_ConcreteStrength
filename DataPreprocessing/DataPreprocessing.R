# ==========================================
# BK_XSTK_Group07_ConcreteStrength_DataPrep.R
# Phần 1: TIỀN XỬ LÝ DỮ LIỆU
# Thành viên: (Ghi tên bạn ở đây)
# ==========================================

# 0. --- Xóa môi trường & làm sạch console (tùy chọn) ---
rm(list = ls())
cat("\014")  # Xóa console

# 1. --- Cài đặt các gói cần thiết (chạy 1 lần nếu chưa cài) ---
goi_can <- c("dplyr")
cai_dat_neu_thieu <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}
invisible(lapply(goi_can, cai_dat_neu_thieu))

# 2. --- Nạp thư viện ---
library(dplyr)

# ==========================================================
# 🧩 GIAI ĐOẠN TIỀN XỬ LÝ DỮ LIỆU
# ==========================================================

# 3. --- Đọc dữ liệu ---
cat("📂 Chọn file dữ liệu CSV (concrete.csv):\n")
duong_dan <- file.choose()  # Chọn file thủ công
data <- read.csv(duong_dan, header = TRUE)

# 4. --- Kiểm tra sơ bộ dữ liệu ---
cat("\n📏 Kích thước dữ liệu (số dòng, số cột):\n")
print(dim(data))
cat("\n🔍 Cấu trúc dữ liệu:\n")
str(data)
cat("\n❓ Có giá trị khuyết (NA) không?\n")
print(anyNA(data))

# 5. --- Chuẩn hóa tên cột ---
names(data) <- tolower(trimws(names(data)))  # chữ thường + bỏ khoảng trắng
cat("\n✅ Tên cột sau khi chuẩn hóa:\n")
print(names(data))

# 6. --- Kiểm tra & xử lý dữ liệu khuyết (NA) ---
cat("\n🧩 Số lượng giá trị khuyết (NA) trong từng cột:\n")
print(colSums(is.na(data)))

# Thay thế NA bằng giá trị trung bình (nếu có)
if (anyNA(data)) {
  cat("\nĐang thay thế các giá trị NA bằng giá trị trung bình...\n")
  for (cot in names(data)) {
    if (is.numeric(data[[cot]])) {
      data[[cot]][is.na(data[[cot]])] <- mean(data[[cot]], na.rm = TRUE)
    }
  }
  cat("✅ Đã thay thế xong giá trị NA.\n")
}

# 7. --- Chuyển đổi kiểu dữ liệu (nếu cần) ---
# Cột dạng ký tự sẽ được chuyển thành factor
for (cot in names(data)) {
  if (is.character(data[[cot]])) {
    data[[cot]] <- as.factor(data[[cot]])
  }
}
cat("\n🔄 Cấu trúc dữ liệu sau khi chuyển đổi kiểu:\n")
str(data)

# 8. --- Phát hiện giá trị ngoại lai (Outlier) ---
cat("\n🚨 Phát hiện giá trị ngoại lai (theo quy tắc IQR):\n")
for (cot in names(data)) {
  if (is.numeric(data[[cot]])) {
    Q1 <- quantile(data[[cot]], 0.25)
    Q3 <- quantile(data[[cot]], 0.75)
    IQR <- Q3 - Q1
    duoi <- Q1 - 1.5 * IQR
    tren <- Q3 + 1.5 * IQR
    ngoai_lai <- sum(data[[cot]] < duoi | data[[cot]] > tren)
    cat(cot, ":", ngoai_lai, "giá trị ngoại lai\n")
  }
}

# 9. --- Kiểm tra và loại bỏ dòng trùng lặp (nếu có) ---
so_trung <- sum(duplicated(data))
cat("\n🔁 Số dòng trùng lặp:", so_trung, "\n")
if (so_trung > 0) {
  data <- data[!duplicated(data), ]
  cat("✅ Đã loại bỏ các dòng trùng lặp.\n")
}

# 10. --- Thêm biến mới (Feature Engineering) ---
if (all(c("water", "cement") %in% names(data))) {
  data$ti_le_nuoc_ximang <- round(data$water / data$cement, 3)
  cat("\n➕ Đã thêm biến 'ti_le_nuoc_ximang'.\n")
}

if (all(c("cement", "slag", "ash", "fineagg", "coarseagg") %in% names(data))) {
  data$tong_vatlieu_ran <- data$cement + data$slag + data$ash +
    data$fineagg + data$coarseagg
  cat("➕ Đã thêm biến 'tong_vatlieu_ran'.\n")
}


# 11. --- Xóa biến không cần thiết (nếu có cột id) ---
if ("id" %in% names(data)) {
  data$id <- NULL
  cat("\n🗑️ Đã xóa biến 'id'.\n")
}

# 12. --- Chuyển đổi biến (log-transform) ---
if ("strength" %in% names(data)) {
  data$log_strength <- log1p(data$strength)
  cat("\n🔧 Đã thêm biến log-strength (log1p của strength).\n")
}

# 13. --- Chuẩn hóa dữ liệu số ---
data_scaled <- as.data.frame(scale(dplyr::select_if(data, is.numeric)))
cat("\n📏 Đã chuẩn hóa các biến số thành công.\n")

# 14. --- Xuất dữ liệu sạch và dữ liệu chuẩn hóa ---
write.csv(data, "D:\\HCMUT\\Probability and Stastistics\\Assignment\\Output\\data_cleaned.csv", row.names = FALSE)
write.csv(data_scaled, "D:\\HCMUT\\Probability and Stastistics\\Assignment\\Output\\data_scaled.csv", row.names = FALSE)
cat("\n💾 Đã lưu file 'data_cleaned.csv' và 'data_scaled.csv' trong thư mục hiện tại.\n")

# 15. --- Hoàn tất ---
cat("\n✅ HOÀN TẤT GIAI ĐOẠN TIỀN XỬ LÝ DỮ LIỆU.\n")
cat("📦 Dữ liệu sạch đã sẵn sàng cho giai đoạn Thống kê mô tả.\n")
