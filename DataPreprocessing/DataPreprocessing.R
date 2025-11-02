# ==========================================
# BK_XSTK_Group07_ConcreteStrength_DataPrep.R
# Phần: Tiền xử lý & Thống kê mô tả dữ liệu
# Thành viên 1: (Ghi tên bạn ở đây)
# Hướng dẫn: Mở file này trong RStudio và bấm "Source" hoặc Ctrl + Shift + Enter
# ==========================================

# 0. --- Xóa môi trường & làm sạch console (tùy chọn) ---
rm(list = ls())
cat("\014")

# 1. --- Cài đặt các gói cần thiết (chỉ cần chạy 1 lần) ---
goi_can <- c("psych", "ggplot2", "Hmisc", "car", "dplyr")
cai_dat_neu_thieu <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}
invisible(lapply(goi_can, cai_dat_neu_thieu))

# 2. --- Nạp thư viện ---
library(psych)    # mô tả thống kê
library(ggplot2)  # vẽ biểu đồ
library(Hmisc)    # tính tương quan kèm p-value
library(car)      # hệ số phóng đại phương sai (VIF)
library(dplyr)    # xử lý dữ liệu

# 3. --- Đọc dữ liệu ---
cat("Chọn file dữ liệu CSV (Concrete_Data.csv):\n")
duong_dan <- file.choose()  # chọn file thủ công
data <- read.csv(duong_dan, header = TRUE)

# 4. --- Kiểm tra sơ bộ ---
cat("\nKích thước dữ liệu (số dòng, số cột):\n")
print(dim(data))
cat("\nCấu trúc dữ liệu:\n")
str(data)
cat("\nCó giá trị khuyết (NA) không?\n")
print(anyNA(data))

# 5. --- Chuẩn hóa tên cột (chữ thường, bỏ khoảng trắng) ---
names(data) <- tolower(trimws(names(data)))
print(names(data))

# ==========================================================
# 🧩 GIAI ĐOẠN TIỀN XỬ LÝ DỮ LIỆU
# ==========================================================

# 5a. --- Kiểm tra dữ liệu khuyết ---
cat("\nSố lượng giá trị khuyết (NA) trong từng cột:\n")
print(colSums(is.na(data)))

# Nếu có NA thì xử lý bằng cách thay thế bằng giá trị trung bình
if (anyNA(data)) {
  cat("\nĐang thay thế các giá trị NA bằng giá trị trung bình...\n")
  for (cot in names(data)) {
    if (is.numeric(data[[cot]])) {
      data[[cot]][is.na(data[[cot]])] <- mean(data[[cot]], na.rm = TRUE)
    }
  }
}

# 5b. --- Kiểm tra & chuyển đổi kiểu dữ liệu ---
# Nếu có cột dạng ký tự, chuyển sang dạng factor
for (cot in names(data)) {
  if (is.character(data[[cot]])) {
    data[[cot]] <- as.factor(data[[cot]])
  }
}

cat("\nCấu trúc dữ liệu sau khi chuyển đổi kiểu:\n")
str(data)

# 5c. --- Kiểm tra giá trị ngoại lai (outlier) ---
cat("\nPhát hiện giá trị ngoại lai (theo quy tắc IQR):\n")
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

# 5d. --- Tạo thêm biến mới ---
# Ví dụ: tỉ lệ nước/xi măng và tổng khối lượng vật liệu rắn
if (all(c("water", "cement") %in% names(data))) {
  data$ti_le_nuoc_ximang <- round(data$water / data$cement, 3)
}

if (all(c("cement", "slag", "ash", "fineaggregate", "coarseaggregate") %in% names(data))) {
  data$tong_vatlieu_ran <- data$cement + data$slag + data$ash +
    data$fineaggregate + data$coarseaggregate
}

# 5e. --- Xóa biến không cần thiết ---
# Ví dụ: nếu có cột “id”, ta bỏ đi
if ("id" %in% names(data)) {
  data$id <- NULL
}

# 5f. --- Chuyển đổi biến ---
# Lấy log của biến cường độ (strength) để giảm độ lệch
if ("strength" %in% names(data)) {
  data$log_strength <- log1p(data$strength)  # log(1 + x)
}

# 5g. --- Chuẩn hóa dữ liệu ---
# Tạo bản sao đã chuẩn hóa (chỉ gồm các biến số)
data_scaled <- as.data.frame(scale(dplyr::select_if(data, is.numeric)))
cat("\nĐã chuẩn hóa các biến số thành công.\n")

# ==========================================================
# 📊 GIAI ĐOẠN THỐNG KÊ MÔ TẢ & KHẢO SÁT DỮ LIỆU
# ==========================================================

# 6. --- Mô tả thống kê cơ bản ---
thongke <- describe(data)
print(thongke)

# --- Tạo thư mục lưu kết quả ---
thu_muc_xuat <- choose.dir(caption = "Chọn nơi lưu kết quả")
thu_muc_xuat <- file.path(thu_muc_xuat, "Output")

# Tạo đầy đủ đường dẫn
dir.create(thu_muc_xuat, recursive = TRUE, showWarnings = FALSE)

# Ghi file
capture.output(print(thongke), file = file.path(thu_muc_xuat, "thongke_psych.txt"))


# --- Chuyển object describe thành data.frame và lưu ---
capture.output(print(thongke), file = file.path(thu_muc_xuat, "thongke_psych.txt"))



# --- Ghi thống kê mô tả chi tiết dạng text (summary base R) ---
capture.output(summary(data), file = file.path(thu_muc_xuat, "summary_base.txt"))

# --- Xuất thêm bản sao dữ liệu sạch (nếu cần) ---
write.csv(data, file = file.path(thu_muc_xuat, "data_cleaned_copy.csv"), row.names = FALSE)








# 7. --- Ma trận tương quan + giá trị p ---
cor_mat <- cor(dplyr::select_if(data, is.numeric))
rc <- rcorr(as.matrix(dplyr::select_if(data, is.numeric)))
write.csv(rc$r, file = file.path(thu_muc_xuat, "ma_tran_tuongquan.csv"))
write.csv(rc$P, file = file.path(thu_muc_xuat, "p_value_tuongquan.csv"))

# 8. --- Vẽ biểu đồ đơn giản ---
png(filename = file.path(thu_muc_xuat, "bieu_do_hist_strength.png"), width = 800, height = 600)
hist(data$strength, main = "Biểu đồ Histogram của Cường độ (Strength)",
     xlab = "Strength (MPa)", col = "skyblue", border = "white")
dev.off()

png(filename = file.path(thu_muc_xuat, "boxplot_cac_bien.png"), width = 1200, height = 800)
boxplot(data, main = "Boxplot cho tất cả biến (kiểm tra ngoại lai)",
        las = 2, col = "lightgray")
dev.off()

png(filename = file.path(thu_muc_xuat, "scatter_cement_strength.png"), width = 800, height = 600)
plot(data$cement, data$strength, main = "Tương quan giữa Xi măng và Cường độ",
     xlab = "Cement (kg/m3)", ylab = "Strength (MPa)", pch = 19, col = "blue")
abline(lm(data$strength ~ data$cement), col = "red", lwd = 2)
dev.off()

png(filename = file.path(thu_muc_xuat, "pairs_plot.png"), width = 1400, height = 1400)
pairs(dplyr::select_if(data, is.numeric), main = "Biểu đồ pairs cho các biến số")
dev.off()

# 9. --- Hồi quy tuyến tính sơ bộ để khảo sát ---
mo_hinh <- lm(strength ~ ., data = data)
ketqua_lm <- summary(mo_hinh)
print(ketqua_lm)

vif_values <- tryCatch({
  vif(mo_hinh)
}, error = function(e) {
  NA
})
print(vif_values)

capture.output(ketqua_lm, file = file.path(thu_muc_xuat, "hoi_quy_daydu.txt"))
capture.output(vif_values, file = file.path(thu_muc_xuat, "vif_daydu.txt"))

# 10. --- Xuất dữ liệu sau khi tiền xử lý ---
write.csv(data, file = file.path(thu_muc_xuat, "du_lieu_sach.csv"), row.names = FALSE)
write.csv(data_scaled, file = file.path(thu_muc_xuat, "du_lieu_chuanhoa.csv"), row.names = FALSE)

# 11. --- Thông báo hoàn tất ---
cat("\n✅ Đã hoàn tất giai đoạn tiền xử lý và thống kê mô tả dữ liệu.")
cat("\n📁 Tất cả kết quả được lưu trong thư mục:", normalizePath(thu_muc_xuat), "\n")
