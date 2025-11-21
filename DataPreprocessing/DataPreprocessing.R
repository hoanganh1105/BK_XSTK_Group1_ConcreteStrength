# =================================================================
# FILE: BK_XSTK_Group07_ConcreteStrength_DataPrep_Optimized.R
# MỤC TIÊU: Tiền xử lý dữ liệu và Tạo biến chiến lược cho mô hình
# =================================================================

# 0. --- THIẾT LẬP MÔI TRƯỜNG ---
rm(list = ls()) # Xóa tất cả đối tượng trong môi trường làm việc
cat("\014") # Xóa console

# 1. --- QUẢN LÝ GÓI ---
goi_can <- c("dplyr")
invisible(lapply(goi_can, function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
  library(p, character.only = TRUE)
}))

# 2. --- ĐỌC DỮ LIỆU ---
cat("📂 Vui lòng chọn file dữ liệu CSV (concrete.csv):\n")
duong_dan <- file.choose()
data <- read.csv(duong_dan, header = TRUE)

# 3. --- KIỂM TRA SƠ BỘ VÀ LÀM SẠCH CƠ BẢN ---
cat("\n📏 Kích thước dữ liệu gốc (hàng x cột):\n"); print(dim(data))

# 3.1. Chuẩn hóa tên cột (sang chữ thường và xóa khoảng trắng)
names(data) <- tolower(trimws(names(data)))

# 3.2. Xử lý giá trị khuyết (Imputation bằng Mean cho cột số)
if (anyNA(data)) {
  cat("\n⚠️ Phát hiện và tiến hành Imputation cho dữ liệu khuyết.\n")
  for (cot in names(data)) {
    if (is.numeric(data[[cot]])) {
      data[[cot]][is.na(data[[cot]])] <- mean(data[[cot]], na.rm = TRUE)
    }
  }
} else {
  cat("\n✅ Dữ liệu không có giá trị khuyết (NA).\n")
}

# 3.3. Xử lý kiểu dữ liệu (Chuyển character sang factor nếu cần)
data <- data %>%
  mutate(across(where(is.character), as.factor))

# 3.4. Loại bỏ cột định danh không cần thiết (Giữ nguyên các biến gốc theo khuyến nghị)
if ("id" %in% names(data)) data$id <- NULL

# 4. --- KỸ THUẬT TẠO BIẾN (FEATURE ENGINEERING) ---

# 4.1. Biến Chiến lược 1: Tỷ lệ Nước/Xi măng (w/c ratio)
# Yếu tố vật lý quan trọng nhất quyết định cường độ bê tông.
if (all(c("water", "cement") %in% names(data))) {
  data$ratio_water_cement <- round(data$water / data$cement, 3)
}

# 4.2. Biến Trung gian: Tổng khối lượng các vật liệu rắn và bột
# Tên biến được sửa để tránh nhầm lẫn với Cốt liệu (aggregates)
if (all(c("cement", "slag", "ash", "fineagg", "coarseagg") %in% names(data))) {
  data$total_solids_and_powders <- data$cement + data$slag + data$ash +
    data$fineagg + data$coarseagg
}

# 4.3. Biến Chiến lược 2 & 3: Phân số Khối lượng (Proportions)
# Tạo tỷ lệ thành phần so với tổng khối lượng khô để tăng tính tương quan.
if ("total_solids_and_powders" %in% names(data)) {
  data$cement_fraction <- round(data$cement / data$total_solids_and_powders, 3)
  data$water_fraction <- round(data$water / data$total_solids_and_powders, 3)
}

# 4.4. Biến Logarit cho Mục tiêu (Target Transformation)
# Giúp chuẩn hóa phân phối và ổn định phương sai cho Strength (Cường độ).
if ("strength" %in% names(data)) {
  data$log_strength <- log1p(data$strength)
}

# 5. --- XUẤT FILE ĐÃ TIỀN XỬ LÝ ---
cat("\n📝 Cấu trúc dữ liệu sau Feature Engineering:\n"); str(data)

# Cảnh báo: Sử dụng đường dẫn tuyệt đối có thể gây lỗi trên máy tính khác.
# Đảm bảo thư mục đích tồn tại!
tryCatch({
  write.csv(data, "D:\\HCMUT\\Probability and Stastistics\\Assignment\\Output\\data_cleaned.csv", row.names = FALSE)
  cat("\n✅ HOÀN TẤT: Dữ liệu đã được tiền xử lý và lưu tại: data_cleaned.csv\n")
  cat("   (Kiểm tra thư mục Output để xem kết quả)\n")
}, error = function(e) {
  cat(paste("\n❌ LỖI KHI XUẤT FILE: Vui lòng kiểm tra xem đường dẫn thư mục có tồn tại không:\n", e$message, "\n"))
})

# =================================================================