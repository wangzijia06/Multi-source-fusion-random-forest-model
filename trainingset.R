# --- 定义感兴趣区域 (ROI) ---
roi_extent <- ext(-67.701, -67.692, 44.479, 44.488) # 示例坐标

# --- 处理第一组数据 (Time 1) ---
opt_t1 <- extract_optical_features("path/to/optical_1.tif", roi_extent)
sar_t1 <- extract_sar_features("path/to/sar_1.tif", opt_t1)
# 假设已通过 process_tide_data 函数处理好潮汐栅格
tide_t1 <- rast("path/to/tide_1.tif") 
tide_t1 <- resample(tide_t1, opt_t1)

stack_t1 <- c(opt_t1, sar_t1, tide_t1)

# --- 处理第二组数据 (Time 2 - Sample Transfer) ---
opt_t2 <- extract_optical_features("path/to/optical_2.tif", roi_extent)
sar_t2 <- extract_sar_features("path/to/sar_22.tif", opt_t1) # 保持几何一致
tide_t2 <- rast("path/to/tide_2.tif")
tide_t2 <- resample(tide_t2, opt_t1)

stack_t2 <- c(opt_t2, sar_t2, tide_t2)

# --- 提取训练点 (从 ICESat-2 或 测深点) ---
train_shp <- st_read("path/to/training_points.shp")
# 确保坐标系一致
if (st_crs(train_shp) != st_crs(stack_t1)) {
  train_shp <- st_transform(train_shp, st_crs(stack_t1))
}

# 分别从两个时相提取特征，并合并 (关键步骤：增加样本多样性)
extract_samples <- function(stack, points, depth_col) {
  vals <- extract(stack, points)
  vals$depth <- points[[depth_col]] # 添加真实水深标签
  return(na.omit(vals))
}

df_t1 <- extract_samples(stack_t1, train_shp, "Field3")
df_t2 <- extract_samples(stack_t2, train_shp, "Field3")

# 合并所有时相的训练数据
final_train_df <- rbind(df_t1, df_t2)
# 移除 ID 列
final_train_df <- final_train_df[, !names(final_train_df) %in% c("ID")]