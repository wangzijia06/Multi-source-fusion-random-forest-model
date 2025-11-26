library(terra)
library(raster)
library(glcm)
library(dplyr)
library(sf)

# --- 1. 光学特征提取函数 ---
extract_optical_features <- function(img_path, roi_extent) {
  # 读取并裁剪
  img <- rast(img_path)
  # 建议根据实际波段顺序修改此处名称
  names(img) <- c("SWIR", "NIR", "Red", "Green", "Blue") 
  img_crop <- crop(img, roi_extent)
  
  # 提取单波段
  Blue  <- img_crop$Blue
  Green <- img_crop$Green
  Red   <- img_crop$Red
  NIR   <- img_crop$NIR
  SWIR  <- img_crop$SWIR
  
  # 计算指数
  NDWI  <- (Green - NIR) / (Green + NIR)
  SIBI  <- (NIR - Blue) / (NIR + Blue)
  BG    <- Blue / Green
  BR    <- Blue / Red
  BN    <- Blue / NIR
  LBG   <- log(Blue) / log(Green)
  MNDWI <- (Green - SWIR) / (Green + SWIR)
  
  # 组合特征堆栈
  feats <- c(Blue, Green, Red, NIR, SWIR, NDWI, SIBI, BG, BR, BN, LBG, MNDWI)
  names(feats) <- c("Blue", "Green", "Red", "NIR", "SWIR", 
                    "NDWI", "SIBI", "BG", "BR", "BN", "LBG", "MNDWI")
  return(feats)
}

# --- 2. SAR GLCM 纹理特征提取函数 ---
extract_sar_features <- function(sar_path, template_raster) {
  # 读取并对齐
  sar_raw <- rast(sar_path)
  sar_aligned <- crop(sar_raw, template_raster)
  sar_aligned <- resample(sar_aligned, template_raster, method = "bilinear")
  
  # 定义内部函数：计算单极化波段纹理
  calc_glcm <- function(raster_layer, prefix) {
    # glcm 包需要 raster 对象
    r_layer <- raster(raster_layer) 
    glcm_res <- glcm(r_layer, 
                     window = c(3, 3), 
                     statistics = c("mean", "variance", "dissimilarity", 
                                    "contrast", "homogeneity", "entropy", 
                                    "second_moment"))
    ras_out <- rast(glcm_res)
    names(ras_out) <- paste0(prefix, "_", names(ras_out))
    return(ras_out)
  }
  
  # 分别处理 VV 和 VH (假设波段1为VH, 波段2为VV，需根据实际情况确认)
  vh_layer <- sar_aligned[[1]]
  vv_layer <- sar_aligned[[2]]
  
  vh_feats <- c(vh_layer, calc_glcm(vh_layer, "VH"))
  vv_feats <- c(vv_layer, calc_glcm(vv_layer, "VV"))
  
  names(vh_feats)[1] <- "VH"
  names(vv_feats)[1] <- "VV"
  
  return(c(vv_feats, vh_feats))
}