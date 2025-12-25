# ===== LOAD OPTIMIZED DATA =====

library(tidyverse)
library(arrow)
options(dplyr.summarise.inform = FALSE)

# Load optimized data
df <- read_parquet("brfss_data_optimized.parquet")

# Load question hierarchies
layerQ <- read_parquet("layerQ_optimized.parquet")
justQ <- read_parquet("justQ_optimized.parquet")

# Clean up memory
gc(verbose = FALSE)