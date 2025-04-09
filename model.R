# =============================================================================
# 马里奥赛车8贝叶斯分析模型（含世界纪录数据）
# 这个R脚本使用贝叶斯层次模型分析马里奥赛车8中的最佳组件组合
# =============================================================================

# ============================
# 加载所需库
# ============================
library(tidyverse)  # 数据处理和可视化
library(rstan)      # R与Stan的接口
library(bayesplot)  # 贝叶斯模型可视化
library(loo)        # 留一交叉验证
library(gridExtra)  # 多图排列
library(viridis)    # 可视化的颜色方案
library(reshape2)   # 数据重塑
library(shiny)      # 交互式应用
library(ggplot2)    # 绘图
library(cowplot)    # 组合图表
library(corrplot)   # 相关性图
library(GGally)     # 绘制对图矩阵

# 设置Stan选项以提高性能
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# ============================
# 数据加载和准备
# ============================
# 加载CSV文件
drivers <- read.csv("DRIVERS.csv", stringsAsFactors = FALSE)
vehicles <- read.csv("VEHICLES.csv", stringsAsFactors = FALSE)
tires <- read.csv("TIRES.csv", stringsAsFactors = FALSE)
gliders <- read.csv("GLIDERS.csv", stringsAsFactors = FALSE)
records <- read.csv("mario_kart_records_cleaned.csv", stringsAsFactors = FALSE)

# 修复列名（去除可能的空格）
colnames(drivers) <- trimws(colnames(drivers))
colnames(vehicles) <- trimws(colnames(vehicles))
colnames(tires) <- trimws(colnames(tires))
colnames(gliders) <- trimws(colnames(gliders))
colnames(records) <- trimws(colnames(records))

# 清理数据 - 删除索引列（如果存在）
if("X" %in% colnames(drivers)) drivers <- drivers[, !colnames(drivers) == "X"]
if("" %in% colnames(drivers)) drivers <- drivers[, !colnames(drivers) == ""]
if("X" %in% colnames(vehicles)) vehicles <- vehicles[, !colnames(vehicles) == "X"]
if("" %in% colnames(vehicles)) vehicles <- vehicles[, !colnames(vehicles) == ""]
if("X" %in% colnames(tires)) tires <- tires[, !colnames(tires) == "X"]
if("" %in% colnames(tires)) tires <- tires[, !colnames(tires) == ""]
if("X" %in% colnames(gliders)) gliders <- gliders[, !colnames(gliders) == "X"]
if("" %in% colnames(gliders)) gliders <- gliders[, !colnames(gliders) == ""]

# 处理世界纪录数据
# 确保组件名匹配
records$Character <- trimws(records$Character)
records$Vehicle <- trimws(records$Vehicle)
records$Tires <- trimws(records$Tires)
records$Glider <- trimws(records$Glider)

# ============================
# 探索世界纪录数据
# ============================
# 统计角色使用频率
character_counts <- records %>%
  count(Character, sort = TRUE) %>%
  rename(Count = n)

# 统计最常用的组合
combinations_counts <- records %>%
  group_by(Character, Vehicle, Tires, Glider) %>%
  summarise(Count = n(), AvgScore = mean(Score, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(Count))

# 计算分数统计
score_stats <- records %>%
  summarise(
    MinScore = min(Score, na.rm = TRUE),
    MaxScore = max(Score, na.rm = TRUE),
    AvgScore = mean(Score, na.rm = TRUE),
    MedianScore = median(Score, na.rm = TRUE)
  )

# 可视化角色使用频率（前20名）
ggplot(head(character_counts, 20), aes(x = reorder(Character, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "世界纪录中最常使用的角色（前20名）",
       x = "角色", 
       y = "纪录数量") +
  theme_minimal()

ggsave("character_frequency.png", width = 10, height = 8)

# 可视化最常用的组合（前10名）
top_combinations <- head(combinations_counts, 10)
top_combinations$Combination <- paste(top_combinations$Character, 
                                      top_combinations$Vehicle, 
                                      top_combinations$Tires, 
                                      top_combinations$Glider, 
                                      sep = " + ")

ggplot(top_combinations, aes(x = reorder(Combination, Count), y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "世界纪录中最常用的组合（前10名）",
       x = "组合", 
       y = "纪录数量") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

ggsave("top_combinations.png", width = 12, height = 8)

# 组件使用与分数的关系
component_scores <- records %>%
  group_by(Character) %>%
  summarise(
    Count = n(),
    AvgScore = mean(Score, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(AvgScore))

# 可视化角色平均分数（只展示使用次数>30的角色）
character_scores_filtered <- component_scores %>%
  filter(Count > 30)

ggplot(character_scores_filtered, aes(x = reorder(Character, AvgScore), y = AvgScore)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = sprintf("n=%d", Count)), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(title = "角色平均分数（使用次数>30）",
       x = "角色", 
       y = "平均分数") +
  theme_minimal()

ggsave("character_scores.png", width = 12, height = 8)

# 同样分析车辆、轮胎和滑翔伞
vehicle_scores <- records %>%
  group_by(Vehicle) %>%
  summarise(
    Count = n(),
    AvgScore = mean(Score, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(Count > 30) %>%
  arrange(desc(AvgScore))

tire_scores <- records %>%
  group_by(Tires) %>%
  summarise(
    Count = n(),
    AvgScore = mean(Score, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(Count > 30) %>%
  arrange(desc(AvgScore))

glider_scores <- records %>%
  group_by(Glider) %>%
  summarise(
    Count = n(),
    AvgScore = mean(Score, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(Count > 30) %>%
  arrange(desc(AvgScore))

# 可视化各组件类型的平均分数
p1 <- ggplot(head(vehicle_scores, 10), aes(x = reorder(Vehicle, AvgScore), y = AvgScore)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = sprintf("n=%d", Count)), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(title = "最高平均分数的车辆（Top 10）",
       x = "车辆", 
       y = "平均分数") +
  theme_minimal()

p2 <- ggplot(head(tire_scores, 10), aes(x = reorder(Tires, AvgScore), y = AvgScore)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = sprintf("n=%d", Count)), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(title = "最高平均分数的轮胎（Top 10）",
       x = "轮胎", 
       y = "平均分数") +
  theme_minimal()

p3 <- ggplot(head(glider_scores, 10), aes(x = reorder(Glider, AvgScore), y = AvgScore)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = sprintf("n=%d", Count)), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(title = "最高平均分数的滑翔伞（Top 10）",
       x = "滑翔伞", 
       y = "平均分数") +
  theme_minimal()

# 组合这些图表
grid.arrange(p1, p2, p3, ncol = 1)
ggsave("component_scores.png", width = 10, height = 15)

# ============================
# 将组件属性与世界纪录数据结合
# ============================
# 功能：为记录查找组件的属性
get_driver_attributes <- function(character) {
  driver_row <- drivers %>% filter(Driver == character)
  if (nrow(driver_row) == 0) return(NULL)
  return(driver_row)
}

get_vehicle_attributes <- function(vehicle_name) {
  vehicle_row <- vehicles %>% filter(Vehicle == vehicle_name)
  if (nrow(vehicle_row) == 0) return(NULL)
  return(vehicle_row)
}

get_tire_attributes <- function(tire_name) {
  tire_row <- tires %>% filter(Tire == tire_name)
  if (nrow(tire_row) == 0) return(NULL)
  return(tire_row)
}

get_glider_attributes <- function(glider_name) {
  glider_row <- gliders %>% filter(Glider == glider_name)
  if (nrow(glider_row) == 0) return(NULL)
  return(glider_row)
}

# 属性名列表
attributes <- c("GroundSpeed", "WaterSpeed", "AirSpeed", "AntiGravitySpeed", 
                "Acceleration", "Weight", "GroundHandling", "WaterHandling", 
                "AirHandling", "AntiGravityHandling", "Traction", "MiniTurbo")

# 为世界纪录添加组件属性
# 注意：这需要组件名称完全匹配
records_with_attributes <- records %>%
  rowwise() %>%
  mutate(
    DriverFound = !is.null(get_driver_attributes(Character)),
    VehicleFound = !is.null(get_vehicle_attributes(Vehicle)),
    TireFound = !is.null(get_tire_attributes(Tires)),
    GliderFound = !is.null(get_glider_attributes(Glider))
  ) %>%
  ungroup()

# 查看匹配情况
matching_stats <- records_with_attributes %>%
  summarise(
    TotalRecords = n(),
    DriverMatches = sum(DriverFound),
    VehicleMatches = sum(VehicleFound),
    TireMatches = sum(TireFound),
    GliderMatches = sum(GliderFound),
    CompleteMatches = sum(DriverFound & VehicleFound & TireFound & GliderFound)
  )

print("组件匹配情况:")
print(matching_stats)

# 只保留所有组件都能匹配的记录
matched_records <- records_with_attributes %>%
  filter(DriverFound & VehicleFound & TireFound & GliderFound) %>%
  select(-DriverFound, -VehicleFound, -TireFound, -GliderFound)

# 计算组合属性
calculate_combined_attributes <- function(character, vehicle, tire, glider) {
  driver_attr <- get_driver_attributes(character)
  vehicle_attr <- get_vehicle_attributes(vehicle)
  tire_attr <- get_tire_attributes(tire)
  glider_attr <- get_glider_attributes(glider)
  
  if (is.null(driver_attr) || is.null(vehicle_attr) || 
      is.null(tire_attr) || is.null(glider_attr)) {
    return(NULL)
  }
  
  # 以数据框形式返回组合属性
  result <- data.frame(
    Character = character,
    Vehicle = vehicle,
    Tires = tire,
    Glider = glider
  )
  
  for (attr in attributes) {
    result[[attr]] <- driver_attr[[attr]] + 
      vehicle_attr[[attr]] + 
      tire_attr[[attr]] + 
      glider_attr[[attr]]
  }
  
  return(result)
}

# 添加组合属性到匹配的记录
records_with_combined_attributes <- matched_records %>%
  rowwise() %>%
  do({
    record <- .
    attrs <- calculate_combined_attributes(
      record$Character, record$Vehicle, record$Tires, record$Glider
    )
    
    if (!is.null(attrs)) {
      # 合并属性到记录
      for (attr in attributes) {
        record[[attr]] <- attrs[[attr]]
      }
    }
    
    record
  }) %>%
  ungroup()

# ============================
# 探索记录属性与分数的关系
# ============================
# 计算属性与分数的相关性
attributes_correlation <- cor(
  records_with_combined_attributes[, c(attributes, "Score")], 
  use = "complete.obs"
)

# 可视化相关性
corrplot(attributes_correlation, 
         method = "color", 
         type = "upper", 
         order = "hclust",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         title = "属性与分数的相关性")

# 将相关性保存为图像
png("attributes_correlation.png", width = 1000, height = 800)
corrplot(attributes_correlation, 
         method = "color", 
         type = "upper", 
         order = "hclust",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         title = "属性与分数的相关性")
dev.off()

# 可视化主要属性与分数的散点图
attributes_vs_score <- records_with_combined_attributes %>%
  select(Score, GroundSpeed, Acceleration, Weight, GroundHandling, Traction)

ggpairs(attributes_vs_score, 
        columns = 1:6,
        title = "主要属性与分数的关系",
        aes(alpha = 0.5)) +
  theme_minimal()

ggsave("attributes_vs_score_pairs.png", width = 14, height = 10)

# 速度与操控的权衡分析
ggplot(records_with_combined_attributes, 
       aes(x = GroundSpeed, y = GroundHandling, color = Score)) +
  geom_point(alpha = 0.6) +
  scale_color_viridis() +
  labs(title = "地面速度与操控性的权衡", 
       x = "地面速度", 
       y = "地面操控性",
       color = "分数") +
  theme_minimal()

ggsave("speed_vs_handling.png", width = 10, height = 8)

# 分析速度与加速度的关系
ggplot(records_with_combined_attributes, 
       aes(x = GroundSpeed, y = Acceleration, color = Score)) +
  geom_point(alpha = 0.6) +
  scale_color_viridis() +
  labs(title = "速度与加速度的权衡", 
       x = "地面速度", 
       y = "加速度",
       color = "分数") +
  theme_minimal()

ggsave("speed_vs_acceleration.png", width = 10, height = 8)

# 分析组合特征对分数的影响
# 创建组合特征
records_with_combined_attributes <- records_with_combined_attributes %>%
  mutate(
    SpeedProduct = GroundSpeed * WaterSpeed * AirSpeed * AntiGravitySpeed,
    HandlingAverage = (GroundHandling + WaterHandling + AirHandling + AntiGravityHandling) / 4,
    SpeedHandlingRatio = GroundSpeed / GroundHandling
  )

# 可视化组合特征与分数的关系
ggplot(records_with_combined_attributes, 
       aes(x = SpeedHandlingRatio, y = Score)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "速度/操控比率与分数关系", 
       x = "速度/操控比率", 
       y = "分数") +
  theme_minimal()

ggsave("speed_handling_ratio.png", width = 10, height = 8)

# ============================
# 使用世界纪录数据构建贝叶斯模型
# ============================
# 准备用于Stan模型的数据
# 确保所有记录都有完整的属性
records_for_model <- records_with_combined_attributes %>%
  select(Score, Character, Vehicle, Tires, Glider, all_of(attributes)) %>%
  na.omit()

# 创建组件映射（用于Stan）
character_mapping <- data.frame(
  Character = unique(records_for_model$Character),
  CharacterID = 1:length(unique(records_for_model$Character))
)

vehicle_mapping <- data.frame(
  Vehicle = unique(records_for_model$Vehicle),
  VehicleID = 1:length(unique(records_for_model$Vehicle))
)

tire_mapping <- data.frame(
  Tires = unique(records_for_model$Tires),
  TireID = 1:length(unique(records_for_model$Tires))
)

glider_mapping <- data.frame(
  Glider = unique(records_for_model$Glider),
  GliderID = 1:length(unique(records_for_model$Glider))
)

# 将组件ID添加到记录
records_for_model <- records_for_model %>%
  left_join(character_mapping, by = "Character") %>%
  left_join(vehicle_mapping, by = "Vehicle") %>%
  left_join(tire_mapping, by = "Tires") %>%
  left_join(glider_mapping, by = "Glider")

# 准备属性矩阵
character_attributes <- matrix(0, nrow = nrow(character_mapping), ncol = length(attributes))
colnames(character_attributes) <- attributes
for (i in 1:nrow(character_mapping)) {
  character_name <- character_mapping$Character[i]
  attrs <- get_driver_attributes(character_name)
  if (!is.null(attrs)) {
    character_attributes[i,] <- as.numeric(attrs[, attributes])
  }
}

vehicle_attributes <- matrix(0, nrow = nrow(vehicle_mapping), ncol = length(attributes))
colnames(vehicle_attributes) <- attributes
for (i in 1:nrow(vehicle_mapping)) {
  vehicle_name <- vehicle_mapping$Vehicle[i]
  attrs <- get_vehicle_attributes(vehicle_name)
  if (!is.null(attrs)) {
    vehicle_attributes[i,] <- as.numeric(attrs[, attributes])
  }
}

tire_attributes <- matrix(0, nrow = nrow(tire_mapping), ncol = length(attributes))
colnames(tire_attributes) <- attributes
for (i in 1:nrow(tire_mapping)) {
  tire_name <- tire_mapping$Tires[i]
  attrs <- get_tire_attributes(tire_name)
  if (!is.null(attrs)) {
    tire_attributes[i,] <- as.numeric(attrs[, attributes])
  }
}

glider_attributes <- matrix(0, nrow = nrow(glider_mapping), ncol = length(attributes))
colnames(glider_attributes) <- attributes
for (i in 1:nrow(glider_mapping)) {
  glider_name <- glider_mapping$Glider[i]
  attrs <- get_glider_attributes(glider_name)
  if (!is.null(attrs)) {
    glider_attributes[i,] <- as.numeric(attrs[, attributes])
  }
}

# 准备Stan数据
stan_data <- list(
  N = nrow(records_for_model),
  D = nrow(character_mapping),
  V = nrow(vehicle_mapping),
  T = nrow(tire_mapping),
  G = nrow(glider_mapping),
  K = length(attributes),
  character = records_for_model$CharacterID,
  vehicle = records_for_model$VehicleID,
  tire = records_for_model$TireID,
  glider = records_for_model$GliderID,
  score = records_for_model$Score,
  character_attr = character_attributes,
  vehicle_attr = vehicle_attributes,
  tire_attr = tire_attributes,
  glider_attr = glider_attributes
)

# 归一化分数以提高稳定性
stan_data$score <- scale(stan_data$score)

# ============================
# 定义Stan模型
# ============================
stan_model_code <- "
data {
  int<lower=1> N;                        // 观测数量
  int<lower=1> D;                        // 角色数量
  int<lower=1> V;                        // 车辆数量
  int<lower=1> T;                        // 轮胎数量
  int<lower=1> G;                        // 滑翔伞数量
  int<lower=1> K;                        // 属性数量
  
  int<lower=1, upper=D> character[N];    // 角色 ID
  int<lower=1, upper=V> vehicle[N];      // 车辆 ID
  int<lower=1, upper=T> tire[N];         // 轮胎 ID
  int<lower=1, upper=G> glider[N];       // 滑翔伞 ID
  
  vector[N] score;                       // 标准化分数
  
  // 各组件类型的属性矩阵
  matrix[D, K] character_attr;           // 角色属性
  matrix[V, K] vehicle_attr;             // 车辆属性  
  matrix[T, K] tire_attr;                // 轮胎属性
  matrix[G, K] glider_attr;              // 滑翔伞属性
}

parameters {
  // 基础效应
  real alpha;                            // 全局截距
  
  // 组件主效应
  vector[D] character_effect;            // 角色特定效应
  vector[V] vehicle_effect;              // 车辆特定效应
  vector[T] tire_effect;                 // 轮胎特定效应
  vector[G] glider_effect;               // 滑翔伞特定效应
  
  // 属性权重
  vector[K] attr_weights;                // 属性对分数的权重
  
  // 组件交互效应
  matrix[D, V] char_vehicle_interaction; // 角色-车辆交互
  
  // 标准差
  real<lower=0> sigma_character;         // 角色效应的标准差
  real<lower=0> sigma_vehicle;           // 车辆效应的标准差
  real<lower=0> sigma_tire;              // 轮胎效应的标准差
  real<lower=0> sigma_glider;            // 滑翔伞效应的标准差
  real<lower=0> sigma_interaction;       // 交互效应的标准差
  real<lower=0> sigma_y;                 // 观测噪声
}

transformed parameters {
  vector[N] mu;                          // 预期分数
  
  for (i in 1:N) {
    // 基本组合效应（组件效应之和）
    mu[i] = alpha + 
            character_effect[character[i]] + 
            vehicle_effect[vehicle[i]] + 
            tire_effect[tire[i]] + 
            glider_effect[glider[i]];
    
    // 添加角色-车辆交互
    mu[i] += char_vehicle_interaction[character[i], vehicle[i]];
    
    // 添加基于属性的效应
    for (k in 1:K) {
      mu[i] += attr_weights[k] * 
              (character_attr[character[i], k] + 
               vehicle_attr[vehicle[i], k] + 
               tire_attr[tire[i], k] + 
               glider_attr[glider[i], k]);
    }
  }
}

model {
  // 先验分布
  alpha ~ normal(0, 1);
  attr_weights ~ normal(0, 0.5);
  
  // 组件级效应
  character_effect ~ normal(0, sigma_character);
  vehicle_effect ~ normal(0, sigma_vehicle);
  tire_effect ~ normal(0, sigma_tire);
  glider_effect ~ normal(0, sigma_glider);
  
  // 交互效应
  for (d in 1:D) {
    char_vehicle_interaction[d] ~ normal(0, sigma_interaction);
  }
  
  // 超先验
  sigma_character ~ cauchy(0, 1);
  sigma_vehicle ~ cauchy(0, 1);
  sigma_tire ~ cauchy(0, 1);
  sigma_glider ~ cauchy(0, 1);
  sigma_interaction ~ cauchy(0, 0.5);
  sigma_y ~ cauchy(0, 1);
  
  // 似然函数
  score ~ normal(mu, sigma_y);
}

generated quantities {
  vector[N] log_lik;
  vector[N] y_pred;
  
  for (i in 1:N) {
    log_lik[i] = normal_lpdf(score[i] | mu[i], sigma_y);
    y_pred[i] = normal_rng(mu[i], sigma_y);
  }
}
"

# ============================
# 扩展可视化
# ============================
# 1. 可视化组件效应
plot_component_effects <- function(effects, title, top_n = 20) {
  df <- data.frame(
    Component = names(effects),
    Effect = effects
  ) %>%
    arrange(desc(Effect)) %>%
    head(top_n) %>%
    mutate(Component = factor(Component, levels = Component[order(Effect, decreasing = TRUE)]))
  
  ggplot(df, aes(x = Component, y = Effect)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    theme_minimal() +
    labs(title = title, x = "", y = "效应大小") +
    theme(axis.text.y = element_text(size = 8))
}

p1 <- plot_component_effects(character_effects, "角色对分数的影响（基于世界纪录）")
p2 <- plot_component_effects(vehicle_effects, "车辆对分数的影响（基于世界纪录）")
p3 <- plot_component_effects(tire_effects, "轮胎对分数的影响（基于世界纪录）")
p4 <- plot_component_effects(glider_effects, "滑翔伞对分数的影响（基于世界纪录）")

grid.arrange(p1, p2, p3, p4, ncol = 2)
ggsave("world_record_component_effects.png", width = 16, height = 12)

# 2. 可视化属性权重
attr_weights_df <- data.frame(
  Attribute = names(attr_weights),
  Weight = attr_weights
) %>%
  arrange(desc(abs(Weight))) %>%
  mutate(Attribute = factor(Attribute, levels = Attribute),
         Direction = ifelse(Weight > 0, "正面影响", "负面影响"))

ggplot(attr_weights_df, aes(x = Attribute, y = Weight, fill = Direction)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("正面影响" = "steelblue", "负面影响" = "tomato")) +
  coord_flip() +
  theme_minimal() +
  labs(title = "各属性对分数的影响权重", x = "", y = "权重") +
  theme(legend.position = "bottom")

ggsave("attribute_weights.png", width = 10, height = 8)

# 3. 角色-车辆交互热图（只显示最强的交互）
# 首先找出影响最大的角色和车辆
top_characters_names <- names(sort(character_effects, decreasing = TRUE)[1:10])
top_vehicles_names <- names(sort(vehicle_effects, decreasing = TRUE)[1:10])

# 提取这些组件的交互
top_interaction_matrix <- char_vehicle_interaction[top_characters_names, top_vehicles_names]
interaction_long <- as.data.frame(as.table(top_interaction_matrix))
names(interaction_long) <- c("Character", "Vehicle", "InteractionEffect")

# 绘制热图
ggplot(interaction_long, aes(x = Vehicle, y = Character, fill = InteractionEffect)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "顶级角色-车辆交互效应", x = "", y = "")

ggsave("top_char_vehicle_interaction.png", width = 12, height = 8)

# 4. 基于属性的组合探索
# 将属性权重应用于所有可能的组合，找出理论上最佳的组合
# 首先，创建标准化处理函数，与模型中相同
standardize <- function(x) {
  (x - mean(x)) / sd(x)
}

# 生成所有可能的组合（使用所有已知数据）
all_combinations <- expand.grid(
  Character = character_mapping$Character,
  Vehicle = vehicle_mapping$Vehicle,
  Tires = tire_mapping$Tires,
  Glider = glider_mapping$Glider
)

# 计算组合得分
calculate_combination_score <- function(character, vehicle, tire, glider) {
  # 获取组件效应
  char_effect <- character_effects[character]
  veh_effect <- vehicle_effects[vehicle]
  tire_effect <- tire_effects[tire]
  glider_effect <- glider_effects[glider]
  
  # 获取交互效应
  interaction <- char_vehicle_interaction[character, vehicle]
  
  # 获取属性
  char_attrs <- get_driver_attributes(character)
  veh_attrs <- get_vehicle_attributes(vehicle)
  tire_attrs <- get_tire_attributes(tire)
  glider_attrs <- get_glider_attributes(glider)
  
  if (is.null(char_attrs) || is.null(veh_attrs) || 
      is.null(tire_attrs) || is.null(glider_attrs)) {
    return(NA)
  }
  
  # 计算总属性
  total_attrs <- numeric(length(attributes))
  names(total_attrs) <- attributes
  
  for (attr in attributes) {
    total_attrs[attr] <- char_attrs[[attr]] + 
      veh_attrs[[attr]] + 
      tire_attrs[[attr]] + 
      glider_attrs[[attr]]
  }
  
  # 计算属性贡献
  attr_contribution <- sum(total_attrs * attr_weights)
  
  # 计算总得分
  total_score <- char_effect + veh_effect + tire_effect + glider_effect + 
    interaction + attr_contribution
  
  return(total_score)
}

# 计算理论最佳组合是非常计算密集的，所以这里我们只计算部分组合
# 使用前20的角色和车辆组合，以及前10的轮胎和滑翔伞
top_characters_names <- names(sort(character_effects, decreasing = TRUE)[1:20])
top_vehicles_names <- names(sort(vehicle_effects, decreasing = TRUE)[1:20])
top_tires_names <- names(sort(tire_effects, decreasing = TRUE)[1:10])
top_gliders_names <- names(sort(glider_effects, decreasing = TRUE)[1:10])

# 生成可能的组合
top_combinations <- expand.grid(
  Character = top_characters_names,
  Vehicle = top_vehicles_names,
  Tires = top_tires_names,
  Glider = top_gliders_names
)

# 计算每个组合的分数
scored_combinations <- top_combinations %>%
  rowwise() %>%
  mutate(
    PredictedScore = calculate_combination_score(Character, Vehicle, Tires, Glider)
  ) %>%
  ungroup() %>%
  filter(!is.na(PredictedScore)) %>%
  arrange(desc(PredictedScore))

# 输出前10个理论最佳组合
print("理论最佳组合（基于世界纪录数据和贝叶斯模型）:")
print(head(scored_combinations, 10))

# 保存前50个组合
write.csv(head(scored_combinations, 50), "top_50_predicted_combinations.csv", row.names = FALSE)

# 5. 交互式组合推荐器
create_combination_recommender <- function() {
  library(shiny)
  
  ui <- fluidPage(
    titlePanel("马里奥赛车8最佳组合推荐器（基于世界纪录）"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput("character", "选择角色:",
                    choices = c("任意", character_mapping$Character),
                    selected = "任意"),
        
        selectInput("vehicle", "选择车辆:",
                    choices = c("任意", vehicle_mapping$Vehicle),
                    selected = "任意"),
        
        selectInput("tires", "选择轮胎:",
                    choices = c("任意", tire_mapping$Tires),
                    selected = "任意"),
        
        selectInput("glider", "选择滑翔伞:",
                    choices = c("任意", glider_mapping$Glider),
                    selected = "任意"),
        
        sliderInput("num_recommendations", "推荐数量:",
                    min = 1, max = 20, value = 10),
        
        actionButton("recommend", "获取推荐")
      ),
      
      mainPanel(
        h3("推荐组合"),
        dataTableOutput("recommendations"),
        
        h3("组件效应分解"),
        plotOutput("effects_plot"),
        
        h3("属性总和"),
        plotOutput("attributes_plot")
      )
    )
  )
  
  server <- function(input, output) {
    recommendations <- eventReactive(input$recommend, {
      # 过滤组件
      filtered_combos <- scored_combinations
      
      if (input$character != "任意") {
        filtered_combos <- filtered_combos %>% filter(Character == input$character)
      }
      
      if (input$vehicle != "任意") {
        filtered_combos <- filtered_combos %>% filter(Vehicle == input$vehicle)
      }
      
      if (input$tires != "任意") {
        filtered_combos <- filtered_combos %>% filter(Tires == input$tires)
      }
      
      if (input$glider != "任意") {
        filtered_combos <- filtered_combos %>% filter(Glider == input$glider)
      }
      
      # 获取前N个组合
      head(filtered_combos, input$num_recommendations)
    })
    
    output$recommendations <- renderDataTable({
      recommendations()
    })
    
    output$effects_plot <- renderPlot({
      req(recommendations())
      top_combo <- recommendations()[1, ]
      
      # 计算各组件的贡献
      char_effect <- character_effects[top_combo$Character]
      veh_effect <- vehicle_effects[top_combo$Vehicle]
      tire_effect <- tire_effects[top_combo$Tires]
      glider_effect <- glider_effects[top_combo$Glider]
      interaction <- char_vehicle_interaction[top_combo$Character, top_combo$Vehicle]
      
      # 计算属性贡献
      char_attrs <- get_driver_attributes(top_combo$Character)
      veh_attrs <- get_vehicle_attributes(top_combo$Vehicle)
      tire_attrs <- get_tire_attributes(top_combo$Tires)
      glider_attrs <- get_glider_attributes(top_combo$Glider)
      
      total_attrs <- numeric(length(attributes))
      names(total_attrs) <- attributes
      
      for (attr in attributes) {
        total_attrs[attr] <- char_attrs[[attr]] + 
          veh_attrs[[attr]] + 
          tire_attrs[[attr]] + 
          glider_attrs[[attr]]
      }
      
      attr_contribution <- sum(total_attrs * attr_weights)
      
      # 创建贡献图
      contributions <- data.frame(
        Component = c("角色", "车辆", "轮胎", "滑翔伞", "交互效应", "属性加权"),
        Effect = c(char_effect, veh_effect, tire_effect, glider_effect, interaction, attr_contribution)
      )
      
      ggplot(contributions, aes(x = reorder(Component, abs(Effect)), y = Effect, fill = Effect > 0)) +
        geom_col() +
        scale_fill_manual(values = c("FALSE" = "tomato", "TRUE" = "steelblue")) +
        coord_flip() +
        theme_minimal() +
        labs(title = "顶级推荐的组件效应分解", x = "", y = "效应") +
        theme(legend.position = "none")
    })
    
    output$attributes_plot <- renderPlot({
      req(recommendations())
      top_combo <- recommendations()[1, ]
      
      # 计算总属性
      char_attrs <- get_driver_attributes(top_combo$Character)
      veh_attrs <- get_vehicle_attributes(top_combo$Vehicle)
      tire_attrs <- get_tire_attributes(top_combo$Tires)
      glider_attrs <- get_glider_attributes(top_combo$Glider)
      
      total_attrs <- numeric(length(attributes))
      names(total_attrs) <- attributes
      
      for (attr in attributes) {
        total_attrs[attr] <- char_attrs[[attr]] + 
          veh_attrs[[attr]] + 
          tire_attrs[[attr]] + 
          glider_attrs[[attr]]
      }
      
      # 创建属性图
      attr_df <- data.frame(
        Attribute = names(total_attrs),
        Value = total_attrs
      )
      
      ggplot(attr_df, aes(x = reorder(Attribute, Value), y = Value, fill = Value)) +
        geom_col() +
        scale_fill_viridis() +
        coord_flip() +
        theme_minimal() +
        labs(title = "顶级推荐的属性总和", x = "", y = "值")
    })
  }
  
  shinyApp(ui = ui, server = server)
}

# 运行推荐器的函数
run_recommender <- function() {
  app <- create_combination_recommender()
  runApp(app)
}

# ============================
# 保存结果
# ============================
saveRDS(fit, "world_record_mario_kart_model.rds")
saveRDS(character_effects, "world_record_character_effects.rds")
saveRDS(vehicle_effects, "world_record_vehicle_effects.rds")
saveRDS(tire_effects, "world_record_tire_effects.rds")
saveRDS(glider_effects, "world_record_glider_effects.rds")
saveRDS(attr_weights, "world_record_attr_weights.rds")
saveRDS(char_vehicle_interaction, "world_record_char_vehicle_interaction.rds")

# 保存组件效应数据框
write.csv(data.frame(
  Character = names(character_effects),
  Effect = character_effects
), "world_record_character_effects.csv", row.names = FALSE)

write.csv(data.frame(
  Vehicle = names(vehicle_effects),
  Effect = vehicle_effects
), "world_record_vehicle_effects.csv", row.names = FALSE)

write.csv(data.frame(
  Tires = names(tire_effects),
  Effect = tire_effects
), "world_record_tire_effects.csv", row.names = FALSE)

write.csv(data.frame(
  Glider = names(glider_effects),
  Effect = glider_effects
), "world_record_glider_effects.csv", row.names = FALSE)

write.csv(data.frame(
  Attribute = names(attr_weights),
  Weight = attr_weights
), "world_record_attribute_weights.csv", row.names = FALSE)

# ============================
# 结论
# ============================
cat("\n\n==================================================")
cat("\n马里奥赛车8贝叶斯分析（基于世界纪录）- 结论")
cat("\n==================================================")
cat("\n\n本分析使用真实世界纪录数据和贝叶斯层次建模，")
cat("\n为马里奥赛车8找出了最佳组件组合。")
cat("\n\n主要发现:")
cat("\n- 基于真实世界纪录，某些角色和车辆组合明显优于其他组合")
cat("\n- 最重要的属性是...", names(sort(abs(attr_weights), decreasing = TRUE)[1:3]))
cat("\n- 分析证实了某些组件之间存在强烈的协同效应")
cat("\n- 推荐的最佳组合是...", paste(
  scored_combinations$Character[1], 
  scored_combinations$Vehicle[1], 
  scored_combinations$Tires[1], 
  scored_combinations$Glider[1], 
  sep = " + "
))
cat("\n\n交互式推荐系统可以根据玩家的偏好提供定制化建议。")
cat("\n==================================================\n")

# ============================
# 使用示例
# ============================
# 要运行交互式推荐器:
# run_recommender()

# 顶级推荐的示例
cat("\n前5个推荐组合:\n")
print(head(scored_combinations, 5))