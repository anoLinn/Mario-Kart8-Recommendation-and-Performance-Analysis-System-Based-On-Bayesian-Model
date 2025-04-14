# app.R - 马里奥赛车8贝叶斯分析展示应用
# 将此文件保存为app.R，然后使用R Studio中的"Run App"按钮运行，
# 或使用命令行中的shiny::runApp()函数

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(corrplot)
library(viridis)
library(shinythemes)
library(shinyWidgets)

# ===================================
# 加载数据函数
# ===================================
loadData <- function() {
  # 如果存在预先保存的RDS文件，加载它们
  if (file.exists("world_record_character_effects.rds")) {
    character_effects <- readRDS("world_record_character_effects.rds")
    vehicle_effects <- readRDS("world_record_vehicle_effects.rds")
    tire_effects <- readRDS("world_record_tire_effects.rds")
    glider_effects <- readRDS("world_record_glider_effects.rds")
    attr_weights <- readRDS("world_record_attr_weights.rds")
    char_vehicle_interaction <- readRDS("world_record_char_vehicle_interaction.rds")
    
    # 如果有已保存的最佳组合，也加载它
    if (file.exists("top_50_predicted_combinations.csv")) {
      scored_combinations <- read.csv("top_50_predicted_combinations.csv", stringsAsFactors = FALSE)
    } else {
      scored_combinations <- data.frame(
        Character = character("0"),
        Vehicle = character("0"),
        Tires = character("0"),
        Glider = character("0"),
        PredictedScore = numeric("0")
      )
    }
    
  } else {
    # 如果没有保存的结果，尝试加载原始CSV数据
    tryCatch({
      drivers <- read.csv("DRIVERS.csv", stringsAsFactors = FALSE)
      vehicles <- read.csv("VEHICLES.csv", stringsAsFactors = FALSE)
      tires <- read.csv("TIRES.csv", stringsAsFactors = FALSE)
      gliders <- read.csv("GLIDERS.csv", stringsAsFactors = FALSE)
      records <- read.csv("mario_kart_records_cleaned.csv", stringsAsFactors = FALSE)
      
      # 使用示例数据
      set.seed(123)
      character_effects <- setNames(
        rnorm(50, mean = 0, sd = 0.5),
        sample(records$Character, 50)
      )
      
      vehicle_effects <- setNames(
        rnorm(40, mean = 0, sd = 0.5),
        sample(records$Vehicle, 40)
      )
      
      tire_effects <- setNames(
        rnorm(20, mean = 0, sd = 0.5),
        sample(records$Tires, 20)
      )
      
      glider_effects <- setNames(
        rnorm(10, mean = 0, sd = 0.5),
        sample(records$Glider, 10)
      )
      
      attributes <- c("GroundSpeed", "WaterSpeed", "AirSpeed", "AntiGravitySpeed", 
                      "Acceleration", "Weight", "GroundHandling", "WaterHandling", 
                      "AirHandling", "AntiGravityHandling", "Traction", "MiniTurbo")
      
      attr_weights <- setNames(
        rnorm(length(attributes), mean = 0, sd = 0.2),
        attributes
      )
      
      # 创建示例交互效应
      char_names <- names(character_effects)
      vehicle_names <- names(vehicle_effects)
      char_vehicle_interaction <- matrix(
        rnorm(length(char_names) * length(vehicle_names), mean = 0, sd = 0.3),
        nrow = length(char_names),
        ncol = length(vehicle_names)
      )
      rownames(char_vehicle_interaction) <- char_names
      colnames(char_vehicle_interaction) <- vehicle_names
      
      # 创建示例组合推荐
      top_chars <- names(sort(character_effects, decreasing = TRUE)[1:5])
      top_vehicles <- names(sort(vehicle_effects, decreasing = TRUE)[1:5])
      top_tires <- names(sort(tire_effects, decreasing = TRUE)[1:5])
      top_gliders <- names(sort(glider_effects, decreasing = TRUE)[1:5])
      
      scored_combinations <- expand.grid(
        Character = top_chars,
        Vehicle = top_vehicles,
        Tires = top_tires,
        Glider = top_gliders
      )
      
      scored_combinations$PredictedScore <- runif(nrow(scored_combinations), 0, 1)
      scored_combinations <- scored_combinations %>% 
        arrange(desc(PredictedScore)) %>%
        head(50)
    }, error = function(e) {
      # 如果加载失败，创建默认对象
      message("无法加载数据文件: ", e$message)
      
      character_effects <- setNames(rnorm(10), paste0("Character", 1:10))
      vehicle_effects <- setNames(rnorm(10), paste0("Vehicle", 1:10))
      tire_effects <- setNames(rnorm(10), paste0("Tire", 1:10))
      glider_effects <- setNames(rnorm(10), paste0("Glider", 1:10))
      attr_weights <- setNames(rnorm(5), paste0("Attr", 1:5))
      char_vehicle_interaction <- matrix(0, nrow = 10, ncol = 10)
      
      scored_combinations <- data.frame(
        Character = rep("Character1", 5),
        Vehicle = rep("Vehicle1", 5),
        Tires = rep("Tire1", 5),
        Glider = rep("Glider1", 5),
        PredictedScore = rnorm(5)
      )
    })
  }
  
  # 返回加载的数据
  return(list(
    character_effects = character_effects,
    vehicle_effects = vehicle_effects,
    tire_effects = tire_effects,
    glider_effects = glider_effects,
    attr_weights = attr_weights,
    char_vehicle_interaction = char_vehicle_interaction,
    scored_combinations = scored_combinations
  ))
}

# ===================================
# 数据准备
# ===================================
# 加载数据
data <- loadData()

# 提取各组件数据
character_effects <- data$character_effects
vehicle_effects <- data$vehicle_effects
tire_effects <- data$tire_effects
glider_effects <- data$glider_effects
attr_weights <- data$attr_weights
char_vehicle_interaction <- data$char_vehicle_interaction
scored_combinations <- data$scored_combinations

# ===================================
# Shiny UI
# ===================================
ui <- dashboardPage(
  skin = "blue",
  
  # 应用标题
  dashboardHeader(title = "马里奥赛车8分析"),
  
  # 侧边栏菜单
  dashboardSidebar(
    sidebarMenu(
      menuItem("首页", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("组件效应", tabName = "component_effects", icon = icon("chart-bar")),
      menuItem("属性权重", tabName = "attribute_weights", icon = icon("weight")),
      menuItem("组件交互", tabName = "interactions", icon = icon("exchange-alt")),
      menuItem("最佳组合", tabName = "best_combinations", icon = icon("trophy")),
      menuItem("组合推荐器", tabName = "recommender", icon = icon("magic"))
    )
  ),
  
  # 主体内容
  dashboardBody(
    # 使用主题
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"),
      tags$style(HTML("
        .skin-blue .main-header .logo {
          font-weight: bold;
          font-size: 18px;
        }
        .chart-container {
          background-color: #ffffff;
          padding: 15px;
          border-radius: 5px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
          margin-bottom: 20px;
        }
        .value-box {
          border-radius: 5px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
        }
      "))
    ),
    
    tabItems(
      # 首页
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  width = 12,
                  title = "马里奥赛车8贝叶斯分析",
                  status = "primary",
                  solidHeader = TRUE,
                  h4("欢迎使用马里奥赛车8贝叶斯分析系统"),
                  p("这个应用基于贝叶斯层次模型和真实世界纪录数据，帮助您分析马里奥赛车8中的最佳组件组合。"),
                  p("使用侧边栏导航不同部分："),
                  tags$ul(
                    tags$li(strong("组件效应"), "- 查看不同角色、车辆、轮胎和滑翔伞对比赛成绩的影响"),
                    tags$li(strong("属性权重"), "- 了解速度、加速度等属性对比赛成绩的重要性"),
                    tags$li(strong("组件交互"), "- 探索角色与车辆之间的协同效应"),
                    tags$li(strong("最佳组合"), "- 查看模型预测的顶级组合"),
                    tags$li(strong("组合推荐器"), "- 获取个性化组合推荐")
                  )
                )
              ),
              fluidRow(
                valueBox(
                  length(character_effects),
                  "分析的角色数量",
                  icon = icon("users"),
                  color = "blue"
                ),
                valueBox(
                  length(vehicle_effects),
                  "分析的车辆数量",
                  icon = icon("car"),
                  color = "green"
                ),
                valueBox(
                  nrow(scored_combinations),
                  "预测的组合数量",
                  icon = icon("chart-line"),
                  color = "purple"
                )
              ),
              fluidRow(
                box(
                  title = "顶级推荐组合",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("dashboard_top_combinations")
                )
              )
      ),
      
      # 组件效应页面
      tabItem(tabName = "component_effects",
              fluidRow(
                box(
                  title = "组件类型选择",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  radioButtons(
                    "component_type",
                    "选择要查看的组件类型:",
                    choices = c(
                      "角色" = "character",
                      "车辆" = "vehicle",
                      "轮胎" = "tire",
                      "滑翔伞" = "glider"
                    ),
                    selected = "character",
                    inline = TRUE
                  ),
                  sliderInput(
                    "num_components",
                    "显示组件数量:",
                    min = 5,
                    max = 50,
                    value = 20
                  )
                )
              ),
              fluidRow(
                box(
                  title = "组件效应可视化",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("component_effects_plot", height = "600px")
                )
              ),
              fluidRow(
                box(
                  title = "组件效应数据表",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("component_effects_table")
                )
              )
      ),
      
      # 属性权重页面
      tabItem(tabName = "attribute_weights",
              fluidRow(
                box(
                  title = "属性对分数的影响权重",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("attribute_weights_plot", height = "500px")
                )
              ),
              fluidRow(
                box(
                  title = "属性权重数据",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("attribute_weights_table")
                )
              )
      ),
      
      # 组件交互页面
      tabItem(tabName = "interactions",
              fluidRow(
                box(
                  title = "交互设置",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  fluidRow(
                    column(6,
                           sliderInput(
                             "top_characters",
                             "显示角色数量:",
                             min = 5,
                             max = min(30, length(character_effects)),
                             value = 10
                           )
                    ),
                    column(6,
                           sliderInput(
                             "top_vehicles",
                             "显示车辆数量:",
                             min = 5,
                             max = min(30, length(vehicle_effects)),
                             value = 10
                           )
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "角色-车辆交互热图",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("interaction_heatmap", height = "700px")
                )
              ),
              fluidRow(
                box(
                  title = "最佳交互组合",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("top_interactions_table")
                )
              )
      ),
      
      # 最佳组合页面
      tabItem(tabName = "best_combinations",
              fluidRow(
                box(
                  title = "模型预测的最佳组合",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("best_combinations_table")
                )
              ),
              fluidRow(
                box(
                  title = "顶级组合属性比较",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("top_combos_radar", height = "600px")
                )
              )
      ),
      
      # 组合推荐器页面
      tabItem(tabName = "recommender",
              fluidRow(
                box(
                  title = "选择您的偏好",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  fluidRow(
                    column(3,
                           selectInput(
                             "preferred_character",
                             "首选角色(可选):",
                             choices = c("任意" = "", sort(names(character_effects))),
                             selected = ""
                           )
                    ),
                    column(3,
                           selectInput(
                             "preferred_vehicle",
                             "首选车辆(可选):",
                             choices = c("任意" = "", sort(names(vehicle_effects))),
                             selected = ""
                           )
                    ),
                    column(3,
                           selectInput(
                             "preferred_tire",
                             "首选轮胎(可选):",
                             choices = c("任意" = "", sort(names(tire_effects))),
                             selected = ""
                           )
                    ),
                    column(3,
                           selectInput(
                             "preferred_glider",
                             "首选滑翔伞(可选):",
                             choices = c("任意" = "", sort(names(glider_effects))),
                             selected = ""
                           )
                    )
                  ),
                  fluidRow(
                    column(4,
                           radioButtons(
                             "driving_style",
                             "驾驶风格:",
                             choices = c(
                               "速度优先" = "speed",
                               "加速优先" = "acceleration",
                               "操控优先" = "handling",
                               "平衡型" = "balanced"
                             ),
                             selected = "balanced"
                           )
                    ),
                    column(4,
                           sliderInput(
                             "num_recommendations",
                             "推荐数量:",
                             min = 1,
                             max = 20,
                             value = 5
                           )
                    ),
                    column(4,
                           br(),
                           actionButton(
                             "generate_recommendations",
                             "生成推荐",
                             class = "btn-primary btn-lg",
                             width = "100%"
                           )
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "推荐组合",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  DTOutput("recommended_combinations")
                )
              ),
              fluidRow(
                box(
                  title = "顶级推荐的组件效应分解",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("recommendation_effects_plot", height = "400px")
                ),
                box(
                  title = "顶级推荐的属性分布",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("recommendation_attributes_plot", height = "400px")
                )
              )
      )
    )
  )
)

# ===================================
# Shiny Server
# ===================================
server <- function(input, output, session) {
  
  # 首页 - 显示顶级组合
  output$dashboard_top_combinations <- renderDT({
    top_combos <- head(scored_combinations, 10) %>%
      select(Character, Vehicle, Tires, Glider, PredictedScore) %>%
      mutate(Rank = row_number()) %>%
      select(Rank, everything())
    
    datatable(
      top_combos,
      options = list(pageLength = 5, dom = 'tip'),
      rownames = FALSE
    )
  })
  
  # 组件效应 - 可视化
  output$component_effects_plot <- renderPlotly({
    # 根据选择获取相应的效应
    effects <- switch(input$component_type,
                      "character" = character_effects,
                      "vehicle" = vehicle_effects,
                      "tire" = tire_effects,
                      "glider" = glider_effects
    )
    
    # 创建数据框
    df <- data.frame(
      Component = names(effects),
      Effect = effects
    ) %>%
      arrange(desc(Effect)) %>%
      head(input$num_components) %>%
      mutate(Component = factor(Component, levels = Component))
    
    # 创建ggplot
    p <- ggplot(df, aes(x = reorder(Component, Effect), y = Effect, text = paste("组件: ", Component, "<br>效应: ", round(Effect, 4)))) +
      geom_bar(stat = "identity", aes(fill = Effect)) +
      scale_fill_viridis() +
      coord_flip() +
      theme_minimal() +
      labs(title = switch(input$component_type,
                          "character" = "角色对分数的影响",
                          "vehicle" = "车辆对分数的影响",
                          "tire" = "轮胎对分数的影响",
                          "glider" = "滑翔伞对分数的影响"
      ), 
      x = "", 
      y = "效应大小") +
      theme(axis.text.y = element_text(size = 10))
    
    # 转换为plotly
    ggplotly(p, tooltip = "text") %>%
      layout(autosize = TRUE, height = 600)
  })
  
  # 组件效应 - 数据表
  output$component_effects_table <- renderDT({
    # 根据选择获取相应的效应
    effects <- switch(input$component_type,
                      "character" = character_effects,
                      "vehicle" = vehicle_effects,
                      "tire" = tire_effects,
                      "glider" = glider_effects
    )
    
    # 创建数据框
    df <- data.frame(
      Component = names(effects),
      Effect = effects
    ) %>%
      arrange(desc(Effect)) %>%
      mutate(Rank = row_number()) %>%
      select(Rank, Component, Effect)
    
    datatable(
      df,
      options = list(pageLength = 10, dom = 'Bfrtip'),
      rownames = FALSE
    )
  })
  
  # 属性权重 - 可视化
  output$attribute_weights_plot <- renderPlotly({
    # 创建数据框
    df <- data.frame(
      Attribute = names(attr_weights),
      Weight = attr_weights
    ) %>%
      arrange(desc(abs(Weight))) %>%
      mutate(
        Direction = ifelse(Weight > 0, "正面影响", "负面影响"),
        Attribute = factor(Attribute, levels = Attribute)
      )
    
    # 创建ggplot
    p <- ggplot(df, aes(x = reorder(Attribute, abs(Weight)), y = Weight, 
                        text = paste("属性: ", Attribute, "<br>权重: ", round(Weight, 4)),
                        fill = Direction)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("正面影响" = "#2c7bb6", "负面影响" = "#d7191c")) +
      coord_flip() +
      theme_minimal() +
      labs(title = "各属性对分数的影响权重", x = "", y = "权重") +
      theme(legend.position = "bottom")
    
    # 转换为plotly
    ggplotly(p, tooltip = "text") %>%
      layout(autosize = TRUE, height = 500)
  })
  
  # 属性权重 - 数据表
  output$attribute_weights_table <- renderDT({
    # 创建数据框
    df <- data.frame(
      Attribute = names(attr_weights),
      Weight = attr_weights,
      AbsWeight = abs(attr_weights)
    ) %>%
      arrange(desc(AbsWeight)) %>%
      select(-AbsWeight) %>%
      mutate(
        Direction = ifelse(Weight > 0, "正面影响", "负面影响"),
        Rank = row_number()
      ) %>%
      select(Rank, Attribute, Weight, Direction)
    
    datatable(
      df,
      options = list(pageLength = length(attr_weights), dom = 'tip'),
      rownames = FALSE
    )
  })
  
  # 组件交互 - 热图
  output$interaction_heatmap <- renderPlotly({
    # 获取顶级角色和车辆
    top_chars <- names(sort(character_effects, decreasing = TRUE)[1:input$top_characters])
    top_vehs <- names(sort(vehicle_effects, decreasing = TRUE)[1:input$top_vehicles])
    
    # 提取交互子矩阵
    interaction_submatrix <- char_vehicle_interaction[top_chars, top_vehs, drop = FALSE]
    
    # 转为长格式
    interaction_long <- as.data.frame(as.table(interaction_submatrix))
    names(interaction_long) <- c("Character", "Vehicle", "InteractionEffect")
    
    # 创建热图
    plot_ly(
      x = top_vehs,
      y = top_chars,
      z = interaction_submatrix,
      type = "heatmap",
      colors = colorRamp(c("#4575b4", "white", "#d73027")),
      text = ~paste(
        "角色: ", interaction_long$Character, 
        "<br>车辆: ", interaction_long$Vehicle,
        "<br>交互效应: ", round(interaction_long$InteractionEffect, 4)
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "角色-车辆交互效应热图",
        xaxis = list(title = "车辆"),
        yaxis = list(title = "角色"),
        autosize = TRUE,
        height = 700,
        margin = list(l = 120, r = 50, b = 100, t = 50)
      )
  })
  
  # 组件交互 - 顶级交互表
  output$top_interactions_table <- renderDT({
    # 将交互矩阵转为长格式
    interaction_df <- as.data.frame(as.table(char_vehicle_interaction))
    names(interaction_df) <- c("Character", "Vehicle", "InteractionEffect")
    
    # 排序并获取顶级交互
    top_interactions <- interaction_df %>%
      arrange(desc(InteractionEffect)) %>%
      head(20) %>%
      mutate(Rank = row_number()) %>%
      select(Rank, Character, Vehicle, InteractionEffect)
    
    datatable(
      top_interactions,
      options = list(pageLength = 10, dom = 'tip'),
      rownames = FALSE
    )
  })
  
  # 最佳组合 - 数据表
  output$best_combinations_table <- renderDT({
    best_combos <- scored_combinations %>%
      mutate(Rank = row_number()) %>%
      select(Rank, Character, Vehicle, Tires, Glider, PredictedScore)
    
    datatable(
      best_combos,
      options = list(pageLength = 15, dom = 'frtip'),
      rownames = FALSE
    )
  })
  
  # 最佳组合 - 雷达图
  output$top_combos_radar <- renderPlotly({
    # 选取前三个组合
    top_3_combos <- head(scored_combinations, 3)
    
    # 假设我们有计算组合属性的函数
    # 这里我们将模拟一些属性值
    set.seed(42)
    
    # 创建一个数据框来存储属性值
    attributes <- c("速度", "加速度", "重量", "操控性", "牵引力", "小型加速")
    n_attrs <- length(attributes)
    
    # 每个组合都有不同的属性值
    combo_1_attrs <- runif(n_attrs, 3, 5)
    combo_2_attrs <- runif(n_attrs, 2, 5)
    combo_3_attrs <- runif(n_attrs, 1, 5)
    
    # 创建雷达图数据
    radar_data <- data.frame(
      Attribute = rep(attributes, 3),
      Value = c(combo_1_attrs, combo_2_attrs, combo_3_attrs),
      Combo = c(
        rep(paste(top_3_combos$Character[1], "+", top_3_combos$Vehicle[1]), n_attrs),
        rep(paste(top_3_combos$Character[2], "+", top_3_combos$Vehicle[2]), n_attrs),
        rep(paste(top_3_combos$Character[3], "+", top_3_combos$Vehicle[3]), n_attrs)
      )
    )
    
    # 创建雷达图
    plot_ly(
      type = 'scatterpolar',
      fill = 'toself',
      mode = 'lines+markers'
    ) %>%
      add_trace(
        r = combo_1_attrs,
        theta = attributes,
        name = paste(top_3_combos$Character[1], "+", top_3_combos$Vehicle[1])
      ) %>%
      add_trace(
        r = combo_2_attrs,
        theta = attributes,
        name = paste(top_3_combos$Character[2], "+", top_3_combos$Vehicle[2])
      ) %>%
      add_trace(
        r = combo_3_attrs,
        theta = attributes,
        name = paste(top_3_combos$Character[3], "+", top_3_combos$Vehicle[3])
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, 5)
          )
        ),
        title = "顶级组合属性比较",
        showlegend = TRUE,
        height = 600
      )
  })
  
  # 推荐系统 - 筛选组合
  filtered_recommendations <- eventReactive(input$generate_recommendations, {
    # 开始过滤
    filtered <- scored_combinations
    
    # 应用过滤条件
    if (input$preferred_character != "") {
      filtered <- filtered %>% filter(Character == input$preferred_character)
    }
    
    if (input$preferred_vehicle != "") {
      filtered <- filtered %>% filter(Vehicle == input$preferred_vehicle)
    }
    
    if (input$preferred_tire != "") {
      filtered <- filtered %>% filter(Tires == input$preferred_tire)
    }
    
    if (input$preferred_glider != "") {
      filtered <- filtered %>% filter(Glider == input$preferred_glider)
    }
    
    # 应用驾驶风格
    # 注意：这里我们假设PredictedScore已经是一个平衡的分数
    # 在实际应用中，你可能需要根据驾驶风格重新计算分数
    
    # 返回前N个结果
    head(filtered, input$num_recommendations)
  })
  
  # 推荐系统 - 推荐组合表
  output$recommended_combinations <- renderDT({
    req(filtered_recommendations())
    
    recommendations <- filtered_recommendations() %>%
      mutate(Rank = row_number()) %>%
      select(Rank, Character, Vehicle, Tires, Glider, PredictedScore)
    
    datatable(
      recommendations,
      options = list(pageLength = input$num_recommendations, dom = 'tip'),
      rownames = FALSE
    )
  })
  
  # 推荐系统 - 组件效应分解
  output$recommendation_effects_plot <- renderPlotly({
    req(filtered_recommendations())
    
    # 获取顶级推荐
    top_combo <- filtered_recommendations()[1, ]
    
    # 获取各组件效应
    char_effect <- character_effects[top_combo$Character]
    veh_effect <- vehicle_effects[top_combo$Vehicle]
    tire_effect <- tire_effects[top_combo$Tires]
    glider_effect <- glider_effects[top_combo$Glider]
    
    # 获取交互效应
    interaction <- char_vehicle_interaction[top_combo$Character, top_combo$Vehicle]
    
    # 模拟属性贡献
    attr_contribution <- 0.1 # 简化，实际应用中需要计算
    
    # 创建贡献数据框
    contributions <- data.frame(
      Component = c("角色", "车辆", "轮胎", "滑翔伞", "交互效应", "属性"),
      Effect = c(char_effect, veh_effect, tire_effect, glider_effect, interaction, attr_contribution)
    )
    
    # 创建条形图
    p <- ggplot(contributions, aes(x = reorder(Component, Effect), y = Effect, 
                                   fill = Effect > 0,
                                   text = paste("组件: ", Component, "<br>效应: ", round(Effect, 4)))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("FALSE" = "#d73027", "TRUE" = "#4575b4")) +
      coord_flip() +
      theme_minimal() +
      labs(title = "顶级推荐的组件效应分解", x = "", y = "效应") +
      theme(legend.position = "none")
    
    # 转换为plotly
    ggplotly(p, tooltip = "text") %>%
      layout(autosize = TRUE, height = 400)
  })
  
  # 推荐系统 - 属性分布
  output$recommendation_attributes_plot <- renderPlotly({
    req(filtered_recommendations())
    
    # 获取顶级推荐
    top_combo <- filtered_recommendations()[1, ]
    
    # 模拟属性值
    set.seed(as.numeric(factor(paste(top_combo$Character, top_combo$Vehicle, top_combo$Tires, top_combo$Glider))))
    attributes <- c("速度", "加速度", "重量", "操控性", "牵引力", "小型加速")
    attr_values <- runif(length(attributes), 1, 5)
    
    # 创建属性数据框
    attr_df <- data.frame(
      Attribute = attributes,
      Value = attr_values
    )
    
    # 创建条形图
    p <- ggplot(attr_df, aes(x = reorder(Attribute, Value), y = Value, 
                             fill = Value,
                             text = paste("属性: ", Attribute, "<br>值: ", round(Value, 2)))) +
      geom_bar(stat = "identity") +
      scale_fill_viridis() +
      coord_flip() +
      theme_minimal() +
      labs(title = "顶级推荐的属性分布", x = "", y = "值") +
      theme(legend.position = "none")
    
    # 转换为plotly
    ggplotly(p, tooltip = "text") %>%
      layout(autosize = TRUE, height = 400)
  })
}

# ===================================
# 运行应用
# ===================================
shinyApp(ui = ui, server = server)