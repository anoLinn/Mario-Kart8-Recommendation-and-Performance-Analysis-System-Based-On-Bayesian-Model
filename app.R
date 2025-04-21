# app.R
# Mario Kart 8 Recommendation Shiny App

library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(gridExtra)
library(grid)
library(viridis)
library(ggplot2)
library(reshape2)

# Load the saved model results
character_effects <- read.csv("world_record_character_effects.csv")
vehicle_effects <- read.csv("world_record_vehicle_effects.csv")
tire_effects <- read.csv("world_record_tire_effects.csv")
glider_effects <- read.csv("world_record_glider_effects.csv")
attribute_weights <- read.csv("world_record_attribute_weights.csv")
top_combinations <- read.csv("top_50_predicted_combinations.csv")

# Load datasets for attribute calculations
drivers <- read.csv("DRIVERS.csv", stringsAsFactors = FALSE)
vehicles <- read.csv("VEHICLES.csv", stringsAsFactors = FALSE)
tires <- read.csv("TIRES.csv", stringsAsFactors = FALSE)
gliders <- read.csv("GLIDERS.csv", stringsAsFactors = FALSE)

# Define attribute columns
attributes <- c("GroundSpeed", "WaterSpeed", "AirSpeed", "AntiGravitySpeed", 
                "Acceleration", "Weight", "GroundHandling", "WaterHandling", 
                "AirHandling", "AntiGravityHandling", "Traction", "MiniTurbo")

# Component lookup functions
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

# Calculate component combo attributes
calculate_combo_attributes <- function(character, vehicle, tire, glider) {
  driver_attr <- get_driver_attributes(character)
  vehicle_attr <- get_vehicle_attributes(vehicle)
  tire_attr <- get_tire_attributes(tire)
  glider_attr <- get_glider_attributes(glider)
  
  if (is.null(driver_attr) || is.null(vehicle_attr) || is.null(tire_attr) || is.null(glider_attr)) {
    return(NULL)
  }
  
  result <- numeric(length(attributes))
  names(result) <- attributes
  
  for (attr in attributes) {
    result[attr] <- driver_attr[[attr]] + vehicle_attr[[attr]] + tire_attr[[attr]] + glider_attr[[attr]]
  }
  
  return(result)
}

# Load character-vehicle interaction matrix if available
# If not available, create an empty matrix
create_interaction_matrix <- function() {
  if (file.exists("char_vehicle_interaction.csv")) {
    interaction_data <- read.csv("char_vehicle_interaction.csv", row.names = 1)
    interaction_matrix <- as.matrix(interaction_data)
  } else {
    chars <- character_effects$Character
    vehs <- vehicle_effects$Vehicle
    interaction_matrix <- matrix(0, nrow = length(chars), ncol = length(vehs))
    rownames(interaction_matrix) <- chars
    colnames(interaction_matrix) <- vehs
  }
  
  return(interaction_matrix)
}

char_vehicle_interaction <- create_interaction_matrix()

# Calculate combination score based on model
calculate_combination_score <- function(character, vehicle, tire, glider) {
  # Check if all components exist in the effects data
  if (!(character %in% character_effects$Character) || 
      !(vehicle %in% vehicle_effects$Vehicle) ||
      !(tire %in% tire_effects$Tires) ||
      !(glider %in% glider_effects$Glider)) {
    return(NA)
  }
  
  # Get component effects
  char_effect <- character_effects$Effect[character_effects$Character == character]
  veh_effect <- vehicle_effects$Effect[vehicle_effects$Vehicle == vehicle]
  tire_effect <- tire_effects$Effect[tire_effects$Tires == tire]
  glider_effect <- glider_effects$Effect[glider_effects$Glider == glider]
  
  # Check if components have attributes
  total_attrs <- calculate_combo_attributes(character, vehicle, tire, glider)
  if (is.null(total_attrs)) {
    return(NA)
  }
  
  # Get interaction effect if available
  if (character %in% rownames(char_vehicle_interaction) && 
      vehicle %in% colnames(char_vehicle_interaction)) {
    interaction <- char_vehicle_interaction[character, vehicle]
  } else {
    interaction <- 0
  }
  
  # Calculate attribute contribution
  attr_contribution <- 0
  for (i in 1:length(attributes)) {
    attr_name <- attributes[i]
    attr_contribution <- attr_contribution + 
      total_attrs[attr_name] * attribute_weights$Weight[attribute_weights$Attribute == attr_name]
  }
  
  # Calculate total score
  total_score <- char_effect + veh_effect + tire_effect + glider_effect + 
    interaction + attr_contribution
  
  return(total_score)
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Mario Kart 8 Recommender"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Recommendations", tabName = "recommendations", icon = icon("dashboard")),
      menuItem("Components", tabName = "components", icon = icon("car")),
      menuItem("Build Your Combo", tabName = "custom", icon = icon("wrench")),
      menuItem("Attribute Analysis", tabName = "attributes", icon = icon("chart-bar")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      # Recommendations tab
      tabItem(tabName = "recommendations",
              fluidRow(
                box(
                  title = "Top Recommended Combinations",
                  width = 12,
                  DT::dataTableOutput("topCombosTable")
                )
              ),
              fluidRow(
                box(
                  title = "Filter Recommendations",
                  width = 6,
                  selectInput("preferredCharacter", "Preferred Character:", 
                              c("Any" = "", as.character(sort(character_effects$Character)))),
                  selectInput("preferredVehicle", "Preferred Vehicle Type:", 
                              c("Any" = "", 
                                "Kart" = "kart", 
                                "Bike" = "bike", 
                                "ATV" = "atv")),
                  actionButton("filterBtn", "Filter", class = "btn-primary")
                ),
                box(
                  title = "Optimization Focus",
                  width = 6,
                  sliderInput("speedWeight", "Speed Importance:", 
                              min = 0, max = 1, value = 0.5, step = 0.1),
                  sliderInput("handlingWeight", "Handling Importance:", 
                              min = 0, max = 1, value = 0.5, step = 0.1),
                  sliderInput("accelerationWeight", "Acceleration Importance:", 
                              min = 0, max = 1, value = 0.5, step = 0.1),
                  actionButton("optimizeBtn", "Optimize", class = "btn-success")
                )
              )
      ),
      
      # Components tab
      tabItem(tabName = "components",
              fluidRow(
                box(
                  title = "Component Effects",
                  width = 12,
                  tabsetPanel(
                    tabPanel("Characters", plotOutput("characterEffectsPlot")),
                    tabPanel("Vehicles", plotOutput("vehicleEffectsPlot")),
                    tabPanel("Tires", plotOutput("tireEffectsPlot")),
                    tabPanel("Gliders", plotOutput("gliderEffectsPlot"))
                  )
                )
              ),
              fluidRow(
                box(
                  title = "属性权重分析",
                  width = 6,
                  plotOutput("attributeWeightsPlot")
                ),
                uiOutput("componentBoxUI")
              )
      ),
      
      # Custom Combo Builder tab
      tabItem(tabName = "custom",
              fluidRow(
                box(
                  title = "Build Your Custom Combination",
                  width = 6,
                  selectInput("customCharacter", "Character:", 
                              as.character(sort(character_effects$Character))),
                  selectInput("customVehicle", "Vehicle:", 
                              as.character(sort(vehicle_effects$Vehicle))),
                  selectInput("customTires", "Tires:", 
                              as.character(sort(tire_effects$Tires))),
                  selectInput("customGlider", "Glider:", 
                              as.character(sort(glider_effects$Glider))),
                  actionButton("evaluateBtn", "Evaluate Combination", class = "btn-primary")
                ),
                box(
                  title = "Combination Score",
                  width = 6,
                  valueBoxOutput("comboScoreBox", width = 12),
                  plotOutput("comboBreakdownPlot")
                )
              ),
              fluidRow(
                box(
                  title = "Combination Attributes",
                  width = 12,
                  plotOutput("comboAttributesPlot", height = "300px")
                )
              )
      ),
      
      # Attribute Analysis tab
      tabItem(tabName = "attributes",
              fluidRow(
                box(
                  title = "Attribute Correlations",
                  width = 6,
                  plotOutput("attrCorrelationPlot")
                ),
                box(
                  title = "Attribute Distributions by Component Type",
                  width = 6,
                  selectInput("attrCompType", "Component Type:",
                              c("Drivers", "Vehicles", "Tires", "Gliders")),
                  plotOutput("attrDistributionPlot")
                )
              ),
              fluidRow(
                box(
                  title = "Key Attribute Relationships",
                  width = 12,
                  tabsetPanel(
                    tabPanel("Speed vs. Handling", plotOutput("speedVsHandlingPlot")),
                    tabPanel("Speed vs. Acceleration", plotOutput("speedVsAccelerationPlot")),
                    tabPanel("Speed/Handling Ratio", plotOutput("speedHandlingRatioPlot"))
                  )
                )
              )
      ),
      
      # About tab
      tabItem(tabName = "about",
              fluidRow(
                box(
                  title = "About This App",
                  width = 12,
                  tags$div(
                    tags$h3("Mario Kart 8 Recommender System"),
                    tags$p("This app is based on an analysis of real world record data and Bayesian hierarchical modeling to identify optimal component combinations for Mario Kart 8."),
                    tags$p("The model incorporates:"),
                    tags$ul(
                      tags$li("Component main effects (character, vehicle, tire, glider)"),
                      tags$li("Interaction effects between components"),
                      tags$li("Attribute weights and contributions"),
                      tags$li("Data from actual world records")
                    ),
                    tags$p("Use this app to:"),
                    tags$ul(
                      tags$li("Explore top recommended combinations"),
                      tags$li("Understand component effects"),
                      tags$li("Evaluate custom combinations"),
                      tags$li("Analyze attribute relationships")
                    ),
                    tags$hr(),
                    tags$p("Created with R Shiny and Stan"),
                    tags$p("Based on Bayesian analysis of Mario Kart 8 world records")
                  )
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Top Combinations Table
  output$topCombosTable <- DT::renderDataTable({
    DT::datatable(top_combinations, 
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })
  
  # Character Effects Plot
  output$characterEffectsPlot <- renderPlot({
    character_df <- character_effects %>%
      arrange(desc(Effect)) %>%
      head(20) %>%
      mutate(Character = factor(Character, levels = Character[order(Effect, decreasing = TRUE)]))
    
    ggplot(character_df, aes(x = Character, y = Effect, fill = Effect)) +
      geom_bar(stat = "identity") +
      scale_fill_viridis_c() +
      coord_flip() +
      theme_minimal() +
      labs(title = "Top 20 Character Effects on Score", x = "", y = "Effect Size")
  })
  
  # Vehicle Effects Plot
  output$vehicleEffectsPlot <- renderPlot({
    vehicle_df <- vehicle_effects %>%
      arrange(desc(Effect)) %>%
      head(20) %>%
      mutate(Vehicle = factor(Vehicle, levels = Vehicle[order(Effect, decreasing = TRUE)]))
    
    ggplot(vehicle_df, aes(x = Vehicle, y = Effect, fill = Effect)) +
      geom_bar(stat = "identity") +
      scale_fill_viridis_c() +
      coord_flip() +
      theme_minimal() +
      labs(title = "Top 20 Vehicle Effects on Score", x = "", y = "Effect Size")
  })
  
  # Tire Effects Plot
  output$tireEffectsPlot <- renderPlot({
    tire_df <- tire_effects %>%
      arrange(desc(Effect)) %>%
      mutate(Tires = factor(Tires, levels = Tires[order(Effect, decreasing = TRUE)]))
    
    ggplot(tire_df, aes(x = Tires, y = Effect, fill = Effect)) +
      geom_bar(stat = "identity") +
      scale_fill_viridis_c() +
      coord_flip() +
      theme_minimal() +
      labs(title = "Tire Effects on Score", x = "", y = "Effect Size")
  })
  
  # Glider Effects Plot
  output$gliderEffectsPlot <- renderPlot({
    glider_df <- glider_effects %>%
      arrange(desc(Effect)) %>%
      mutate(Glider = factor(Glider, levels = Glider[order(Effect, decreasing = TRUE)]))
    
    ggplot(glider_df, aes(x = Glider, y = Effect, fill = Effect)) +
      geom_bar(stat = "identity") +
      scale_fill_viridis_c() +
      coord_flip() +
      theme_minimal() +
      labs(title = "Glider Effects on Score", x = "", y = "Effect Size")
  })
  
  # Attribute Weights Plot
  output$attributeWeightsPlot <- renderPlot({
    attr_df <- attribute_weights %>%
      arrange(desc(abs(Weight))) %>%
      mutate(Attribute = factor(Attribute, levels = Attribute),
             Direction = ifelse(Weight > 0, "Positive", "Negative"))
    
    ggplot(attr_df, aes(x = Attribute, y = Weight, fill = Weight)) +
      geom_bar(stat = "identity") +
      scale_fill_gradient2(low = "#053061", mid = "white", high = "#67001F", midpoint = 0) +
      coord_flip() +
      theme_minimal() +
      labs(title = "Attribute Weights Impact on Score", x = "", y = "Weight")
  })
  
  # 全新的角色属性雷达图可视化
  output$characterRadarPlot <- renderPlot({
    req(input$interactionChar)
    
    # 获取选定角色的数据
    selected_char <- input$interactionChar
    driver_data <- get_driver_attributes(selected_char)
    
    if(is.null(driver_data)) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = paste("无法获取", selected_char, "的属性数据")) + 
               theme_void())
    }
    
    # 从驾驶员数据中提取属性
    char_attrs <- data.frame(
      Attribute = attributes,
      Value = as.numeric(driver_data[1, attributes])
    )
    
    # 创建有序因子，按照分组组织属性
    char_attrs$Group <- case_when(
      char_attrs$Attribute %in% c("GroundSpeed", "WaterSpeed", "AirSpeed", "AntiGravitySpeed") ~ "速度",
      char_attrs$Attribute %in% c("GroundHandling", "WaterHandling", "AirHandling", "AntiGravityHandling") ~ "操控性",
      char_attrs$Attribute == "Acceleration" ~ "加速度",
      char_attrs$Attribute == "Weight" ~ "重量",
      char_attrs$Attribute == "Traction" ~ "抓地力",
      char_attrs$Attribute == "MiniTurbo" ~ "小型涡轮增压",
      TRUE ~ "其他"
    )
    
    # 按属性组创建分组的条形图
    p1 <- ggplot(char_attrs, aes(x = reorder(Attribute, -Value), y = Value, fill = Group)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Set2") +
      coord_flip() +
      theme_minimal() +
      labs(title = paste(selected_char, "的属性分布"),
           x = "", y = "属性值") +
      theme(legend.title = element_blank(),
            plot.title = element_text(size = 14, face = "bold"))
    
    # 获取所有驾驶员的平均值创建比较
    all_drivers_data <- drivers[, c("Driver", attributes)]
    
    # 计算属性的平均值和范围
    attr_stats <- data.frame(
      Attribute = attributes,
      Mean = sapply(all_drivers_data[, attributes], mean),
      Min = sapply(all_drivers_data[, attributes], min),
      Max = sapply(all_drivers_data[, attributes], max)
    )
    
    # 合并选定角色的属性和平均值/范围
    comparison_data <- char_attrs %>%
      select(Attribute, Value, Group) %>%
      left_join(attr_stats, by = "Attribute") %>%
      mutate(
        RelativeStrength = (Value - Mean) / (Max - Min) * 10,  # 相对强度指标
        IsStrength = Value > Mean
      )
    
    # 创建相对强度可视化
    p2 <- ggplot(comparison_data, aes(x = reorder(Attribute, RelativeStrength), y = RelativeStrength, fill = IsStrength)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("FALSE" = "#FF9999", "TRUE" = "#99FF99"), 
                        labels = c("FALSE" = "弱点", "TRUE" = "强项")) +
      coord_flip() +
      theme_minimal() +
      labs(title = paste(selected_char, "的相对强度分析"),
           x = "", y = "相对强度 (与平均值比较)") +
      theme(legend.title = element_blank(),
            plot.title = element_text(size = 14, face = "bold"))
    
    # 创建建议搭配部分
    # 基于角色弱点，推荐可以弥补这些弱点的组件
    weaknesses <- comparison_data %>%
      filter(RelativeStrength < 0) %>%
      arrange(RelativeStrength) %>%
      head(3) %>%
      pull(Attribute)
    
    # 找出能弥补这些弱点的顶级车辆
    top_complementary_vehicles <- data.frame()
    
    if(length(weaknesses) > 0) {
      # 简化为仅考虑车辆
      vehicle_scores <- data.frame()
      
      for(veh_name in vehicle_effects$Vehicle) {
        veh_data <- get_vehicle_attributes(veh_name)
        
        if(!is.null(veh_data)) {
          # 计算该车辆在弥补角色弱点方面的得分
          weakness_score <- sum(as.numeric(veh_data[1, weaknesses]))
          
          vehicle_scores <- rbind(vehicle_scores, 
                                  data.frame(Vehicle = veh_name, 
                                             Score = weakness_score,
                                             Effect = vehicle_effects$Effect[vehicle_effects$Vehicle == veh_name]))
        }
      }
      
      if(nrow(vehicle_scores) > 0) {
        top_complementary_vehicles <- vehicle_scores %>%
          arrange(desc(Score + Effect)) %>%
          head(5)
      }
    }
    
    # 创建建议车辆可视化
    if(nrow(top_complementary_vehicles) > 0) {
      p3 <- ggplot(top_complementary_vehicles, aes(x = reorder(Vehicle, Score), y = Score, fill = Effect)) +
        geom_bar(stat = "identity") +
        scale_fill_viridis_c(name = "车辆效果") +
        coord_flip() +
        theme_minimal() +
        labs(title = paste("推荐与", selected_char, "搭配的车辆"),
             subtitle = paste("基于弥补以下弱点:", paste(weaknesses, collapse=", ")),
             x = "", y = "弥补弱点的得分") +
        theme(plot.title = element_text(size = 14, face = "bold"),
              plot.subtitle = element_text(size = 10))
      
      # 组合三个图表
      grid.arrange(p1, p2, p3, ncol = 1)
    } else {
      # 如果没有找到建议车辆，只展示前两个图表
      grid.arrange(p1, p2, ncol = 1)
    }
  })
  
  # 更新UI
  output$componentBoxUI <- renderUI({
    box(
      title = "角色属性分析与推荐组合",
      width = 6,
      selectInput("interactionChar", "选择角色:",
                  as.character(sort(character_effects$Character))),
      p("以下可视化展示了所选角色的属性分布、相对强弱项分析，以及基于弱点的车辆推荐。"),
      plotOutput("characterRadarPlot", height = "600px")
    )
  })
  
  # Custom Combo Evaluation
  observeEvent(input$evaluateBtn, {
    # Get combo score
    combo_score <- calculate_combination_score(
      input$customCharacter,
      input$customVehicle,
      input$customTires,
      input$customGlider
    )
    
    if (is.na(combo_score)) {
      output$comboScoreBox <- renderValueBox({
        valueBox(
          "N/A", "Unable to calculate score - component may be missing", 
          icon = icon("exclamation-triangle"), color = "red"
        )
      })
      
      # Empty plots if score can't be calculated
      output$comboBreakdownPlot <- renderPlot({ NULL })
      output$comboAttributesPlot <- renderPlot({ NULL })
      
    } else {
      # Calculate percentile of this combo among top combinations
      percentile <- round(100 * (1 - (sum(top_combinations$PredictedScore > combo_score) / nrow(top_combinations))), 1)
      
      output$comboScoreBox <- renderValueBox({
        valueBox(
          round(combo_score, 2), 
          paste0("Combo Score (", percentile, "th percentile)"),
          icon = icon("star"), 
          color = if(percentile > 75) "green" else if(percentile > 50) "yellow" else "orange"
        )
      })
      
      # Get component effects for breakdown
      char_effect <- character_effects$Effect[character_effects$Character == input$customCharacter]
      veh_effect <- vehicle_effects$Effect[vehicle_effects$Vehicle == input$customVehicle]
      tire_effect <- tire_effects$Effect[tire_effects$Tires == input$customTires]
      glider_effect <- glider_effects$Effect[glider_effects$Glider == input$customGlider]
      
      # Get interaction effect if available
      if (input$customCharacter %in% rownames(char_vehicle_interaction) && 
          input$customVehicle %in% colnames(char_vehicle_interaction)) {
        interaction <- char_vehicle_interaction[input$customCharacter, input$customVehicle]
      } else {
        interaction <- 0
      }
      
      # Get total attributes
      total_attrs <- calculate_combo_attributes(
        input$customCharacter,
        input$customVehicle,
        input$customTires,
        input$customGlider
      )
      
      # Calculate attribute contribution
      attr_contribution <- 0
      for (i in 1:length(attributes)) {
        attr_name <- attributes[i]
        attr_contribution <- attr_contribution + 
          total_attrs[attr_name] * attribute_weights$Weight[attribute_weights$Attribute == attr_name]
      }
      
      # Create breakdown plot
      contributions <- data.frame(
        Component = c("Character", "Vehicle", "Tires", "Glider", "Interaction", "Attributes"),
        Effect = c(char_effect, veh_effect, tire_effect, glider_effect, interaction, attr_contribution)
      )
      
      output$comboBreakdownPlot <- renderPlot({
        ggplot(contributions, aes(x = reorder(Component, abs(Effect)), y = Effect, fill = Effect)) +
          geom_col() +
          scale_fill_gradient2(low = "tomato", mid = "white", high = "steelblue", midpoint = 0) +
          coord_flip() +
          theme_minimal() +
          labs(title = "Combo Score Breakdown",
               x = "", y = "Effect")
      })
      
      # Create attributes plot
      attr_df <- data.frame(
        Attribute = names(total_attrs),
        Value = as.numeric(total_attrs)
      )
      
      output$comboAttributesPlot <- renderPlot({
        ggplot(attr_df, aes(x = reorder(Attribute, Value), y = Value, fill = Value)) +
          geom_col() +
          scale_fill_viridis_c() +
          coord_flip() +
          theme_minimal() +
          labs(title = "Total Attributes for Custom Combination", x = "", y = "Value")
      })
    }
  })
  
  # Attribute Distribution Plot by Component Type
  output$attrDistributionPlot <- renderPlot({
    comp_type <- input$attrCompType
    
    if (comp_type == "Drivers") {
      data <- drivers
      id_col <- "Driver"
    } else if (comp_type == "Vehicles") {
      data <- vehicles
      id_col <- "Vehicle"
    } else if (comp_type == "Tires") {
      data <- tires
      id_col <- "Tire"
    } else {
      data <- gliders
      id_col <- "Glider"
    }
    
    # Reshape data for plotting
    long_data <- data %>%
      select(-id_col) %>%
      pivot_longer(cols = everything(), 
                   names_to = "Attribute", 
                   values_to = "Value")
    
    # Generate boxplot
    ggplot(long_data, aes(x = Attribute, y = Value)) +
      geom_boxplot(fill = "skyblue") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = paste("Attribute Distributions -", comp_type),
           y = "Value", x = "")
  })
  
  # Additional attribute plots (placeholders - would need actual data to implement fully)
  output$attrCorrelationPlot <- renderPlot({
    # Create a correlation matrix from the attribute data
    attr_data <- rbind(
      drivers[, attributes],
      vehicles[, attributes],
      tires[, attributes],
      gliders[, attributes]
    )
    
    cormat <- cor(attr_data)
    melted_cormat <- melt(cormat)
    
    ggplot(melted_cormat, aes(Var1, Var2, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "#053061", mid = "white", high = "#67001F", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Correlation") +
      geom_text(aes(label = ifelse(abs(value) > 0.5, round(value, 2), "")), 
                size = 3) +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            plot.title = element_text(hjust = 0.5)) +
      coord_fixed() +
      labs(title = "Correlation Between Attributes",
           x = "", y = "")
  })
  
  # Speed vs. Handling Plot
  output$speedVsHandlingPlot <- renderPlot({
    # Create combined data with all combinations
    all_combos <- expand.grid(
      Character = sample(character_effects$Character, 10),
      Vehicle = sample(vehicle_effects$Vehicle, 10),
      Tires = sample(tire_effects$Tires, 5),
      Glider = sample(glider_effects$Glider, 3)
    )
    
    # Calculate attributes for each combo
    combos_with_attrs <- all_combos %>%
      rowwise() %>%
      mutate(
        attrs = list(calculate_combo_attributes(Character, Vehicle, Tires, Glider)),
        GroundSpeed = if(is.null(attrs)) NA else attrs["GroundSpeed"],
        GroundHandling = if(is.null(attrs)) NA else attrs["GroundHandling"],
        Score = calculate_combination_score(Character, Vehicle, Tires, Glider)
      ) %>%
      ungroup() %>%
      select(-attrs) %>%
      filter(!is.na(Score), !is.na(GroundSpeed), !is.na(GroundHandling))
    
    # Generate plot
    ggplot(combos_with_attrs, aes(x = GroundSpeed, y = GroundHandling, color = Score)) +
      geom_point(alpha = 0.6) +
      scale_color_viridis() +
      labs(title = "Ground: Speed VS Handling", 
           x = "Ground Speed", 
           y = "Ground Handling",
           color = "Score") +
      theme_minimal()
  })
  
  # Speed vs. Acceleration Plot
  output$speedVsAccelerationPlot <- renderPlot({
    # Similar approach as speedVsHandlingPlot but with Acceleration
    all_combos <- expand.grid(
      Character = sample(character_effects$Character, 10),
      Vehicle = sample(vehicle_effects$Vehicle, 10),
      Tires = sample(tire_effects$Tires, 5),
      Glider = sample(glider_effects$Glider, 3)
    )
    
    combos_with_attrs <- all_combos %>%
      rowwise() %>%
      mutate(
        attrs = list(calculate_combo_attributes(Character, Vehicle, Tires, Glider)),
        GroundSpeed = if(is.null(attrs)) NA else attrs["GroundSpeed"],
        Acceleration = if(is.null(attrs)) NA else attrs["Acceleration"],
        Score = calculate_combination_score(Character, Vehicle, Tires, Glider)
      ) %>%
      ungroup() %>%
      select(-attrs) %>%
      filter(!is.na(Score), !is.na(GroundSpeed), !is.na(Acceleration))
    
    ggplot(combos_with_attrs, aes(x = GroundSpeed, y = Acceleration, color = Score)) +
      geom_point(alpha = 0.6) +
      scale_color_viridis() +
      labs(title = "Relationship Between Speed and Acceleration", 
           x = "Ground Speed", 
           y = "Acceleration",
           color = "Score") +
      theme_minimal()
  })
  
  # Speed/Handling Ratio Plot
  output$speedHandlingRatioPlot <- renderPlot({
    all_combos <- expand.grid(
      Character = sample(character_effects$Character, 10),
      Vehicle = sample(vehicle_effects$Vehicle, 10),
      Tires = sample(tire_effects$Tires, 5),
      Glider = sample(glider_effects$Glider, 3)
    )
    
    combos_with_attrs <- all_combos %>%
      rowwise() %>%
      mutate(
        attrs = list(calculate_combo_attributes(Character, Vehicle, Tires, Glider)),
        GroundSpeed = if(is.null(attrs)) NA else attrs["GroundSpeed"],
        GroundHandling = if(is.null(attrs)) NA else attrs["GroundHandling"],
        SpeedHandlingRatio = if(is.null(attrs)) NA else attrs["GroundSpeed"] / attrs["GroundHandling"],
        Score = calculate_combination_score(Character, Vehicle, Tires, Glider)
      ) %>%
      ungroup() %>%
      select(-attrs) %>%
      filter(!is.na(Score), !is.na(SpeedHandlingRatio))
    
    ggplot(combos_with_attrs, aes(x = SpeedHandlingRatio, y = Score)) +
      geom_point(alpha = 0.3) +
      geom_smooth(method = "lm", color = "red") +
      labs(title = "Relationship Between Speed/Handling Ratio and Score", 
           x = "Speed/Handling Ratio", 
           y = "Score") +
      theme_minimal()
  })
  
  # Filter recommendations
  observeEvent(input$filterBtn, {
    filtered_combos <- top_combinations
    
    # Apply character filter if provided
    if (input$preferredCharacter != "") {
      filtered_combos <- filtered_combos %>%
        filter(Character == input$preferredCharacter)
    }
    
    # Apply vehicle type filter if provided
    if (input$preferredVehicle != "") {
      # Define vehicle categories
      kart_vehicles <- c("Standard Kart", "Pipe Frame", "Mach 8", "Steel Driver", "Cat Cruiser",
                         "Circuit Special", "Tri-Speeder", "Badwagon", "Prancer", "Biddybuggy",
                         "Landship", "Sneeker", "Sports Coupe", "Gold Standard", "GLA",
                         "W 25 Silver Arrow", "300 SL Roadster", "Blue Falcon", "Tanooki Kart", "B Dasher",
                         "Streetle", "P-Wing", "Koopa Clown")
      
      bike_vehicles <- c("Standard Bike", "Comet", "Sport Bike", "The Duke", "Flame Rider", 
                         "Varmint", "Mr. Scooty", "Jet Bike", "Yoshi Bike", "Master Cycle", "City Tripper")
      
      atv_vehicles <- c("Standard ATV", "Wild Wiggler", "Teddy Buggy", "Bone Rattler", "Splat Buggy", "Inkstriker")
      
      vehicle_filter <- switch(input$preferredVehicle,
                               "kart" = kart_vehicles,
                               "bike" = bike_vehicles,
                               "atv" = atv_vehicles,
                               NULL)
      
      if (!is.null(vehicle_filter)) {
        filtered_combos <- filtered_combos %>%
          filter(Vehicle %in% vehicle_filter)
      }
    }
    
    output$topCombosTable <- DT::renderDataTable({
      DT::datatable(filtered_combos,
                    options = list(pageLength = 10),
                    rownames = FALSE)
    })
  })
  
  # Optimize based on preferences
  observeEvent(input$optimizeBtn, {
    # Create custom attribute weights based on user preferences
    custom_weights <- attribute_weights
    
    # Modify speed-related attributes
    speed_attrs <- c("GroundSpeed", "WaterSpeed", "AirSpeed", "AntiGravitySpeed")
    custom_weights$Weight[custom_weights$Attribute %in% speed_attrs] <- 
      custom_weights$Weight[custom_weights$Attribute %in% speed_attrs] * input$speedWeight * 2
    
    # Modify handling-related attributes
    handling_attrs <- c("GroundHandling", "WaterHandling", "AirHandling", "AntiGravityHandling", "Traction")
    custom_weights$Weight[custom_weights$Attribute %in% handling_attrs] <- 
      custom_weights$Weight[custom_weights$Attribute %in% handling_attrs] * input$handlingWeight * 2
    
    # Modify acceleration
    custom_weights$Weight[custom_weights$Attribute == "Acceleration"] <- 
      custom_weights$Weight[custom_weights$Attribute == "Acceleration"] * input$accelerationWeight * 2
    
    # Use top 500 combinations from the original list to re-score based on custom weights
    temp_attr_weights <- custom_weights
    
    # Score combinations with new weights
    custom_scored_combos <- top_combinations %>%
      rowwise() %>%
      mutate(
        CustomScore = calculate_custom_score(Character, Vehicle, Tires, Glider, temp_attr_weights)
      ) %>%
      ungroup() %>%
      filter(!is.na(CustomScore)) %>%
      arrange(desc(CustomScore))
    
    output$topCombosTable <- DT::renderDataTable({
      DT::datatable(custom_scored_combos %>% select(-PredictedScore) %>% rename(PredictedScore = CustomScore),
                    options = list(pageLength = 10),
                    rownames = FALSE)
    })
  })
  
  # Custom scoring function with different attribute weights
  calculate_custom_score <- function(character, vehicle, tire, glider, cust_weights) {
    # Get component effects
    char_effect <- character_effects$Effect[character_effects$Character == character]
    veh_effect <- vehicle_effects$Effect[vehicle_effects$Vehicle == vehicle]
    tire_effect <- tire_effects$Effect[tire_effects$Tires == tire]
    glider_effect <- glider_effects$Effect[glider_effects$Glider == glider]
    
    # Get attributes
    total_attrs <- calculate_combo_attributes(character, vehicle, tire, glider)
    if (is.null(total_attrs)) {
      return(NA)
    }
    
    # Get interaction effect if available
    if (character %in% rownames(char_vehicle_interaction) && 
        vehicle %in% colnames(char_vehicle_interaction)) {
      interaction <- char_vehicle_interaction[character, vehicle]
    } else {
      interaction <- 0
    }
    
    # Calculate attribute contribution with custom weights
    attr_contribution <- 0
    for (i in 1:length(attributes)) {
      attr_name <- attributes[i]
      attr_contribution <- attr_contribution + 
        total_attrs[attr_name] * cust_weights$Weight[cust_weights$Attribute == attr_name]
    }
    
    # Calculate total score
    total_score <- char_effect + veh_effect + tire_effect + glider_effect + 
      interaction + attr_contribution
    
    return(total_score)
  }
}

# Run the application
shinyApp(ui = ui, server = server)