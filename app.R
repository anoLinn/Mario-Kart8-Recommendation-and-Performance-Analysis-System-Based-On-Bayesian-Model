library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(gridExtra)
library(grid)
library(viridis)
library(ggplot2)
library(reshape2)

character_effects <- read.csv("world_record_character_effects.csv")
vehicle_effects <- read.csv("world_record_vehicle_effects.csv")
tire_effects <- read.csv("world_record_tire_effects.csv")
glider_effects <- read.csv("world_record_glider_effects.csv")
attribute_weights <- read.csv("world_record_attribute_weights.csv")
top_combinations <- read.csv("top_50_predicted_combinations.csv")

drivers <- read.csv("DRIVERS.csv", stringsAsFactors = FALSE)
vehicles <- read.csv("VEHICLES.csv", stringsAsFactors = FALSE)
tires <- read.csv("TIRES.csv", stringsAsFactors = FALSE)
gliders <- read.csv("GLIDERS.csv", stringsAsFactors = FALSE)

attributes <- c("GroundSpeed", "WaterSpeed", "AirSpeed", "AntiGravitySpeed", 
                "Acceleration", "Weight", "GroundHandling", "WaterHandling", 
                "AirHandling", "AntiGravityHandling", "Traction", "MiniTurbo")

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

calculate_combination_score <- function(character, vehicle, tire, glider) {
  if (!(character %in% character_effects$Character) || 
      !(vehicle %in% vehicle_effects$Vehicle) ||
      !(tire %in% tire_effects$Tires) ||
      !(glider %in% glider_effects$Glider)) {
    return(NA)
  }
  
  char_effect <- character_effects$Effect[character_effects$Character == character]
  veh_effect <- vehicle_effects$Effect[vehicle_effects$Vehicle == vehicle]
  tire_effect <- tire_effects$Effect[tire_effects$Tires == tire]
  glider_effect <- glider_effects$Effect[glider_effects$Glider == glider]
  
  total_attrs <- calculate_combo_attributes(character, vehicle, tire, glider)
  if (is.null(total_attrs)) {
    return(NA)
  }
  
  if (character %in% rownames(char_vehicle_interaction) && 
      vehicle %in% colnames(char_vehicle_interaction)) {
    interaction <- char_vehicle_interaction[character, vehicle]
  } else {
    interaction <- 0
  }
  
  attr_contribution <- 0
  for (i in 1:length(attributes)) {
    attr_name <- attributes[i]
    attr_contribution <- attr_contribution + 
      total_attrs[attr_name] * attribute_weights$Weight[attribute_weights$Attribute == attr_name]
  }
  
  total_score <- char_effect + veh_effect + tire_effect + glider_effect + 
    interaction + attr_contribution
  
  return(total_score)
}

ui <- dashboardPage(
  dashboardHeader(title = "Mario Kart 8 Recommender"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarmenu",
      menuItem("Recommendations", tabName = "recommendations", icon = icon("dashboard")),
      menuItem("Components", tabName = "components", icon = icon("car")),
      menuItem("Build Your Combo", tabName = "custom", icon = icon("wrench")),
      menuItem("Attribute Analysis", tabName = "attributes", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
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
                  title = "Attribute Weight Analysis",
                  width = 6,
                  plotOutput("attributeWeightsPlot")
                ),
                uiOutput("componentBoxUI")
              )
      ),
      
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
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$topCombosTable <- DT::renderDataTable({
    DT::datatable(top_combinations, 
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })
  
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
  
  output$characterRadarPlot <- renderPlot({
    req(input$interactionChar)
    
    selected_char <- input$interactionChar
    driver_data <- get_driver_attributes(selected_char)
    
    if(is.null(driver_data)) {
      return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, label = paste("Cannot retrieve attribute data for", selected_char)) + 
               theme_void())
    }
    
    char_attrs <- data.frame(
      Attribute = attributes,
      Value = as.numeric(driver_data[1, attributes])
    )
    
    char_attrs$Group <- case_when(
      char_attrs$Attribute %in% c("GroundSpeed", "WaterSpeed", "AirSpeed", "AntiGravitySpeed") ~ "Speed",
      char_attrs$Attribute %in% c("GroundHandling", "WaterHandling", "AirHandling", "AntiGravityHandling") ~ "Handling",
      char_attrs$Attribute == "Acceleration" ~ "Acceleration",
      char_attrs$Attribute == "Weight" ~ "Weight",
      char_attrs$Attribute == "Traction" ~ "Traction",
      char_attrs$Attribute == "MiniTurbo" ~ "Mini-Turbo",
      TRUE ~ "Other"
    )
    
    p1 <- ggplot(char_attrs, aes(x = reorder(Attribute, -Value), y = Value, fill = Group)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Set2") +
      coord_flip() +
      theme_minimal() +
      labs(title = paste(selected_char, "Attribute Distribution"),
           x = "", y = "Attribute Value") +
      theme(legend.title = element_blank(),
            plot.title = element_text(size = 14, face = "bold"))
    
    all_drivers_data <- drivers[, c("Driver", attributes)]
    
    attr_stats <- data.frame(
      Attribute = attributes,
      Mean = sapply(all_drivers_data[, attributes], mean),
      Min = sapply(all_drivers_data[, attributes], min),
      Max = sapply(all_drivers_data[, attributes], max)
    )
    
    comparison_data <- char_attrs %>%
      select(Attribute, Value, Group) %>%
      left_join(attr_stats, by = "Attribute") %>%
      mutate(
        RelativeStrength = (Value - Mean) / (Max - Min) * 10,
        IsStrength = Value > Mean
      )
    
    p2 <- ggplot(comparison_data, aes(x = reorder(Attribute, RelativeStrength), y = RelativeStrength, fill = IsStrength)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("FALSE" = "#FF9999", "TRUE" = "#99FF99"), 
                        labels = c("FALSE" = "Weakness", "TRUE" = "Strength")) +
      coord_flip() +
      theme_minimal() +
      labs(title = paste(selected_char, "Relative Strength Analysis"),
           x = "", y = "Relative Strength (vs Average)") +
      theme(legend.title = element_blank(),
            plot.title = element_text(size = 14, face = "bold"))
    
    weaknesses <- comparison_data %>%
      filter(RelativeStrength < 0) %>%
      arrange(RelativeStrength) %>%
      head(3) %>%
      pull(Attribute)
    
    top_complementary_vehicles <- data.frame()
    
    if(length(weaknesses) > 0) {
      vehicle_scores <- data.frame()
      
      for(veh_name in vehicle_effects$Vehicle) {
        veh_data <- get_vehicle_attributes(veh_name)
        
        if(!is.null(veh_data)) {
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
    
    if(nrow(top_complementary_vehicles) > 0) {
      p3 <- ggplot(top_complementary_vehicles, aes(x = reorder(Vehicle, Score), y = Score, fill = Effect)) +
        geom_bar(stat = "identity") +
        scale_fill_viridis_c(name = "Vehicle Effect") +
        coord_flip() +
        theme_minimal() +
        labs(title = paste("Recommended Vehicles to Pair with", selected_char),
             subtitle = paste("Based on offsetting weaknesses:", paste(weaknesses, collapse=", ")),
             x = "", y = "Weakness Compensation Score") +
        theme(plot.title = element_text(size = 14, face = "bold"),
              plot.subtitle = element_text(size = 10))
      
      grid.arrange(p1, p2, p3, ncol = 1)
    } else {
      grid.arrange(p1, p2, ncol = 1)
    }
  })
  
  output$componentBoxUI <- renderUI({
    box(
      title = "Character Analysis and Combination Recommendations",
      width = 6,
      selectInput("interactionChar", "Select Character:",
                  as.character(sort(character_effects$Character))),
      plotOutput("characterRadarPlot", height = "600px")
    )
  })
  
  observeEvent(input$evaluateBtn, {
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
      
      output$comboBreakdownPlot <- renderPlot({ NULL })
      output$comboAttributesPlot <- renderPlot({ NULL })
      
    } else {
      percentile <- round(100 * (1 - (sum(top_combinations$PredictedScore > combo_score) / nrow(top_combinations))), 1)
      
      output$comboScoreBox <- renderValueBox({
        valueBox(
          round(combo_score, 2), 
          paste0("Combo Score (", percentile, "th percentile)"),
          icon = icon("star"), 
          color = if(percentile > 75) "green" else if(percentile > 50) "yellow" else "orange"
        )
      })
      
      char_effect <- character_effects$Effect[character_effects$Character == input$customCharacter]
      veh_effect <- vehicle_effects$Effect[vehicle_effects$Vehicle == input$customVehicle]
      tire_effect <- tire_effects$Effect[tire_effects$Tires == input$customTires]
      glider_effect <- glider_effects$Effect[glider_effects$Glider == input$customGlider]
      
      if (input$customCharacter %in% rownames(char_vehicle_interaction) && 
          input$customVehicle %in% colnames(char_vehicle_interaction)) {
        interaction <- char_vehicle_interaction[input$customCharacter, input$customVehicle]
      } else {
        interaction <- 0
      }
      
      total_attrs <- calculate_combo_attributes(
        input$customCharacter,
        input$customVehicle,
        input$customTires,
        input$customGlider
      )
      
      attr_contribution <- 0
      for (i in 1:length(attributes)) {
        attr_name <- attributes[i]
        attr_contribution <- attr_contribution + 
          total_attrs[attr_name] * attribute_weights$Weight[attribute_weights$Attribute == attr_name]
      }
      
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
    
    long_data <- data %>%
      select(-id_col) %>%
      pivot_longer(cols = everything(), 
                   names_to = "Attribute", 
                   values_to = "Value")
    
    ggplot(long_data, aes(x = Attribute, y = Value)) +
      geom_boxplot(fill = "skyblue") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = paste("Attribute Distributions -", comp_type),
           y = "Value", x = "")
  })
  
  output$attrCorrelationPlot <- renderPlot({
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
  
  output$speedVsHandlingPlot <- renderPlot({
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
        Score = calculate_combination_score(Character, Vehicle, Tires, Glider)
      ) %>%
      ungroup() %>%
      select(-attrs) %>%
      filter(!is.na(Score), !is.na(GroundSpeed), !is.na(GroundHandling))
    
    ggplot(combos_with_attrs, aes(x = GroundSpeed, y = GroundHandling, color = Score)) +
      geom_point(alpha = 0.6) +
      scale_color_viridis() +
      labs(title = "Ground: Speed VS Handling", 
           x = "Ground Speed", 
           y = "Ground Handling",
           color = "Score") +
      theme_minimal()
  })
  
  output$speedVsAccelerationPlot <- renderPlot({
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
  
  observeEvent(input$filterBtn, {
    filtered_combos <- top_combinations
    
    if (input$preferredCharacter != "") {
      filtered_combos <- filtered_combos %>%
        filter(Character == input$preferredCharacter)
    }
    
    if (input$preferredVehicle != "") {
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
  
  observeEvent(input$optimizeBtn, {
    custom_weights <- attribute_weights
    
    speed_attrs <- c("GroundSpeed", "WaterSpeed", "AirSpeed", "AntiGravitySpeed")
    custom_weights$Weight[custom_weights$Attribute %in% speed_attrs] <- 
      custom_weights$Weight[custom_weights$Attribute %in% speed_attrs] * input$speedWeight * 2
    
    handling_attrs <- c("GroundHandling", "WaterHandling", "AirHandling", "AntiGravityHandling", "Traction")
    custom_weights$Weight[custom_weights$Attribute %in% handling_attrs] <- 
      custom_weights$Weight[custom_weights$Attribute %in% handling_attrs] * input$handlingWeight * 2
    
    custom_weights$Weight[custom_weights$Attribute == "Acceleration"] <- 
      custom_weights$Weight[custom_weights$Attribute == "Acceleration"] * input$accelerationWeight * 2
    
    temp_attr_weights <- custom_weights
    
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
  
  calculate_custom_score <- function(character, vehicle, tire, glider, cust_weights) {
    char_effect <- character_effects$Effect[character_effects$Character == character]
    veh_effect <- vehicle_effects$Effect[vehicle_effects$Vehicle == vehicle]
    tire_effect <- tire_effects$Effect[tire_effects$Tires == tire]
    glider_effect <- glider_effects$Effect[glider_effects$Glider == glider]
    
    total_attrs <- calculate_combo_attributes(character, vehicle, tire, glider)
    if (is.null(total_attrs)) {
      return(NA)
    }
    
    if (character %in% rownames(char_vehicle_interaction) && 
        vehicle %in% colnames(char_vehicle_interaction)) {
      interaction <- char_vehicle_interaction[character, vehicle]
    } else {
      interaction <- 0
    }
    
    attr_contribution <- 0
    for (i in 1:length(attributes)) {
      attr_name <- attributes[i]
      attr_contribution <- attr_contribution + 
        total_attrs[attr_name] * cust_weights$Weight[cust_weights$Attribute == attr_name]
    }
    
    total_score <- char_effect + veh_effect + tire_effect + glider_effect + 
      interaction + attr_contribution
    
    return(total_score)
  }
}

shinyApp(ui = ui, server = server)