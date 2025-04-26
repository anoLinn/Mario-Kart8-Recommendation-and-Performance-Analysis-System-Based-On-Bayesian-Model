dir.create("~/mario-kart-8-recommender")

files_to_copy <- c(
  "app.R",
  "world_record_character_effects.csv",
  "world_record_vehicle_effects.csv",
  "world_record_tire_effects.csv",
  "world_record_glider_effects.csv",
  "world_record_attribute_weights.csv",
  "top_50_predicted_combinations.csv",
  "DRIVERS.csv",
  "VEHICLES.csv",
  "TIRES.csv",
  "GLIDERS.csv",
  "char_vehicle_interaction.csv" 
)

for (file in files_to_copy) {
  if (file.exists(file)) {
    file.copy(file, "~/mario-kart-8-recommender/")
  }
}

setwd("~/mario-kart-8-recommender")
