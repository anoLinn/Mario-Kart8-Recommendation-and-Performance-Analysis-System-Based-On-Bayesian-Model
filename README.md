# 🏎️ Let's Find Mario Kart 8's Ultimate Combinations: A Bayesian Adventure on the Racetrack

**Team Members: Linn Wang, Maddy Nomer**

## 📋 Overview

Have you ever wondered why some people always seem to win at Mario Kart 8? The secret might be hiding in their choice of character, vehicle, tires, and glider! Our project will use Bayesian statistics to analyze world record data and discover which combinations reign supreme on each track.

Unlike the simplified "this character + this kart = fastest" formulas, our model will consider the complex relationship between track characteristics and combinations. Want to know what combo to use on Rainbow Road? Or how to get an edge at Wario Stadium? Our Bayesian model will tell you!

## 🔍 Datasets

- **World record data**: Who set records on which tracks using what combinations
- **Component performance data**: Detailed stats for each character, vehicle, tire, and glider
- **Track feature data**: Characteristics of different tracks like number of turns, straight sections, jump points, etc.

## 🧠 Bayesian Model

In this project, we'll:
1. Build **Bayesian hierarchical models** to analyze how different combinations perform across tracks
2. Explore **component interactions** (like how certain characters work exceptionally well with certain vehicles)
3. Find **how track features influence optimal choices** (some combinations excel on tracks with lots of turns)
4. Develop a **personalized recommendation system** that suggests the best combinations based on your preferences and the track

## 🎮 Goals

When this project is complete, we want to provide:
- **Top 10 combination recommendations** for each track
- **Visual guides** to combination selection, including performance heatmaps
- A simple **interactive tool** where you input a track and get personalized recommendations

## 🚀 Other Details

We'll be using R for our analysis:
- **Stan** for Bayesian modeling and MCMC sampling
- **tidyverse** for data wrangling and creating gorgeous visualizations
