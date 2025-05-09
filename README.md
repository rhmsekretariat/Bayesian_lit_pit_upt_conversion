# Bayesian LIT/PIT to UPT Conversion

This repository contains R scripts and pre-trained Bayesian models to convert benthic cover data collected using LIT (Line Intercept Transect) or PIT (Point Intercept Transect) into equivalent UPT (Underwater Photo Transect) estimates.

## Contents
- Scripts/: Raw data and R scripts for running the model and application for converting new LIT/PIT data
- Models/: Pre-trained `.rds` models for LIT and PIT

## How to Use for Data Conversion
1. Prepare your LIT/PIT data in wide format (like the example)
2. Load the model using `readRDS()`
3. Use the provided script (Application.R) to run predictions

#Note
Note: The fitUPT_LIT.rds and fitUPT_PIT models are pre-trained, meaning they have been fitted using the available data. You do not need to fit the model again — simply load it with readRDS() and apply it to your input data.
