# Bayesian LIT/PIT to UPT Conversion

This repository contains R scripts and pre-trained Bayesian models to convert benthic cover data collected using LIT (Line Intercept Transect) or PIT (Point Intercept Transect) into equivalent UPT (Underwater Photo Transect) estimates.

## Contents
- Scripts/: R scripts for running the model and applying it
- Models/: Pre-trained `.rds` models for LIT and PIT
- Data/: Example raw data

## How to Use
1. Prepare your LIT/PIT data in wide format (like the example)
2. Load the model using `readRDS()`
3. Use the provided script to run predictions