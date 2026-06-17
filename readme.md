# Data and R scripts for the article Bostal et al 2026 
## MAINTAINING LOW DENSITIES OR ACHIEVING ERADICATION? EVALUATING FERAL HORSE MANAGEMENT STRATEGIES UNDER DIFFERENT PREDATION SCENARIOS.

Authors:

Bostal Franco

Amodeo Martín R.

Scorolli Alberto L. 

Zalba Sergio M.

This repository will be preserved by Zenodo at publication

## Project description
This R project consists in the datasets and scripts used for the analyses and plots exposed in the article.

## R Project Structure:

Stage-based demographic matrix for deterministic and scotchastic projections. Every script is supposed to be run within the Rproject environment (relative path indicated).

* Folder /data. Datasets in CSV format used for stochastic projection (mean and standard deviation of each demographic parameter with and without predation) and deterministic projection (maximum value of each demographic parameter with and without predation).

* Folder /R. R scripts containing custom functions used for data loading and matrix constructions. They are called from the main scripts in the analyses folder (relative path indicated).

* Folder /analysis. R scripts for conducting the main analyses in the article. They are supposed to be run within the Rproject environment (relative path indicated).

  * 01_simulation.R. Data loading and run base population projections.

  * 02_simulation_loop.R. Data loading and run population projections for all the scenarios.

  * 03_bubble_plot.R, 04_parallel_plot.R, 05_recovery_plot.R. Build the Figures of the manuscript.

  * Folder /fig and /table. Figures and tables included in the manuscript and supplementary material.

For a full description of the study, data, experimental design, analyses and conclusions please see [DOI].
