# Chess Engine Rating and Optimization with a Bayesian Approach (**ongoing**)
A Bayesian statistics project for analyzing and optimizing the **SchachMaus** chess engine using Elo-based rating systems, tournament predictions, sequential testing, and Bayesian optimization.

## Project Overview

This project applies Bayesian methods to chess engine evaluation and parameter tuning. The work is structured in progressive tasks, from basic rating systems to automated parameter optimization.

## Tasks

### Task 1: Tournament Ranking
**Objective**: Build a Bayesian Elo rating system for chess engines

- Implement Bayesian Elo ratings
- Analyze game data to rank engines (A, B, C, D, E, SchachMaus)
- Use MCMC sampling via Stan for posterior inference
- Generate tier lists with uncertainty quantification

### Task 2: Time Control Integration
**Objective**: Model rating dependence on time control

- Extend base model to include time control effects (linear dependence)
- Convention: compute expected game duration as `time + 40 × increment`
- Produce rating across different time formats (Ultra Bullet, Bullet, Blitz, Rapid)
- Visualize rating uncertainty bands

### Task 3: Live Tournament Prediction
**Objective**: Predict ongoing tournament outcomes using Monte Carlo simulation

- Given results of first 2 rounds, simulate remaining rounds of a 5-round round-robin tournament
- Update posterior beliefs (on engines' ratings) using importance sampling based on observed results
- Calculate probability distributions for SchachMaus's final placement

### Task 4: Sequential Testing
**Objective**: Develop Bayesian sequential testing for engine comparison

- Implement Bayesian SPRT (Sequential Probability Ratio Test)
- Test null hypothesis: `R_base - R_new ≥ E₀` (improvement threshold)
- Determine optimal stopping criteria for game sequences
- Relate test outcomes to established ratings

### Task 5: Manual Engine Tuning
**Objective**: Explore parameter space using sequential testing

- Use the `Rschach` package interface to SchachMaus engine
- Test parameter configurations against baseline
- Employ opening books to ensure game diversity
- Document parameter effects and build initial tuning dataset

### Task 6: Automated Bayesian Optimization
**Objective**: Optimize LMR slope parameter using Gaussian Process optimization

- Apply Bayesian optimization after kernel selection
- Target Late Move Reduction (LMR) slope parameter
- Use rating models to evaluate engine configurations
- Refine parameter selection based on tournament results

## Model Specifications

### Core Rating Model
We use the typical ELO formula:
$E_A = 1 / (1 + 10^\{((R_B - R_A) / 400)\})$
Where `E_A` is the expected score (win rate + 0.5 × draw rate).

### Time-Adjusted Ratings
R_A(tc) = rating[A] + beta[A] × tc


### Draw Probability
$\mathbb{P}$(draw) = p_draw_base × exp(-|rating_diff| / draw_scale)


## Authors

- Dominik Mandić
- Fausto Morando 
- Niccolò Signorelli 
- Fabio Vicig


## Course Information

**VU 105.173 Bayesian Statistics**  
TU Wien, Winter Semester 2025  
**Instructor**: Dr. Daniel Kapla
