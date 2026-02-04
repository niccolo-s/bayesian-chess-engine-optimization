# Bayesian Chess Engine Optimization

A comprehensive statistical framework for rating, analyzing, and optimizing the **SchachMaus** chess engine. This project implements Bayesian Elo modeling, sequential testing (SPRT), and Gaussian Process optimization to rigorously improve engine performance.

**Course:** VU 105.173 Bayesian Statistics (TU Wien, Winter 2025)

## Project Highlights

The project is divided into two primary phases: **Tournament Analysis** (inferring latent skill from game data) and **Engine Optimization** (tuning internal parameters to maximize playing strength).

### 1. Bayesian Elo Rating System
We developed a hierarchical Bayesian model to estimate engine strength while accounting for real-world nuances:
- **Time Control Dependence:** Modeled rating as a linear function of time control (`rating + beta * tc`), allowing predictions across formats (Bullet, Blitz, Rapid).
- **Draw Probability:** Implemented a rating-dependent draw model where the probability of a draw decays exponentially as the skill gap increases.
- **First-Move Advantage:** Explicitly quantified the "White advantage" in Elo terms.

### 2. Live Tournament Prediction
Using **Sampling Importance Resampling (SIR)**, we updated our pre-computed posterior beliefs with live game results from an ongoing tournament. This allowed us to:
- Dynamically refine rating estimates in real-time.
- Calculate the expected utility of betting on specific tournament outcomes via Monte Carlo simulation.

### 3. Sequential Testing (SPRT)
To efficiently benchmark engine modifications, we implemented a Bayesian **Sequential Probability Ratio Test (SPRT)**.
- **Efficiency:** Replaced fixed-length matches with a likelihood-ratio test that terminates early when sufficient evidence supports (or rejects) an Elo improvement.
- **Robustness:** Validated parameter changes against a diverse opening book to ensure statistical independence.

### 4. Bayesian Parameter Optimization
We applied **Gaussian Process (GP)** regression to guide the tuning of critical search parameters (LMR and RFP).
- **Surrogate Modeling:** Approximated the expensive "Elo vs. Parameter" function with a GP kernel.
- **Acquisition Functions:** Used Expected Improvement (EI) to identify the most promising parameter values to test next.
- **Iterative Process:** Manually executed the optimization loop (Tournament → GP Update → Acquisition) to refine parameters over multiple steps, culminating in a simultaneous 2D optimization for `RFP_intercept` and `RFP_slope`.

## Key Results

Our optimization pipeline produced **Engine 4_RFP**, which demonstrated statistically significant superiority over the baseline.

| Parameter | Optimal Value | Description |
| :--- | :--- | :--- |
| **NMP Intercept** | `4` | Null Move Pruning base depth |
| **LMR Intercept** | `1` | Late Move Reduction base |
| **LMR Slope** | `0.3015` | Reduction scaling factor (Task 6) |
| **RFP Intercept** | `300` | Reverse Futility Pruning margin (Task 7) |
| **RFP Slope** | `0` | Margin scaling with depth (Task 7) |

*Final verification via MCMC confirmed a probability of superiority > 99% against the baseline engine.*

## Methodology & Tools

- **Statistical Modeling:** Stan (RStan) for MCMC sampling.
- **Optimization:** `DiceKriging` (R) for Gaussian Process regression.
- **Engine Interface:** Custom `Rschach` package.
- **Visualization:** `ggplot2` for posterior distributions and optimization landscapes.

## Authors

*   **Dominik Mandić**
*   **Fausto Morando**
*   **Niccolò Signorelli**
*   **Fabio Vicig**
