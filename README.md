# Metal Exposures and Coronary Artery Calcification

## Overview
We longitudinally assessed the link between baseline exposure to metals as measured in urine and changes in coronary artery calcification in the Multi-Ethnic Study of Atherosclerosis (n=6,418). We found that toxic metals cadmium, tungsten, uranium, and essential metals cobalt, copper, and zinc were significantly associated with changes in coronary artery calcification over time, separately and as a mixture.

## Publication
McGraw, K, Schilling, K, Glabonjat, R. et al. Urinary Metal Levels and Coronary Artery Calcification: Longitudinal Evidence in the Multi-Ethnic Study of Atherosclerosis. JACC. 2024 Oct, 84 (16) 1545–1557.
- Journal: Journal of the American College of Cardiology
- Year: 2024
- DOI/Link: https://doi.org/10.1016/j.jacc.2024.07.020
- https://www.jacc.org/doi/epdf/10.1016/j.jacc.2024.07.020

## Research Question
Does exposure to metals contribute to subclinical cardiovascular disease via increased coronary artery calcification?

## Methods
- **Study population:** The Multi-Ethnic Study of Atherosclerosis
- **Exposures:** Urinary cadmium, tungsten, uranium, cobalt, copper, and zinc
- **Outcome:** Spatially-weighted coronary artery calcification (CAC-SW)
- **Statistical analysis:** Linear mixed models for repeated measures of CAC-SW, Bayesian Kernel Machine Regression for metal mixtures

## Key Findings
- Comparing the highest to lowest quartile of urinary cadmium, CAC levels were 51% (95% CI: 32%, 74%) higher at baseline and 75% (95% CI: 47%, 107%) higher over the 10-year period. 
- For urinary tungsten, uranium, and cobalt, the corresponding CAC levels over the 10-year period were 45% (95% CI: 23%, 71%), 39% (95% CI: 17%, 64%), and 47% (95% CI: 25%, 74%) higher, respectively. 
- For copper and zinc, the corresponding estimates dropped from 55% to 33% and from 85% to 57%, respectively, after adjustment for clinical factors.
- The associations of metals with CAC were comparable in magnitude to those for classical CVD risk factors.

## Repository Contents
- `analysis_script.R` - Complete analysis code with comments
- `published_article.pdf` - Published manuscript (if included)
- `README.md` - This file

## Data
[Explain data availability - e.g., "Data available upon request from [source]" 
or "Synthetic data included for demonstration purposes"]

## Requirements
**R version:** [your R version]

**Required packages:**
- tidyverse
- nlme
- lme4
- bkmr

Install with:
```R
install.packages(c("tidyverse", "nlme", "lme4", "bkmr"))
```

## Usage
[Brief instructions on how to run the analysis]

## Contact
For questions about this research, contact katlyn.mcgraw.km@gmail.com

## Citation
If you use this code, please cite:

**Published research:**
McGraw, K, Schilling, K, Glabonjat, R. et al. Urinary Metal Levels and Coronary Artery Calcification: Longitudinal Evidence in the Multi-Ethnic Study of Atherosclerosis. JACC. 2024 Oct, 84 (16) 1545–1557. DOI: https://doi.org/10.1016/j.jacc.2024.07.020

**Code repository:**
McGraw KE. (2024). Metal Exposures and Coronary Artery Calcification Analysis Code. 
GitHub repository: https://github.com/kemcgraw/metal-exposures-coronary-calcification
