# ndhs2024-neonatal-death-risk-model
Survey-weighted neonatal death risk stratification using nationally representative Nigeria DHS 2024 data. This study develops and internally validates a prediction model integrating maternal, perinatal, and immediate newborn-care factors to improve population-level neonatal risk assessment, surveillance, and policy targeting.
# NDHS 2024 Neonatal Death Risk Model

**Survey-weighted neonatal death risk stratification in Nigeria using the 2024 Demographic and Health Survey: a nationally representative cross-sectional prediction study with internal validation**

## Overview

This repository contains the analytic pipeline, tables, and figures for a nationally representative neonatal death risk-stratification study using the **Nigeria Demographic and Health Survey (NDHS) 2024** birth recode. The study evaluates whether adding a prespecified **perinatal and immediate newborn-care block** improves prediction of neonatal death beyond maternal sociodemographic characteristics alone.

## Authors

**Sunday A. Adetunji, MD, MPH**  
College of Health, Oregon State University  
ORCID: https://orcid.org/0000-0001-9321-9957  
Email: adetunjs@oregonstate.edu

**Rhoda O. Oyewusi, RN, RM, PON**  
Department of Midwifery, University of Lagos  
Email: royebunmi@gmail.com

## Study summary

We conducted a survey-weighted cross-sectional prediction study of **104,557 live births** recorded in the NDHS 2024 birth recode. The primary outcome was **neonatal death**, defined as death at age 0 months, with all surviving live births retained as non-events.

The extended model incorporated maternal sociodemographic variables plus a prespecified perinatal/newborn-care block including:

- birthweight  
- plurality  
- mode and place of delivery  
- skilled birth attendance  
- skin-to-skin contact  
- breastfeeding counselling  
- observed breastfeeding  
- duration of pregnancy  
- preceding birth interval

## Key findings

- Overall survey-weighted neonatal death rate: **36.4 per 1000 live births**
- Strongest adjusted predictors included:
  - **duration of pregnancy ≤8 months**: aOR 10.07 (95% CI 7.99–12.69)
  - **multiple gestation**: aOR 3.93 (3.31–4.67)
  - **birthweight ≥4.0 kg**: aOR 3.13 (2.05–4.78)
- **Breastfeeding counselling** was associated with lower odds of neonatal death: aOR 0.29 (0.12–0.69)
- Model discrimination improved from **AUC 0.607** (baseline) to **0.679** (extended); optimism-corrected AUC for the extended model was **0.672**
- The extended model showed **excellent internal calibration**

## Repository structure

```text
.
├── figures/
├── tables/
├── ndhs2024_neonatal_dashboard_pipeline.R
└── README.md
Reproducibility

The main analysis script is:

ndhs2024_neonatal_dashboard_pipeline.R

To reproduce the analysis:

Obtain access to the NDHS 2024 birth recode through The DHS Program.
Place the approved source dataset in your local working directory.
Update file paths in the R script as needed.
Run the pipeline to regenerate analytic tables, model outputs, and figures.
Data availability

The raw NDHS 2024 individual-level data are not redistributed in this repository. Data access must be requested directly from The DHS Program in accordance with their data-use policies.

Interpretation

This repository supports a programmatic neonatal risk-stratification framework for Nigeria. The model is intended for population-level surveillance, policy planning, and quality improvement, not yet for stand-alone individual bedside decision-making. External validation is still required.

Cite this work
Preferred manuscript citation

Adetunji SA, Oyewusi RO. Survey-weighted neonatal death risk stratification in Nigeria using the 2024 Demographic and Health Survey: a nationally representative cross-sectional prediction study with internal validation. Unpublished manuscript. 2026.

Software / repository citation

Adetunji SA, Oyewusi RO. ndhs2024-neonatal-death-risk-model [GitHub repository]. 2026. Available at:
https://github.com/drsunday-ade/ndhs2024-neonatal-death-risk-model

BibTeX
@misc{adetunji2026ndhsneonatalrepo,
  author       = {Adetunji, Sunday A. and Oyewusi, Rhoda O.},
  title        = {ndhs2024-neonatal-death-risk-model},
  year         = {2026},
  publisher    = {GitHub},
  howpublished = {\url{https://github.com/drsunday-ade/ndhs2024-neonatal-death-risk-model}},
  note         = {Repository for: Survey-weighted neonatal death risk stratification in Nigeria using the 2024 Demographic and Health Survey: a nationally representative cross-sectional prediction study with internal validation}
}

@unpublished{adetunji2026ndhsneonatalmanuscript,
  author = {Adetunji, Sunday A. and Oyewusi, Rhoda O.},
  title  = {Survey-weighted neonatal death risk stratification in Nigeria using the 2024 Demographic and Health Survey: a nationally representative cross-sectional prediction study with internal validation},
  year   = {2026},
  note   = {under-peer-review/Springer Maternal and Childhealth Journal}
}
Contact

For questions about the analysis, collaboration, or reproducibility, contact:

Sunday A. Adetunji, MD, MPH
adetunjs@oregonstate.edu


This version is aligned with your manuscript’s title, design, sample size, principal findings, and interpretation. :contentReference[oaicite:0]{index=0} :contentReference[oaicite:1]{index=1}

Best final upgrade: mint a **Zenodo DOI** for the GitHub release and replace the GitHub-only citation with the DOI-based citation.
