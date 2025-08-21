# Global Fishing Watch, Ocean Fronts, and Megafauna Analysis in the Mozambique Channel
Analysis of Global Fishing Watch datasets combined with ocean front dynamics to study the spatial distribution and interactions of megafauna in 
the Mozambique Channel.

## 📂 Project Structure

```text
gfw-fronts-megafauna/
├── .git/                     # Git repository metadata
├── .Rproj.user/             # RStudio project-specific settings
├── data-raw/                # Raw data inputs (unprocessed)
│   ├── fronts_dynamical/    # Dynamical fronts data
│   ├── fronts_thermal/      # Thermal fronts data
│   ├── gfw_mzc_rds/        # GFW MZC RDS files
│   └── untitled folder/     # Placeholder folder (rename later)
├── legacy/                  # Old/deprecated scripts or data
├── outputs/                 # Generated outputs (figures, tables, results)
├── R/                       # R source functions & scripts
├── renv/                    # R environment managed by {renv}
│   ├── library/            # Cached R packages for this project
│   ├── staging/            # Temporary staging area for installs
│   ├── activate.R          # Renv project activation script
│   ├── settings.json      # Renv settings and config
│   └── .gitignore         # Ignore renv cache in Git
├── scripts/                # Main analysis scripts
├── tests/                  # Unit tests and reproducibility checks
├── .gitignore              # Git ignore rules
├── .gitattributes          # Git attributes configuration
├── .Rprofile               # R startup configuration
├── gfw-fronts-megafauna.Rproj  # RStudio project file
├── LICENSE                 # License file
├── README.md               # Project documentation
└── renv.lock              # Locked R package versions for reproducibility