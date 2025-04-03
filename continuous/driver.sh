mkdir SIMS
mkdir SIMStype1error
mkdir figs
Rscript driver.R 1 1e4 1e2
Rscript driver.R 1 1e4 1e3

Rscript driver_type1_error.R 1 1e4 1e2
Rscript driver_type1_error.R 1 1e4 1e3

python analysis.py
python analysis_type1_error.py

