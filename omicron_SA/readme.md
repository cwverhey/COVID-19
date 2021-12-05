# omicron_SA
Simulation app to estimate R-values for Delta and Omicron, based on total confirmed SARS-CoV-2 cases in South Africa. Written in R Shiny.

## Use
The app is live here: https://cwverhey.shinyapps.io/omicron_SA/

## Purpose
This simulation allows the user to fit the rising SARS-CoV-2 cases in South Africa to hand-picked values for R(delta) and R(omicron), as well as the date of the initial Omicron patient.

At the time of writing, this illustrates how uncertain the ratio between R(delta) and R(omicron) is: real-world data will fit the model similarly over a vast range of R(omicron) values, as long as a suitable date for the first omicron case is chosen. The later the initial omicron case, the higher R(omicron) needs to be.

The data is updated daily. The initial parameters are chosen for a (local) optimum fit. In the app, the 'Info' tab provides further information.

## Adapt

* `load_cache.R` generates (or updates) cached data; it stores case data from Our World In Data in `owid_SA.RData`, and calls `find_optimum.R` which stores the optimal simulation parameters in `owid_SA_rmse.RData`. Edit `load_cache.R` it to set the desired working directory where it will create these cache files.
* run `app.R`
