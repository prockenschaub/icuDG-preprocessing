if (!require("renv")) install.packages("renv")

renv::activate()
renv::status()
renv::restore()

# utils package isn't installed automatically
install.packages("units")

# same for the demo datasets
install.packages("mimic.demo", repos="https://eth-mds.github.io/physionet-demo")
install.packages("eicu.demo", repos="https://eth-mds.github.io/physionet-demo")
