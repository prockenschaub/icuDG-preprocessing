from rpy2.robjects.packages import importr

# Install renv for reproducible R package management
utils = importr('utils')
utils.chooseCRANmirror(ind=1)
utils.install_packages('renv')

# Use renv to install all necessary packages
renv = importr('renv')
renv.activate()
renv.restore()
utils.install_packages('units')

# Additionally install demo data
utils.install_packages("mimic.demo", repos="https://eth-mds.github.io/physionet-demo")
utils.install_packages("eicu.demo", repos="https://eth-mds.github.io/physionet-demo")
