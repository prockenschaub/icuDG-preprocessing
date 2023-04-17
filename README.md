# Patient cohorts for Yet Another ICU Benchmark - Setting the Standard for Clinical Prediction Modeling

## Paper

This repo uses the `ricu` R package to derive patient cohorts for prediction tasks from the following four intensive care databases: 

* [AUMCdb](https://github.com/AmsterdamUMC/AmsterdamUMCdb)
* [HiRID](https://hirid.intensivecare.ai/)
* [eICU](https://eicu-crd.mit.edu/)
* [MIMIC IV](https://mimic.mit.edu/)


If you use this code in your research, please cite the following publication:

```
@article{rockenschaub2023generalisability,
      title={Generalisability of deep learning-based early warning in the intensive care unit: a retrospective empirical evaluation}, 
      author={Patrick Rockenschaub and Adam Hilbert and Tabea Kossen and Falk von Dincklage and Vince Istvan Madai and Dietmar Frey},
      year={2023},
      eprint={2303.15354},
      archivePrefix={arXiv},
      primaryClass={cs.LG}
}

```

This paper can be found on arxiv: https://arxiv.org/abs/2303.15354


## Acknowledgements

The code in this repository heavily utilises the `ricu` R package, without which deriving these cohorts would have been much more difficult. If you use the code in this repo, please go give their repo a star :)


## To replicate the cohorts:

### Step 0: Environment and Prerequisites

Run the following commands to clone this repo:

```
git clone https://github.com/prockenschaub/icuDG-preprocessing.git
cd icuDG-preprocessing/
```

All data extractions were run using R 4.2.2 on an Apple M1 Max with Ventura 13.2.1. An `renv` lock file was created to install all necessary package dependencies. To recreate the environment, start an R session and call the following commands: 

```r

install.packages("renv")
renv::restore()

```

### Step 1: Load the datasets into `ricu`

In order to access the full datasets through R, you need to download them and make them available to `ricu`. Please follow the instructions given by the `ricu` package: `?ricu::import_src`.

For quick experimentation, `ricu` comes with two demo datasets: `mimic.demo` and `eicu.demo`. These are small, openly available subsets of mimic and eicu that allow for easy prototyping. They should have been installed by `renv`. If they aren't, please see the respective Github pages [here](https://github.com/eth-mds/mimic-demo) and [here](https://github.com/eth-mds/eicu-demo).


### Step 2: Generate the Cohorts

This repository currently allows for the extraction of three distinct prediction tasks: 

1. ICU mortality after 24 hours: [mortality.R](mortality.R)
2. Acute Kidney Injury within the next 6 hours: [aki.R](aki.R)
3. Sepsis within the next 6 hours: [sepsis.R](sepsis.R)

All three tasks rely on a shared data cleaning provided in [base_cohort.R](base_cohort.R), which defines and stores a subset of patients in each dataset with sufficient data quality. [base_cohort.R](base_cohort.R) therefore needs to be called *before* any of the task-specific cohorts can be generated. 

Once [base_cohort.R](base_cohort.R) was run, data for a single task from a single dataset can be extracted via: 
```bash 
Rscript "mortality.R" --src mimic_demo
```

where `mortality.R` should be replaced with the task file of interest and `mimic_demo` with the database of interest (one of `mimic_demo`, `eicu_demo`, `aumc`, `hirid`, `eicu`, `mimic`, `miiv`). Data can be extracted from all datasets simultaneously via `bash gen_cohort.sh mortality`.

The output directory for the extracted data can be set in [config.json](config.json)


## License
This source code is released under the MIT license, included [here](LICENSE).
