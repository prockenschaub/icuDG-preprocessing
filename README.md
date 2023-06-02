# Generating Patient cohorts for Yet Another ICU Benchmark

## Paper

This repo uses the `ricu` R package to derive patient cohorts for prediction tasks from the following four intensive care databases: 

* [AUMCdb](https://github.com/AmsterdamUMC/AmsterdamUMCdb)
* [HiRID](https://hirid.intensivecare.ai/)
* [eICU](https://eicu-crd.mit.edu/)
* [MIMIC IV](https://mimic.mit.edu/)


If you use this code in your research, please cite the following publication:

```


```

This paper can be found on arxiv: 


## Acknowledgements

The code in this repository heavily utilises the `ricu` R package, without which deriving these cohorts would have been much more difficult. If you use the code in this repo, please go give their repo a star :)

This repo is based on earlier work by [Rockenschaub et al. (2023)](https://arxiv.org/abs/2303.15354), which can be found at https://github.com/prockenschaub/icuDG-preprocessing


## To replicate the cohorts:

Run the following commands to clone this repo:

```
git clone https://github.com/rvandewater/YAIB-cohorts.git
cd YAIB-cohorts
```

Once you have cloned the repo, all cohorts can be created directly from within R or via an interface from python. Instructions for each can be found at: 

- R: [README.md](R/README.md)
- Python: [README.md](Python/README.md)  

Note: due to some recent bug fixes in ricu, the extracted cohorts might differ marginally to those published in the benchmarking paper. 


## License
This source code is released under the MIT license, included [here](LICENSE).
