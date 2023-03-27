#!/bin/bash

for src in aumc eicu eicu_demo hirid miiv mimic mimic_demo
do
	Rscript "$1.R" --src $src
done
