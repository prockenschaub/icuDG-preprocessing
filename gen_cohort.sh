#!/bin/bash

for src in aumc eicu hirid miiv mimic
do
	Rscript "$1.R" --src $src
done
