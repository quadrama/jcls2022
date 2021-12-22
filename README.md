# Who Knows What in German Dramas? A Composite Annotation Scheme for Knowledge Transfer

This repository contains code and data to reproduce results reported in a publication submitted in the [Journal of Computational Literary Studies](https://jcls.io).

Contents of this repository:

## `data`

This folder contains the annotated plays that are reported in the article. The plays are provided both in the format as used by the annotation tool ([CorefAnnotator](https://doi.org/10.5281/zenodo.1228105)), as well as CSV files exported from the annotation tool. The CSV files are used for the analysis.

## `section-4`: Calculating Inter-Annotator Agreement

This folder contains the code needed to calculate inter-annotator agreement with Gamma.

With bash on a Unix system, you can run it with `python3 iaa.py ../data/round-2/V1/csv/guenderode-udohla_0?.csv`, to compare the two annotations of Günderrodes' *Udohla*. The output is a line formatted to be used as a LaTeX table.

To generate an entire table, you can use the following command:
```sh
for i in $( ls ../data/round-2/V1/csv/*01.csv)
do 
    python3 iaa.py $i ${i/01/02}
done
```

This will iterate over all files in `data/round-2/V1`, and call the python script for each file. The python script gets the versions by two annotators as arguments.


### Performance
The script makes use of the [pygamma-agreement](https://github.com/bootphon/pygamma-agreement) library, which in turn relies on a highly optimized library for integer linear programming. Please follow their installation instructions to use the CBC solver.

## `section-5`: Analysing Annotated Knowledge Transfers

The python (v. 3.10.1) script can be run using the command
```sh
$ python3 annotations_per_x_tokens.py ../data -x 1000
```

To install the needed packages for the R scripts, issue the following command in a R console:
```R
> install.packages(c("DramaAnalysis", "tidyverse", "knitr", "kableExtra"))
```
