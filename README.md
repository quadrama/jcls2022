# Who Knows What in German Dramas? A Composite Annotation Scheme for Knowledge Transfer

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

This will iterate over all files in `data/round-2/V1`, and call the python script for each file. The python scripts gets the versions by two annotators as arguments.

## `section-5`: Analysing Annotated Knowledge Transfers