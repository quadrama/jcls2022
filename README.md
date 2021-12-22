# Who Knows What in German Dramas? A Composite Annotation Scheme for Knowledge Transfer

Contents of this repository:

## `data`

This folder contains the annotated plays that are reported in the article. The plays are provided both in the format as used by the annotation tool ([CorefAnnotator](https://doi.org/10.5281/zenodo.1228105)), as well as CSV files exported from the annotation tool. The CSV files are used for the analysis.

## `section-4`: Calculating Inter-Annotator Agreement

This folder contains the code needed to calculate inter-annotator agreement with Gamma.

With bash on a Unix system, you can run it with `python3 iaa.py ../data/round-2/V1/csv/guenderode-udohla_0?.csv`, to compare the two annotations of Günderrodes' *Udohla*. The output is a LaTeX table.

## `section-5`: Analysing Annotated Knowledge Transfers