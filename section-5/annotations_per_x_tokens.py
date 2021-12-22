#!/usr/bin/env python3

"""
Script for counting the number of transfer annotations per x tokens
"""

import argparse
import glob
import os
import statistics


def flatten(d):
    v = [[i] if not isinstance(i, list) else flatten(i) for i in d]
    return [i for b in v for i in b]


def get_args():
    """Handle command line argument parsing"""
    parser = argparse.ArgumentParser(
        description="Transfer annotations per x tokens"
    )
    parser.add_argument(
        "path", metavar="PATH", type=str, help="Path to the corpus"
    )
    parser.add_argument(
        "--xtokens",
        "-x",
        default=1000,
        metavar="NUM",
        type=int,
        help="Per how many tokens shouls the annotations be counted?",
    )
    return parser.parse_args()


def get_filepaths(root_path: str, file_glob: str):
    """Retreive all filepaths in dir"""
    filelist = []
    for (dirpath, subdirs, filenames) in os.walk(root_path):
        filelist.extend(
            [
                glob.glob(os.path.join(dirpath, subdir, file_glob))
                for subdir in subdirs
            ]
        )
    return flatten(filelist)


def annotations_per_x_tokens(filepaths, args, docAvg=True):
    mean_annotations_per_x_tokens = []
    for filepath in filepaths:
        xtoken_annotation_count_map = {}
        annotation_counter = 0
        with open(filepath, "r") as f:
            file = f.read()
        fsplit = file.split()
        for i, token in enumerate(fsplit):
            if token.startswith("<rs"):
                annotation_counter += 1
            if i % args.xtokens == 0 and i != 0:
                if docAvg:
                    xtoken_annotation_count_map[i] = annotation_counter
                else:
                    xtoken_annotation_count_map[
                        filepath + str(i)
                    ] = annotation_counter
                annotation_counter = 0
        if docAvg:
            try:
                mean_annotations_per_x_tokens.append(
                    sum(xtoken_annotation_count_map.values())
                    / len(xtoken_annotation_count_map.values())
                )
            except ZeroDivisionError:
                print(
                    "Not enough annotations per {} tokens in {}. Exluding file from calculation.".format(
                        args.xtokens, filepath
                    )
                )
    if docAvg:
        return (
            sum(mean_annotations_per_x_tokens)
            / len(mean_annotations_per_x_tokens),
            statistics.stdev(mean_annotations_per_x_tokens),
        )
    else:
        return (
            sum(xtoken_annotation_count_map.values())
            / len(xtoken_annotation_count_map.values()),
            statistics.stdev(xtoken_annotation_count_map.values()),
        )


def main():
    args = get_args()
    filepaths_01 = get_filepaths(args.path, "*01*.xml")
    filepaths_02 = get_filepaths(args.path, "*02*.xml")
    filepaths = filepaths_01 + filepaths_02
    x_tokens_01_mean, x_tokens_01_sd = annotations_per_x_tokens(
        filepaths_01, args
    )
    x_tokens_02_mean, x_tokens_02_sd = annotations_per_x_tokens(
        filepaths_02, args
    )
    x_tokens_all_mean, x_tokens_all_sd = annotations_per_x_tokens(
        filepaths, args
    )
    print(
        "Anno 1 ({} files): {} (+/-{})\nAnno 2 ({} files): {} (+/-{})\nAll ({} files): {} (+/-{})".format(
            len(filepaths_01),
            x_tokens_01_mean,
            x_tokens_01_sd,
            len(filepaths_02),
            x_tokens_02_mean,
            x_tokens_02_sd,
            len(filepaths),
            x_tokens_all_mean,
            x_tokens_all_sd,
        )
    )


if __name__ == "__main__":
    main()
