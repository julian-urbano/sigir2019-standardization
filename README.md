This repository contains the data and source code for the following paper:

* J. Urbano, H. Lima and A. Hanjalic, "[A New Perspective on Score Standardization](http://julian-urbano.info/files/publications/077-new-perspective-score-standardization.pdf)", *International ACM SIGIR Conference on Research and Development in Information Retrieval*, 2019.

A [single ZIP file](https://github.com/julian-urbano/sigir2019-standardization/master.zip) can be downloaded as well.

## Project Structure

* `data/` Input data files.
* `output/` Generated output files.
* `R/` Source code in R.
* `scratch/` Temporary files generated in the process.

All code is written for [R](https://www.r-project.org). You will need the following packages installed from CRAN: `rio` (>=0.5.19), `ircor`, `doParallel`.

## How to reproduce the results in the paper 

The source files in `R/` need to be run in order. You can run each file individually by running `Rscript R/<file>.R`. They will store intermediate data in `scratch/` and the final data in `out/`.

**It is important that you always run from the base directory**.

1. `R/01-within.R` fits all the marginal distributions (section 4.1).
2. `R/02-between.R` transforms all distributtions fitted in step 1 to have the same expected value as in the original data (section 4.1).
7. `R/99-paper.R` generates all figures in the paper and stores them in `out/figs/`.

It takes several days to run all the code, so it is ready to run in parallel. Most of the above code parallelizes using function `foreach` in R's package [`doParallel`](https://cran.r-project.org/web/packages/doParallel/index.html). In particular, it will use all available cores in the machine. Check the individual code files to modify this behavior.

## Custom test collections

You can easily run the code with your own initial test collection. Add the matrix of topic-by-system scores in `data/` using the name `<collection>_<measure>.csv` (see for instance file [`data/web2012_ap.csv`](/data/web2012_ap.csv)). Then, edit file `src/common.R` to add the new data:

```r
.COLLECTIONS <- c("web2010", "web2011", "web2012", "web2013", "web2014")
.MEASURES <- c("ap", "ndcg20", "err20", "p10", "p20", "rr")
```

Note that the code will run for all combinations of collection and measure. For more specific modifications, edit the corresponding source file in `src/` (see above). Note also that the script `src/99-paper.R` is only intended to generate the figures in the paper. If you customize something and want a similar analysis, you will need to extend this script yourself.


## License

* The TREC results in `data/` are anonymized and posted here with permission from the organizers.
* Databases and their contents are distributed under the terms of the [Creative Commons Attribution-ShareAlike 4.0 International License](http://creativecommons.org/licenses/by-sa/4.0/).
* Software is distributed under the terms of the [MIT License](https://opensource.org/licenses/MIT).

When using this archive, please [cite](CITE.bib) the above paper:

    @inproceedings{urbano2019standardization,
      author = {Urbano, Juli\'{a}n and Lima, Harlley and Hanjalic, Alan},
      booktitle = {International ACM SIGIR Conference on Research and Development in Information Retrieval},
      title = {{A New Perspective on Score Standardization}},
      year = {2019}
    }
