# Construct a SummarizedExperiment object from raw expression table

This function reads a raw expression table, performs feature-level
filtering, optional outlier masking, missing-value imputation, and
stability filtering, and returns a `SummarizedExperiment` object
together with diagnostic gene lists.

## Usage

``` r
rawdata2se(
  exp_file,
  obs_col = "Genes",
  raw_prefix = "auto",
  enable_detect_outlier_gene = FALSE,
  fc_threshold = 5,
  min_valid_groups = 0,
  frac_NA_threshold = 0.5,
  min_stable_groups = 0,
  cv_threshold = 0.5
)
```

## Arguments

- exp_file:

  Path to the raw expression table file. The file must contain one
  column specifying feature identifiers and multiple columns containing
  raw expression values.

- obs_col:

  Name of the column in `exp_file` used to define feature identifiers.
  Only the substring before the first semicolon (`;`) will be used as
  the feature name.

- raw_prefix:

  Character string used to identify raw expression columns. If \\auto\\
  (default), raw intensity columns will be detected automatically from
  common raw file suffixes such as \\raw\\, \\mzML\\, \\mzXML\\, and
  similar filename-style endings.

- enable_detect_outlier_gene:

  Logical flag indicating whether to mask extreme outlier measurements
  within each condition based on fold-change from the median.

- fc_threshold:

  Numeric threshold for fold-change–based outlier detection. Values
  exceeding this fold change relative to the condition median will be
  set to `NA` when outlier detection is enabled.

- min_valid_groups:

  Minimum number of conditions in which a feature must pass the
  missing-value filter to be retained. Features with fewer valid
  conditions are removed.

- frac_NA_threshold:

  Maximum allowed fraction of missing values within a condition for that
  condition to be considered valid for a given feature.

- min_stable_groups:

  Minimum number of conditions in which a feature must show low
  variability (coefficient of variation below `cv_threshold`) to be
  retained.

- cv_threshold:

  Numeric threshold on coefficient of variation (CV) used to define
  feature stability within each condition.

## Value

A list with class `"RawDataSE"` containing:

- `se`: A `SummarizedExperiment` object storing raw intensities, imputed
  intensities, CPM-normalized values, and z-scored expression.

- `un_stable_gene`: A data frame of features removed due to high
  variability across conditions.

- `missing_gene_df`: A data frame of features removed due to excessive
  missing values.
