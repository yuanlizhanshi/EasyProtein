# Save plots in multiple formats with standardized resolution

Save a plot object into PDF, TIFF, and PNG formats simultaneously with
predefined scaling rules for width, height, and resolution. Output files
are automatically organized into subdirectories under a `Fig/` folder.

## Usage

``` r
saveplot(object, filenames = NULL, width = NULL, height = NULL, dpi = 600)
```

## Arguments

- object:

  A plot object to be saved. Can be a `ggplot` object or a grid
  graphical object such as `gList`.

- filenames:

  A character string specifying the base filename (without file
  extension) for the output figures.

- width:

  Numeric. Figure width in inches. Must be provided.

- height:

  Numeric. Figure height in inches. Must be provided.

- dpi:

  Numeric. Resolution in dots per inch. Default is `600`.

## Value

Invisibly returns `NULL`. The function is called for its side effect of
writing figure files to disk.

## Details

This function is designed for publication-quality figure export,
supporting both ggplot objects and grid graphical objects (e.g.,
`gList`).

The function creates a directory structure:

- Fig/PDF:

  PDF output

- Fig/TIFF:

  TIFF output

- Fig/PNG:

  PNG output

File dimensions are scaled internally:

- PDF: width and height multiplied by 2.

- TIFF: width and height multiplied by 1000.

- PNG: width and height multiplied by 500.

For PNG output, if `dpi > 600`, the resolution is automatically set to
300; otherwise, `dpi/2` is used.

If `object` is of class `gList`, the plot is rendered using
[`grid.draw()`](https://rdrr.io/r/grid/grid.draw.html); otherwise,
[`print()`](https://rdrr.io/r/base/print.html) is used.

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()

saveplot(
  object = p,
  filenames = "mtcars_scatter",
  width = 6,
  height = 4,
  dpi = 600
)
} # }
```
