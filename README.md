# gnfidatR


**Download and process data from the German National Forest Inventory (Bundeswaldinventur, BWI).**

`gnfidatR` provides easy access to data from the **German National Forest Inventory (Bundeswaldinventur, BWI)** supplied by the [Thünen Institute](https://www.thuenen.de/). In addition to downloading and accessing BWI data, the package provides functions for preparing the data for use with the forest growth simulator **WEHAM** (Waldentwicklungs- und Holzaufkommensmodellierung).

> **Development version:** `gnfidatR` is currently under active development. The functionality may change.

## Features

The package is designed to simplify working with German National Forest Inventory data in R.

Currently, the main functionality includes:

* Downloading BWI data supplied by the Thünen Institute
* Easy access to BWI datasets from R
* Preparing and processing BWI data
* Generating predefined input databases for the forest growth simulator **WEHAM**
* Supporting reproducible workflows for analyses based on BWI data

## Installation

The development version can be installed directly from this GitLab repository:

```r
# install.packages("remotes")
remotes::install_gitlab("YOUR_NAMESPACE/gnfidatR")
```

Alternatively, clone the repository and install the package locally:

```bash
git clone <REPOSITORY_URL>
```

Then install it from R:

```r
# install.packages("devtools")
devtools::install()
```

## Getting started

Load the package:

```r
library(gnfidatR)
```

A typical workflow consists of downloading the required BWI data and subsequently preparing the data for further analysis or for use with WEHAM.

```r
# Example workflow
bwi_data <- ...
weham_data <- ...
```

More detailed examples and documentation will be added as the package develops.

## German National Forest Inventory

The **Bundeswaldinventur (BWI)** is a nationwide inventory of forests in Germany. It provides comprehensive information on forest area, tree species, growing stock, increment, forest structure and other characteristics of German forests.

The BWI data used by this package are supplied by the **Thünen Institute**.

For further information about the German National Forest Inventory, see:

* [Thünen Institute – Bundeswaldinventur](https://www.thuenen.de/)
* [Bundeswaldinventur](https://www.bundeswaldinventur.de/)

Please refer to the original data provider for information on data licensing, methodology, interpretation and appropriate citation of the BWI data.

## WEHAM

`gnfidatR` also provides functionality for preparing input data for **WEHAM – Waldentwicklungs- und Holzaufkommensmodellierung**.

WEHAM is a forest growth and yield simulation model used to project forest development and timber availability under different scenarios.

The package aims to simplify the preparation of the often complex input databases required for WEHAM and to make these workflows reproducible in R.

## Development

This project is currently under development.

Contributions, bug reports and suggestions are welcome. Please use the GitLab issue tracker for:

* Bug reports
* Feature requests
* Questions about the package
* Suggestions for improvements

For development, clone the repository and work from the package source:

```bash
git clone <REPOSITORY_URL>
cd gnfidatR
```

Then install the package locally:

```r
devtools::load_all()
```

Run the package checks with:

```r
devtools::check()
```

## Authors

* **Nils Steuernagel** – author and maintainer
  [nils.steuernagel@gmail.com](mailto:nils.steuernagel@gmail.com)

* **Stefan Paul** – author and maintainer
  [stefan.paul@forst.bwl.de](mailto:stefan.paul@forst.bwl.de)

## License

`gnfidatR` is released under the **GNU General Public License version 3 (GPL-3)**.

See the `LICENSE` file for the full license text.

## Disclaimer

`gnfidatR` is an independent R package for accessing and processing data provided by the Thünen Institute. It is not an official product of the Thünen Institute unless explicitly stated otherwise.

Please consult the original data provider for authoritative information regarding the BWI data, methodology and data use.
