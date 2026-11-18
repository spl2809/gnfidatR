# gnfidatR

**Access, retrieve and process data from the German National Forest Inventory (Bundeswaldinventur, BWI) in R.**

`gnfidatR` provides an R interface to data from the **German National Forest Inventory (Bundeswaldinventur, BWI)** supplied by the [Thünen Institute](https://www.thuenen.de/). The package facilitates retrieving, filtering and processing inventory data through the Thünen Institute's REST API. It also provides functionality for preparing standardized input databases for the forest growth simulator **WEHAM** (Waldentwicklungs- und Holzaufkommensmodellierung).


## Features

The package aims to simplify access to German National Forest Inventory data and support reproducible workflows for forest modelling and analysis.

Main functionalities include:

* **API access:** Retrieve BWI data directly from the Thünen Institute's REST API.
* **Database inspection:** Explore available API schemas, tables, views and column metadata.
* **Data retrieval and filtering:** Access and filter inventory plots and associated tree data.
* **Spatial information:** Retrieve cluster identifiers and coordinates, including functionality for working with shapefiles.
* **WEHAM data preparation:** Generate predefined input databases for the forest growth simulator WEHAM.
* **Reproducible workflows:** Provide a consistent interface for accessing and preparing BWI data in R.

## Installation

The development version can be installed directly from the GitHub repository:

```r
# install.packages("remotes")
remotes::install_github("spl2809/gnfidatR")
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

### Inspect the available database structure

The function `database_structure()` retrieves metadata from the API's OpenAPI document. It allows users to inspect available tables, views and columns for a selected schema.

```r
schema <- database_structure(
  accept_profile = "lookup",
  view = FALSE
)

head(schema)
```

The available schemas include:

* `inventory_archive`
* `derived_tmp`
* `derived`
* `lookup`
* `public`

### Retrieve BWI data

The package provides functions to retrieve and filter inventory data through the API. The retrieved data can be used directly for analyses or as a basis for further processing.

### Prepare WEHAM input databases

The function `get_weham_data()` provides a standardized interface for generating the input databases required by WEHAM.

```r
?get_weham_data
```

See the function documentation for available settings, data options and output structures.

## German National Forest Inventory

The **Bundeswaldinventur (BWI)** is a nationwide inventory of forests in Germany. It provides comprehensive information on forest area, tree species composition, growing stock, increment, forest structure and other characteristics of German forests.

The BWI data used by this package are supplied by the **Thünen Institute**.

Further information:

* [Thünen Institute – Bundeswaldinventur](https://www.thuenen.de/)
* [Bundeswaldinventur](https://www.bundeswaldinventur.de/)

Please refer to the original data provider for information on data licensing, methodology, interpretation and appropriate citation of the BWI data.

## WEHAM

`gnfidatR` provides functionality for preparing input data for **WEHAM – Waldentwicklungs- und Holzaufkommensmodellierung**.

WEHAM is a forest development and timber availability projection model used to simulate forest development under different scenarios.

Preparing WEHAM input data requires processing several inventory datasets and combining information on plots, trees, regeneration and other forest characteristics. `gnfidatR` aims to simplify this process by providing predefined database structures and standardized data preparation workflows.

The package focuses on data access and preparation; it does not replace the WEHAM simulation model itself.

## Development

This project is currently under active development.

Contributions, bug reports and suggestions are welcome. Please use the GitHub issue tracker for:

* Bug reports
* Feature requests
* Questions about the package
* Suggestions for improvements


## Authors

* **Nils Steuernagel** – Author and maintainer  
  [nils.steuernagel@gmail.com](mailto:nils.steuernagel@gmail.com)

* **Stefan Paul** – Author  
  [stefan.paul@futureforests.uni-freiburg.de](mailto:stefan.paul@futureforests.uni-freiburg.de)

## License

`gnfidatR` is released under the **GNU General Public License version 3 (GPL-3)**.

See the `LICENSE` file for the full license text.

## Disclaimer

`gnfidatR` is an independent R package for accessing and processing data provided by the Thünen Institute. It is not an official product of the Thünen Institute unless explicitly stated otherwise.

Please consult the original data provider for authoritative information regarding the BWI data, methodology and data use.
