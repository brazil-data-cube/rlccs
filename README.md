
# rlccs - R Client Library for Land Cover Classification System Web Service

<!-- badges: start -->

[![Software
License](https://img.shields.io/badge/license-MIT-green)](https://github.com/brazil-data-cube/rlccs/blob/master/LICENSE)
[![Software Life
Cycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Join us at
Discord](https://img.shields.io/discord/689541907621085198?logo=discord&logoColor=ffffff&color=7389D8)](https://discord.com/channels/689541907621085198#)

<!-- badges: end -->

# About

Currently, there are several data sets on regional, national and global
scales with information on land use and land cover that aim to support a
large number of applications, including the management of natural
resources, climate change and its impacts, and biodiversity
conservation. These data products are generated using different
approaches and methodologies, which present information about different
classes of the earth’s surface, such as forests, agricultural
plantations, among others. Initiatives that generate land use and land
cover maps normally develop their own classification system, with
different nomenclatures and meanings of the classes used.

As more data on land use and land cover are made available, there are
also greater variations in the class definitions of these maps.
Significant obstacles arise in the joint use of these data. It demands
great effort from researchers, students and specialists to carry out
comparative analyzes and validations of different maps of land use and
cover and with different ones that have different classification
systems. There are several studies and techniques that aim to verify the
similarity between classes in order to obtain harmonized classification
systems and thus carry out joint analyzes of different mapping products.

In this context, the **L**and **C**over **C**lassification **S**ystem
**W**eb **S**ervice (LCCS-WS) aims to provide a simple interface to
access the various classification systems in use and their respective
classes. Therefore, this service proposes a representation for the
classification systems and provides an API to access the classes and
their symbolization. It is also possible to establish mappings between
classes of different systems.

If you want to know more about LCCS service, please, take a look at its
[specification](https://github.com/brazil-data-cube/lccs-ws-spec).

## Installation

To install the development version of `rlccs`, run the following
commands:

``` r
# load necessary libraries
library(devtools)
install_github("brazil-data-cube/rlccs")
```

Importing `rlccs` package:

``` r
library(rlccs)
library(magrittr) # for pipe (%>%) in examples
```

## Usage

The following are an overview of some operations implemented by `rlccs`:

| **LCCS-WS** operations                                        | `rlccs` functions          |
|:--------------------------------------------------------------|:---------------------------|
| `/classification_systems`                                     | `classification_systems()` |
| `/classification_systems/{system_id}/classes`                 | `classes()`                |
| `/classification_system/{system_id}/styles/{style_format_id}` | `styles()`                 |
| `/mappings/{system_id_source}/{system_id_target}`             | `mappings()`               |

These functions can be used to retrieve information from a LCCS-WS
service.

The code bellow list the available classification system of the LCCS-WS
of the [Brazil Data Cube](http://brazildatacube.org/) project of the
Brazilian National Space Research Institute [INPE](http://www.inpe.br/).

``` r
lccs("https://brazildatacube.dpi.inpe.br/dev/lccs/") %>%
  classification_systems() %>%
  get_request()
#> [[1]]
#> [[1]]$authority_name
#> [1] "INPE"
#> 
#> [[1]]$description
#> [1] "Brazil Data Cube Project "
#> 
#> [[1]]$id
#> [1] 1
#> 
#> [[1]]$links
#> [[1]]$links[[1]]
#> [[1]]$links[[1]]$href
#> [1] "https://brazildatacube.dpi.inpe.br/dev/lccs/classification_systems/1"

...
```

In the code bellow, we get the metadata of a specific classification
system:

``` r
lccs("https://brazildatacube.dpi.inpe.br/dev/lccs/") %>%
  classification_systems(system_id = 2) %>%
  get_request()
#> $authority_name
#> [1] "INPE"

...

#> $links[[6]]
#> $links[[6]]$href
#> [1] "https://brazildatacube.dpi.inpe.br/dev/lccs/"
#> 
#> $links[[6]]$rel
#> [1] "root"
#> 
#> $links[[6]]$title
#> [1] "API landing page."
#> 
#> $links[[6]]$type
#> [1] "application/json"

...

```

This example shows the classes of a specific classification system:

``` r
lccs("https://brazildatacube.dpi.inpe.br/dev/lccs/") %>%
  classification_systems(system_id = 2) %>%
  classes() %>%
  get_request()
#> [[1]]
#> [[1]]$href
#> [1] "https://brazildatacube.dpi.inpe.br/dev/lccs/classification_systems/2/classes"
...
#> 
#> 
#> [[4]]
#> [[4]]$href
#> [1] "https://brazildatacube.dpi.inpe.br/dev/lccs/"
#> 
#> [[4]]$rel
#> [1] "root"
#> 
#> [[4]]$title
#> [1] "API landing page"
#> 
#> [[4]]$type
#> [1] "application/json"
#> 
#> 
#> [[5]]
#> [[5]]$href
#> [1] "https://brazildatacube.dpi.inpe.br/dev/lccs/classification_systems/2/classes/51"
#> 
...
```

# License

    Copyright (C) 2021 INPE.

    R client for LCCS-WS is free software; you can redistribute it and/or modify it
    under the terms of the MIT License; see LICENSE file for more details.
