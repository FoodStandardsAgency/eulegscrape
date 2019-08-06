# eulegscrape

A package of functions and scripts to fetch and clean union lists of regulated products.

Union lists of products and their permitted uses are in the `/data` folder. These are currently clean and human readable tables of the content described in each section below. Further processing/steps to increase machine readability can be added as required.

R scripts to produce the data and the URLs of each piece of legislation are in the `/data-raw` folder.

Helper functions are in the `/R` folder.

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/helen-food/eulegscrape.svg?branch=master)](https://travis-ci.org/helen-food/eulegscrape)
<!-- badges: end -->

Link to the relevant legislation for each product [in the raw-data folder here](https://github.com/helen-food/eulegscrape/blob/master/data-raw/legislation-urls.csv)

## Additives

Regulation (EC) No 1333/2008 of the European Parliament and of the Council of 16 December 2008 on food additives 

Annex II - additives that can be used in food. Permitted usage in each food category (Annex II part E). See legislation for group composition/specific limits.

*In progress: Annex III - additives that can be used in food additives, enzymes and flavourings*

## Flavourings

Regulation (EC) No 1334/2008 of the European Parliament and of the Council of 16 December 2008 on flavourings and certain food ingredients with flavouring properties for use in and on foods and amending Council Regulation (EEC) No 1601/91, Regulations (EC) No 2232/96 and (EC) No 110/2008 and Directive 2000/13/EC

Annex I part A section 2 - list of flavourings, details about the flavouring and any specific restrictions on its use (all food groups unless otherwise stated). For footnotes and additional information see legislation.
