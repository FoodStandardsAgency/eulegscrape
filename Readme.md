# eulegscrape

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/helen-food/eulegscrape.svg?branch=master)](https://travis-ci.org/helen-food/eulegscrape)
<!-- badges: end -->

## Overview

A package of functions and scripts to fetch and clean union lists of regulated products.

CSV files with union lists of products and their permitted uses are in the `/csv` folder. These are currently clean and human readable tables of the content described in each section below. Further processing/steps to increase machine readability can be added as required.

The file `update.R` checks to see whether a new consolidated version has been uploaded, and recreates the tables if so. 
Apart from for feed additives, which needs to be run separately if necessary.

Scripts to produce the CSVs are in `/create_files`

R scripts with generic functions are in the `/R` folder.

The `/reference` folder contains the URL of each piece of legislation (on legislation.gov.uk).

## Additives

### Relevant legislation

Regulation (EC) No 1333/2008 of the European Parliament and of the Council of 16 December 2008 on food additives 

### Data tables

* *additives.csv*: Annex II - additives that can be used in food. Permitted usage in each food category (Annex II part E). See legislation for the composition and specific limits of additive groups.

* *additives2_n.csv*: Annex III - additives that can be used in food additives, enzymes and flavourings (Annex III parts 1-5). Separate files for parts 1, 2, 3, 4, 5A and 5B - 6 files in total.
See legislation for definition of additive groups used in Annex III.

## Contaminants

### Relevant legislation

Commission Regulation (EC) No 1881/2006 of 19 December 2006 setting maximum levels for certain contaminants in foodstuffs

### Data tables

Annex - Maximum levels for certain contaminants in foodstuffs

* *nitrate.csv*: Annex section 1 - nitrate. 
* *mycotoxins.csv*: Annex section 2 - mycotoxins.
* *metals.csv*: Annex section 3 - metals.
* *mcpd.csv*: Annex section 4 - 3-monochloropropanediol (3-MCPD) and glycidyl fatty acid esters.
* *dioxins.csv*: Annex section 5 - dioxins and PCBs.
* *poly.csv*: Annex section 6 - polycyclic aromatic hydrocarbons.
* *melamine.csv*: Annex section 7 - melamine and its structural analogues.
* *toxins.csv*: Annex section 8 - inherent plant toxins.
* *footnotes.csv*: Footnotes for all tables.

## Feed additives

### Relevant legislation

The Register of Feed Additives is pursuant to Regulation (EC) No 1831/2003.

### Data tables

* *feed-additives.csv*: Annex I - List of Additives from the the EU Register of Feed Additives

## Flavourings

### Relevant legislation

Regulation (EC) No 1334/2008 of the European Parliament and of the Council of 16 December 2008 on flavourings and certain food ingredients with flavouring properties for use in and on foods and amending Council Regulation (EEC) No 1601/91, Regulations (EC) No 2232/96 and (EC) No 110/2008 and Directive 2000/13/EC

### Data tables

* *flavourings.csv*: Annex I part A section 2 - list of flavourings, details about the flavouring and any specific restrictions on its use (all food groups unless otherwise stated). For footnotes and additional information see legislation.

## Food contact materials - plastic

### Relevant legislation

Commission Regulation (EU) No 10/2011 of 14 January 2011 on plastic materials and articles intended to come into contact with food 

### Data tables

* *substances.csv*: Annex I, Table 1 - Union list of authorised monomers, other starting substances, macromolecules obtained from microbial fermentation, additives and polymer production aids.
* *group-restrictions.csv*: Anned I, Table 2 (the group level restrictions referred to in *substances.csv*).
* *compliance.csv*: Annex I, Table 3 (the notes on verification of compliance referred to in *substances.csv*).
* *material-restrictions.csv*: Annex II - specific migration limits.
* *simulants.csv*: Annex III - food category specific assignment of food simulants.

## Food contact materials - cellulose

### Relevant legislation

Commission Directive 2007/42/EC of 29 June 2007 relating to materials and articles made of regenerated cellulose film intended to come into contact with foodstuffs

### Data tables

Annex II: list of substances authorised in the manufacture of regenerated cellulose film

* *uncoated.csv*: Annex II, First part - uncoated regenerated cellulose film
* *coated.csv*: Annex II, Second part - coated regenerated cellulose film

## Novel foods

### Relevant legislation

Commission Implementing Regulation (EU) 2017/2470 of 20 December 2017 establishing the Union list of novel foods in accordance with Regulation (EU) 2015/2283 of the European Parliament and of the Council on novel foods

### Data tables

* *authorised-novel-foods.csv*: Annex - Union list of novel foods

## Smoke flavourings

### Relevant legislation

COMMISSION IMPLEMENTING REGULATION (EU) No 1321/2013 of 10 December 2013 
establishing the Union list of authorised smoke flavouring primary products 
for use as such in or on foods and/or for the production of derived smoke flavourings

### Data tables

* *smoke.csv*: Annex. See legislation for notes and footnotes.







