# Ofsted MI All Data [![Build Status](https://travis-ci.org/dfe-analytical-services/ofsted.svg?branch=master)](https://travis-ci.org/dfe-analytical-services/ofsted)

## Background

This project combines all Ofsted Management Information publications that are posted [here](https://www.gov.uk/government/statistical-data-sets/monthly-management-information-ofsteds-school-inspections-outcomes) on Gov.UK.

The aim of this is to support analysis where we are interested in Ofsted at a point in, or over, time.

## Summary

Running the run.R will do the following:

1. Scan the Gov.UK page and extract links to all files
2. Download all of the files to a temporary directory
3. Read in and clean the datasets based on a predefined set of naming conventions (names have changed over time).
4. Stack all of the datasets together
5. Utilise Gias all data extract to identify inspection urn for all inspections.
6. Utilise Gias and Gias links to calculate all predecessor schools (urns) whose previous inspections should count against each urn. This now follows the same method as ofsted statistics.
7. Impute ofsted inspection rows for successor urns that each ofsted should conut against.
8. Append any left overs not capture by methedology but in original dataset.
9. Output the clean dataset ofsted_all.csv to the outputs folder

If there are no changes to the naming conventions for fields used in the files then running this script will work over time. If there are changes it will fail and the names should be added to the code for recoding.

Two further csv's are outputted from the procedure. These are as follows:

- predecessors.csv - this is the list of all predecessors urns from step 6.
- current_urn.csv - this is a by-product of the methedology and simply maps each urn to its current urn as of the extract. I.e. latest or last open successor.

## Travis + GitHub

This project is set up with Travis CI such that the code can routinely be run on open source machines at speed and ouptuts automatically published to GitHub. You can find published data files under the releases tab. 

The code is run daily, and if new data is found, a new release is created. 
