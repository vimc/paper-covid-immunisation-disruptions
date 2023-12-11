# Introduction

This repository accompanies the third VIMC consortium-wide paper "Projecting health impact of COVID-19 related immunisation disruptions in 112 countries from 2020-2030: A modelling study".

The repository contains the output of an [orderly](https://mrc-ide.github.io/orderly2/) report which generates the figures and tables for the paper. As such, it includes datasets that the code depends on (except combined_data.rds due to filesize- this only informs one figure: disease_stacked_bar), a statement of the requirements (in orderly.yml), the log of this orderly run (orderly.log) and a readout of the session info (in orderly_run.rds). There are outputs included through both tables (in csvs) and figures. The main code used can be found in script.R, report.Rmd and in the function files (in the R folder). Note: it is not possible to run all the code as some relies on accessing the VIMC database which contains sensitive information.
