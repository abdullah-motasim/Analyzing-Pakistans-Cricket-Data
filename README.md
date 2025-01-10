## Overview

This repository contains all the necessary data and scripts for analyzing factors that influence the performance of the Pakistani cricket team from 1952 to 2024. The match statistics, along with a Bayesian Logistic Regression model, were used to determine that both the opponent and the outcome of the coin toss significantly impacted Pakistan's chances of winning, as detailed in the "paper" folder. 

To make edits to this repository, click the green "Code" button, then select "Download ZIP." After downloading, transfer the folder to your desired workspace on your computer and modify it as needed.

## File Structure

The repo is structured as follows:

-   `data/raw_data` contains the raw data as obtained from the cricketdata R package.
-   `data/analysis_data` contains the cleaned dataset that was constructed.
-   `model` contains fitted models. 
-   `other/sketches` contains the preliminary sketches for the datasets and graphs.
-   `other/llm` contains the LLM chat history.
-   `paper` contains the files used to generate the paper, including the Quarto document and reference bibliography file, as well as the PDF of the paper. 
-   `scripts` contains the R scripts for simulating, downloading, cleaning, testing the data, and training the model.


## Statement on LLM usage

Aspects of the abstract, title, introduction, model, results, discussion, appendix, and code such as the simulation script, cleaning script, testing script, model script, and R code within the Quarto paper were written with the help of chatGPT and the entire chat history is available in `other/llms/usage.txt`.
