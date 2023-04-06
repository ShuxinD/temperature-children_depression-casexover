## prepare the dataset ##
rm(list=ls())
gc()
require(data.table)

## load the extracted Medicare dataset ----
dt_dir <- "/n/dominici_nsaph_l3/Lab/projects/medicaid_children_icd/data/individual_records"
dt <- fread(file.path(dt_dir,"disease_classification_demographics.csv"))
colnames(dt)
dt
