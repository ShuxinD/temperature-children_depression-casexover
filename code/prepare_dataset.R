## prepare the dataset ##
rm(list=ls())
gc()
require(data.table)

## load the extracted Medicare dataset ----
dt_dir <- "/n/dominici_nsaph_l3/Lab/projects/medicaid_children_icd/data/individual_records"
dt_o <- fread(file.path(dt_dir,"disease_classification_demographics.csv"))
anyDuplicated(dt_o) # check duplicated rows
# > anyDuplicated(dt) # check duplicated rows
# [1] 147
# > anyDuplicated(dt[,c("admission_date","bene_id")])
# [1] 410
# ## duplicated rows exist, unique the data
dt <- unique(dt_o)

## check data ----
colnames(dt)
summary(dt)
# diag1              diag2              diag3              diag4          
# Length:1562890     Length:1562890     Length:1562890     Length:1562890    
# Class :character   Class :character   Class :character   Class :character  
# Mode  :character   Mode  :character   Mode  :character   Mode  :character  
# 
# 
# 
# diag5              diag6              diag7              diag8          
# Length:1562890     Length:1562890     Length:1562890     Length:1562890    
# Class :character   Class :character   Class :character   Class :character  
# Mode  :character   Mode  :character   Mode  :character   Mode  :character  
# 
# 
# 
# diag9            diagnosis              year      admission_date      
# Length:1562890     Length:1562890     Min.   :1999   Min.   :1999-01-01  
# Class :character   Class :character   1st Qu.:2003   1st Qu.:2003-08-08  
# Mode  :character   Mode  :character   Median :2007   Median :2007-03-01  
# Mean   :2006   Mean   :2006-11-13  
# 3rd Qu.:2010   3rd Qu.:2010-05-18  
# Max.   :2012   Max.   :2012-12-31  
# bene_id          n_th_admission      depression        anxiety       
# Length:1562890     Min.   :  1.000   Min.   :0.0000   Min.   :0.00000  
# Class :character   1st Qu.:  1.000   1st Qu.:0.0000   1st Qu.:0.00000  
# Mode  :character   Median :  1.000   Median :0.0000   Median :0.00000  
# Mean   :  2.358   Mean   :0.1811   Mean   :0.07611  
# 3rd Qu.:  2.000   3rd Qu.:0.0000   3rd Qu.:0.00000  
# Max.   :132.000   Max.   :1.0000   Max.   :1.00000  
# emotion_disturb adole_reaction   disturb_conduct       zip        race_ethnicity_code
# Min.   :0.000   Min.   :0.0000   Min.   :0.0000   Min.   :    0   Length:1562890     
# 1st Qu.:0.000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:29671   Class :character   
# Median :0.000   Median :0.0000   Median :0.0000   Median :48504   Mode  :character   
# Mean   :0.147   Mean   :0.1442   Mean   :0.1353   Mean   :49957                      
# 3rd Qu.:0.000   3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:70791                      
# Max.   :1.000   Max.   :1.0000   Max.   :1.0000   Max.   :99950                      
# sex                 age       
# Length:1562890     Min.   : 0.00  
# Class :character   1st Qu.:10.00  
# Mode  :character   Median :14.00  
# Mean   :12.62  
# 3rd Qu.:16.00  
# Max.   :18.00  
# dt[n_th_admission==100,] problem with this column

## diag1 -- diag9, diagnosis: good

## year: the year of admission_date (admission year)

## bene_id: good

### n_th_admission: PROBLEMATIC, not consistent, but will not affect the final dataset----
setorder(dt[n_th_admission==20,],bene_id)[]
## depression, anxiety, emotion_disturb, adole_reaction, disturb_conduct: processed variable, decide to trust the data processing

### zip: need validation----
official_zip <- fread(file.path("data", "zip_code_database.csv"))
dt[,zip_validity := zip %in% official_zip[,zip]] # ZIP codes in the official database is valid
# summary(dt[,zip_validity])
# > summary(dt[,zip_validity])
# Mode   FALSE    TRUE 
# logical    7881 1555009
cat("invalid ZIP percentage", 1-mean(dt[,zip_validity]))
# invalid ZIP percentage 0.005042581

### race_ethnicity_code: need processing ----
## check the zero portion
dt[, rec_contain0:=race_ethnicity_code %like% "0"]
sum(dt[,rec_contain0])/dim(dt)[1]
setorder(dt[,.(count_rec0=sum(rec_contain0),
               percent_rec=sum(rec_contain0)/.N), by = year],year)[]

dt[,race_ethnicity_code:=as.factor(race_ethnicity_code)]
levels(dt[,race_ethnicity_code])
## https://resdac.org/cms-data/variables/raceethnicity-msis
## work on it later

### sex: need processing ----
dt[,sex:=as.factor(sex)]
summary(dt[,sex]);summary(dt[,sex])/dim(dt)[1]*100
# check_sex <- dt[,.(sex_unique=uniqueN(sex)), by= bene_id]
# check_sex[sex_unique!=1,]
# > check_sex[sex_unique!=1,]
# Empty data.table (0 rows and 2 cols): bene_id,sex_unique
## no sex inconsistency
## work on it later

### age: ok ----
hist(dt[,age])
pdf(file = file.path("results","processing_data","original_age_distribution.pdf"), paper = "USr")
hist(dt[,age])
dev.off()

## subset the first hospitalization ----
setorder(dt, bene_id,admission_date)
dt_1hosp <- dt[, .SD[1], by = bene_id]
mean(dt_1hosp[, zip_validity])
# [1] 0.9949654