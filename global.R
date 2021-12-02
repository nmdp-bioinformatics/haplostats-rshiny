library('shiny')
library('shinydashboard')
library('rCharts')
library('DT')
library('HaplotypeServiceClient')
library('BayesModel')
library('htmltools')


source("./matrixCustom.R")

race_map <- read.csv('./data/race_mapping.csv', header=TRUE)

HAPLOTYPE_HOST_ADDRESS <- 'http://p1mri-s1:8080'

FREQUENCY <- c('NMDP high res 2007'='data2007', 'NMDP full 2011'='data2011')
LOCI = c('A', 'B', 'C', 'DRB1', 'DQB1', 'DRB3','DRB4','DRB5')
BROAD = c('CAU','AFA','HIS','API','NAM')
AFA = c('AAFA','AFB','CARB','SCAMB')
API = c('AINDI','FILII','HAWI','JAPI', 'KORI','NCHI','SCSEAI', 'VIET')
CAU = c('MENAFC','NAMER')
HIS = c('CARHIS','MSWHIS','SCAHIS')
NAM = c('AISC','ALANAM','AMIND','CARIBI')

display_mug <- data.frame(locus = LOCI, 
                          type1 = c('11:01','08:01','01:02','01:01','05:01','','',''), 
                          type2 = c('24:02','27:05','07:01','03:01','02:01','','',''),
                          stringsAsFactors = FALSE)

empty_mug <- data.frame(locus = LOCI, 
                        type1 = '',
                        type2 = '',
                        stringsAsFactors = FALSE)
