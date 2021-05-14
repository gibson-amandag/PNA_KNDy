#### LOAD LIBRARIES ######################################
library(shiny)
library(tidyverse)
library(readr)
library(rlang)
library(purrr)
library(scales)
library(knitr)
library(officer)
library(GGally)
library(dplyr)
library(openxlsx)
library(car)
library(kableExtra)
#https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html#Overview

#### SET UP ENVIRONMENT, FOLDER PATHS, SOURCE FUNCTIONS ###########################################################################

readRenviron("./.Renviron") #read the .Renviron document from the project root folder

# Name of excel file - defined in .REnviron
KNDyPNA_Data_Excel = Sys.getenv("EXCEL_FILE_NAME")

MouseInfoSheetName = "MouseInfo"
CellInfoSheetName = "CellInfo"
ExclusionSheetName = "Exclusion"
FiringRateSheetName = "FiringData"
CyclesSheetName = "Cycles"
BurstSheetName = "BurstData"
ClusterSheetName = "ClusterData"
TimingSheetName = "TimingData"

file_prefix = Sys.getenv("FILE_PREFIX")

rateForQuiet = 0.005

#type of images to save for plots. .png or .svg, for example
imgTypePlots = ".png"

#save individual plot files?
savePlots = TRUE

### These should be stable ###

#Where R Notebook and R Functions files are saved
ScriptsFolder <- "./Scripts"
FunctionsFolder <- file.path(ScriptsFolder, "Functions")

#Where data files are saved - defined in .Renviron
DataFolder <- Sys.getenv("DATA_FOLDER")

#Where output, like plots, should be saved
#Output folder defined in .Renviron for local system
OutputFolder <- Sys.getenv("OUTPUT_FOLDER")

DataOutputFolder <- file.path(OutputFolder, "Data")
PlotOutputFolder <- file.path(OutputFolder, "Plots")

#File names and paths
GetDataReadyFileName = "AG_KNDyPNA_GetDataReady.R"
VariableNamesFileName = "AG_KNDyPNA_VariableNames.R"
FunctionsFileName = "AG_KNDyPNA_Functions.R"
GraphFunctionsFileName = "AG_KNDyPNA_GraphFunctions.R"
DateTrackingFileName = "AG_KNDyPNA_DateTracking.R"

#Source the function files
source(file.path(FunctionsFolder, GetDataReadyFileName))
source(file.path(FunctionsFolder, VariableNamesFileName))
source(file.path(FunctionsFolder, FunctionsFileName))
source(file.path(FunctionsFolder, GraphFunctionsFileName))
source(file.path(FunctionsFolder, DateTrackingFileName))

#Load datasets
KNDy_mouse_demo <- myXLSX_func(DataFolder, KNDyPNA_Data_Excel, MouseInfoSheetName)
KNDy_cells <- myXLSX_func(DataFolder, KNDyPNA_Data_Excel, CellInfoSheetName)
KNDy_exclude <- myXLSX_func(DataFolder, KNDyPNA_Data_Excel, ExclusionSheetName)
KNDy_firingRate <- myXLSX_func(DataFolder, KNDyPNA_Data_Excel, FiringRateSheetName)
KNDy_cycles <- myXLSX_func(DataFolder, KNDyPNA_Data_Excel, CyclesSheetName)
KNDy_burstData <- myXLSX_func(DataFolder, KNDyPNA_Data_Excel, BurstSheetName)
KNDy_clusterData <- myXLSX_func(DataFolder, KNDyPNA_Data_Excel, ClusterSheetName)
KNDy_TimingData <- myXLSX_func(DataFolder, KNDyPNA_Data_Excel, TimingSheetName)

dateToday = Sys.Date()
