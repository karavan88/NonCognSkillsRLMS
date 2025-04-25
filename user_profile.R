#-------------------------------------------------------------------
# Project: Non-Cognitive Skills
# Script: R Profile 
# Author: Garen Avanesian
# Date: 21 October 2023
#-------------------------------------------------------------------

# set working directories and all directories 
# this is the profile for the Non-Cognitive Skills research project
# this profile should be loaded before running any other script

USERNAME    <- Sys.getenv("USERNAME")
USER        <- Sys.getenv("USER")

#version from everyone, the profile works for everyone

if (USER == "karavan88"){
  projectFolder  <- "/Users/karavan88/Documents/GitHub/NonCognSkillsRLMS" #getwd()
} 


# confirm that the main directory is correct
# check if the folders exist
stopifnot(dir.exists(projectFolder))

# set up key folders
inputData     <-    file.path(projectFolder, "01_input_data")
processedData <-    file.path(inputData, "processed")
rCodes        <-    file.path(projectFolder, "02_codes")

emplNcs      <-     file.path(rCodes, "01_EmplNCS")
returnsNcs   <-     file.path(rCodes, "02_ReturnsNCS")
jobSatisfNcs <-     file.path(rCodes, "03_JobSatisfNCS")



stopifnot(dir.exists(projectFolder))
stopifnot(dir.exists(inputData))
stopifnot(dir.exists(processedData))
stopifnot(dir.exists(rCodes))


