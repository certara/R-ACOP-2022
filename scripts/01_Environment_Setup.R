# Environment Setup ----
#   * Load libraries ------

# library used to create PK/PD model in R
library(Certara.RsNLME)

# data processing and visualization libraries
library(tidyverse)

# table libraries
library(flextable)
library(gtsummary)

# packages using to create common diagnostic plots
library(xpose)
library(Certara.Xpose.NLME)

# packages used to do VPC plots
library(tidyvpc)

## Packages used to facilitate building and executing model, result analysis, and VPC analysis through shiny
## app
library(Certara.RsNLME.ModelBuilder)
library(Certara.RsNLME.ModelExecutor)
library(Certara.ModelResults)
library(Certara.VPCResults)


# * Set seed ----
set.seed(99)


# * Define Hosts ----

# host setup: run locally without MPI
localHost <-
  NlmeParallelHost(
    sharedDirectory = Sys.getenv("NLME_ROOT_DIRECTORY"),
    installationDirectory = Sys.getenv("INSTALLDIR"),
    parallelMethod = NlmeParallelMethod("None"),
    hostName = "Local",
    numCores = 2
  )


# host setup: run locally with MPI enabled
localMPIHost <-
  NlmeParallelHost(
    sharedDirectory = Sys.getenv("NLME_ROOT_DIRECTORY"),
    installationDirectory = Sys.getenv("INSTALLDIR"),
    parallelMethod = NlmeParallelMethod("LOCAL_MPI"),
    hostName = "Local_MPI",
    numCores = 4
  )


# host setup: run locally with multicore enabled
localMultiCoreHost <-
  NlmeParallelHost(
    sharedDirectory = Sys.getenv("NLME_ROOT_DIRECTORY"),
    installationDirectory = Sys.getenv("INSTALLDIR"),
    parallelMethod = NlmeParallelMethod("multicore"),
    hostName = "Local_MultiCore",
    numCores = 4
  )
