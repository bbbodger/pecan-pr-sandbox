library(PEcAn.settings)
library(PEcAn.workflow)
library(PEcAn.logger)
library(PEcAn.utils)
library(PEcAn.remote)
library(PEcAn.uncertainty)
library(dplyr)
library(ggplot2)
library(data.table)
library(assertthat)
library(lubridate)
library(sensitivity)
library(PEcAn.SIPNET)
# File paths



# Site info
site_ids <- c("1000004924")
setwd("pecan/base/workflow")
devtools::document()
devtools::load_all()


#!/usr/bin/env Rscript
#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

# ----------------------------------------------------------------------
# Load required libraries
# ----------------------------------------------------------------------



library("PEcAn.all")

# --------------------------------------------------
# get command-line arguments
args <- get_args()
args <- list(continue = FALSE)

# make sure always to call status.end
options(warn = 1)

options(error = quote({
  try(PEcAn.utils::status.end("ERROR"))
  try(PEcAn.remote::kill.tunnel(settings))
  if (!interactive()) {
    q(status = 1)
  }
}))



# ----------------------------------------------------------------------
# PEcAn Workflow
# ----------------------------------------------------------------------

# Report package versions for provenance
PEcAn.all::pecan_version()

# Open and read in settings file for PEcAn run.
settings <- PEcAn.settings::read.settings("/projectnb/dietzelab/bthomas/pecan_runs/sipnet_test/pecan_updated.xml")
# Check for additional modules that will require adding settings
if ("benchmarking" %in% names(settings)) {
  library(PEcAn.benchmark)
  settings <- papply(settings, read_settings_BRR)
}

if ("sitegroup" %in% names(settings)) {
  if (is.null(settings$sitegroup$nSite)) {
    settings <- PEcAn.settings::createSitegroupMultiSettings(settings,
                                                             sitegroupId = settings$sitegroup$id
    )
  } else {
    settings <- PEcAn.settings::createSitegroupMultiSettings(
      settings,
      sitegroupId = settings$sitegroup$id,
      nSite = settings$sitegroup$nSite
    )
  }
  # zero out so don't expand a second time if re-reading
  settings$sitegroup <- NULL
}


# Update/fix/check settings.
# Will only run the first time it's called, unless force=TRUE
settings <-
  PEcAn.settings::prepare.settings(settings, force = FALSE)

# Write pecan.CHECKED.xml
PEcAn.settings::write.settings(settings, outputfile = "pecan.CHECKED.xml")

# start from scratch if no continue is passed in
status_file <- file.path(settings$outdir, "STATUS")
if (args$continue && file.exists(status_file)) {
  file.remove(status_file)
}




if (PEcAn.utils::status.check("CONFIG") == 0) {
  #PEcAn.utils::status.start("CONFIG")
  settings <-
    PEcAn.workflow::runModule.run.write.configs(settings)
  PEcAn.settings::write.settings(settings, outputfile = "pecan.CONFIGS.xml")
  PEcAn.utils::status.end()
} else if (file.exists(file.path(settings$outdir, "pecan.CONFIGS.xml"))) {
  settings <- PEcAn.settings::read.settings(file.path(settings$outdir, "pecan.CONFIGS.xml"))
}

if ((length(which(commandArgs() == "--advanced")) != 0)
    && (PEcAn.utils::status.check("ADVANCED") == 0)) {
  PEcAn.utils::status.start("ADVANCED")
  q()
}

  
ensemble_size=50
  
input_design <- PEcAn.uncertainty::generate_joint_ensemble_design(settings=settings[1],ensemble_size = ensemble_size)
  
  input_design
  
  
  
  
  
  
  
  
  
  
  
  

site_ids<-list()
for (i in 1 : length(settings$run)){
  site_ids[i] <- settings[i]$run$site$id
  
}
  length(settings$run)
base_dir <- "/projectnb/dietzelab/bthomas/pecan_runs/sipnet_test/outdir"
pecan_settings_path <- "/projectnb/dietzelab/bthomas/pecan_runs/sipnet_test/pecan_updated.xml"
samples_path <- "/projectnb/dietzelab/bthomas/pecan_runs/sipnet_test/outdir/samples.Rdata"
output_path <- "/projectnb/dietzelab/bthomas/pecan_runs/sipnet_test/outdir"

model_output_file <- file.path(output_path, "sobol1500fixed_site4924.csv")


load(samples_path)

#input_design <- PEcAn.uncertainty::generate_joint_ensemble_design(settings=settings[1],ensemble_size=ensemble_size)

#settings <- PEcAn.settings::read.settings(pecan_settings_path)
#settings <- PEcAn.settings::prepare.settings(settings)
settings$host <- list(name = "localhost")

model_write_config <- paste("write.config.", settings$model$type, sep = "")
PEcAn.utils::load.modelpkg(settings$model$type)

# ------------------------------------------------------------------------------
# 1. Generate Sobol samples
# ------------------------------------------------------------------------------
load(samples_path)
all_params <- ensemble.samples$temperate.deciduous.HPDA
X1 <- all_params[1:50, ]
X2 <- all_params[51:100, ]
sobol_obj <- soboljansen(model = NULL, X1 = X1, X2 = X2)
U <- sobol_obj$X
# ------------------------------------------------------------------------------
# 2. Run SIPNET forward model with U
# --------------------------------------------s----------------------------------
fwd_mul <- function(U, itr = 1) {
  itr=1
  gen_pars <- U[, !grepl("_site", colnames(U)), drop = FALSE]
  y_list <- list()
  for (i in seq_along(site_ids)) {
    i=1
    site_ics <- U[, grepl(paste0("_site", i, "$"), colnames(U)), drop = FALSE]  
    colnames(site_ics) <- sub(paste0("_site", i, "$"), "", colnames(site_ics))
    colnames(site_ics)
    site <- site_ids[i]
    settings <- update_dirs(settings, itr, site)
    
    settings$run <- settings$run[[i]]
    #settings$run
    run_ids <- get_configs(gen_pars, settings)
    run_ids
    tryCatch(PEcAn.workflow::start_model_runs(settings, write = FALSE),
             error = function(e) message("Skipping model run: ", e$message))
    y_site <- obs_op(run_ids, settings)
    y_list[[paste0("site_", i)]] <- y_site
    settings <- PEcAn.settings::read.settings(pecan_settings_path)
    settings$outdir <- file.path(base_dir, paste0("sobol_out"))
  }
  return(y_list)
}

obs_op <- function(run_ids, settings) {
  run_ids <- readLines(file.path(settings$rundir, "runs.txt"))
  output_vars <- c("time", "NEE", "LAI", "GPP", "TotSoilCarb", "AbvGrndWood", 
                   "leaf_carbon_content", "fine_root_carbon_content", "AGB", 
                   "coarse_root_carbon_content")
  output_list <- list()
  for (run_id in run_ids) {
    run_id_path <- file.path(settings$modeloutdir, run_id)
    model_out <- PEcAn.utils::read.output(run_id, outdir = run_id_path, variables = output_vars, dataframe = TRUE)
    model_dt <- as.data.table(model_out)
    model_dt[, run_id := run_id]
    output_list[[run_id]] <- model_dt
  }
  model_output <- rbindlist(output_list, use.names = TRUE)
  fwrite(model_output, model_output_file)
  return(model_output)
}

get_configs <- function(gen_pars , settings) {
  model_write_config <- paste("write.config.", settings$model$type, sep = "")
  run_ids <- paste0("ens_", 1:nrow(gen_pars))
  for (i in seq_along(run_ids)) {
    i=1
    run_id <- run_ids[i]
    dir.create(file.path(settings$rundir, run_id), recursive = TRUE)
    dir.create(file.path(settings$modeloutdir, run_id), recursive = TRUE)
    par_ens <- list(as.data.frame.list(gen_pars[i, ]))
    par_ens
    
    defaults = settings$pfts
    defaults
    do.call(model_write_config, list(defaults = defaults, trait.values = par_ens,
                                     settings = settings, run.id = run_id))
    cat(run_id, file = file.path(settings$rundir, "runs.txt"), sep = "\n", append = (i != 1L))
  }
  return(run_ids)
}

update_dirs <- function(settings, itr, site_id) {
  sub_dir <- paste0("itr_", itr, "/", site_id)
  settings$outdir <- file.path(settings$outdir, sub_dir)
  settings$host$rundir <- settings$rundir <- file.path(settings$outdir, "run")
  settings$host$outdir <- settings$modeloutdir <- file.path(settings$outdir, "out")
  dir.create(settings$rundir, recursive = TRUE)
  dir.create(settings$modeloutdir, recursive = TRUE)
  return(settings)
}


fwd_mul(U)

# ------------------------------------------------------------------------------
# compute Sobol indices
# ------------------------------------------------------------------------------
df <- fread(model_output_file)
df[, `:=`(year = year(posix), month = month(posix))]
june_dt <- df[month == 6]
avg_dt <- june_dt[, lapply(.SD, mean, na.rm = TRUE), by = run_id, .SDcols = 3:12]

# Y <- avg_dt$NEE
# tell(sobol_obj, Y)
# write.csv(sobol_obj$T, file.path(output_path, "sobol_T_indices_NEE.csv"))
output_vars <- names(avg_dt)[2:11]  # Skip "run_id"

sobol_T_matrix <- matrix(NA, nrow = ncol(X1), ncol = length(output_vars))
rownames(sobol_T_matrix) <- colnames(X1)
colnames(sobol_T_matrix) <- output_vars

for (i in seq_along(output_vars)) {
  y <- avg_dt[[output_vars[i]]]  
  sobol_obj <- soboljansen(model = NULL, X1 = X1, X2 = X2)
  sobol_obj <- tell(sobol_obj, y)
  sobol_T_matrix[, i] <- sobol_obj$T$original
}

write.csv(sobol_T_matrix, "/projectnb/dietzelab/bthomas/sobol/sobol_totalInd.csv")






















fwd_mul <- function(U, itr = 1) {
  gen_pars <- U[, !grepl("_site", colnames(U)), drop = FALSE]
  y_list <- list()
  itr = 1
  for (i in seq_along(site_ids)) {
    i=1
    # Create a deep copy of settings to avoid structure issues
    temp_settings <- settings  # Simple copy; use dget/dput if needed for deep copy
    
    site <- site_ids[i]
    temp_settings <- update_dirs(temp_settings, itr, site)
    
    # Assign run config without overwriting structure
    if (is.list(temp_settings$run) && length(temp_settings$run) >= i) {
      temp_settings$run <- temp_settings$run[[i]]  # This should now be safe
    } else {
      message(paste("Invalid run structure for site", i, "- skipping"))
      next
    }
    
    # Fix multiple paths (as before)

    run_ids <- get_configs(gen_pars, temp_settings)  # Use temp_settings
    
    tryCatch({
      PEcAn.workflow::start_model_runs(temp_settings, write = FALSE)
      y_site <- obs_op(run_ids, temp_settings)
      y_list[[paste0("site_", i)]] <- y_site
    }, error = function(e) {
      message("Error in model run for site ", site, ": ", e$message)
    })
  }
  return(y_list)
}
