#loading libs
library("PEcAn.all")
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

#reading XML 

settings <- PEcAn.settings::read.settings(xmlpath)

#conducting the sampling 

input_design <- PEcAn.uncertainty::generate_joint_ensemble_design(settings=settings,ensemble_size=ensemble_size, sobol=TRUE)


PEcAn.workflow::run.write.configs(
                                  settings = settings,
                                  write = isTRUE(settings$database$bety$write), # treat null as FALSE
                                  posterior.files = posterior.files,
                                  overwrite = TRUE ,
                                  input_design = input_design
                                 )




#running the model 
PEcAn.workflow::runModule_start_model_runs(settings, stop.on.error = stop_on_error)
 

#reading output 

runs_file <- file.path(settings$outdir, "runs.txt")
if (file.exists(runs_file)) {
  run_ids <- readLines(runs_file) 
} else {
  stop("runs.txt not found - check settings$outdir")
}

# Loop to read outputs for each run
all_model_out <- list()
for (i in run_ids) { 
  run_specific_outdir <- file.path(settings$outdir, i)  
  
  # Read output 
  model_out <- read.output(runid = i, 
                           outdir = run_specific_outdir)
                           
  all_model_out[[i]] <- model_out
}

#conducting the sobol

y <- sapply(run_ids, function(rid) {
  out_list <- all_model_out[[rid]]
  mean(out_list$GPP, na.rm = TRUE) 
})


#print(length(y))  
#print(nrow(sobol_obj$X))  

# Compute indices
tell(sobol_obj, y)

# View/plot results
print(sobol_obj)
