#' Generate model-specific run configuration files for one or more PEcAn runs
#'
#' @param settings a PEcAn Settings or MultiSettings object
#' @param overwrite logical: Replace config files if they already exist?
#' @param input_design the input indices for samples 
#' @return A modified settings object, invisibly
#' @importFrom dplyr %>%
#' @export


runModule.run.write.configs <- function(settings, overwrite = TRUE, input_design = NULL) {

  if (PEcAn.settings::is.MultiSettings(settings)) {
    if (overwrite && file.exists(file.path(settings$rundir, "runs.txt"))) {
      PEcAn.logger::logger.warn("Existing runs.txt file will be removed.")
      unlink(file.path(settings$rundir, "runs.txt"))
    }
    if (is.null(input_design)) {
      ensemble_size <- settings$ensemble$size
      input_design <- PEcAn.uncertainty::generate_joint_ensemble_design(settings=settings[1],ensemble_size=ensemble_size)
      }
    return(PEcAn.settings::papply(settings, runModule.run.write.configs, overwrite = FALSE,input_design=input_design))
  } else if (PEcAn.settings::is.Settings(settings)) {
    # double check making sure we have method for parameter sampling
    if (is.null(settings$ensemble$samplingspace$parameters$method)) {
      settings$ensemble$samplingspace$parameters$method <- "uniform"
    }

      if (is.null(input_design)) {
      ensemble_size <- settings$ensemble$size
      input_design <- PEcAn.uncertainty::generate_joint_ensemble_design( settings = settings, ensemble_size = ensemble_size )
    }
    



    #check to see if there are posterior.files tags under pft
    posterior.files <-   settings$pfts %>%
      purrr::map_chr("posterior.files", .default = NA_character_)
      
    
    
    samples.file <- file.path(settings$outdir, "samples.Rdata")
    if (file.exists(samples.file)) {
      samples <- new.env()
      load(samples.file, envir = samples) ## loads ensemble.samples, trait.samples, sa.samples, runs.samples, env.samples
      trait.samples <- samples$trait.samples
      
      
      trait_sample_indices <- input_design[["param"]]
      ensemble.samples <- list()
      for (pft in names(trait.samples)) {
        pft_traits <- trait.samples[[pft]]
        ensemble.samples[[pft]] <- as.data.frame(
          lapply(
            names(pft_traits),
            function(trait) pft_traits[[trait]][trait_sample_indices]
          )
        )
        names(ensemble.samples[[pft]]) <- names(pft_traits)
      }
      sa.samples <- samples$sa.samples
      runs.samples <- samples$runs.samples
      ## env.samples <- samples$env.samples
      
      all.param.samples<- list(
        trait.samples = trait.samples,
        ensemble.samples = ensemble.samples,
        sa.samples = sa.samples,
        runs.samples = runs.samples
        # env.samples = samples$env.samples  # Uncomment if needed
      )
      
      
    } else {
      PEcAn.logger::logger.error(samples.file, "not found, this file is required by the run.write.configs function")
    }
    
    
    return(PEcAn.workflow::run.write.configs(
      settings = settings,
      write = isTRUE(settings$database$bety$write), # treat null as FALSE
      posterior.files = posterior.files,
      overwrite = overwrite,
      input_design = input_design,
      all.param.samples = all.param.samples
    ))
  } else {
    stop("runModule.run.write.configs only works with Settings or MultiSettings")
  }
}
