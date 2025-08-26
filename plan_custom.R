## -- Here you can define your custom plan which will be appended to the current pipeline's plan.
## -- The last variable returned in this file must be a drake plan (drake::drake_plan()).
## -- Don't forget to add your target/s to DRAKE_TARGETS in pipeline.yaml
## --
## -- You can use all variables defined in _drake_single_sample.R or _drake_integration.R,
## -- but most important are probably cfg and cfg_pipeline lists with config parameters.
## -- All variables defined in _drake_single_sample.R or _drake_integration.R are locked
## -- and error is thrown on attempt to modify them.
## --
## -- This file is sourced in _drake_single_sample.R and _drake_integration.R and its path is taken from the
## -- SCDRAKE_PLAN_CUSTOM_FILE environment variable (defaults to "plan_custom.R").
## --
## -- Example:
# drake::drake_plan(
#   my_target = scater::plotExpression(sce_final_input_qc, "NOC2L", exprs_values = "counts", swap_rownames = "SYMBOL")
# )
