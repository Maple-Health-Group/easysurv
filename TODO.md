# easysurv to do list

## This release

### High priority
* Add an option to include KMs in predict_and_plot.
* Add hazard plots in predict_and_plot.
* Update `print` method for pred_plot. 
    * See the write_to_xl function for inspiration on how to handle situations 
    with accessing profiles if they are provided.
* Consider group_labels style arguments for all plotting functions.

### Medium priority
* Add an inspect_surv_data() function to inspect the survival data.
* Remove anything plotly-related (for this release)
* Update quick_template_lung to include the new features.
* Update quick_start, and also update/remove other quick_template files 
(to discuss).
* Re-instate a brief vignette that includes the new features


### Low priority
* ...

### Final steps (to save til end)
* Update the version number in the DESCRIPTION file to one we're happy with.
* Update NEWS.md with the latest changes.
* Update the README.Rmd with the latest changes.



## Future releases

* Re-instate plotly support.
* Look into adding integrated brier scores that the censored package can help 
with.
* Vignettes for other workflows.

