shot_spotter
===

Analysis corresponding to paper "Scalable Bayesian inference for self-excitatory processes applied to big American gunfire data" (2020) by Holbrook, Loeffler, Flaxman and Suchard.

[![DOI](https://zenodo.org/badge/248603027.svg)](https://zenodo.org/badge/latestdoi/248603027)

### Scripts guide:

combine_and_thin_data.R: merges two D.C. shotspotter datasets to create comprehensive data, and thins data to check check how results change with changing temporal precision.  

combine_and_data.R: same as above but without thinning.  

RvsC.R: compares compute times for single-core, unvectorized C++ vs naive R.  

get_probs_se.R: computes posterior probabilities that individual events are self-excitatory in nature.  

individual_se_probs_fig.R: creates density plot for the above for a handful of events.  

lengthscale_images.R: posterior density plots for various model parameters.  

make_prob_se_data.R: merges posterior probabilities self-excitatory with spatiotemporal data.  

*run.R: different MCMC runs for different data settings.  

performFigure.R: creates figure comparing performance of GPU and multi-core/vectorized CPU.  

timeByNFigure.R: creates figure comparing performance of GPU and multi-core/vectorized CPU.  

utils.R: helper functions for processing of lat-lon data.
