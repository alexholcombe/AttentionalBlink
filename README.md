
Patrick Goodbourn programmed mixture modeling RSVP serial position errors in MATLAB. [Certifiedwaif](https://github.com/certifiedwaif/) [ported](https://github.com/certifiedwaif/AttentionalBlink) it to R.

### To-do

            % Test for a significant difference in log likelihoods
            [h,pValue,stat,cValue] = lratiotest(-minNegLogLikelihood,-uniformNegLogLikelihood,nFreeParameters,pCrit);
        
- Instead of using parameterBounds.R, should probably create a list of everything specific to a particular experiment/implementation, a bit like optim has a list of parameters.
- Look at variation in fit across fits so know how many different starting points I need.
- Why is the blue continuous Gaussian in AA right canonical MATLAB not intersect the light blue discretised Gaussian in the bin middles.
 
        
## Issues

Learn how to catch errors that seem uncatchable, like
*  Still having trouble capturing error msgs like "Error in eigen(nhatend) : infinite or missing values in 'x'". which optimx's author [says might be fine](
http://r.789695.n4.nabble.com/Error-in-eigen-nhatend-td4708274.html)

* Error in grad.default(ufn, ans$par, ...) :

* This prevents compatibility with separate local repo setting path into mixture modeling:  Need to adjust path because Testthat might not work because path gets set to mixtureModeling/tests/
Best thing to do is figure out how to avoid testthat needing that. I guess could call testthat from elsewhere?
But that won't be sufficient to solve the problem, because can't source things still without having the path to mixture modeling which local file can't know about.

Someday switch to Bayesian  Stan via brms. See "mixture" function in [brms manual](https://cran.r-project.org/web/packages/brms/brms.pdf) and [this post](http://andrewgelman.com/2017/08/21/mixture-models-stan-can-use-log_mix/) by Gelman on mixture models in stan

## Analysis to-do

* Work out something for excluding participants, see if different number excluded in discarded backwards-ltrs subjects than happened in MATLAB


## Questions for Pat

Why did he pad with zeros the pseudo_uniform distribution?

### Known discrepancies with Pat's MATLAB code

* the area of bin thing
* I use actual targetSPs to calculate guessing component, he used theoretical?


## Implementation choices that could be revised

We can now accomplish truncation of the Gaussian by summing the area of all the bins and dividing that into each bin, to normalize it so that the total of all the bins =1.
An alternative, arguably better way to do it would be to assume that anything in the Gaussian tails beyond the  bins on either end ends up in those end bins.

Also ideally wouldn't have to taper the Gaussian component, instead would send the target position accompanying each SPE into the likelihood calculation as well, so that instead of generic tapering, could calculate the precise predicted probability because would know the domain of possible errors for that particular observation. That would also make it easier to pile up the tails at the extrema rather than using a truncated Gaussian.