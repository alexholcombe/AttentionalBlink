Needs of the R code

## General for the experiment

numItemsInStream - number of items in the stream


## Specific to each trial

The dataframe should contain one trial for each row.
* $targetSP, the serial position of the target on that trial
* $SPE, the serial position error

## Wrangling psychopy output

Most of Alex's psychopy RSVP programs have two targets potentially, recorded on the same row. So, need to melt into separate rows while preserving trialnum.