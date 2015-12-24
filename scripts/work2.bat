@echo off
@rem source: http://stackoverflow.com/questions/2591758/batch-script-loop
@rem

for /l %%x in (1, 1, 2) do (
   echo starting worker number %%x
   start R --slave -f simple-worker.R
)