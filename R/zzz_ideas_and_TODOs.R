# TODO:
# use these best practices
# https://en.wikipedia.org/wiki/Best_coding_practices
# http://chris.beams.io/posts/git-commit/
# https://sethrobertson.github.io/GitBestPractices/

# TODO: ----
# find R packages that use 'bigmemory' package with 'foreach'
# a solution for small-RAM (non server-grade) computers is to use the following front-end
# for quantstrat:

# RAM-HDD(SDD) map:
# write functions that create a set of tuples:
#    "path/to/paramset_result" :: "memory-mapped name of that particular portfolio"
#

# TODO: ----
# write into debug info: run time (start, completion, duration) of the combo
# computer name & pid
#

# TODO: ----
# convert the templates I am using (re-worked luxor demos from QuantStrat framework)
# so they exclude any private info and include them into the package as
# strategy development templates

# TODO: ---
# write a proper description for the robustApplyParamset() function:
# mention that as a side effect, one can even kill the master process and nothing bad
# will happen as all the tasks for the job are in the redis server. (New) workers will
# connect to the server whenever they do, even after the master process has been shut
# down (or crashed for whatever reason). Workers then get all the remaining tasks
# and complete the job.
# As a verification, the master process can be restarted at the very end (after workers
# processed all there was to process from the redis server), the master process will
# check all the backup files, and
# will resubmit all the tasks that have not been processed or failed to save for any reason.
# The master process combine the result and save it.
#


# TODO: profile code & see the 'call tree' using these standard tools: ----
# https://mikedewar.wordpress.com/2010/05/13/profiling-in-r/
# other references on code profiling
# https://cran.r-project.org/doc/manuals/R-exts.html#Tidying-and-profiling-R-code
# https://stat.ethz.ch/R-manual/R-devel/library/utils/html/Rprof.html
# http://www.noamross.net/blog/2013/4/25/faster-talk.html
# http://www.stat.berkeley.edu/~nolan/stat133/Fall05/lectures/profilingEx.html
# https://cran.r-project.org/web/packages/aprof/aprof.pdf
# A MUST-READ: http://adv-r.had.co.nz/Profiling.html
#

# TODO: syslog ----
# https://en.wikipedia.org/wiki/Syslog
# use syslog as a logger: http://www.aboutdebian.com/syslog.htm
# find out how easy it is to send messages
# http://sysbible.org/2008/07/25/configuring-syslog-to-receive-messages-from-the-network-aka-listen/
# adjust flog.appender ---- Manage appenders for loggers
# write flog.appender for syslog to be able to send data to syslog !!!
#
# hadoop uses syslog!
# http://stackoverflow.com/questions/17450816/running-a-r-script-using-hadoop-streaming-job-failing-pipemapred-waitoutputthr
# via what java library ?
# maybe that java library could be plugged into R code?
#
#


# TODO: write a manual on speeding up calcs ----
# when to use path dependence and how to turn it off
# (it's 'on' by default
# what functions in QS use path dependence
#



# TODO: create functions to work with time zones properly (check/change, etc.) ----
#
# 1. read help ?Sys.timezone
#
# 2. TZ Reference Windows: R: R_HOME\share\zoneinfo\ Linux: /usr/share/zoneinfo
#
#    how to read zoneinfo database:
#    zdump - timezone dumper
#    zdump -v "file_name"
#
#      will dump for each zonename on the command line,
#
#      print the time at the lowest possible time value,
#      the time one day after the lowest possible time value,
#
#      the times both one second before and exactly at
#      each detected time discontinuity,
#
#      the time at one day less than the highest ossible time value, and
#      the time at the highest possible time value.
#
#      Each line ends with isdst=1 if the given time is Daylight
#      Saving Time or isdst=0 otherwise.
#
# 3. The proper way to save market data would be to assign market time zone
#    prior to saving the data or _after_ saving the data (in *.rda) format
#    and then re-saving it
#
# 4. zipped latest data
#    http://www.iana.org/time-zones
#
#

# TODO: determine the number of cores for each node within a shell script / batch ! ----
# (for running the max number of workers automatically)
#


# slidingTradeStats: function prototype
# steps:
# 1a. Load prices
# 1b. Load portfolio (see PosPL) data, and base calculations on time index
#     in there, not the time index in prices.
# 2a. Use 'endpoints' to determine intervals to plug into 'Dates' in the
#     scoped tradeStats (tradeStatsExt)
#     TODO: create a patch for 'scoped' tradeStats() ----
#     ( use endpoints() the same way as updatePortf() does )
# 3a. Create a vector of lists with the number equal to the number of
#     intervals calculated in the previous step(s).
#     - use the following structure:
#         * scope.timeFirst
#         * scope.timeLast
#         * tradeStats
# 4a. Process the 'portfolio' to produce slidingTradeStats
#
# Draft #2 ------ -
# 1a. load prices
# 2a. load portfolio
# 3a. use PosPL and endpoints() to generate the overall index of the data
# 4a. calculate and store index (or even retrieved time, via index()) locations
#     to be used with tradeStats
# 5a. use tradeStats
#


# processSavedParamsetOutput: function prototype (TODO: find a better function name) ----
# 1a. get a list of files (backups)
# 2a. process & save processed slidingTradeStats as one file
# 3a. adjust apply.paramset so it saves this data in the same way as the
#     ordinary tradeStats is saved


# define the "walk.forward" / "slidingTradeStats" by 2 parameters
# 1. the number of steps
# 2. the length of the scope/span/range to calculate statistics over
# the following was copied from walk.forward() as it makes sense to use existing conventions
#' @param period the period unit, as a character string, eg. 'days' or 'months'
#' @param k.stats the number of periods to use for calculating tradeStats, eg. '3' periods
# TODO: documentation HOWTOs: https://cran.r-project.org/web/packages/roxygen2/vignettes/rd.html
#       ( @param lines can be split )
# The whole available data is split by endpoints into intervals
#     endpoints generates indexed "locations" within PosPL data, which can be
#     used to retrieve date/time and plug into tradeStats

# TODOs: ----
# create a function that reads the data and produces the output
#     based on the index
#
# length of the slidingTradeStats is simply the length of the vector
#

# TODO: make a link to this repo: https://github.com/google/styleguide
# TODO: read this http://www.yacoset.com/Home/naming-tips

# TODO: try out a new approach to developing functions:
#       2-way argument submission:
#         1. the regular way -- explicitly stated args
#         2. alternative submission via a context (by reference)
#       2-way output
#         1. the regular way -- explicitly as a value
#         2. value might alternatively be stored within the
#            'context' environment instead of

# TODO: test files must use reference data in plain text files
#       to minimize manual work when adding new tests / modifying input data


# TODO: make a proper reference to this debugging 
#       manual: http://www.biostat.jhsph.edu/~rpeng/docs/R-debug-tools.pdf


