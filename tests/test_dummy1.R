
# TODO: turn this into a proper test!
 
# Test of a function getRemainingParamsets
# this function will 'knock out' 3 processedComboNums to test how those
# knocked out combos will appear in the output
# strategy -- name of a strategy or strategy object
test_getRemainingCombos <- function(customParamsets=NULL, strategy, paramsetLabel, processedComboNums=NULL)
{
    if(is.null(processedComboNums))
        stop ("processedComboNums must be provided")

    if(!(class(processedComboNums)=="character"))
        stop ("processedComboNums must be a character vector")

    if(length(processedComboNums)<4)
        stop ("provide a vector of processedComboNums of length greater than 3")

    # generate all the paramsets as a dataframe (if needed)
    # strategy <- quantstrat:::must.be.strategy(strategy.st)
    # paramsets <- quantstrat:::paramset.generate(strategy.st,"SMA")
    if(customParamsets==NULL) {
        allCombos.df <- paramset.generate(strategy.st,paramsetLabel)
    } else {
        allCombos.df <- customParamsets
    }
    if(nrow(allCombos.df)<4)
        stop ("a strategy setup must generate more than 3 param. combos for this test")

    #--------------------------------------------------------------------------|
    # knock out some combos for testing
    combosToRemove <-  sample(x=processedComboNums, size=3)
    whichToRemove <- which(processedComboNums %in% combosToRemove)
    processedComboNums_reduced <- processedComboNums[-whichToRemove]
    #--------------------------------------------------------------------------|

    # selection vector - existing combos
    processedCombosSelection <-  row.names(allCombos.df) %in% processedComboNums_reduced
    processedCombosSelection

    unprocessedCombos.df <- allCombos.df[!processedCombosSelection,]

    rc <- (nrow(unprocessedCombos.df) +
               length(processedComboNums_reduced) == nrow(allCombos.df))

    if(rc) print("PASS")
    else print("FAIL")
}
