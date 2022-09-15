
#' @title runCorrelation
#' @description Calculates the spearman correlation on the input expression matrix & saves the results in SCENIC format
#' @param exprMat_filtered Expression matrix (filtered)
#' @param scenicOptions Fields used: Intermediate file name "corrMat"
#' @return Writes the output in the file name stored in: \code{getIntName(scenicOptions, "corrMat")}
#' @examples 
#' library(SCENIC)
#' scenicOptions <- readRDS("int/scenicOptions.Rds")
#' 
#' loomPath <- system.file(package="SCENIC", "examples/mouseBrain_toy.loom")
#' exprMat <- SCopeLoomR::get_dgem(SCopeLoomR::open_loom(loomPath))
#' 
#' genesKept <- loadInt(scenicOptions, "genesKept")
#' exprMatrix_filtered <- exprMat[genesKept,]
#' exprMat_filtered <- log2(exprMatrix_filtered+1) 
#' 
#' runCorrelation(exprMat_filtered, scenicOptions)
#' @export 
runCorrelation <- function (exprMat_filtered, scenicOptions) 
{
    corrMat <- foreach(i = seq_len(ncol(exprMat_filtered)),
                       .combine = rbind,
                       .multicombine = TRUE,
                       .inorder = FALSE,
                       .packages = c('data.table', 'doParallel')) %dopar% {
                         cor(exprMat_filtered[,i], exprMat_filtered, method = 'spearman')
}

    
    saveRDS(corrMat, file = getIntName(scenicOptions, "corrMat"))



