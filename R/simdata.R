#' A simulated data with correlated ordinal outcome for cluster randomized trial
#'
#' A dataset contains 50 clusters, in which 25 clusters are in group 1 and the other 25 clusters are in group 0
#'   Each cluster has 9 observations, each observation has an ordinal outcome Y with three levels (i.e., 0, 1, 2).
#'   The outcomes within each cluster are correlated.
#'
#' @format a data frame with 450 rows and 5 variables:
#' \describe{
#'  \item{Obs}{number of observations per cluster}
#'  \item{Y}{ordinal outcome with three levels, possible values are 0, 1, and 2}
#'  \item{Cluster}{number of clusters}
#'  \item{X1}{a cluster-level binary covariate: X1=1 if in group 1 and X1=0 otherwise}
#'  \item{X2}{an observation-level continuous covariate: generatd from normal distribution with mean=1 and SD=1}
#' }
"simdata"
