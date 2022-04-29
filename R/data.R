#' Geochronology samples from the Honda Group in Colombia
#'
#' A dataset containing geochronology data from several samples along
#' the stratigraphic column of the Honda and Huila groups in the Tatacoa
#' Desert area. The dataset was compiled from the Table 3.2 in Flynn et al.
#' (1997).
#'
#' @usage data(laventa)
#' 
#' @format A data frame with 87 rows and 7 variables:
#' \describe{
#' \item{age}{Estimated age (in Ma) from a given rock sample}
#' \item{one_sigma}{Standard deviation of the age estimate}
#' \item{sample}{Sample code as in Table 3.2}
#' \item{unit}{Stratigraphic unit in either the Honda Group or the
#'             Huila Group}
#' \item{elevation}{Position in the stratigraphic column, in meters}
#' \item{mineral}{The mineral used for dating the sample}
#' \item{comments}{Comments from footnotes in the original table}
#' }
#' @references Flynn, J.J., Guerrero, J. & Swisher III, C.C. (1997) Geochronology of the Honda Group. In: R. F. Kay, R. H. Madden, R. L. Cifelli, and J. J. Flynn (Eds), Vertebrate Paleontology in the Neotropics: the Miocene Fauna of La Venta, Colombia. Smithsonian Institution Press, pp. 44–60. 
"laventa"

#' Divergence-time estimation data for cis-trans-Andean pairs
#'
#' A dataset containing point estimates and uncertainty intervals of
#' divergence times for clade pairs east and west of the Andes,
#' compiled by Ballen (2020).
#'
#' @usage data(andes)
#' 
#' @format A data frame with three columns:
#' \describe{
#'   \item{ages}{Estimated age (in Ma) from a given rock sample}
#'   \item{min}{Standard deviation of the age estimate}
#'   \item{max}{Sample code as in Table 3.2}
#' }
#' @references Ballen, Gustavo A. 2020. Fossil freshwater fishes and the biogeography of northern South America. 2020. PhD thesis, Museu de Zoologia, Universidade de São Paulo, São Paulo, 2020. doi:10.11606/T.38.2020.tde-06052020-181631. 
"andes"
