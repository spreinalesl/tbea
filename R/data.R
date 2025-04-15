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

#' Prior samples for the family Cynodontidae
#'
#' A data frame with prior MCMC samples from a divergence time estimation
#' analysis of the family Cynodontidae using Beast2. From an original sample of 20000, these
#' include the last 1000 samples and only preserve the columns with node ages.
#'
#' @usage data(cynodontidae.prior)
#' 
#' @format A data frame named cynodontidae.prior with six columns:
#' \describe{
#'   \item{Sample}{ID of the generation during sampling}
#'   \item{mrca.date.backward.Hydrolycus.}{Node ages for the node Hydrolycus}
#'   \item{mrca.date.backward.Cynodontidae.}{Node ages for the node Cynodontidae}
#'   \item{mrca.date.backward.Cynodon_Hydrolycus.}{Node ages for the node Cynodon-Hydrolycus}
#'   \item{mrca.date.backward.H_scomberoides.}{Node ages for the node H. scomberoides}
#'   \item{mrca.date.backward.Cynodon.}{Node ages for the node Cynodon}
#' }
#' @references Ballen, G.A. and Reinales, S. 2025. tbea: tools for pre- and post-processing in Bayesian evolutionary analyses. BioRxiv https://www.biorxiv.org/content/10.1101/2024.06.18.599561.
#' @references Ballen, G.A., Moreno-Bernal, J.W. & Jaramillo, C. 2022. The fossil record of Saber-Tooth Characins (Teleostei: Characiformes: Cynodontinae), their phylogenetic relationships, and paleobiogeographical implications. Journal of Systematic Palaeontology 19:24, 1679-1692, DOI: 10.1080/14772019.2022.2070717
"cynodontidae.prior"

#' Posterior samples for the family Cynodontidae
#'
#' A data frame with posterior MCMC samples from a divergence time estimation
#' analysis of the family Cynodontidae using Beast2. From an original sample of 20000, these
#' include the last 1000 samples and only preserve the columns with node ages.
#'
#' @usage data(cynodontidae.posterior)
#' 
#' @format A data frame named cynodontidae.posterior with six columns:
#' \describe{
#'   \item{Sample}{ID of the generation during sampling}
#'   \item{mrca.date.backward.Hydrolycus.}{Node ages for the node Hydrolycus}
#'   \item{mrca.date.backward.Cynodontidae.}{Node ages for the node Cynodontidae}
#'   \item{mrca.date.backward.Cynodon_Hydrolycus.}{Node ages for the node Cynodon-Hydrolycus}
#'   \item{mrca.date.backward.H_scomberoides.}{Node ages for the node H. scomberoides}
#'   \item{mrca.date.backward.Cynodon.}{Node ages for the node Cynodon}
#' }
#' @references Ballen, G.A. and Reinales, S. 2025. tbea: tools for pre- and post-processing in Bayesian evolutionary analyses. BioRxiv https://www.biorxiv.org/content/10.1101/2024.06.18.599561.
#' @references Ballen, G.A., Moreno-Bernal, J.W. & Jaramillo, C. 2022. The fossil record of Saber-Tooth Characins (Teleostei: Characiformes: Cynodontinae), their phylogenetic relationships, and paleobiogeographical implications. Journal of Systematic Palaeontology 19:24, 1679-1692, DOI: 10.1080/14772019.2022.2070717
"cynodontidae.posterior"

