#' Return the data source \code{shallowgroundwater} as an \code{sf}
#' multipolygon layer
#'
#' Returns the raw data source \code{shallowgroundwater}
#' as an \code{sf} multipolygon
#' layer.
#' The coordinate reference system is 'BD72 / Belgian Lambert 72'
#' (EPSG-code \href{https://epsg.io/31370}{31370}).
#'
#' The data source \code{shallowgroundwater} represents the areas in
#' the Flemish region of Belgium where the mean lowest groundwater level (MLW;
#' Knotters & Van Walsum, 1997; Van Heesen, 1970) is estimated to be less than
#' approximately 2 m below soil surface (hence, 'shallow' groundwater).
#' Groundwater dependent species and communities can be expected to be present
#' mostly within these areas.
#'
#' The data source is a GeoPackage, available at
#' \href{https://doi.org/10.5281/zenodo.5902880}{Zenodo}, and contains a single
#' spatial multipolygon layer \code{shallowgroundwater}.
#'
#' The data source has been compiled from subsets of
#' various other spatial data sources.
#' This process is described in more detail at Zenodo.
#' The R code for finishing the \code{shallowgroundwater} data source,
#' starting from an intermediate result, can be found in the
#' \href{https://github.com/inbo/n2khab-preprocessing}{n2khab-preprocessing}
#' repository.
#'
#' @inheritParams read_habitatmap_stdized
#'
#' @return
#' A Simple feature collection of geometry type \code{MULTIPOLYGON}.
#'
#' All attribute variables are logical
#' variables referring to a data source (subset) or procedure that has
#' contributed to
#' the \code{shallowgroundwater} data source.
#' If a variable is \code{TRUE} for a multipolygon, then the related data
#' source (subset) or procedure has
#' contributed to the multipolygon.
#' For one multipolygon, several variables can be \code{TRUE} at the same time.
#' Each combination of values occurs in only one multipolygon.
#'
#' The attribute variables listed below are described in more detail at
#' \href{https://doi.org/10.5281/zenodo.5902880}{Zenodo}; their description
#' typically refers to the meaning of \code{TRUE}:
#' \itemize{
#'
#' \item \code{geomorph_wcoast}: mainly concerns dune slacks, mud flats and
#' salt marshes.
#'   \emph{Derived from: Cosyns et al. (2019)}
#'
#' \item \code{anthrop_gwdep}: zones located within a 100 m buffer around
#' (almost) everywhere groundwater dependent habitat types (or regionally
#' important biotopes) ánd situated within zones classified as 'anthropogenic'
#' areas within the soil map.
#'   \emph{Derived from: \code{\link[=read_soilmap]{soilmap_simple}} and
#'   \code{\link[=read_habitatmap_terr]{habitatmap_terr}}
#'    data sources}
#'
#' \item \code{narrowanthrop_gwdep}: narrow zones classified as 'anthropogenic'
#' areas within the soil map that include (almost) everywhere groundwater
#' dependent habitat types (or regionally important biotopes).
#'   \emph{Derived from: \code{\link[=read_soilmap]{soilmap_simple}} and
#'   \code{\link[=read_habitatmap_terr]{habitatmap_terr}}
#'    data sources}
#'
#' \item \code{drainage}: soils that are at least moderately gleyic or wet.
#'   \emph{Derived from: \code{\link[=read_soilmap]{soilmap_simple}}
#'    data source}
#'
#' \item \code{dunes_gwdep}: zones located within a 100 m buffer around (almost)
#' everywhere groundwater dependent habitat types (or regionally important
#' biotopes) ánd situated within zones classified as 'dunes' areas within the
#' soil map.
#'   \emph{Derived from: \code{\link[=read_soilmap]{soilmap_simple}} and
#'   \code{\link[=read_habitatmap_terr]{habitatmap_terr}}
#'    data sources}
#'
#' \item \code{peat_profile}: variant of the soil profile indicates a
#' superficial peaty cover, mostly on gleyic or permanently water saturated
#' soil with or without profile development (\code{(v)}), eventually combined
#' with strong anthropogenic influence (\code{(o)}).
#'   \emph{Derived from: \code{\link[=read_soilmap]{soilmap_simple}}
#'    data source}
#'
#' \item \code{peat_substr}: soil substrate (layer underlying superficial layer,
#' and lithologically diverging from it) consists of peat material starting at
#' small (less than 75 cm; \code{v}) or moderate depths (75-125 cm;
#' \code{(v)}), or a combination of the previous (\code{v-}).
#'   \emph{Derived from: \code{\link[=read_soilmap]{soilmap_simple}}
#'    data source}
#'
#' \item \code{peat_parentmat}: parent material contains a mixture of at least
#' 30\% of peaty material.
#'   \emph{Derived from: \code{\link[=read_soilmap]{soilmap_simple}}
#'    data source}
#'
#' \item \code{peat_texture}: soil consists of plain peat material.
#'   \emph{Derived from: \code{\link[=read_soilmap]{soilmap_simple}}
#'    data source}
#'
#' \item \code{phys_system}: polygons designated as seepage areas where
#' groundwater is supposed to gather after having infiltrated elsewhere
#' (infiltration areas) and being transported through the landscape (passage
#' areas).
#'   \emph{Derived from: Lhermitte & Honnay (1994)}
#'
#' \item \code{zwin}: the contour of the Zwin Nature Reserve in the most
#' eastern part of the Flemish coastal area.
#'   \emph{Derived from: Open Street Map, consulted 2021-10-06 by QGIS plugin
#'   QuickOSM}
#'
#' \item \code{habitat_1130}: polygons located within estuaries (habitat type
#' \code{1130}).
#'   \emph{Derived from: \code{\link[=read_habitatmap]{habitatmap}}
#'    data source}
#'
#' \item \code{gwdepth_coast}: locations with estimated average lowest
#' groundwater table less than 2.5 m below soil surface (shallow groundwater),
#' based on interpolation of measured groundwater levels in areas along the
#' Flemish coast with sufficient gauge densities.
#'   \emph{Derived from:
#'   the \href{http://data.inbo.be/watina}{Watina}
#'   database of the Research Institute for Nature and Forest (INBO)}
#'
#' \item \code{gwdepth_local}: mean lowest groundwater level less than 2 m
#' below soil surface (MLW) in a large military training site for which no
#' information is available in the soil map of Flanders.
#'   \emph{Derived from: Batelaan et al. (2012)}
#'
#' \item \code{seepage}: area with modelled seepage fluxes of at least
#' 0.8 mm/day in the central part of eastern Flanders.
#'   \emph{Derived from: Batelaan & De Smedt (1994)}
#'
#' \item \code{peat_survey}: local peaty zones evaluated by simple measurements
#' of the depth of plain and superficial peaty soil layers at regular intervals
#' (about 20 m).
#'   \emph{Derived from: local inventories executed or compiled by the Research
#'   Institute for Nature and Forest (INBO)}
#'
#' \item \code{duneslack}: polygons with dune slack vegetations along the
#' Flemish coast that typically imply shallow groundwater levels.
#'   \emph{Derived from: Provoost et al. (2020)}
#'
#' }
#'
#'
#' @family functions returning environmental data sets
#'
#' @references
#' \itemize{
#' \item Batelaan O., De Becker P., El-Rawy M., Herr C.,
#' Schneidewind, U. (2012). Doorrekenen van maatregelen voor herstel van
#' vochtige heidevegetaties op het Schietveld van Houthalen-Helchteren via
#' grondwatermodellering. Vrije Universiteit Brussel/Instituut voor Natuur en
#' Bosonderzoek.
#' \item Batelaan O., De Smedt F. (1994). Regionale
#' grondwaterstroming rond een aantal kwelafhankelijke natuurgebieden. Vrije
#' Universiteit Brussel.
#' \item Cosyns E., Bollengier B., Provoost S. (2019).
#' Masterplan en juridische basis voor grensoverschrijdende samenwerking en
#' bescherming als een transnationaal natuurpark van de duinen tussen Dunkerque
#' (Frankrijk) en Westende (België). Partim Masterplan. Rapport in opdracht van
#' Agentschap Natuur en Bos, Conservatoire de l'espace littoral et des rivages
#' lacustres, Conseil Général Département du Nord.
#' \url{https://www.natuurenbos.be/sites/default/files/inserted-files/masterplan_flandre_ned20200210def.pdf}
#' \item Knotters M. & van Walsum P.E.V. (1997). Estimating fluctuation
#' quantities from time series of water-table depths using models with a
#' stochastic component. Journal of Hydrology 197 (1): 25–46.
#' \doi{10.1016/S0022-1694(96)03278-7}.
#' \item Lhermitte K., Honnay O. (1994). Kartering van het Fysisch Systeem en
#' de Ruimtelijke structuren in Vlaanderen op schaal 1 : 50 000. Stichting
#' Plattelandsbeleid 1994, Boerenbond, Leuven.
#' \item Provoost S., Van Gompel W. & Vercruysse E. (2020). Beheerevaluatie
#' kust. Eindrapport 2015-2019. Rapporten van het Instituut voor Natuur- en
#' Bosonderzoek 2020 (18). Instituut voor Natuur- en Bosonderzoek, Brussel.
#' \doi{10.21436/inbor.18039583}.
#' \item Van Heesen H.C. (1970).
#' Presentation of the seasonal fluctuation of the water table on soil maps.
#' Geoderma 4 (3): 257–278. \doi{10.1016/0016-7061(70)90006-6}.
#' }
#'
#' @examples
#' \dontrun{
#' shallowgroundwater <- read_shallowgroundwater()
#' shallowgroundwater
#' }
#'
#' @importFrom assertthat
#' assert_that
#' @importFrom sf
#' read_sf
#' @export
read_shallowgroundwater <-
    function(file = file.path(fileman_up("n2khab_data"),
                              "10_raw/shallowgroundwater/shallowgroundwater.gpkg")){

        assert_that(file.exists(file))

        shallowgroundwater <-
            suppressWarnings(
                read_sf(file,
                        crs = 31370)
            )

        return(shallowgroundwater)

    }
