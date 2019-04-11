#' Derive surface form by sequentially applying an ordered set of phonological rules
#'
#' @param underlying_form a unicode character string, e.g. 'akwelye'
#' @param ordered_rules character vector of phonological rules (given as names of R functions), e.g. \code{c("round_schwas", "front_initial_schwas")}
#'
#' @export

derive_surface <- function(underlying_form, ordered_rules) {

        # Stop if function name not in global environment or akwelye package
        stopifnot(all(ordered_rules %in% c(lsf.str(), lsf.str("package:akwelye"))))

        derivation <- c(underlying_form)

        for(i in 1:length(ordered_rules)) {

                output <- do.call(ordered_rules[[i]], list(last(derivation)))

                derivation <- c(derivation, output)

        }

        tibble(step = 0:length(ordered_rules), rule = c("-", ordered_rules), form = derivation)

}
