#' Determine reduplicant from base and suffix accordingly
#'
#' @param base a unicode character string
#' @param type type of reduplication to perform (e.g. 2-3)
#'
#' @export

reduplicate <- function(base, type = "2-3") {

        stopifnot(type %in% c("2-3"))

        if(type == "2-3") {
                # reduplicant is 2nd and 3rd consonantl-initial syllables
                # remove irrelevant vowel-initial syllable if present
                reduplicant <- str_remove(base, "^[aə]\\.")

                if(str_count(reduplicant, "\\.") < 2) {
                        # return NA if fewer than C-initial syllables
                        NA
                } else {
                        reduplicant %>%
                                str_split("\\.",simplify = TRUE) %>%
                                .[,2:3] %>%
                                paste0(collapse = ".") %>%
                                paste0(base, "-", .)
                }

        }

}
