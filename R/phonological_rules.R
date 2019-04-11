#' Assign primary and secondary stress to syllables
#'
#' @export

assign_stress <- function(form) {

        form %>%
                str_split("#") %>%
                unlist %>%
                str_replace_all("^([aəe][^aəui]*\\.)?([^aəui]+)", "\\1ˈ\\2") %>%
                paste0(collapse = "#") %>%
                I()
}

#' Change /ə/ vowels preceding a glide-/ə/ sequence to appropriate quality
#'
#' @details
#' a. ə → ɔ / _wə
#'
#' @export

change_preGV_vowel <- function(form) {

        form %>%
                str_replace_all("ə(\\+)?wə", "ɔ\\1wə")

}

#' Delete stem-final /ə/ where suffixing element is vowel-initial (i.e. resolve hiatuses)
#'
#' @details
#' ə → ∅ / _]V, where ] word- or morpheme-boundary
#'
#' @export

delete_sfinal_schwas <- function(form) {

        form %>%
                str_replace_all("ə([+|#])([aəe])", "\\1\\2")

}

#' Front /ə/ vowels in word-initial position
#'
#' @details
#' ə → e / #_
#'
#' @export

front_initial_schwas <- function(form) {

        form %>%
                str_replace("^", "#") %>%
                str_replace_all("(?<=#)ə", "e") %>%
                str_remove("^#")

}

#' Round /ə/ vowels preceding a rounded consonant /Cʷ/ or labial approximant /w/
#'
#' @details
#' a. ə → u / Cʷ_
#'
#' b. ə → u / w_
#'
#' @export

round_schwas <- function(form) {

        form %>%
                str_replace_all("ʷə", "u") %>%
                str_replace_all("wə(?!#|$)", "wu")

}
