#' Convert transcriptions in IPA to working representations
#'
#' @export

ipa_to_cand_str <- function(ipa_str) {

        ipa_str %>%
                remove_diacritics() %>%
                str_remove_all("ˈ") %>%
                str_replace_all("[a|ə|i|u|ɔ|e]", "V") %>%          # replace vowels with V
                str_replace_all("[^V|\\.|#|=|\\-|\\+]", "C") %>%   # replaces non-vowels with C
                str_replace_all("C+VC*", "σ") %>%                  # σ = syllables with onsets
                str_replace_all("VC*", "α") %>%                    # α = syllables without onsets
                str_remove_all("\\.")                              # remove syllable boundaries

}

#' Count number of syllables in a candidate string
#'
#' @details
#' ασσσ#σ+σ → 6
#'
#' @export

count_sylls <- function(cand_str) {

        cand_str %>%
                str_count("α|σ|τ|κ")

}

remove_diacritics <- function(ipa_string) {

        diacritics <- read_csv(system.file("extdata/diacritics.csv", package = "akwelye"), col_types = "ccc")

        reduce(
                .x = diacritics$unicode_hex,
                .f = ~ str_remove_all(.x, .y),
                .init = ipa_string
        )

}
