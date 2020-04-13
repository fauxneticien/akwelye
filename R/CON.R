#' Assign a violation to every prosodic word whose left edge is not aligned with the left edge of some bar
#'
#'
#' @export

mconst_al_pwd_bar <- function(cand_str) {

        map_int(
                .x = cand_str,
                .f = function(cand_str) {

                        temp_str <- str_remove(cand_str, "\\|$") %>% # remove line-final |
                                str_remove_all("\\.+|\\-|\\+")       # remove non-syllabic characters

                        bar_starts <- str_locate_all(temp_str, "\\|") %>%
                                as.data.frame() %>%
                                pull(start)

                        pwd_starts <- str_locate_all(temp_str, "#|=") %>%
                                as.data.frame() %>%
                                pull(start)

                        # for every = or #, find shortest distance to a |
                        expand.grid(
                                bar_start = bar_starts,
                                pwd_start = pwd_starts
                        ) %>%
                        mutate(dist = abs(pwd_start - bar_start) - 1) %>%
                        group_by(pwd_start) %>%
                        summarise(min_dist = min(dist)) %>%
                        pull(min_dist) %>%
                        # sum the shortest distances for total violation for given candidate
                        sum() %>%
                        as.integer()
                }
        )

}

#' Assign a violation to every syllable (whose duration is greater than a quaver) which is not immediately followed by the right edge of a bar.
#'
#' @export

mconst_ar_lng_bar <- function(cand_str) {

        map_int(
                .x = cand_str,
                .f = function(cand_str) {

                        temp_str <- str_remove(cand_str, "^\\|") %>%     # remove line-initial |
                                str_remove_all("#|=|\\-|\\+") %>%        # remove word/morpheme boundary characters
                                str_replace_all("(σ|α|τ|κ)\\.+", "L")    # mark up all syllables with 1 or 2 dots following them

                        L_starts <- str_locate_all(temp_str, "L") %>%
                                as.data.frame() %>%
                                pull(start)

                        bar_ends <- str_locate_all(temp_str, "\\|") %>%
                                as.data.frame() %>%
                                pull(start)

                        # for every bar, find shortest distance to a # or =
                        expand.grid(
                                L_start = L_starts,
                                bar_end = bar_ends
                        ) %>%
                                mutate(dist = bar_end - L_start - 1) %>%
                                filter(dist >= 0) %>%
                                group_by(L_start) %>%
                                summarise(min_dist = min(dist)) %>%
                                pull(min_dist) %>%
                                # sum the shortest distances for total violation for given candidate
                                sum() %>%
                                as.integer()
                }
        )

}

#' Assign a violation for every bar with 4 syllables in it
#'
#' @export

mconst_no_four <- function(cand_str) {

        map_int(
                .x = cand_str,
                .f = function(cand_str) {

                        str_remove(cand_str, "^\\|") %>%
                                str_remove("\\|$") %>%
                                str_split("\\|") %>%
                                unlist() %>%
                                str_count("σ|α|τ|κ") %>%
                                tibble(num_sylls = .) %>%
                                mutate(has_four = num_sylls == 4) %>%
                                .$has_four %>%
                                sum(na.rm = TRUE)

                })

}

#' Assign a violation where for every pair of consecutive bars, the number of syllables in the latter bar is greater than the number
#' of syllables in the former bar.
#'
#' @export
mconst_no_long <- function(cand_str) {

        map_int(
                .x = cand_str,
                .f = function(cand_str) {

                        str_remove(cand_str, "^\\|") %>%
                                str_remove("\\|$") %>%
                                str_split("\\|") %>%
                                unlist() %>%
                                str_count("σ|α|τ|κ") %>%
                                tibble(num_sylls = .) %>%
                                mutate(got_longer = num_sylls > lag(num_sylls)) %>%
                                .$got_longer %>%
                                sum(na.rm = TRUE)

                })

}

#' Assign a violation where for every pair of consecutive bars, the number of syllables in the latter bar is not fewer than the number
#' of syllables in the former bar.
#'
#' @export
mconst_get_short <- function(cand_str) {

        map_int(
                .x = cand_str,
                .f = function(cand_str) {

                str_remove(cand_str, "^\\|") %>%
                        str_remove("\\|$") %>%
                        str_split("\\|") %>%
                        unlist() %>%
                        str_count("σ|α|τ|κ") %>%
                        tibble(num_sylls = .) %>%
                        mutate(got_longer_or_same = !num_sylls < lag(num_sylls)) %>%
                        .$got_longer_or_same %>%
                        sum(na.rm = TRUE)

        })

}

#' Assign a violation to every bar which does not contain exactly one prosodic word
#'
#' @export

mconst_pwd_in_bar <- function(cand_str) {

        map_int(
        .x = cand_str,
        .f = function(cand_str) {

                str_remove(cand_str, "^\\|") %>%
                        str_remove("\\|$") %>%
                        str_split("\\|") %>%
                        unlist() %>%
                        str_count("#|=") %>%
                        tibble(num_pwds = .) %>%
                        mutate(no_or_many_pwds = num_pwds != 1) %>%
                        .$no_or_many_pwds %>%
                        sum(na.rm = TRUE)

        })

}

#' Assign a violation to every onsetless syllable
#'
#' @export

mconst_onset <- function(cand_str) { str_count(cand_str, "α") }

#' Assign a violation to every syllable in the output with an epenthetic onset consonant
#'
#' @export

fconst_depc <- function(input_str, cand_str) { str_count(cand_str, "κ") }

#' #' Assign a violation to every syllable in the output that was not in the input
#' #'
#' #' @export
#'
#' fconst_max_syll <- function(input_str, cand_str) {
#'
#'         num_input_sylls <- str_count(input_str, "σ|α")
#'
#'         map_int(
#'                 .x = cand_str,
#'                 .f = ~ abs(num_input_sylls - str_count(., "σ|α|τ|κ"))
#'         )
#'
#' }

#' Assign a violation if the line-initial vowel in the input has been deleted in the output
#'
#' @export

fconst_max_v <- function(input_str, cand_str) {

        as.integer(str_detect(input_str, "^α") & str_detect(cand_str, "^\\|#σ"))

}

#' Assign a violation if the line-initial vowel in the input has been deleted in the output
#'
#' @export

fconst_no_transfer <- function(input_str, cand_str) {

        as.integer(str_detect(cand_str, "τ"))

}

#' Assign a violation if the transferred line-initial syllable was part of a line-final root form
#'
#' @export

fconst_ident_root <- function(input_str, cand_str) {

        map_int(
                .x = cand_str,
                .f = function(cand_str) {

                        as.integer(str_detect(cand_str, "^\\|#τ") && !str_detect(cand_str, "-\\|$"))

                }
        )

}

