#' Generate all candidates for a vector of input strings
#'
#'
#' @export
#'

gen_all_cands <- function(syll_str, add_hinge = TRUE) {

      sylls_to_lengths <- read_csv(system.file("extdata", "sylls_to_lengths.csv", package = "akwelye"), col_types = "icinccc", na = "NA")

      if(add_hinge) {
        if(str_detect(syll_str, "-C#α")) {

          hinge_left_str  <- str_replace(syll_str, "-C#α", "-σ#")
          hinge_right_str <- str_replace(syll_str, "-C#α", "#-σ")

          syll_str <- c(hinge_left_str, hinge_right_str)

        }
      } else {
        syll_str  <- str_replace(syll_str, "-C#α", "-σ#")
      }

      if(any(str_detect(syll_str, "^α"))) {
              syll_str <- c(
                      syll_str,
                        str_replace(syll_str, "^α", "κ"),
                        str_replace(syll_str,  "^α", "τ") %>% str_remove("σ$"),
                        str_remove(syll_str, "^α")
                )
        }

        tibble(syll_str) %>%
        mutate(
          cand_no    = 1:n(),
          num_sylls  = count_sylls(syll_str),
          syll_type  = map(syll_str, ~ unlist(str_split(., "")) %>% discard(~ nchar(.) == 0))
        ) %>%
        unnest(cols = c(syll_type)) %>%
        group_by(cand_no) %>%
        mutate(
          syll_no = ifelse(str_detect(syll_type, "σ|α|τ|κ"), 1, 0) %>% cumsum()
        ) %>%
        left_join(sylls_to_lengths, by = c("num_sylls", "syll_no")) %>%
        arrange(cand_no, combo_no, syll_no) %>%
        mutate(syll_type = ifelse(str_detect(syll_type, "σ|α|τ|κ"), paste(bar_start, syll_type, add_length, bar_end, sep = ""), syll_type)) %>%
        group_by(cand_no, combo_no) %>%
        summarise(cand_str = paste(syll_type, collapse = "")) %>%
        # post-hoc fixes :(
        mutate(cand_str = str_replace(cand_str, "\\|(\\+|\\-)$", "\\1|")) %>%
        .$cand_str %>%
        I()

}

