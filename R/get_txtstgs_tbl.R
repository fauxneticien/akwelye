#' Generate all rhythmically valid note combinations for n syllables
#'
#' @export
#'

get_txtstgs_tbl <- function(num_sylls) {

        tmp <- lapply(as.list(rep(3, num_sylls)), seq)
        res1 <- expand.grid(tmp)

        as_tibble(res1) %>%
                mutate(combo_no = 1:n()) %>%
                gather(digit, value, -combo_no) %>%
                mutate(
                        digit = str_remove(digit, "Var") %>% as.integer(),
                        syll_length = case_when(
                                value == 1 ~ 0.5,
                                value == 2 ~ 1.0,
                                value == 3 ~ 1.5
                        )
                ) %>%
                arrange(combo_no, digit) %>%
                group_by(combo_no) %>%
                summarise(
                        val_str    = list(as.numeric(syll_length))
                ) %>%
                mutate(ok_dur_seq = map_lgl(val_str, dur_seq_ok_bc)) %>%
                filter(ok_dur_seq) %>%
                select(combo_no, note_vals = val_str)

}

dur_seq_ok <- function(dur_vector, divisor = 2) {

        dur_sum <- 0

        for(l in dur_vector) {
                dur_sum <- dur_sum + l

                if(dur_sum > divisor) {
                        return(FALSE)
                } else if (dur_sum %% divisor == 0) {
                        dur_sum <- 0
                        next()
                }
        }

        if(dur_sum != 0) {
                FALSE
        } else {
                TRUE
        }
}

dur_seq_ok_bc <- compiler::cmpfun(dur_seq_ok)
