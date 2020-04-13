#' Generate all candidates for a vector of input strings
#'
#'
#' @export
#'

EVAL <- function(input_str, cand_strs, .progress = TRUE) {

        # plan to evaluate functions in parallel with, e.g. furrr::future_map
        plan(multiprocess)

        lsf.str("package:akwelye") %>%                                  # from list of functions in package,
                keep(~ str_detect(., "^[f|m]const_")) %>%               # keep only functions implementing a f(aithfulness) or m(arkedness) const(raint)
                set_names(x  = as.list(.), nm = .) %>%                  # turn into a named list
                future_map(
                        .progress = .progress,
                        .f = function(const_str) {

                                const_func <- get(const_str)            # get actual function given name of function

                                if(str_detect(const_str, "^f")) {       # if faithfulness constraint, supply input to function

                                        const_func(input_str, cand_strs)

                                } else {                                # otherwise, supply only candidates

                                        const_func(cand_strs)

                                }

                        }) %>%
                bind_cols()

}
