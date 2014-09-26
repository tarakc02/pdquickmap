chunk <- function(vec, chunk_size) {
    # this function owes much to the chunk functions in BBmisc
    len <- length(vec)
    last_bit <- len %% chunk_size > 0L
    chunks <- len %/% chunk_size + last_bit
    chunks <- sort(seq.int(0L, len - 1L) %% chunks)
    split(vec, chunks)
}