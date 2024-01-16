.faster_subsequent_times_test <- function ()
{
    first_time <- microbenchmark::microbenchmark(
        `first time` = .External2(.C_sys_path),
        times = 1
    )
    subsequent <- microbenchmark::microbenchmark(
        subsequent = .External2(.C_sys_path),
        times = 100
    )
    rbind(first_time, subsequent)
}
