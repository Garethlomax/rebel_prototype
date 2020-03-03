l1 = c("a", "b", "c", "c", "d")
gid = c(1,2,3,4,4)
period_start = c(1,2,2,4,4)
df = data.frame("event" = l1, "gid" = gid, "period_start" = period_start)
# can put this in here
#
vectors <- I(list(c(1,2,3,4,5), c(4,5,6), c(7,8,9), c(12,2,2), c(1)))
vectors2 <- I(list(c(1,2), c(5), c(1), c(12,2), c(1)))
df$neighbours <- vectors
df$vecs <- vectors2

# make list for all possibe neighbours
# ut in to adjacecncy matrix.
# combine over columns

# x <- dplyr::mutate(datax@dataset@events, vals = vectorized_lag_neighbours(priogrid_gid, 1))
i <- df %>% dplyr::group_by(period_start) %>% dplyr::summarize(new_var = list(unique(gid)))
# j <- df %>% dplyr::group_by(period_start) %>% dplyr::mutate(new_var = I(list(unique(gid))))





summarise_active_grids <- function(x){
  # summarises the starting period and gives all existing priogrids in the time i.e the active ones.
  out <- x %>% dplyr::group_by(period_start) %>% dplyr::summarize(new_var = list(unique(priogrid_gid)))
}

list_set_intersect <- function(x, y){
  # quick function to take two lists, and find the intersection of the lists
  x <- intersect(x, y)
  # unlist() <- for unlisting and turning to vector.
}
