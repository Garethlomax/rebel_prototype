l1 = c("a", "b", "c", "c")
l2 = c(1,2,3,4)
df = data.frame("col1" = l1, "col2" = l2)
# can put this in here
#
vectors <- I(list(c(1,2,3,4,5), c(4,5,6), c(7,8,9), c(12,2,2)))
df$new <- vectors

# make list for all possibe neighbours
# ut in to adjacecncy matrix.
# combine over columns

