gid_to_coords <- function(gid){
  # returns coords from gid in gid format
  # x, y where south west is 1, 1
  # again complicated by stupid prio not following standard matrix notation
  gid <- gid - 1
  y <- gid %/% 720
  x <- gid %% 720
  x <- x + 1
  y <- y + 1
  return(c(x, y))
}

coords_to_gid <- function(coords){
  # takes coords and turns into gid
  return(coords[1] + (coords[2]-1)*720)
}

enforce_periodic_boundaries <- function(gid){
  coords <- gid_to_coords(gid)
  coords <- periodic_boundary_conditions(coords)
  gid <- coords_to_gid(coords)
  return(gid)
}

periodic_boundary_conditions <- function(coords){
  # enforces periodic boundary conditions
  # TODO: double check this
  if (coords[1] > 720){
    coords[1] <- coords[1] - 720
  }
  if (coords[1] < 1){
    coords[1] <- coords[1] + 720
  }
  if (coords[2] > 360){
    coords[2] <- coords[2] - 360
  }
  if (coords[2] < 1){
    coords[2] <- coords[2] + 360
  }
  return(coords)
}


border_overlap_check <- function(gid, order){
  # can be either close on top bottom, or left right, or both.
  # grid is 720 lengthwise
  # 360 height
  # bottom left is origin (this is a genuinely awful idea)

}

h_bound_check <- function(gid, bounds){
  # enforces periodic boundary conditions on the lags.
  coords <- gid_to_coords(gid)
  x_coords <- coords[1] + bounds

}

lag_neighbours <- function(gid, order){
  # presume isnt time dependant - probably - depending on function.
  # function to find list of gids included in lag.
  # at current we remove all negative indices
  # THIS MEANS THAT OVERLAPS ARE NOT CONSIDERED BY DEFINITION i.e edge cases where adjaceny could be between 1 and 720
  # are not considered.
  d <- 2 * order + 1
  # First check if lag will overlap with border


  horizontal_bounds <- c(-order : order)

  vertical_bounds <- seq(-order * 720 , 720 * order, by = 720) # for increments of prio grid size.
  mesh = meshgrid(horizontal_bounds, vertical_bounds)
  # print(mesh$x)
  # print(mesh$y)
  ans <- c(gid + mesh$x + mesh$y) # turns into list
  for (i in 1:length(ans)){
    ans[i] <- enforce_periodic_boundaries(ans[i]) # enforces boundary conditions and corrects - makes periodic.
  }
  # remove the center gid - do not include in the lag.
  ans <- ans[ans != gid]
  return(list(ans))
}

active_cells <- function(x){
  # fimds the number of grid cell that are currently active - so can use these for adjacency matrix
  group <- x@dataset@events %>%
    dplyr::group_by(period_start) # just find if there are events
  # TODO: we may need to add a mutate here
  group_summary <- dplyr::summarize(
    group, .var = dplyr::distinct(priogrid_gid)) # distinct values for a period

  .rebeltrack_dataframe(dataset = x@dataset, data = data)

}

lag_for_active <- function(x){
  # lags for active units in dataframe
  x <- active_cells(x) # finds active units.

}

.vectorized_lag_neighbours <- Vectorize(lag_neighbours)

vectorized_lag_neighbours <- function(gid, order){
  # must take direct reference to the column
  lag <- .vectorized_lag_neighbours(gid, order)
  # lag <- aperm(lag, c(2,1)) # re arrange the permutation
  return(lag)
}



add_neighbour_column <- function(x){
  # adds column of neighbours to each event
  # first make list of neighbours
  x <- dplyr::mutate(x@dataset@events, vals = vectorized_lag_neighbours(priogrid_gid, 1))
}

summarise_active_grids <- function(x){
  # summarises the starting period and gives all existing priogrids in the time i.e the active ones.
  out <- x %>% dplyr::group_by(period_start) %>% dplyr::summarize(new_var = list(unique(priogrid_gid)))
}

total_active_grids <- function(x){
  # adds active grids for given time period as new column
  # incase it wasnt obvious this is a massively inneficient way of doing this
  # TODO: sort this
  out <- x %>% dplyr::group_by(period_start) %>% dplyr::mutate(active_grids = I(list(unique(priogrid_gid))))
}

list_set_intersect <- function(x, y){
  # quick function to take two lists, and find the intersection of the lists
  x <- intersect(x, y)
  # unlist() <- for unlisting and turning to vector.
}

vectorized_intersect <- Vectorize(list_set_intersect) # seems to be working

active_neighbours <- function(x){
  # finds active neighbours by finding intersect between active neighbours and possible current neighbours
  active <- add_neighbour_column(x)

  active <- total_active_grids(active)

  active <- active %>% dplyr::mutate(grid_intersect = I(vectorized_intersect(active_grids, vals)))
}

# now all we have to do is remove duplicates to get the neighbours at each time
# and put into array.
# and try to avoid some horrific O(n^?) times.

extract_summary <- function(x){
  # takes relevant dataframe with neighborus found and computes into a summary of information with date, gid and neighbours
  group <- x %>% dplyr::group_by(priogrid_gid, period_start) %>% dplyr::summarise(new_var = grid_intersect[1])#filter(dplyr::row_number()==1)
}

unique_events <- function(x){
  # is dataframe of events - gives one event per gid month
  unique <- x %>% dplyr::group_by(priogrid_gid, period_start) %>%  dplyr::filter(dplyr::row_number() == 1)
}


# extracts number of events for each time
#group <- test3 %>% dplyr::group_by(priogrid_gid, period_start) %>% dplyr::tally()

# df %>% arrange(stopSequence) %>% group_by(id) %>% slice(c(1,n()))
labelled_matrix <- function(dim = 3, gid_vec){
  # takes dataframe with column of lists - makes them into matrix.
  # dim is the size of the matrix
  # gid_vec is a vector of all gids - to rename the matrix.
  # this function takes a dataframe that has already been filtered - i.e for one time period.
  mat <- matrix(0, dim, dim, dimnames = list(gid_vec, gid_vec)) # gid vec to refer to matrix
  return(mat)
  # not efficient at all
  # need to sort by priogrid
  # for (gid in x$priogrid_gid){
  #   for (adj in x$grid_intersect[])
  # }

}

adj_list_to_mat <- function(gid, adj, mat){
  # adds all in single list for grid to matrix.
  # matrix remains same
  # mat must have had columns renamed
  for (i in adj) {
    mat[sprintf("%i", gid), sprintf("%i", i)] = 1 #over list of adjacent for each gid
    # going for column ordered.
    }
  return(mat) # dont know if we want to do this or act on a general one.
  # you cannot pass by refernce in R. This is a garbage language.


}


looped_adj_list_to_mat <- function(unique_gids, gid, adj){
  # unique gids should be all possible gids
  # loops over adj list as bodge
  mat <- labelled_matrix(length(unique_gids), unique_gids)
  for (i in 1:length(gid)){
    mat <- adj_list_to_mat(gid[i], adj[i], mat) # over writes matrix
  }
  # print(isSymmetric(mat)) # matrix is adjacency matrix so should be symmetric.
  return(mat)
}

adj_wrapper <- function(x){
  # for unique gids the unique gids should come from outside the loop - i.e not be time variant
  unique_gids <- unique(dplyr::arrange(x, priogrid_gid)$priogrid_gid)

  mat <- looped_adj_list_to_mat(unique_gids, x$priogrid_gid, x$grid_intersect)
  return(mat) # over looped variable.
}

full_adj_matrix <- function(x){
  # naive iteration over all grids
  un <- unique(dplyr::arrange(x, priogrid_gid)$priogrid_gid)

  dim = length(un)

  start_date = unique(dplyr::arrange(x, period_start)$period_start)

  matrices <- vector("list", length(start_date))
  # loops are bad.
  i = 1
  for (date in start_date){
    print(i)
    matrices[i] = adj_wrapper(x[x$period_start == date, ])
    i = i + 1

  }
  return(matrices)
}
.vectorized_adj_list_to_mat <- Vectorize(adj_list_to_mat)

vectorized_adj_list_to_mat <- function(gid, adj, mat){
  # trying to replicate pass by reference.
  return(.vectorized_adj_list_to_mat(gid, adj, mat))
}



adj_matrix <- function(x){
  # function to define adjacency matrix for geographic
  # first we construct the empty matrix - can we do csr sparse?
  time_steps <- dplyr::n_distinct(x@dataset@events$period_start)
  n_grids <- dplyr::n_distinct(x@dataset@events$priogrid_gid)
  arr <- array(0, dim = c(n_grids, n_grids, time_steps)) # empty array of zeros.
  # we arrange the gids in ascending order
  # arrange by gids
  arranged <- dplyr::arrange(x@dataset@events, priogrid_gid)
  # unique gids
  gids <- unique(arranged) # arrange unique
  # summarise lists.
  # unique neighbours
  uniq <- unique_events(x@dataset@events)


}





