library(nxcore)
library(tcor)
library(threejs)
library(igraph)
library(doMC)
library(Matrix)
library(igraph)

# set time zone and load data
Sys.setenv(TZ='UTC')
x <- readRDS("data/taq_20100506.rds")
eft_symb <- readLines("data/eft.txt")
data(sp); sp_symb <- sp$symbol

# set global parameters of parameter windows
stime  <- strptime("2010-05-06 13:00:00", format="%Y-%m-%d %H:%M:%S")
window <- 60 * 15
delta  <- 60
n <- (60 * 60 * 2) / delta - 1


# (1) get network induced by co-integration / correlation
output <- list(network = vector("list", n),
               time = stime + seq(0, by = delta, length.out=n))
for (i in 1:n) {
  # construct datasets from the time period
  time_range <- c(stime, stime + window) + delta * (i - 1) - window
  t1 <- stime - window
  trades <- x[time(x) <= time_range[2] & time(x) >= time_range[1]]
  trades <- trades[,apply(trades, 2, sd) > 0, drop=FALSE]

  if (nrow(trades) == 0) next

  # find correlation matrix
  tc <- tcor(trades, 0.95)
  if (!is.matrix(tc$indices)) next

  # Create the adjacency matrix
  s_mat = spMatrix(nrow=ncol(trades), ncol=ncol(trades),
                   i=tc$indices[,"i"], j=tc$indices[,"j"],
                   x=tc$indices[,"val"])
  rownames(s_mat) = colnames(s_mat) = colnames(trades)

  # create the graph
  s_mat <- as.matrix(s_mat > 0.99)
  s_mat[lower.tri(s_mat)] <- FALSE
  gr <- graph.adjacency(s_mat)

  # find biggest component
  cmp <- components(gr)
  id <- which.max(cmp$csize)
  sgr <- induced_subgraph(gr, which(cmp$membership == id))

  output$network[[i]] <- networkD3::igraph_to_networkD3(sgr)
  output$network[[i]]$nodes$group <- (output$network[[i]]$nodes$name %in% sp_symb[1]) + 1

  print(i)
  saveRDS(output, "app_data/output_network.Rds")
}


# (2) get total S&P 500 values over the data
all_sp_trades <- x[time(x) <= stime + delta * n & time(x) >= stime]
all_sp_trades <- all_sp_trades[,names(all_sp_trades) %in% sp_symb]
sp_val <- round(apply(all_sp_trades, 1, sum))
sp_val <- data.frame(time = strptime(names(sp_val), format="%Y-%m-%d %H:%M:%S"),
                     val = as.numeric(sp_val))


# (3) Get size of the giant component over time
nc <- lapply(output$network, function(v) nrow(v$nodes))
nc <- sapply(nc, function(v) {if(is.null(v)) v <- 0; v})
names(nc) <- NULL
nc <- data.frame(time = output$time, val = nc)

out <- list(nc = nc, sp_val = sp_val)
saveRDS(out, "app_data/time_series_data.Rds")



