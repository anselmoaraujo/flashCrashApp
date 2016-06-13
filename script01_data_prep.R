library(devtools)
library(nxcore)
library(tcor)
library(threejs)
library(igraph)
library(doMC)
library(Matrix)

data(sp)

Sys.setenv(TZ='UTC')

# download the data from S3 if needed
fin <- "data/taq_20100506_trades_all.csv"
if (!file.exists(fin)) {
  dir.create("data", FALSE)
  download.file(
    "https://s3.amazonaws.com/archived-taq-data/taq_20100506_trades_all.csv.gz",
    "data/taq_20100506_trades_all.csv.gz")
  system(paste("gunzip", "data/taq_20100506_trades_all.csv.gz"))
}

# load data as taq file and save (fairly slow)
eft <- readLines("data/eft.txt")
x <- read_taq("data/taq_20100506_trades_all.csv",
              symbols=c(sp$symbol,eft))
saveRDS(x, "data/taq_20100506.rds")
