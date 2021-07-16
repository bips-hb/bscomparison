### collecting results
res <- readRDS("results.rds")

x <- res[[2]][1][[1]]
sapply(2:length(res), function(i) {x <<- rbind(x, res[[2]][i][[1]]) })
x ## results


o <- readRDS("output-example.rds")
o

## small
s = readRDS("small-toy.rds")

sapply(s$result, function(l) {any(is.nan(l$F1))})

s %>% filter(any(is.nan(result$F1)))

s %>% rowwise() %>% 
  mutate(F1 = max(result$F1, na.rm = T)) %>% View()
  mutate(s, F1 = max(result$F1, rm.na = T))


  mutate(F1 = result[[]])


res <- readRDS("data/high-n100-p1000-s10-block-0.35-first-snr6-run1.rds")



X <- res$X
y <- res$y 

res <- fs(X,y)

enet(X, y)

o <- enet(X, y, alpha = .5)
as.integer(gsub("[^0-9\\.]", "", names(o$rank)))
length(o$id)
length(o$lambda)

n <- length(y)
p <- dim(X)[2]

filename <- "data/high-n100-p1000-s10-block-0.35-first-snr0.05-run1.rds"

# remove extension and directory
filename <- tools::file_path_sans_ext( basename(filename) )

temp <- strsplit(filename, split = "-")[[1]]

return_value <- function(string, conv = as.integer) { 
   conv( gsub("[^0-9\\.]", "", string) )
}

params <- list()
params$dimensionality <- temp[1]
params$n <- return_value( temp[2] )
params$p <- return_value( temp[3] )
params$s <- return_value( temp[4] )
params$corr_type <- temp[5]
params$rho <- return_value( temp[6], as.numeric )
params$beta <- temp[7]
params$snr <- return_value( temp[8], as.numeric )
params$run <- return_value( temp[9] )

gsub("[^0-9]", "", x) 
