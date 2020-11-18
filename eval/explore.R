setwd("../output")
rm(list = ls())

# preamble
##########

tikz <- function(formula, name, ..., data=NULL, meta=list(), reorder=T, digits=5) {
  dir.create("../output/tex", showWarnings=F)
  fname <- sprintf("../output/tex/%s.tex", name)
  con <- file(fname, open="w")
  writeLines(paste("% R-generated plot:", name), con=con)
  mf <- model.frame(formula, data=data)
  y <- mf[[1]]
  x <- mf[[2]]
  if (!is.matrix(y)) {
    y <- cbind(y=y)
  }
  if (!is.matrix(x)) {
    x <- cbind(x=x)
  }
  stopifnot(ncol(x)==1)
  x <- round(x, digits)
  y <- round(y, digits)
  dict <- meta
  dict$xLab <- colnames(x)[1]
  dict$minX <- min(x)
  dict$maxX <- max(x)
  dict$minY <- min(y)
  dict$maxY <- max(y)
  for ( j in  1:ncol(y) ) {
    vx <- x[, 1]
    vy <- y[, j]
    if (reorder) {
      k <- order(vx)
      vx <- vx[k]
      vy <- vy[k]
    }
    path <- paste0("(", vx, ",", vy, ")", collapse=" -- ")
    dict[colnames(y)[j]] <- sprintf("\\path %s;", path)
  }
  writeLines(paste0("\\def\\", names(dict), "{", dict, "}"), con=con)
  close(con)
}

# load data
###########

runs <- read.csv('runs.csv')
str(runs)

runs$delta.dist <- as.factor(runs$delta.dist)
runs$block.orphan.rate <- with(runs, (blocks.observed - blocks.confirmed) / blocks.observed)
runs$vote.orphan.rate <- with(runs, (votes.observed - votes.confirmed) / votes.observed)

# aggregate iterations
runs.agg <- aggregate(cbind(vote.orphan.rate, block.orphan.rate) ~
                      tag + pow.scale + quorum.size + delta.block + delta.vote + delta.dist + n.nodes + n.blocks,
                      runs,
                      function (x) c("mean"=mean(x), "sd"=sd(x)))
runs.agg <- do.call("data.frame", runs.agg)
runs.agg$n.iterations <- nrow(runs) / nrow(runs.agg)

# list of experiments
unique(runs$tag)

# fixed-rate experiment
#######################
# delta: expected propagation time 1, both votes and blocks
# rate: 1 puzzle solution per delta
# x: quorum size
# y: orphan votes and blocks

fr.title <- function(d) {
  with(d,
       title(main=sprintf("tag: %s    rate: %.1f    delta: %.1f    nodes: %i    blocks: %i",
                          unique(tag), unique(1/pow.scale), unique(delta.vote),
                          unique(n.nodes), unique(n.blocks)
                          )))
}
fr <- subset(runs, tag=="fixed-rate")
fr.color   <- colorspace::rainbow_hcl(length(levels(fr$delta.dist)))
fr.color.3 <- colorspace::rainbow_hcl(length(levels(fr$delta.dist)), alpha=0.3)
plot(block.orphan.rate ~ quorum.size,
     data=fr,
     log='x', xaxt='n',
     col=fr.color.3[delta.dist],
     ylim=c(0,0.5),
     ylab='block orphan rate',
     xlab='quorum size')
ignore <- lapply(unique(fr$delta.dist), function (x) {
                   d <- subset(runs.agg, tag=="fixed-rate" & delta.dist==x)
                   lines(block.orphan.rate.mean ~ quorum.size, col=fr.color[x], data=d)
                   lines(block.orphan.rate.mean - block.orphan.rate.sd ~ quorum.size, col=fr.color[x], lty=2, data=d)
                   lines(block.orphan.rate.mean + block.orphan.rate.sd ~ quorum.size, col=fr.color[x], lty=2, data=d)
     })
axis(side=1, at=unique(fr$quorum.size))
fr.title(fr)
legend('bottomleft', title='latency model', legend = levels(fr$delta.dist),
       col=fr.color, pch=1)

plot(vote.orphan.rate ~ quorum.size,
     data=fr,
     log='x', xaxt='n',
     col=fr.color.3[delta.dist],
     ylim=c(0,0.5),
     ylab='vote orphan rate',
     xlab='quorum size')
ignore <- lapply(unique(fr$delta.dist), function (x) {
                   d <- subset(runs.agg, tag=="fixed-rate" & delta.dist==x)
                   lines(vote.orphan.rate.mean ~ quorum.size, col=fr.color[x], data=d)
                   lines(vote.orphan.rate.mean - vote.orphan.rate.sd ~ quorum.size, col=fr.color[x], lty=2, data=d)
                   lines(vote.orphan.rate.mean + vote.orphan.rate.sd ~ quorum.size, col=fr.color[x], lty=2, data=d)
     })
axis(side=1, at=unique(fr$quorum.size))
fr.title(fr)
legend('bottomleft', title='latency model', legend = levels(fr$delta.dist),
       col=fr.color, pch=1)

ddist= "uniform"
fr.tikz <- subset(runs.agg, delta.dist == ddist & tag=="fixed-rate")
tikz((cbind(mean = block.orphan.rate.mean,
            lower = block.orphan.rate.mean - block.orphan.rate.sd,
            upper = block.orphan.rate.mean + block.orphan.rate.sd) ~ cbind(qsize = quorum.size)),
     name="block-orphans-over-qsize-fast", data=fr.tikz,
     meta=list(deltaDist = ddist,
               nNodes = unique(fr.tikz$n.nodes),
               nBlocks = unique(fr.tikz$n.blocks),
               nIterations = unique(fr.tikz$n.iterations)
               ))
print(fr.tikz)
tikz((cbind(mean = vote.orphan.rate.mean,
            lower = vote.orphan.rate.mean - vote.orphan.rate.sd,
            upper = vote.orphan.rate.mean + vote.orphan.rate.sd) ~ cbind(qsize = quorum.size)),
     name="vote-orphans-over-qsize-fast", data=fr.tikz,
     meta=list(deltaDist = ddist))

# fixed-quorum experiment
#########################
# delta: expected propagation time 1, both votes and blocks
# keep quorum size fixed, change pow rate
# we look for orphan votes and blocks

fq.title <- function(d) {
  with(d,
       title(main=sprintf("tag: %s    delta: %.1f (%s)    nodes: %i    blocks: %i",
                          unique(tag), unique(delta.vote),
                          unique(delta.dist), unique(n.nodes), unique(n.blocks)
                          )))
}

fq <- subset(runs, tag=="fixed-quorum" & delta.dist=="uniform")
fq$quorum.size <- as.factor(fq$quorum.size)

fq.color   <- colorspace::rainbow_hcl(length(levels(fq$quorum.size)))
fq.color.3 <- colorspace::rainbow_hcl(length(levels(fq$quorum.size)), alpha=0.3)
plot(block.orphan.rate ~ pow.scale,
     data=fq,
     log='xy', xaxt='n',
     col=fq.color.3[quorum.size],
     ylim=c(10^-4,1),
     ylab='block orphan rate',
     xlab='expected puzzle solving time')
ignore <- lapply(unique(fq$quorum.size), function (x) {
                   d <- subset(runs.agg, tag=="fixed-quorum" & delta.dist=="uniform" & quorum.size==x)
                   lines(block.orphan.rate.mean ~ pow.scale, col=fq.color[x], data=d)
                   lines(block.orphan.rate.mean - block.orphan.rate.sd ~ pow.scale, col=fq.color[x], lty=2, data=d)
                   lines(block.orphan.rate.mean + block.orphan.rate.sd ~ pow.scale, col=fq.color[x], lty=2, data=d)
     })
axis(side=1, at=unique(fq$pow.scale))
fq.title(fq)
legend('bottomleft', title='quorum size', legend = levels(fq$quorum.size),
       col=fq.color, pch=1)

plot(vote.orphan.rate ~ pow.scale,
     data=fq,
     log='xy', xaxt='n',
     col=fq.color.3[quorum.size],
     ylim=c(10^-4,1),
     ylab='vote orphan rate',
     xlab='expected puzzle solving time')
ignore <- lapply(unique(fq$quorum.size), function (x) {
                   d <- subset(runs.agg, tag=="fixed-quorum" & delta.dist=="uniform" & quorum.size==x)
                   lines(vote.orphan.rate.mean ~ pow.scale, col=fq.color[x], data=d)
                   lines(vote.orphan.rate.mean - vote.orphan.rate.sd ~ pow.scale, col=fq.color[x], lty=2, data=d)
                   lines(vote.orphan.rate.mean + vote.orphan.rate.sd ~ pow.scale, col=fq.color[x], lty=2, data=d)
     })
axis(side=1, at=unique(fq$pow.scale))
fq.title(fq)
legend('bottomleft', title='quorum size', legend = levels(fq$quorum.size),
       col=fq.color, pch=1)

# empirical block interval
##########################

block.file <- function(id, iteration) paste0("blocks-",id,"-",iteration,".csv")

# assume fixed number of blocks
read.block.files <- function(ids, iterations) {
  # preallocate space
  blocks <- read.csv(block.file(ids[1], iterations[1]))
  n_blocks <- max(blocks$height)
  n_files <- length(iterations) * length(ids)
  n_rows <- n_files * n_blocks
  id <- character(n_rows)
  iteration <- numeric(n_rows)
  interval <- numeric(n_rows)
  height <- numeric(n_rows)
  published.at <- numeric(n_rows)
  offsets <- matrix(0:(n_files - 1) * n_blocks, nrow=length(ids), ncol=length(iterations))
  for (i in 1:length(ids)) {
    for (j in 1:length(iterations)) {
      blocks <- read.csv(block.file(ids[i], iterations[j]))
      o <- order(blocks$published.at)[as.logical(blocks$confirmed)]
      k <- 1:n_blocks + offsets[i,j]
      stopifnot(length(k) == n_blocks)
      id[k] = ids[i]
      iteration[k] = iterations[j]
      interval[k] = diff(c(0,blocks$published.at[o]))
      published.at[k] = blocks$published.at[o]
      height[k] = blocks$height[o]
    }
  }
  data.frame(id, iteration, interval, published.at, height)
}

ebi <- function(q, s, ..., latency.model="uniform") {
  ebi.runs <- subset(runs, delta.dist==latency.model & quorum.size == q & pow.scale == s & tag == "fixed-quorum")
  ebi.id <- unique(ebi.runs$id)
  stopifnot(length(ebi.id)==1)
  ebi.delta <- unique(c(ebi.runs$delta.vote, ebi.runs$delta.vote))
  stopifnot(length(ebi.delta)==1)
  ebi.data <- read.block.files(ebi.id, unique(ebi.runs$iteration))
  ebi.intervals <- ebi.data$interval
  ebi.dinterval <- density(ebi.intervals, from=min(ebi.intervals))
  plot(ebi.dinterval$x,
       dgamma(ebi.dinterval$x, shape=q, scale=s),
       col=2, type='l',
       xlab="block interval",
       ylab="estimated probability density")
  lines(ebi.dinterval, col=1)
  abline(v=q * s, col=2, lty=2)
  abline(v=mean(ebi.intervals), col=1, lty=2)
  title(main=sprintf("quorum size: %i    log2(pow scale): %i    delta: %.1f (%s)", q, log2(s), ebi.delta, latency.model),
        xlab="block interval")
  legend("topright", c("observed", "theoretical (delta: 0)"), col=1:2, lty=1)
}

# ebi(8, 2^-3)
# ebi(8, 2^-2)
# ebi(8, 2^-1)
# ebi(8, 2^0)
# ebi(8, 2^1)
# ebi(8, 2^2)
# ebi(8, 2^3)
# ebi(8, 2^4)
# ebi(8, 2^5)
# ebi(8, 2^6)
# ebi(8, 2^7)
