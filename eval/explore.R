# preamble
##########

setwd("../output")
rm(list = ls())

if (! dir.exists("../eval/tex")) {
  dir.create("../eval/tex", recursive=T)
}
do.call(file.remove, list(list.files("../eval/tex/", full.names = TRUE)))

pgf <- function(name, data, meta) {
  write.table(data,
              sprintf("../eval/tex/%s.csv", name),
              quote=F, sep=",", row.names=F)
  dict <- meta
  dict$name <- name
  writeLines(paste0("\\def\\", names(dict), "{", dict, "}"),
             con=sprintf("../eval/tex/%s.tex", name))
}

# load data
###########

runs <- read.csv('runs.csv')
str(runs)

runs$delta.dist <- as.factor(runs$delta.dist)
runs$block.orphan.rate <- with(runs, (blocks.observed - blocks.confirmed) / blocks.observed)
runs$vote.orphan.rate <- with(runs, (votes.observed - votes.confirmed) / votes.observed)

# aggregate iterations
runs.agg <- aggregate(cbind(vote.orphan.rate, block.orphan.rate, mean.interval) ~
                      tag + pow.scale + quorum.size + delta.block + delta.vote + delta.dist + n.nodes + n.blocks + confirmations,
                      runs,
                      function (x) c("mean"=mean(x), "sd"=sd(x)))
runs.agg <- do.call("data.frame", runs.agg)
runs.agg$n.iterations <- nrow(runs) / nrow(runs.agg)

# list of experiments
unique(runs$tag)

uniq.or.fail <- function (v) {
  uv <- unique(v)
  return (ifelse(length(uv) == 1, uv, "\\NotUnique"))
}
pgf("all",
    runs.agg,
    with(runs.agg,
         list(nNodes = uniq.or.fail(n.nodes),
              nBlocks = uniq.or.fail(n.blocks),
              nConfirmations = uniq.or.fail(confirmations),
              nIterations = uniq.or.fail(n.iterations))))

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


with(subset(runs.agg, delta.dist == "uniform" & tag=="fixed-rate"),
     pgf("orphans-qsize-fast-uniform",
         cbind(quorum.size,
               log2.quorum.size = log2(quorum.size),
               block.mean = block.orphan.rate.mean,
               block.low =  block.orphan.rate.mean - 1.96 * block.orphan.rate.sd,
               block.high = block.orphan.rate.mean + 1.96 * block.orphan.rate.sd,
               vote.mean = vote.orphan.rate.mean,
               vote.low =  vote.orphan.rate.mean - 1.96 * vote.orphan.rate.sd,
               vote.high = vote.orphan.rate.mean + 1.96 * vote.orphan.rate.sd),
         list(deltaDist = "uniform",
              nNodes = unique(n.nodes),
              nBlocks = unique(n.blocks),
              nIterations = unique(n.iterations))))

with(subset(runs.agg, delta.dist == "exponential" & tag=="fixed-rate"),
     pgf("orphans-qsize-fast-exponential",
         cbind(quorum.size,
               log2.quorum.size = log2(quorum.size),
               block.mean = block.orphan.rate.mean,
               block.low =  block.orphan.rate.mean - 1.96 * block.orphan.rate.sd,
               block.high = block.orphan.rate.mean + 1.96 * block.orphan.rate.sd,
               vote.mean = vote.orphan.rate.mean,
               vote.low =  vote.orphan.rate.mean - 1.96 * vote.orphan.rate.sd,
               vote.high = vote.orphan.rate.mean + 1.96 * vote.orphan.rate.sd),
         list(deltaDist = "exponential",
              nNodes = unique(n.nodes),
              nBlocks = unique(n.blocks),
              nIterations = unique(n.iterations))))

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

for (d in unique(runs.agg$delta.dist)) {
  with(subset(runs.agg, delta.dist == d & tag=="fixed-quorum"),
       pgf(sprintf("orphans-scale-%s", unique(delta.dist)),
           reshape(data.frame(pow.scale,
                              log2.pow.scale=log2(pow.scale),
                              quorum.size,
                              block.mean = block.orphan.rate.mean,
                              block.low =  block.orphan.rate.mean - 1.96 * block.orphan.rate.sd,
                              block.high = block.orphan.rate.mean + 1.96 * block.orphan.rate.sd,
                              vote.mean = vote.orphan.rate.mean,
                              vote.low =  vote.orphan.rate.mean - 1.96 * vote.orphan.rate.sd,
                              vote.high = vote.orphan.rate.mean + 1.96 * vote.orphan.rate.sd),
                   idvar=c("pow.scale", "log2.pow.scale"),
                   timevar="quorum.size",
                   direction="wide"),
           list(deltaDist = as.character(unique(delta.dist)),
                qSizes = paste0(sort(unique(quorum.size)), collapse=","),
                nNodes = unique(n.nodes),
                nBlocks = unique(n.blocks),
                nIterations = unique(n.iterations))))
}

# target-orphan-rate
####################
# set target vote orphan rate
# x: quorum size
# y: minimal viable block interval

tor.palette <- colorspace::diverging_hcl(10, palette="Tofino")
tor.color <- function(or) {
  mapped <- or / max(or)
  tor.palette[floor((mapped * 9) + 1)]
}
tor.palette
tor.color(0:2)

tor <- function(or, ..., latency.model="uniform") {
  tor.df <- subset(runs.agg, vote.orphan.rate.mean <= or & delta.dist==latency.model & tag=="max-orphan-rate")
  tor.qs <- unique(runs.agg$quorum.size)
  tor.bi <- sapply(tor.qs, function(x) {
                     min(subset(tor.df, quorum.size == x)$mean.interval.mean)
       })
  plot(tor.qs, tor.bi, log='x', xaxt='n',
       main=sprintf("max orphan rate: %.3f    ", or),
       xlab="quorum size", ylab="block interval")
  axis(side=1, at=tor.qs)
  cbind(quorum.size = tor.qs, block.interval=tor.bi)
}
tor(0.1)
tor(0.01)
tor(0.015)

# plot all combinations, put orphan rate as color
all.df <- subset(runs.agg, delta.dist=="uniform" & tag=="max-orphan-rate")
plot(mean.interval.mean ~ quorum.size, data=all.df, ylim=c(100, 300), col=tor.color(all.df$vote.orphan.rate.mean), xaxt='n', log='x')
axis(side=1, at=unique(all.df$quorum.size))

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
