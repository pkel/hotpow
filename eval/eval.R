# preamble
##########

setwd("../output")
rm(list = ls())

if (! dir.exists("../eval/plots")) {
  dir.create("../eval/plots", recursive=T)
}
ignore <- (do.call(file.remove, list(list.files("../eval/plots/", full.names = TRUE))))

pgf <- function(name, data, meta) {
  write.table(data,
              sprintf("../eval/plots/%s.csv", name),
              quote=F, sep=",", row.names=F)
  dict <- meta
  dict$name <- name
  writeLines(paste0("\\def\\", names(dict), "{", dict, "}"),
             con=sprintf("../eval/plots/%s.tex", name))
}

pgf.csv <- function(data, fname) {
  write.table(data, paste0("../eval/plots/", fname),
              sep=',', row.names=F, quote=F, na='nan')
}

# load data
###########

runs <- read.csv('runs.csv')
if(interactive()) str(runs)

runs$messages.sent.rel <- with(runs, messages.sent / quorum.size / (blocks.confirmed + confirmations))
runs$delta.dist <- as.factor(runs$delta.dist)
runs$block.orphan.rate <- with(runs, (blocks.observed - blocks.confirmed) / blocks.observed)
runs$vote.orphan.rate <- with(runs, (votes.observed - votes.confirmed) / votes.observed)
runs$attacker.share.blocks <- with(runs, attacker.blocks.confirmed / blocks.confirmed)
runs$attacker.share.votes <- with(runs, attacker.votes.confirmed / votes.confirmed)

# aggregate iterations
runs.agg <- aggregate(cbind(mean.interval, attacker.share.blocks, attacker.share.votes, vote.orphan.rate, block.orphan.rate, messages.sent, messages.sent.rel) ~
                      tag + pow.scale + quorum.size + delta.block + delta.vote + delta.dist + n.nodes + n.blocks + confirmations + churn + leader.failure.rate + alpha + strategy,
                      runs,
                      function (x) c("mean"=mean(x), "sd"=sd(x)))
runs.agg <- do.call("data.frame", runs.agg)
runs.agg$n.iterations <- nrow(runs) / nrow(runs.agg)

# list of experiments
tags <- sort(unique(runs$tag), decreasing=T)

uniq.or.fail <- function (v) {
  uv <- unique(v)
  return (ifelse(length(uv) == 1, uv, "\\NotUnique"))
}
pgf("all",
    runs.agg,
    with(runs.agg,
         list(nNodes = uniq.or.fail(median(n.nodes)),
              nBlocks = uniq.or.fail(n.blocks),
              nConfirmations = uniq.or.fail(confirmations),
              nIterations = uniq.or.fail(n.iterations))))

# block interval distribution ( basic orphan rate stats )
#########################################################
# target block interval 600 second, δ=2
# two(three) scenarios: (nc-fast), nc-slow, k=51/proposed
# two networks: uniform, exponential

block.file <- function(id, iteration) paste0("blocks-",id,"-",iteration,".csv")

read.block.files <- function(ids, iterations) {
  # preallocate space
  blocks <- read.csv(block.file(ids[1], iterations[1]))
  # assume constant number of blocks
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

bi.tags <- tags[startsWith(tags, "simple") | startsWith(tags, "realistic")]
#
# summarize all configs in a table
bi.row.of.tag <- function(tag) {
  s <- strsplit(tag, "-")[[1]]
  net.lat <- s[1]
  net.dis <- s[2]
  cfg <- paste0(tail(s, -2), collapse="-")
  bi.runs <- runs[runs$tag == tag, ]
  bi.data <- read.block.files(unique(bi.runs$id), unique(bi.runs$iteration))
  intervals <- bi.data$interval
  q <- quantile(intervals, c(0.01, 0.05, 0.5, 0.95, 0.99),
                names=F)
  data.frame(net.dis=net.dis, net.lat=net.lat, cfg=cfg,
             mean=mean(intervals), sd=sd(intervals),
             q1= q[1], q5= q[2], q50=q[3], q95=q[4], q99=q[5],
             block.orphan.rate=mean(bi.runs$block.orphan.rate),
             vote.orphan.rate=mean(bi.runs$vote.orphan.rate))
}
bi.stats.long <- data.frame(do.call(rbind, lapply(bi.tags, bi.row.of.tag)))
bi.stats.wide <- reshape(subset(bi.stats.long, cfg %in% c('proposed','nc-slow')),
                         idvar=c("net.lat", "net.dis"),
                         timevar=c("cfg"),
                         direction="wide")
colnames(bi.stats.wide) <- sub("nc-slow", "btc", colnames(bi.stats.wide))
bi.rows <- with(bi.stats.wide,
                paste(sprintf("\\textbf{%s}", net.lat),
                      sprintf("\\textbf{%s}", net.dis),
                      sprintf("%.1f", round(mean.proposed, 1)),
                      sprintf("%.1f", round(mean.btc     , 1)),
                      sprintf("%.1f", round(sd.proposed  , 1)),
                      sprintf("%.1f", round(sd.btc       , 1)),
                      sprintf("%.1f", round(q50.proposed , 1)),
                      sprintf("%.1f", round(q50.btc      , 1)),
                      sprintf("%.1f", round(q95.proposed , 1)),
                      sprintf("%.1f", round(q95.btc      , 1)),
                      sprintf("%.1f", round(q99.proposed , 1)),
                      sprintf("%.1f", round(q99.btc      , 1)),
                      sep = " & "))
bi.lns <- c(paste0("\\begin{tabular}{ll",
                     "S[table-format=3.1]", # mean
                     "S[table-format=3.1]",
                     "S[table-format=3.1]", # sd
                     "S[table-format=3.1]",
                     "S[table-format=3.1]", # q50
                     "S[table-format=3.1]",
                     "S[table-format=3.1]", # q95
                     "S[table-format=4.1]",
                     "S[table-format=3.1]", # q99
                     "S[table-format=4.1]",
                     "}"),
            "\\toprule",
            paste(sapply(c("network", "mean", "std.\\,dev.", "50\\,\\%", "95\\,\\%", "99\\,\\%"),
                         function (x) sprintf("\\multicolumn{2}{c}{%s}", x)),
                  collapse=" & "),
            "\\\\",
            sapply(1:6, function (i) sprintf("\\cmidrule(lr){%i-%i}", i * 2 - 1, i * 2)),
            paste(c("{latency}", "{distribution}", rep(c("{\\proposed}", "{\\btc}"), 5)), collapse= " & "),
            "\\\\",
            "\\midrule",
            paste(bi.rows, "\\\\"),
            "\\bottomrule",
            "\\end{tabular}")
if(interactive()) {
  writeLines(bi.lns)
} else {
  fname <- "tab-block-interval.tex"
  print(fname)
  writeLines(bi.lns, paste0("../eval/plots/", fname))
}

# plot distribution
ebi <- function(t, ...) {
  ebi.runs <- subset(runs, tag == t)
  s <- unique(ebi.runs$pow.scale)
  q <- unique(ebi.runs$quorum.size)
  l <- unique(ebi.runs$delta.dist)
  stopifnot(length(s) == 1)
  stopifnot(length(q) == 1)
  stopifnot(length(l) == 1)
  ebi.id <- unique(ebi.runs$id)
  stopifnot(length(ebi.id)==1)
  ebi.delta <- unique(ebi.runs$delta.vote)
  stopifnot(length(ebi.delta)==1)
  ebi.data <- read.block.files(ebi.id, unique(ebi.runs$iteration))
  ebi.intervals <- ebi.data$interval
  ebi.dinterval <- density(ebi.intervals, from=min(ebi.intervals))
  r <- hist(ebi.intervals, probability=T, main="", xlab="", ylab="", las=1, ...)
  lines(ebi.dinterval$x,
        dgamma(ebi.dinterval$x, shape=q, scale=s),
        col=2, type='l')
  # lines(ebi.dinterval, col=1)
  abline(v=q * s, col=2, lty=2)
  abline(v=mean(ebi.intervals), col=1, lty=2)
  title(main=t,
        xlab="block interval")
  legend("topright",
         c("Gamma distribution", "observed mean", "target interval"),
         col=c(2,1,2), lty=c(1,2,2))
  invisible(r)
}
#
if (interactive()) {
  ebi("simple-uniform-proposed")
  ebi("simple-uniform-nc-slow")
  ebi("simple-uniform-proposed", breaks=seq(0,1800,50), ylim=c(0, 0.005))
  ebi("simple-uniform-nc-slow", breaks=seq(0,9999,50), xlim=c(0, 1800), ylim=c(0, 0.005))
} else {
  fname <- paste0("block-interval-simple-uniform-proposed.pdf")
  print(fname)
  cairo_pdf(paste0("../eval/plots/", fname), width=7, height=4)
  ebi("simple-uniform-proposed", breaks=seq(0,1800,50), ylim=c(0, 0.005))
  #
  fname <- paste0("block-interval-simple-uniform-nc-slow.pdf")
  print(fname)
  cairo_pdf(paste0("../eval/plots/", fname), width=7, height=4)
  ebi("simple-uniform-nc-slow", breaks=seq(0,9999,50), xlim=c(0, 1800), ylim=c(0, 0.005))
  #
  fname <- paste0("block-interval-simple-exponential-proposed.pdf")
  print(fname)
  cairo_pdf(paste0("../eval/plots/", fname), width=7, height=4)
  ebi("simple-exponential-proposed", breaks=seq(0,1800,50), ylim=c(0, 0.005))
  #
  fname <- paste0("block-interval-simple-exponential-nc-slow.pdf")
  print(fname)
  cairo_pdf(paste0("../eval/plots/", fname), width=7, height=4)
  ebi("simple-exponential-nc-slow", breaks=seq(0,9999,50), xlim=c(0, 1800), ylim=c(0, 0.005))
  invisible(dev.off())
}

hist.df <- function(t) {
  d <- subset(runs, tag==t)
  intervals <- read.block.files(unique(d$id), unique(d$iteration))$interval
  with(hist(intervals, breaks=seq(0,10000, 50), plot=F),
       data.frame(interval.left=breaks,
                  density=c(density, 0),
                  counts=c(counts, 0),
                  tag=t)[0:(1500/50 + 1), ])
}
bi.hist.long <- do.call(rbind, lapply(c("simple-uniform-proposed",
                                        "simple-uniform-nc-slow",
                                        "simple-exponential-proposed",
                                        "simple-exponential-nc-slow",
                                        "realistic-uniform-proposed",
                                        "realistic-uniform-nc-slow",
                                        "realistic-exponential-proposed",
                                        "realistic-exponential-nc-slow"
                                        ),
                                      hist.df))
bi.hist.wide <- reshape(bi.hist.long, direction="wide",
                        idvar="interval.left", timevar="tag")
pgf.csv(bi.hist.wide, "block-interval-hist.csv")

bi.ref.x <- seq(0,1500, 1500/200)
bi.ref.wide <- data.frame(interval = bi.ref.x,
                          exponential = dexp(bi.ref.x, rate=1/600),
                          gamma51 = dgamma(bi.ref.x, rate=51/600, shape=51))
pgf.csv(bi.ref.wide, "block-interval-ref.csv")


# orphan rate as a function of network latency
##############################################
# target block interval 600 seconds
# two networks: uniform, exponential, simple latency model
# three scenarios: nc-fast, nc-slow, k=51/proposed
# x-axis: δ = 1/4 ... 16
# y-axis: orphan rate blocks + votes for k>1

or.tags <- tags[startsWith(tags, "latency-simple")]
or.df.of.tag <- function(tag) {
  d <- runs.agg[runs.agg$tag == tag, ]
  s <- strsplit(sub("latency-simple-", "", tag), "-")[[1]]
  d$net <- s[1]
  d$cfg <- paste0(tail(s, -1), collapse="-")
  return(d)
}
or <- data.frame(do.call(rbind, lapply(or.tags, or.df.of.tag)))
or$net <- as.factor(or$net)
or$cfg <- as.factor(or$cfg)
if(interactive()){
  str(or)
}

or.color   <- colorspace::rainbow_hcl(length(levels(or$cfg)))
or.color.3 <- colorspace::rainbow_hcl(length(levels(or$cfg)), alpha=0.3)

or.plot.net <- function(net) {
  ss <- or[or$net==net, ]
  plot(1, 1,
       log='xy', xaxt='n', type='n', las=2,
       ylim=range(with(or,c(block.orphan.rate.mean, vote.orphan.rate.mean))),
       xlim=range(ss$delta.block),
       ylab='',
       xlab='δ')
  lapply(unique(ss$cfg), function (x) {
           d <- subset(ss, cfg==x)
             polygon(c(rev(d$delta.block), d$delta.block),
                     c(rev(d$block.orphan.rate.mean + d$block.orphan.rate.sd),
                       pmax(10e-16,d$block.orphan.rate.mean - d$block.orphan.rate.sd)),
                     col = or.color.3[x], border = NA)
           lines(block.orphan.rate.mean ~ delta.block, col=or.color[x], data=d)
           if (unique(d$quorum.size) > 1) {
             polygon(c(rev(d$delta.block), d$delta.block),
                     c(rev(d$vote.orphan.rate.mean + d$vote.orphan.rate.sd),
                       d$vote.orphan.rate.mean - d$vote.orphan.rate.sd),
                     col = or.color.3[x], border = NA)
             lines(vote.orphan.rate.mean ~ delta.block, col=or.color[x], lty=2, data=d)
           }
       })
  axis(side=1, at=unique(ss$delta.block))
  with(ss,
       title(main=sprintf("orphan rate\nsimple/%s    nodes: %i    blocks: %i    iterations: %g",
                          unique(net), unique(n.nodes), unique(n.blocks), unique(n.iterations))))
  legend('bottomright', title='configuration', legend = levels(or$cfg),
         col=or.color, pch=15)
  legend('topleft', legend = c("orphaned blocks", "orphaned votes"), lty=1:2)
}
#
if(interactive()) {
  # or.plot.net('uniform')
  or.plot.net('exponential')
} else {
  fname <- paste0("latency-orphan-rate-simple-exponential",".pdf")
  print(fname)
  cairo_pdf(paste0("../eval/plots/", fname), width=7, height=5)
  or.plot.net('exponential')
  invisible(dev.off())
  #
  # fname <- paste0("latency-orphan-rate-simple-uniform",".pdf")
  # print(fname)
  # cairo_pdf(paste0("../eval/plots/", fname), width=7, height=5)
  # or.plot.net('uniform')
  # invisible(dev.off())
}

# block interval as a function of network latency
#################################################
# target block interval 600 seconds
# two networks: uniform, exponential, simple latency model
# two scenarios: nc-slow, k=51/proposed
# x-axis: δ = 1/4 ... 16
# y-axis: block interval

lat.plot.net <- function(net) {
  ss <- or[or$net==net & as.character(or$cfg) %in% c('proposed', 'nc-slow'), ]
  plot(1, 1,
       log='x', xaxt='n', type='n', las=2,
       ylim=range(ss$mean.interval.mean),
       xlim=range(ss$delta.block),
       ylab='',
       xlab='δ')
  lapply(unique(ss$cfg), function (x) {
           d <- subset(ss, cfg==x)
             polygon(c(rev(d$delta.block), d$delta.block),
                     c(rev(d$mean.interval.mean + d$mean.interval.sd),
                       d$mean.interval.mean - d$mean.interval.sd),
                     col = or.color.3[x], border = NA)
           lines(mean.interval.mean ~ delta.block, col=or.color[x], data=d)
       })
  axis(side=1, at=unique(ss$delta.block))
  with(ss,
       title(main=sprintf("block interval\nsimple/%s    nodes: %i    blocks: %i    iterations: %g",
                          unique(net), unique(n.nodes), unique(n.blocks), unique(n.iterations))))
  legend('bottomright', title='configuration', legend = levels(or$cfg)[unique(ss$cfg)],
         col=or.color[unique(ss$cfg)], pch=15)
}
#
if(interactive()) {
  # lat.plot.net('uniform')
  lat.plot.net('exponential')
} else {
  fname <- paste0("latency-block-interval-simple-exponential",".pdf")
  print(fname)
  cairo_pdf(paste0("../eval/plots/", fname), width=7, height=5)
  lat.plot.net('exponential')
  invisible(dev.off())
  #
  # fname <- paste0("latency-block-interval-simple-uniform",".pdf")
  # print(fname)
  # cairo_pdf(paste0("../eval/plots/", fname), width=7, height=5)
  # lat.plot.net('uniform')
  # invisible(dev.off())
}

lat <- with(or, data.frame(latency = delta.block, # x-axis
                           mean.interval = mean.interval.mean, # y-axis
                           mean.interval.low = mean.interval.mean - 1.96 * mean.interval.sd,
                           mean.interval.high = mean.interval.mean + 1.96 * mean.interval.sd,
                           distribution = net,
                           protocol = cfg)
)
lat.wide <- reshape(lat, direction="wide", idvar=c("latency", "distribution"), timevar="protocol")
lat.wide <- reshape(lat.wide, direction="wide", idvar="latency", timevar="distribution")
pgf.csv(lat.wide, "latency.csv")

# block interval as a function of network size
##############################################
# target block interval 600 seconds
# two networks: uniform, exponential, simple latency model
# two scenarios: nc-slow, k=51/proposed
# x-axis: number of nodes
# y-axis: block interval

size.tags <- tags[startsWith(tags, "nodes-realistic")]
size.df.of.tag <- function(tag) {
  d <- runs.agg[runs.agg$tag == tag, ]
  s <- strsplit(sub("nodes-realistic-", "", tag), "-")[[1]]
  d$net <- s[1]
  d$cfg <- paste0(tail(s, -1), collapse="-")
  return(d)
}
size <- data.frame(do.call(rbind, lapply(size.tags, size.df.of.tag)))
size$net <- as.factor(size$net)
size$cfg <- as.factor(size$cfg)
if(interactive()){
  str(size)
}

size.color   <- colorspace::rainbow_hcl(length(levels(size$cfg)))
size.color.3 <- colorspace::rainbow_hcl(length(levels(size$cfg)), alpha=0.3)

size.plot.net <- function(net) {
  ss <- size[size$net==net & as.character(size$cfg) %in% c('proposed', 'nc-slow'), ]
  plot(1, 1,
       log='x', xaxt='n', type='n', las=2,
       ylim=range(ss$messages.sent.rel.mean),
       xlim=range(ss$n.nodes),
       ylab='',
       xlab='number of nodes')
  lapply(unique(ss$cfg), function (x) {
           d <- subset(ss, cfg==x)
             polygon(c(rev(d$n.nodes), d$n.nodes),
                     c(rev(d$messages.sent.rel.mean + d$messages.sent.rel.sd),
                       d$messages.sent.rel.mean - d$messages.sent.rel.sd),
                     col = or.color.3[x], border = NA)
           lines(messages.sent.rel.mean ~ n.nodes, col=or.color[x], data=d)
       })
  axis(side=1, at=unique(ss$n.nodes))
  with(ss,
       title(main=sprintf("messages sent\nrealistic/%s    blocks: %i    iterations: %g",
                          unique(net) , unique(n.blocks), unique(n.iterations))))
  legend('bottomright', title='configuration', legend = levels(or$cfg)[unique(ss$cfg)],
         col=or.color[unique(ss$cfg)], pch=15)
}
#
if(interactive()) {
  # size.plot.net('uniform')
  size.plot.net('exponential')
} else {
  fname <- paste0("size-block-interval-realistic-exponential",".pdf")
  print(fname)
  cairo_pdf(paste0("../eval/plots/", fname), width=7, height=5)
  size.plot.net('exponential')
  invisible(dev.off())
  #
  # fname <- paste0("size-block-interval-realistic-uniform",".pdf")
  # print(fname)
  # cairo_pdf(paste0("../eval/plots/", fname), width=7, height=5)
  # lat.plot.net('uniform')
  # invisible(dev.off())
}

size2 <- with(size, data.frame(size = n.nodes, # x-axis
                               mean.interval = mean.interval.mean, # y-axis
                               mean.interval.low = mean.interval.mean - 1.96 * mean.interval.sd,
                               mean.interval.high = mean.interval.mean + 1.96 * mean.interval.sd,
                               messages = messages.sent.mean,
                               messages.low = messages.sent.mean - 1.96 * messages.sent.sd,
                               messages.high = messages.sent.mean + 1.96 * messages.sent.sd,
                               messages.rel = messages.sent.rel.mean,
                               messages.rel.low = messages.sent.rel.mean - 1.96 * messages.sent.rel.sd,
                               messages.rel.high = messages.sent.rel.mean + 1.96 * messages.sent.rel.sd,
                               distribution = net, # group
                               protocol = cfg)
)
size.wide <- reshape(size2, direction="wide", idvar=c("size", "distribution"), timevar="protocol")
size.wide <- reshape(size.wide, direction="wide", idvar="size", timevar="distribution")
pgf.csv(size.wide, "size.csv")

# block interval as a function of churn
#######################################
# target block interval 600 seconds
# two networks: uniform, exponential, "realistic" latency
# two scenarios: nc-slow, k=51/proposed
# x-axis: churn = 0 ... 1/2
# y-axis: block interval

churn.tags <- tags[startsWith(tags, "churn-realistic")]
churn.df.of.tag <- function(tag) {
  d <- runs.agg[runs.agg$tag == tag, ]
  s <- strsplit(sub("^churn-realistic-", "", tag), "-")[[1]]
  d$net <- s[1]
  d$cfg <- paste0(tail(s, -1), collapse="-")
  return(d)
}
churn <- data.frame(do.call(rbind, lapply(churn.tags, churn.df.of.tag)))
churn$net <- as.factor(churn$net)
churn$cfg <- as.factor(churn$cfg)
if(interactive()){
  str(churn)
}

churn.color   <- colorspace::rainbow_hcl(length(levels(churn$cfg)))
churn.color.3 <- colorspace::rainbow_hcl(length(levels(churn$cfg)), alpha=0.3)

churn.plot.net <- function(net) {
  ss <- churn[churn$net==net & as.character(churn$cfg) %in% c('proposed', 'nc-slow'), ]
  plot(1, 1,
       xaxt='n', type='n', las=2,
       ylim=range(c(600, ss$mean.interval.mean, 1200)),
       xlim=range(ss$churn),
       ylab='',
       xlab='churn ratio')
  lapply(unique(ss$cfg), function (x) {
           d <- subset(ss, cfg==x)
           polygon(c(rev(d$churn), d$churn),
                   c(rev(d$mean.interval.mean + d$mean.interval.sd),
                     pmax(10e-16,d$mean.interval.mean - d$mean.interval.sd)),
                   col = churn.color.3[x], border = NA)
           lines(mean.interval.mean ~ churn, col=churn.color[x], data=d)
       })
  axis(side=1, at=unique(ss$churn))
  with(ss,
       title(main=sprintf("block interval\nrealistic/%s    nodes: %i    blocks: %i    iterations: %g",
                          unique(net), unique(n.nodes), unique(n.blocks), unique(n.iterations))))
  legend('bottomright', title='configuration', legend = levels(churn$cfg)[unique(ss$cfg)],
         col=churn.color[unique(ss$cfg)], pch=15)
}
#
if(interactive()) {
  # churn.plot.net('uniform')
  churn.plot.net('exponential')
} else {
  fname <- "churn-block-interval-realistic-exponential.pdf"
  print(fname)
  cairo_pdf(paste0("../eval/plots/", fname), width=7, height=5)
  churn.plot.net('exponential')
  invisible(dev.off())
  #
  # fname <- "churn-block-interval-realistic-uniform.pdf"
  # print(fname)
  # cairo_pdf(paste0("../eval/plots/", fname), width=7, height=5)
  # churn.plot.net('uniform')
  # invisible(dev.off())
}

churn2 <- with(churn, data.frame(churn = churn, # x-axis
                                 mean.interval = mean.interval.mean, # y-axis
                                 mean.interval.low = mean.interval.mean - 1.96 * mean.interval.sd,
                                 mean.interval.high = mean.interval.mean + 1.96 * mean.interval.sd,
                                 distribution = net,
                                 protocol = cfg)
)
churn.wide <- reshape(churn2, direction="wide", idvar=c("churn", "distribution"), timevar="protocol")
churn.wide <- reshape(churn.wide, direction="wide", idvar="churn", timevar="distribution")
pgf.csv(churn.wide, "churn.csv")

# block interval as a function of leader failure
################################################
# target block interval 600 seconds
# two networks: uniform, exponential, "realistic" latency
# two scenarios: nc-slow, k=51/proposed
# x-axis: failure rate = 0 ... 1/2
# y-axis: block interval

lf.tags <- tags[startsWith(tags, "failure-realistic")]
lf.df.of.tag <- function(tag) {
  d <- runs.agg[runs.agg$tag == tag, ]
  s <- strsplit(sub("^failure-realistic-", "", tag), "-")[[1]]
  d$net <- s[1]
  d$cfg <- paste0(tail(s, -1), collapse="-")
  return(d)
}
lf <- data.frame(do.call(rbind, lapply(lf.tags, lf.df.of.tag)))
lf$net <- as.factor(lf$net)
lf$cfg <- as.factor(lf$cfg)
if(interactive()){
  str(lf)
}

lf.color   <- colorspace::rainbow_hcl(length(levels(lf$cfg)))
lf.color.3 <- colorspace::rainbow_hcl(length(levels(lf$cfg)), alpha=0.3)

lf.plot.net <- function(net) {
  ss <- lf[lf$net==net & as.character(lf$cfg) == 'proposed', ]
  plot(1, 1,
       xaxt='n', type='n', las=2,
       ylim=range(c(600, ss$mean.interval.mean, 620)),
       xlim=range(ss$leader.failure.rate),
       ylab='',
       xlab='leader failure rate')
  lapply(unique(ss$cfg), function (x) {
           d <- subset(ss, cfg==x)
           polygon(c(rev(d$leader.failure.rate), d$leader.failure.rate),
                   c(rev(d$mean.interval.mean + d$mean.interval.sd),
                     pmax(10e-16,d$mean.interval.mean - d$mean.interval.sd)),
                   col = lf.color.3[x], border = NA)
           lines(mean.interval.mean ~ leader.failure.rate, col=lf.color[x], data=d)
       })
  axis(side=1, at=unique(ss$leader.failure.rate))
  with(ss,
       title(main=sprintf("block interval\nproposed    realistic/%s    nodes: %i    blocks: %i    iterations: %g",
                          unique(net), unique(n.nodes), unique(n.blocks), unique(n.iterations))))
}
#
if(interactive()) {
  # lf.plot.net('uniform')
  lf.plot.net('exponential')
} else {
  fname <- "failure-block-interval-realistic-exponential.pdf"
  print(fname)
  cairo_pdf(paste0("../eval/plots/", fname), width=7, height=5)
  lf.plot.net('exponential')
  invisible(dev.off())
  #
  # fname <- "failure-block-interval-realistic-uniform.pdf"
  # print(fname)
  # cairo_pdf(paste0("../eval/plots/", fname), width=7, height=5)
  # lf.plot.net('uniform')
  # invisible(dev.off())
}

lf2 <- with(lf, data.frame(failure = leader.failure.rate, # x-axis
                           mean.interval = mean.interval.mean, # y-axis
                           mean.interval.low = mean.interval.mean - 1.96 * mean.interval.sd,
                           mean.interval.high = mean.interval.mean + 1.96 * mean.interval.sd,
                           distribution = net,
                           protocol = cfg)
)
lf.wide <- reshape(lf2, direction="wide", idvar=c("failure", "distribution"), timevar="protocol")
lf.wide <- reshape(lf.wide, direction="wide", idvar="failure", timevar="distribution")
pgf.csv(lf.wide, "failure.csv")

# attacker block/vote share as a function of alpha
##################################################
# target block interval 600 seconds
# two networks: uniform, exponential, "realistic" latency
# two scenarios: nc-slow, k=51/proposed
# censor strategy (TODO: vs honest participation)
# x-axis: alpha = 0 ... 1/2
# y-axis: attacker share in blocks/votes

cs.tags <- tags[startsWith(tags, "censor-") & endsWith(tags, "-proposed")]
cs.df.of.tag <- function(tag) {
  d <- runs.agg[runs.agg$tag == tag, ]
  d$net <- sub("-proposed$", "", sub("^censor-", "", tag))
  d$cfg <- "proposed"
  return(d)
}
cs <- data.frame(do.call(rbind, lapply(cs.tags, cs.df.of.tag)))
cs$net <- as.factor(cs$net)
cs$cfg <- as.factor(cs$cfg)
if(interactive()){
  str(cs)
}

cs.color   <- colorspace::rainbow_hcl(2)
cs.color.3 <- colorspace::rainbow_hcl(2, alpha=0.3)

cs.ref <- read.csv("../eval/old-mc.csv")
cs.ref <- cs.ref[order(cs.ref$alpha), ]

draw.interval <- function(x, y, sd, ...) {
  polygon(c(rev(x), x),
          c(rev(y + sd), y - sd),
          border = NA, ...)
}
#
cs.plot.net <- function(net) {
  ss <- cs[cs$net==net, ]
  plot(1, 1,
       type='n', las=1,
       ylim=range(ss$attacker.share.blocks.mean),
       xlim=range(ss$alpha),
       ylab='',
       xlab='α')
  lines(ss$alpha, ss$alpha, col="gray")
  draw.interval(ss$alpha, ss$attacker.share.votes.mean,
                ss$attacker.share.votes.sd, col = cs.color.3[1])
  draw.interval(ss$alpha, ss$attacker.share.blocks.mean,
                ss$attacker.share.blocks.sd, col = cs.color.3[2])
  lines(ss$alpha, ss$attacker.share.votes.mean, col=cs.color[1], type="b")
  lines(ss$alpha, ss$attacker.share.blocks.mean, col=cs.color[2], type="b")
  lines(share_of_blocks ~ alpha, data=cs.ref, col=cs.color[2], lty=2)
  lines(share_of_votes ~ alpha, data=cs.ref, col=cs.color[1], lty=2)
  with(ss,
       title(main=sprintf("attacker share\nproposed    %s    nodes: %i    blocks: %i    iterations: %g",
                          unique(net), unique(n.nodes), unique(n.blocks), unique(n.iterations))))
  legend("topleft", c("votes", "blocks", "honest (= α)"), pch=c(15,15,NULL), col=c(cs.color, "gray"))
  legend("bottomright", c("simulator", "mcmc"), lty=c(1,2))
}
#
if(interactive()) {
  # cs.plot.net('realistic-uniform')
  cs.plot.net('realistic-exponential')
  # cs.plot.net('zero')
} else {
  for (n in c('realistic-exponential')) {
    fname <- paste0("alpha-share-", n, ".pdf")
    print(fname)
    cairo_pdf(paste0("../eval/plots/", fname), width=7, height=5)
    cs.plot.net(n)
    dev.off()
  }
}

cs2 <- with(cs, data.frame(attacker = alpha , # x-axis
                           share.blocks = attacker.share.blocks.mean, # y-axis
                           share.blocks.low = attacker.share.blocks.mean - 1.96 * attacker.share.blocks.sd,
                           share.blocks.high = attacker.share.blocks.mean + 1.96 * attacker.share.blocks.sd,
                           share.votes = attacker.share.votes.mean, # y-axis
                           share.votes.low = attacker.share.votes.mean - 1.96 * attacker.share.votes.sd,
                           share.votes.high = attacker.share.votes.mean + 1.96 * attacker.share.votes.sd,
                           net = net # config
       )
)
cs.wide <- reshape(cs2, direction="wide", idvar="attacker", timevar="net")
pgf.csv(cs.wide, "attacker.csv")

stop("end of script")

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
