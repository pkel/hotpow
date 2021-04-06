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

# load data
###########

runs <- read.csv('runs.csv')
if(interactive()) str(runs)

runs$delta.dist <- as.factor(runs$delta.dist)
runs$block.orphan.rate <- with(runs, (blocks.observed - blocks.confirmed) / blocks.observed)
runs$vote.orphan.rate <- with(runs, (votes.observed - votes.confirmed) / votes.observed)

# aggregate iterations
runs.agg <- aggregate(cbind(vote.orphan.rate, block.orphan.rate, block.interval=mean.interval) ~
                      tag + pow.scale + quorum.size + delta.block + delta.vote + delta.dist + n.nodes + n.blocks + confirmations + churn + leader.failure.rate,
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
         list(nNodes = uniq.or.fail(n.nodes),
              nBlocks = uniq.or.fail(n.blocks),
              nConfirmations = uniq.or.fail(confirmations),
              nIterations = uniq.or.fail(n.iterations))))

# block interval distribution and basic orphan rate stats
#########################################################
# target block interval 600 second, δ=2
# three scenarios: nc-fast, nc-slow, k=51/proposed
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

bi.tags <- tags[startsWith(tags, "simplified") | startsWith(tags, "realistic")]
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
bi.stats <- data.frame(do.call(rbind, lapply(bi.tags, bi.row.of.tag)))
bi.rows <- with(bi.stats,
                paste(sprintf("\\textbf{%s}", net.lat),
                      sprintf("\\textbf{%s}", net.dis),
                      sprintf("\\textbf{%s}", cfg),
                      sprintf("$%g''$", signif(mean, 4)),
                      sprintf("$%g''$", signif(sd  , 4)),
                      sprintf("$%g''$", signif(q1  , 4)),
                      sprintf("$%g''$", signif(q5  , 4)),
                      sprintf("$%g''$", signif(q50 , 4)),
                      sprintf("$%g''$", signif(q95 , 4)),
                      sprintf("$%g''$", signif(q99 , 4)),
                      sprintf("$%g\\,\\%%$", signif(block.orphan.rate*100, 3)),
                      sprintf("$%g\\,\\%%$", signif(vote.orphan.rate*100, 3)),
                      sep = " & "))
bi.lns <- c("\\begin{tabular}{lllccccccccc}",
            "\\toprule",
            "\\multicolumn{3}{c}{configuration} & \\multicolumn{2}{c}{block interval} & \\multicolumn{5}{c}{quantiles} & \\multicolumn{2}{c}{orphan rate} \\\\",
            "\\cmidrule(r){1-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-10} \\cmidrule(l){11-12}",
            "latency & distribution & protocol & mean & std.\\,dev. & 1\\,\\% & 5\\,\\% & 50\\,\\% & 95\\,\\% & 99\\,\\% & block & vote \\\\",
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
ebi <- function(t) {
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
  hist(ebi.intervals, probability=T, main="", xlab="", las=1, nclass=20)
  lines(ebi.dinterval$x,
        dgamma(ebi.dinterval$x, shape=q, scale=s),
        col=2, type='l')
  # lines(ebi.dinterval, col=1)
  abline(v=q * s, col=2, lty=2)
  abline(v=mean(ebi.intervals), col=1, lty=2)
  title(main=t,
        xlab="block interval")
  legend("topright",
         c("observation", "Gamma distribution", "observed mean", "target interval"),
         col=c(1,2,1,2), lty=c(1,1,2,2))
}
#
if (interactive()) {
  ebi("simplified-exponential-nc-fast")
  ebi("simplified-exponential-proposed")
  ebi("simplified-exponential-nc-slow")
  ebi("simplified-uniform-nc-fast")
  ebi("simplified-uniform-proposed")
  ebi("simplified-uniform-nc-slow")
  ebi("realistic-exponential-nc-fast")
  ebi("realistic-exponential-proposed")
  ebi("realistic-exponential-nc-slow")
  ebi("realistic-uniform-nc-fast")
  ebi("realistic-uniform-proposed")
  ebi("realistic-uniform-nc-slow")
} else {
  for (tag in bi.tags) {
    fname <- paste0("block-interval-", tag,".pdf")
    print(fname)
    cairo_pdf(paste0("../eval/plots/", fname), width=7, height=4)
    ebi(tag)
    dev.off()
  }
}

# orphan rate as a function of network latency
##############################################
# target block interval 600 seconds
# two networks: uniform, exponential, simplified latency model
# three scenarios: nc-fast, nc-slow, k=51/proposed
# x-axis: δ = 1/4 ... 16
# y-axis: orphan rate blocks + votes for k>1

or.tags <- tags[startsWith(tags, "latency-simplified")]
or.df.of.tag <- function(tag) {
  d <- runs.agg[runs.agg$tag == tag, ]
  s <- strsplit(sub("latency-simplified-", "", tag), "-")[[1]]
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
       title(main=sprintf("orphan rate\nsimplified/%s    nodes: %i    blocks: %i    iterations: %g",
                          unique(net), unique(n.nodes), unique(n.blocks), unique(n.iterations))))
  legend('bottomright', title='configuration', legend = levels(or$cfg),
         col=or.color, pch=15)
  legend('topleft', legend = c("orphaned blocks", "orphaned votes"), lty=1:2)
}
#
if(interactive()) {
  or.plot.net('uniform')
  or.plot.net('exponential')
} else {
  fname <- paste0("latency-orphan-rate-simplified-exponential",".pdf")
  print(fname)
  cairo_pdf(paste0("../eval/plots/", fname), width=7, height=5)
  or.plot.net('exponential')
  invisible(dev.off())
  #
  fname <- paste0("latency-orphan-rate-simplified-uniform",".pdf")
  print(fname)
  cairo_pdf(paste0("../eval/plots/", fname), width=7, height=5)
  or.plot.net('uniform')
  invisible(dev.off())
}

# block interval as a function of network latency
#################################################
# target block interval 600 seconds
# two networks: uniform, exponential, simplified latency model
# three scenarios: nc-slow, k=51/proposed
# x-axis: δ = 1/4 ... 16
# y-axis: block interval

lat.plot.net <- function(net) {
  ss <- or[or$net==net & as.character(or$cfg) %in% c('proposed', 'nc-slow'), ]
  plot(1, 1,
       log='x', xaxt='n', type='n', las=2,
       ylim=range(ss$block.interval.mean),
       xlim=range(ss$delta.block),
       ylab='',
       xlab='δ')
  lapply(unique(ss$cfg), function (x) {
           d <- subset(ss, cfg==x)
             polygon(c(rev(d$delta.block), d$delta.block),
                     c(rev(d$block.interval.mean + d$block.interval.sd),
                       d$block.interval.mean - d$block.interval.sd),
                     col = or.color.3[x], border = NA)
           lines(block.interval.mean ~ delta.block, col=or.color[x], data=d)
       })
  axis(side=1, at=unique(ss$delta.block))
  with(ss,
       title(main=sprintf("block interval\nsimplified/%s    nodes: %i    blocks: %i    iterations: %g",
                          unique(net), unique(n.nodes), unique(n.blocks), unique(n.iterations))))
  legend('bottomright', title='configuration', legend = levels(or$cfg)[unique(ss$cfg)],
         col=or.color[unique(ss$cfg)], pch=15)
}
#
if(interactive()) {
  lat.plot.net('uniform')
  lat.plot.net('exponential')
} else {
  fname <- paste0("latency-block-interval-simplified-exponential",".pdf")
  print(fname)
  cairo_pdf(paste0("../eval/plots/", fname), width=7, height=5)
  lat.plot.net('exponential')
  invisible(dev.off())
  #
  fname <- paste0("latency-block-interval-simplified-uniform",".pdf")
  print(fname)
  cairo_pdf(paste0("../eval/plots/", fname), width=7, height=5)
  lat.plot.net('uniform')
  invisible(dev.off())
}

# block interval as a function of churn
#######################################
# target block interval 600 seconds
# two networks: uniform, exponential, "realistic" latency
# scenario: nc-slow, k=51/proposed
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
       ylim=range(c(600, ss$block.interval.mean, 1200)),
       xlim=range(ss$churn),
       ylab='',
       xlab='churn ratio')
  lapply(unique(ss$cfg), function (x) {
           d <- subset(ss, cfg==x)
           polygon(c(rev(d$churn), d$churn),
                   c(rev(d$block.interval.mean + d$block.interval.sd),
                     pmax(10e-16,d$block.interval.mean - d$block.interval.sd)),
                   col = churn.color.3[x], border = NA)
           lines(block.interval.mean ~ churn, col=churn.color[x], data=d)
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
  churn.plot.net('uniform')
  churn.plot.net('exponential')
} else {
  fname <- "churn-block-interval-realistic-exponential.pdf"
  print(fname)
  cairo_pdf(paste0("../eval/plots/", fname), width=7, height=5)
  churn.plot.net('exponential')
  invisible(dev.off())
  #
  fname <- "churn-block-interval-realistic-uniform.pdf"
  print(fname)
  cairo_pdf(paste0("../eval/plots/", fname), width=7, height=5)
  churn.plot.net('uniform')
  invisible(dev.off())
}

# block interval as a function of leader failure
################################################
# target block interval 600 seconds
# two networks: uniform, exponential, "realistic" latency
# three scenarios: nc-fast, nc-slow, k=51/proposed
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
       ylim=range(c(600, ss$block.interval.mean, 620)),
       xlim=range(ss$leader.failure.rate),
       ylab='',
       xlab='leader failure rate')
  lapply(unique(ss$cfg), function (x) {
           d <- subset(ss, cfg==x)
           polygon(c(rev(d$leader.failure.rate), d$leader.failure.rate),
                   c(rev(d$block.interval.mean + d$block.interval.sd),
                     pmax(10e-16,d$block.interval.mean - d$block.interval.sd)),
                   col = lf.color.3[x], border = NA)
           lines(block.interval.mean ~ leader.failure.rate, col=lf.color[x], data=d)
       })
  axis(side=1, at=unique(ss$lf))
  with(ss,
       title(main=sprintf("block interval\nproposed    realistic/%s    nodes: %i    blocks: %i    iterations: %g",
                          unique(net), unique(n.nodes), unique(n.blocks), unique(n.iterations))))
}
#
if(interactive()) {
  lf.plot.net('uniform')
  lf.plot.net('exponential')
} else {
  fname <- "failure-block-interval-realistic-exponential.pdf"
  print(fname)
  cairo_pdf(paste0("../eval/plots/", fname), width=7, height=5)
  lf.plot.net('exponential')
  invisible(dev.off())
  #
  fname <- "failure-block-interval-realistic-uniform.pdf"
  print(fname)
  cairo_pdf(paste0("../eval/plots/", fname), width=7, height=5)
  lf.plot.net('uniform')
  invisible(dev.off())
}

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
