rm(list = ls())

runs <- read.csv('output/runs.csv')
str(runs)

runs$delta.dist <- as.factor(runs$delta.dist)
runs$block.orphan.rate <- with(runs, (blocks.observed - blocks.confirmed) / blocks.confirmed)
runs$vote.orphan.rate <- with(runs, (votes.observed - votes.confirmed) / votes.confirmed)

# list of experiments
unique(runs$tag)

# fixed-rate experiment
#######################
# delta: expected propagation time 1, both votes and blocks
# rate: 1 puzzle solution per delta
# x: quorum size
# y: orphan votes and blocks

add.title <- function(d) {
  with(d,
       title(main=sprintf("tag: %s    rate: %.1f    delta: %.1f    nodes: %i    blocks: %i",
                          unique(tag), unique(1/pow.scale), unique(delta.vote),
                          unique(n.nodes), unique(n.blocks)
                          )))
}

fr <- subset(runs, tag=="fixed-rate")

plot(block.orphan.rate ~ quorum.size,
     data=fr,
     log='x', xaxt='n',
     col=delta.dist,
     pch=as.numeric(delta.dist),
     ylim=c(0,1),
     ylab='block orphan rate',
     xlab='quorum size')
axis(side=1, at=unique(fr$quorum.size))
add.title(fr)
legend('bottomleft', title='latency model', legend = levels(fr$delta.dist),
       col=1:length(levels(fr$delta.dist)),
       pch=1:length(levels(fr$delta.dist)))

plot(vote.orphan.rate ~ quorum.size,
     data=fr,
     log='x', xaxt='n',
     col=delta.dist,
     pch=as.numeric(delta.dist),
     ylim=c(0,1),
     ylab='vote orphan rate',
     xlab='quorum size')
axis(side=1, at=unique(fr$quorum.size))
add.title(fr)
legend('bottomleft', title='latency model', legend = levels(fr$delta.dist),
       col=1:length(levels(fr$delta.dist)),
       pch=1:length(levels(fr$delta.dist)))

# fixed-quorum experiment
#########################
# delta: expected propagation time 1, both votes and blocks
# keep quorum size fixed, change pow rate
# we look for orphan votes and blocks

add.title <- function(d) {
  with(d,
       title(main=sprintf("tag: %s    delta: %.1f (%s)    nodes: %i    blocks: %i",
                          unique(tag), unique(delta.vote),
                          unique(delta.dist), unique(n.nodes), unique(n.blocks)
                          )))
}

fq <- subset(runs, tag=="fixed-quorum" & delta.dist=="uniform")
fq$quorum.size <- as.factor(fq$quorum.size)

plot(block.orphan.rate ~ pow.scale,
     data=fq,
     log='xy', xaxt='n',
     col=quorum.size,
     pch=as.numeric(quorum.size),
     ylim=c(10^-4,5),
     ylab='block orphan rate',
     xlab='expected puzzle solving time')
axis(side=1, at=unique(fq$pow.scale))
add.title(fq)
legend('bottomleft', title='quorum size', legend = levels(fq$quorum.size),
       col=1:length(levels(fq$quorum.size)),
       pch=1:length(levels(fq$quorum.size)))

plot(vote.orphan.rate ~ pow.scale,
     data=fq,
     log='xy', xaxt='n',
     col=quorum.size,
     pch=as.numeric(quorum.size),
     ylim=c(10^-4,5),
     ylab='vote orphan rate',
     xlab='expected puzzle solving time')
axis(side=1, at=unique(fq$pow.scale))
add.title(fq)
legend('bottomleft', title='quorum size', legend = levels(fq$quorum.size),
       col=1:length(levels(fq$quorum.size)),
       pch=1:length(levels(fq$quorum.size)))


