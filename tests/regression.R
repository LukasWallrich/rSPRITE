# Run with Rscript tests/regression.R from the repository root.
# Only computational definitions are loaded; no Shiny app or packages are needed.
sourceFile <- if (length(commandArgs(TRUE))) commandArgs(TRUE)[1] else "rSPRITE.R"
u <- new.env(parent = globalenv())
for (ex in parse(sourceFile)) {
  if (is.call(ex) && identical(ex[[1]], as.name("<-")) && is.symbol(ex[[2]]) &&
      startsWith(as.character(ex[[2]]), "rSprite.") &&
      (is.numeric(ex[[3]]) ||
       (is.call(ex[[3]]) && identical(ex[[3]][[1]], as.name("function"))))) {
    eval(ex, u)
  }
}
u$rSprite.message <- function(...) invisible(NULL)
checks <- 0L
check <- function(value) {
  stopifnot(isTRUE(value))
  checks <<- checks + 1L
}

# A binary scale with response 1 forbidden must return only 2s. R's sample(2)
# shorthand previously reintroduced forbidden 1s, including in returned solutions.
for (seed in 1:30) {
  set.seed(seed)
  result <- u$rSprite.getSample(1, 10, 2, 0, 1, 2, dp = 0, never = 1)$rows
  check(length(result) == 10 && all(result == 2))
}

# Ordinary five-point scale and two-decimal reports. A random starting vector
# containing only the fixed response used to leave an empty replacement pool.
for (seed in 1:30) {
  set.seed(seed)
  result <- u$rSprite.getSample(1, 20, 1.2, .41, 1, 5, dp = 2,
                               fixed = rep(1, 16))$rows
  check(length(result) == 20 && sum(result == 1) == 16 && sum(result == 2) == 4)
}

# Non-restricted numerical behaviour and restrictions outside the free scale
# remain supported. A fixed response may intentionally be outside the scale.
set.seed(1)
result <- u$rSprite.getSample(1, 4, 1.3, .5, 1, 2, dp = 1)$rows
check(length(result) == 4 && mean(result) == 1.25 && sd(as.numeric(result)) == .5)
set.seed(1)
result <- u$rSprite.seekVector(4, 2, sqrt(2), 1, 3, dp = 2, fixed = 0, label = "")
check(length(result) == 3 && !any(result == 0))

# This actual sample was rejected: sdLimits chose only total round(3.22*250),
# rather than all totals whose means round to 3.22.
x <- c(rep(3, 196), rep(4, 54))
check(round(mean(x), 2) == 3.22 && round(sd(x), 2) == .41)
limits <- u$rSprite.sdLimits(250, 3.22, 1, 5, 2)
check(limits[1] <= .41 && limits[2] >= .41)
check(identical(u$rSprite.sdLimits(2, 0, 0, 2, 0), c(0, 1)))
check(isTRUE(all.equal(u$rSprite.sdLimits(4, 1e8+1.5, 1e8+1, 1e8+2, 2), c(.58, .58))))

# Compare bounds against actual samples, enumerated independently as counts.
comparisons <- 0L
for (values in list(0:2, -2:2, 1:5)) for (n in 2:6) {
  counts <- as.matrix(expand.grid(rep(list(0:n), length(values))))
  counts <- counts[rowSums(counts) == n, , drop = FALSE]
  samples <- lapply(seq_len(nrow(counts)), function(i) rep(values, counts[i, ]))
  means <- vapply(samples, mean, 0.0)
  sds <- vapply(samples, sd, 0.0)
  for (dp in 0:2) for (m in unique(round(means, dp))) {
    compatible <- abs(means - m) <= .5 * 10^-dp + 1e-12
    expected <- c(ceiling((min(sds[compatible]) - .5*10^-dp - 1e-12)*10^dp),
                  floor((max(sds[compatible]) + .5*10^-dp + 1e-12)*10^dp)) / 10^dp
    expected[1] <- max(0, expected[1])
    actual <- u$rSprite.sdLimits(n, m, min(values), max(values), dp)
    check(isTRUE(all.equal(actual, expected, tolerance = 1e-10)))
    comparisons <- comparisons + 1L
  }
}

# Controlled worker: success appears only on the last permitted adjustment.
# This is a boundary-condition regression, not evidence of its natural frequency.
e <- new.env(parent = u)
e$calls <- 0L
target <- c(1, 1, 2, 2)
e$rSprite.delta <- function(...) {
  e$calls <- e$calls + 1L
  if (e$calls == 20000L) target else c(1, 1, 1, 3)
}
f <- u$rSprite.seekVector
environment(f) <- e
set.seed(1)
result <- f(4, 1.5, sd(target), 1, 3, dp = 2, label = "")
check(e$calls == 20000L && identical(result, target))

# A never-succeeding worker still gets exactly the same adjustment budget.
e$calls <- 0L
e$rSprite.delta <- function(...) { e$calls <- e$calls + 1L; c(1, 1, 1, 3) }
set.seed(1)
result <- f(4, 1.5, sd(target), 1, 3, dp = 2, label = "")
check(e$calls == 20000L && length(result) == 0)
# Invalid direct calls must not expose the internal impossible-bound sentinel.
messages <- character()
u$rSprite.message <- function(s, ...) messages <<- c(messages, s)
for (args in list(list(1, 1, 2, 0, 1, 5), list(1, 10, 8, 1, 1, 5),
                  list(1, 10, 1, 0, 1, 1))) {
  messages <- character()
  result <- do.call(u$rSprite.getSample, args)
  check(length(result$rows) == 0 &&
        identical(messages, "No SD range is defined for this sample size, mean, and scale."))
}
cat(checks, "checks passed, including", comparisons, "exhaustive SD-bound comparisons.\n")
