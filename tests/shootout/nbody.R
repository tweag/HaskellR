# ------------------------------------------------------------------
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Leo Osvald
# ------------------------------------------------------------------

pi <- 3.141592653589793
solar_mass <- 4 * pi * pi
days_per_year <- 365.24
n_bodies <- 5

body_x <- c(
    0, # sun
    4.84143144246472090e+00, # jupiter
    8.34336671824457987e+00, # saturn
    1.28943695621391310e+01, # uranus
    1.53796971148509165e+01 # neptune
)
body_y <- c(
    0, # sun
    -1.16032004402742839e+00, # jupiter
    4.12479856412430479e+00, # saturn
    -1.51111514016986312e+01, # uranus
    -2.59193146099879641e+01 # neptune
)
body_z <- c(
    0, # sun
    -1.03622044471123109e-01, # jupiter
    -4.03523417114321381e-01, # saturn
    -2.23307578892655734e-01, # uranus
    1.79258772950371181e-01 # neptune
)

body_vx <- c(
    0, # sun
    1.66007664274403694e-03 * days_per_year, # jupiter
    -2.76742510726862411e-03 * days_per_year, # saturn
    2.96460137564761618e-03 * days_per_year, # uranus
    2.68067772490389322e-03 * days_per_year # neptune
)
body_vy <- c(
    0, # sun
    7.69901118419740425e-03 * days_per_year, # jupiter
    4.99852801234917238e-03 * days_per_year, # saturn
    2.37847173959480950e-03 * days_per_year, # uranus
    1.62824170038242295e-03 * days_per_year # neptune
)
body_vz <- c(
    0, # sun
    -6.90460016972063023e-05 * days_per_year, # jupiter
    2.30417297573763929e-05 * days_per_year, # saturn
    -2.96589568540237556e-05 * days_per_year, # uranus
    -9.51592254519715870e-05 * days_per_year # neptune
)

body_mass <- c(
    solar_mass, # sun
    9.54791938424326609e-04 * solar_mass, # jupiter
    2.85885980666130812e-04 * solar_mass, # saturn
    4.36624404335156298e-05 * solar_mass, # uranus
    5.15138902046611451e-05 * solar_mass # neptune
)

offset_momentum <- function() {
    body_vx[[1]] <<- -sum(body_vx * body_mass) / solar_mass
    body_vy[[1]] <<- -sum(body_vy * body_mass) / solar_mass
    body_vz[[1]] <<- -sum(body_vz * body_mass) / solar_mass
}

advance <- function(dt) {
    dxx <- outer(body_x, body_x, "-")  # ~2x faster then nested for loops
    dyy <- outer(body_y, body_y, "-")
    dzz <- outer(body_z, body_z, "-")
    distance <- sqrt(dxx * dxx + dyy * dyy + dzz * dzz)
    mag <- dt / (distance * distance * distance)  # ~fast as distance^3
    diag(mag) <- 0
    body_vx <<- body_vx - as.vector((dxx * mag) %*% body_mass)
    body_vy <<- body_vy - as.vector((dyy * mag) %*% body_mass)
    body_vz <<- body_vz - as.vector((dzz * mag) %*% body_mass)
    body_x <<- body_x + dt * body_vx
    body_y <<- body_y + dt * body_vy
    body_z <<- body_z + dt * body_vz
}

energy <- function() {
    dxx <- outer(body_x, body_x, "-")
    dyy <- outer(body_y, body_y, "-")
    dzz <- outer(body_z, body_z, "-")
    distance <- sqrt(dxx * dxx + dyy * dyy + dzz * dzz)
    q <- (body_mass %o% body_mass) / distance
    return(sum(0.5 * body_mass *
               (body_vx * body_vx + body_vy * body_vy + body_vz * body_vz)) -
           sum(q[upper.tri(q)]))
}

nbody <- function(args) {
    n = if (length(args)) as.integer(args[[1]]) else 1000L
    options(digits=9)
    offset_momentum()
    cat(energy(), "\n")
    for (i in 1:n)
        advance(0.01)
    cat(energy(), "\n")
}

if (!exists("i_am_wrapper"))
    nbody(commandArgs(trailingOnly=TRUE))
