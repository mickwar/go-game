go.board.init = function(n, m = n){
    out = list("board"=matrix(0, n, m), "n"=n, "m"=m, "p"=max(n+1, m+1),
        "hoff"=ifelse(n > m, (n-m)/2, 0), "voff"=ifelse(m > n, (m-n)/2, 0))
    return (out)
    }

go.plot.init = function(x){
    # Initialize plot
    par(mar = rep(0, 4))
    plot(seq(-1, x$p+1), seq(-1, x$p+1), type='n', ax = FALSE, xlab = "", ylab = "")

    # Board color
    polygon(c(0, 0, x$p, x$p), c(0, x$p, x$p, 0), col=rgb(0.8, 0.5, 0.2), lwd=10)

    # Grid
    for (i in 1:x$n)
        lines(c(1+x$hoff, x$m+x$hoff), c(i+x$voff, i+x$voff), lwd=2, col='gray30')
    for (i in 1:x$m)
        lines(c(i+x$hoff, i+x$hoff), c(1+x$voff, x$n+x$voff), lwd=2, col='gray30')
    for (i in 1:x$n)
        lines(c(1+x$hoff, x$m+x$hoff), c(i+x$voff, i+x$voff), lwd=0.5)
    for (i in 1:x$m)
        lines(c(i+x$hoff, i+x$hoff), c(1+x$voff, x$n+x$voff), lwd=0.5)

    # Label axes
    axis(2, at = seq(1+x$voff, x$n+x$voff, by=1), labels = 1:x$n, line = -3)
    axis(4, at = seq(1+x$voff, x$n+x$voff, by=1), labels = 1:x$n, line = -3)
    axis(1, at = seq(1+x$hoff, x$m+x$hoff, by=1), labels = 1:x$m, line = -3)
    axis(3, at = seq(1+x$hoff, x$m+x$hoff, by=1), labels = 1:x$m, line = -3)

    # Other Markers (for identify())
#   points(-0.75, -0.75, cex = 5, lwd = 5, pch = 4, col='red')
#   points(x$p+0.75, -0.75, cex = 5, lwd = 5, pch = 1, col='darkgreen')
    text(-0.25, x$p+0.75, "Pass", cex = 3)
    }

go.plot.add = function(where, x){
#   cex = 0.95*(100/(x$p-1) + 80*dnorm(x$p, 10, 10))
    cex = 0.6*(100/(x$p-1) + 80*dnorm(x$p, 10, 10))
    color = x$board[where]
    g = expand.grid(seq(1+x$voff, x$n+x$voff, by = 1), seq(1+x$hoff, x$m+x$hoff, by = 1))
    g = g[,c(2, 1)]
    if (color == 1)
        points(g[where,], col = 'black', pch=20, cex = cex)
    if (color == -1)
        points(g[where,], col = 'white', pch=20, cex = cex)
    }

go.plot.remove = function(where, x){

#   cex = 1.05*(100/(x$p-1) + 80*dnorm(x$p, 10, 10))
    cex = 0.65*(100/(x$p-1) + 80*dnorm(x$p, 10, 10))

    g = expand.grid(seq(1+x$voff, x$n+x$voff, by = 1), seq(1+x$hoff, x$m+x$hoff, by = 1))
    g = g[,c(2, 1)]

    # cover the piece with the board color
    points(g[where,], col = rgb(0.8, 0.5, 0.2), pch=20, cex = cex)

    # re-add the grid lines
    lines(rep(g[where,1], 2), g[where,2] + c(-0.5, 0.5), lwd=2, col = 'gray30')
    lines(g[where,1] + c(-0.5, 0.5), rep(g[where,2], 2), lwd=2, col = 'gray30')
    lines(rep(g[where,1], 2), g[where,2] + c(-0.5, 0.5), lwd=0.5)
    lines(g[where,1] + c(-0.5, 0.5), rep(g[where,2], 2), lwd=0.5)
    }

go.play = function(n, m = n, old_game, machine_black = FALSE, machine_white = FALSE){

    if (missing(old_game)){
        xglobal <<- go.board.init(n, m)
        go.plot.init(xglobal)
        # Black: turn == 1
        # White: turn == -1
        xglobal$turn <<- 1
    } else {
        xglobal <<- old_game
        go.plot.init(xglobal)
        for (b in which(xglobal$board != 0))
            go.plot.add(b, xglobal)
    }

    error.check = function(where){
        # No points clicked
        if (length(where) == 0){
            cat("    Click where to place piece then right clight to confirm.\n")
            return (TRUE)
            }

        # Too many points clicked
        if (length(where) > 2){
            cat("    Too many points clicked. Starting turn over.\n")
            return (TRUE)
            }

        # Pass clicked, no error
        if (where[1] == (xglobal$n * xglobal$m + 1))
            return (FALSE)

        # Cannot place in existing location
        if (xglobal$board[min(where)] != 0){
            cat("    Invalid move: piece already there\n")
            return (TRUE)
            }

        # All good, no errors
        return (FALSE)
        }

    capture.recurse = function(where){
        if (abort)
            return (0)

        checked[where] <<- 1
        h = c(where + xglobal$n, where - 1, where - xglobal$n, where + 1)

        # Set to NA those that aren't neighbors
        if ((where %% xglobal$n) == 0) # on lower edge
            h[4] = NA
        if ((where %% xglobal$n) == 1) # on upper edge
            h[2] = NA
        if (any(where == 1:xglobal$n)) # on left edge
            h[3] = NA
        if (any(where == (xglobal$n*xglobal$m-xglobal$n + 1):(xglobal$n*xglobal$m))) # on right edge
            h[1] = NA

        # Check to see if the piece is surrended on all possible sides (doesn't matter the owner)
        if (sum(xglobal$board[h[!is.na(h)]] != 0) != sum(!is.na(h))){
            abort <<- TRUE
            checked <<- rep(1, xglobal$n * xglobal$m)
            return (0)
            }

        for (i in h[!is.na(h)]){
            # capture.recurse() hasn't already been run, and
            # the neighbor is friendly
            if ((checked[i] == 0) && (xglobal$board[i] == -xglobal$turn))
                capture.recurse(i)
            }
        }

    capture = function(where, xglobal){
        # possible neighbors (right, down, left, up)
        h = c(where + xglobal$n, where - 1, where - xglobal$n, where + 1)

        # Set to NA those that aren't neighbors
        if ((where %% xglobal$n) == 0) # on lower edge
            h[4] = NA
        if ((where %% xglobal$n) == 1) # on upper edge
            h[2] = NA
        if (any(where == 1:xglobal$n)) # on left edge
            h[3] = NA
        if (any(where == (xglobal$n*xglobal$m-xglobal$n + 1):(xglobal$n*xglobal$m))) # on right edge
            h[1] = NA


        # Only need to check if a capture was made if an adjacent piece to
        # `where` is an enemy stone
        for (i in h[!is.na(h)][which(xglobal$board[h[!is.na(h)]] == -xglobal$turn)]){
            checked <<- double(xglobal$n * xglobal$m)
            abort <<- FALSE
            capture.recurse(i)

            # The recursion was never aborted, i.e. a capture of a group of pieces
            if (!abort){
                for (i in 1:(xglobal$n * xglobal$m)){
                    if (checked[i] == 1){
                        capture.made <<- TRUE
                        xglobal$board[i] <<- 0
                        go.plot.remove(i, xglobal)
                        }
                    }
                }
            }
        }

    cat("Starting the game.\n")
    cat("Left-click on a position for a stone to be placed.\n")
    cat("Right-click to confirm selection.\n")

    skip = 0
    prev_turn = xglobal$turn
    while (TRUE){
        if (skip > 100){
            xglobal$turn <<- -xglobal$turn
            prev_turn = xglobal$turn
            skip = 0
            }
        g = expand.grid(seq(1+xglobal$voff, xglobal$n+xglobal$voff, by = 1),
                        seq(1+xglobal$hoff, xglobal$m+xglobal$hoff, by = 1))
        g = g[,c(2, 1)]
        g = rbind(g, c(-0.25, xglobal$p + 0.75))

#       red x and green circle
#       g = rbind(g, c(-0.75, -0.75), c(xglobal$p + 0.75, -0.75))

        cat(ifelse(xglobal$turn == 1, "Black", "White"), "'s turn\n", sep="")
        if ((machine_black == TRUE && xglobal$turn == 1) ||
            (machine_white == TRUE && xglobal$turn == -1)){
            # Machine's turn

            # Now how to make this probability vector better

            h = matrix(NA, xglobal$n * xglobal$m, 4)
            for (i in 1:(xglobal$n*xglobal$m)){
                if (xglobal$board[i] == -xglobal$turn){
                    h[i,] = c(i + xglobal$n, i - 1, i - xglobal$n, i + 1)
                    if ((i %% xglobal$n) == 0) # on lower edge
                        h[i,4] = NA
                    if ((i %% xglobal$n) == 1) # on upper edge
                        h[i,2] = NA
                    if (any(i == 1:xglobal$n)) # on left edge
                        h[i,3] = NA
                    if (any(i == (xglobal$n*xglobal$m-xglobal$n + 1):(xglobal$n*xglobal$m))) # on right edge
                        h[i,1] = NA
                    for (j in 1:NCOL(h))
                        h[i,j] = ifelse(xglobal$board[h[i,j]] != 0, NA, h[i,j])
                    }
                }
            poss = h[!is.na(h)]
            machine_prob = rep(1, length(poss))
            where = ifelse(length(poss) == 0, xglobal$n * xglobal$m + 1, sample(poss, 1, prob = machine_prob))
        } else {
            (where = identify(g, offset = 0, plot = FALSE))
            }

        if (!error.check(where)){
            if (where == (xglobal$n * xglobal$m + 1)){
                cat("    Player has skipped\n")
                xglobal$turn <<- -xglobal$turn
            } else {
                where = min(where)
                xglobal$board[where] <<- xglobal$turn

                capture.made <<- FALSE
                capture(where, xglobal)


                # Check to make sure a move won't result in immediate capture by enemy
                # This needs to be run after checking if the move results in a capture
                # of the enemy, so it's in the wrong place right now
                checked <<- double(xglobal$n * xglobal$m)
                abort <<- FALSE
                possible.error <<- FALSE
                xglobal$turn <<- -xglobal$turn # Temporarily change turns
                capture.recurse(where)
                xglobal$turn <<- -xglobal$turn
                if (!abort)
                    possible.error <<- TRUE

                # A capture was made, proceed as normal
                if (capture.made){
                    go.plot.add(where, xglobal)
                    xglobal$turn <<- -xglobal$turn
                } else { # No capture
                    if (!possible.error){ # Not being captured
                        go.plot.add(where, xglobal)
                        xglobal$turn <<- -xglobal$turn
                    } else { # No capture AND the piece would have captured by an enemy
                        cat("    Invalid move: piece would be immediately captured\n")
                        xglobal$board[where] <<- 0
                        }
                    }
                }
            }

        if (prev_turn == xglobal$turn){
            skip = skip + 1
        } else {
            skip = 0
            prev_turn = xglobal$turn
            }

        }

    }

go.play(50, machine_black = FALSE, machine_white = FALSE)

new = xglobal
go.play(old_game = new, machine_black = TRUE, machine_white = TRUE)

new2 = xglobal
go.play(old_game = new2, machine_black = TRUE, machine_white = TRUE)
