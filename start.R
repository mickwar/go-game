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
    points(-0.75, -0.75, cex = 5, lwd = 5, pch = 4, col='red')
    points(x$p+0.75, -0.75, cex = 5, lwd = 5, pch = 1, col='darkgreen')
    text(-0.25, x$p+0.75, "Pass", cex = 3)
    }

go.plot.add = function(where, x){
    cex = 100/(x$p-1) + 80*dnorm(x$p, 10, 10)
    color = x$board[where]
    g = expand.grid(seq(1+x$voff, x$n+x$voff, by = 1), seq(1+x$hoff, x$m+x$hoff, by = 1))
    g = g[,c(2, 1)]
    if (color == 1)
        points(g[where,], col = 'black', pch=20, cex = cex)
    if (color == -1)
        points(g[where,], col = 'white', pch=20, cex = cex)
    }

go.play = function(n, m = n){

    x = go.board.init(n, m)
    go.plot.init(x)

    error.check = function(where){
        # No points clicked
        if (length(where) == 0){
            cat("    Click where to place piece, then click green circle, then right clight.\n")
            return (TRUE)
            }

        # Too many points clicked
        if (length(where) > 2){
            cat("    Too many points clicked. Starting turn over.\n")
            return (TRUE)
            }

        # Only one location clicked that was not a pass
        if ((length(where) == 1) && (where[1] != n*m + 3)){
            cat("    Click the green circle to confirm a move. Starting over.\n")
            return (TRUE)
            }

        # Red X was clicked
        if (any(where == n*m + 1)){
            cat("    Redoing turn.\n")
            return (TRUE)
            }

        # Green O not clicked
        if (!any(where == n*m + 2)){
            cat("    Green circle must be clicked to confirm a move. Starting over.\n")
            return (TRUE)
            }

        # Cannot place in existing location
        if (x$board[min(where)] != 0){
            cat("    Invalid move: piece already there\n")
            return (TRUE)
            }

        # All good, no errors
        return (FALSE)
        }

    # Black: turn == 1
    # White: turn == -1
    turn = 1
    while (TRUE){
        g = expand.grid(seq(1+x$voff, x$n+x$voff, by = 1), seq(1+x$hoff, x$m+x$hoff, by = 1))
        g = g[,c(2, 1)]
        g = rbind(g, c(-0.75, -0.75), c(x$p + 0.75, -0.75))

        cat(ifelse(turn == 1, "Black", "White"), "'s turn: \n", sep="")
        (where = identify(g, offset = 0, plot = FALSE))

        if (!error.check(where)){
            where = min(where)
            x$board[where] = turn
            go.plot.add(where, x)
            turn = -turn
            }
        
        }



    }

go.play(0)

go.add.piece = function(where, board, turn){

    }
