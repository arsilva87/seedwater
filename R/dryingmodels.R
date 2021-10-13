dryingmodels <- 
function(moisture, time) 
{
    if (!requireNamespace(c("tcltk", "tkrplot"), quietly = TRUE)) 
        stop("packages tcltk and tkrplot are required")
    geterrmessage()
    done <- tclVar(0)
    dryingModels.env <- new.env()
    assign("dryingModels.tmp", list(), envir = dryingModels.env)
    x <- NULL

    # Moisture ratio
    Mf <- min(moisture)
    Mi <- max(moisture)
    Mr <- (moisture - Mf)/(Mi - Mf)

    # equations
    page <- function(x, K, n) exp(-K*x^n)
    HandP <- function(x, A, K) A*exp(-K*x)
    mHandP <- function(x, A, K, b, K0, K1) A*exp(-K*x) + b*exp(-K0*x) + exp(-K1*x)
    midilli <- function(x, A, K, n, b) A*exp(-K*x^n) + b*x
    diffusion.approx <- function(x, A, K, b) A*exp(-K*x) + (1-A)*exp(-K*b*x)
    two.terms <- function(x, A, b, K0, K1) A*exp(-K0*x) + b*exp(-K1*x)
    two.term.exp <- function(x, A, K) A*exp(-K*x) + (1-A)*exp(-K*A*x)
    logarithmic <- function(x, A, K, b) A*exp(-K*x) + b
    thompson <- function(x, A, b) exp(-A - sqrt(A^2 + 4*b*x)) / (2*b)
    newton <- function(x, K) exp(-K*x)
    verma <- function(x, A, K, K1) A*exp(-K*x) + (1-A)*exp(-K1*x)
    WandS <- function(x, A, b) 1 + A*x + b*x^2

    # input-start variables
    f <- tclVar("Henderson.and.Pabis")
    A <- tclVar("0.9")
    K <- tclVar("0.1")
    n <- tclVar("0.9")
    b <- tclVar("0.1")
    K0 <- tclVar("0.1")
    K1 <- tclVar("0.1")

    replot <- function(...) {
        fun <- as.character(tclObj(f))
        A <- as.numeric(tclObj(A))
        K <- as.numeric(tclObj(K))
        n <- as.numeric(tclObj(n))
        b <- as.numeric(tclObj(b))
        K0 <- as.numeric(tclObj(K0))
        K1 <- as.numeric(tclObj(K1))
        plot(time, Mr, xlab = "Time", ylab = "Moisture ratio")
        if (fun == "Page") {
            curve(page(x, K, n), add = TRUE, col = "red")
        } else if (fun == "Henderson.and.Pabis") {
            curve(HandP(x, A, K), add = TRUE, col = "red")
        } else if (fun == "Modified.Henderson.and.Pabis") {
            curve(mHandP(x, A, K, b, K0, K1), add = TRUE, col = "red")
        } else if (fun == "Midilli") {
            curve(midilli(x, A, K, n, b), add = TRUE, col = "red")
        } else if (fun == "Diffusion.Approximation") {
            curve(diffusion.approx(x, A, K, b), add = TRUE, col = "red")
        } else if (fun == "Two.Terms") {
            curve(two.terms(x, A, b, K0, K1), add = TRUE, col = "red")
        } else if (fun == "Two.Term.Exponential") {
            curve(two.term.exp(x, A, K), add = TRUE, col = "red")
        } else if (fun == "Logarithmic") {
            curve(logarithmic(x, A, K, b), add = TRUE, col = "red")
        } else if (fun == "Thompson") {
            curve(thompson(x, A, b), add = TRUE, col = "red")
        } else if (fun == "Newton") {
            curve(newton(x, K), add = TRUE, col = "red")
        } else if (fun == "Verma") {
            curve(verma(x, A, K, K1), add = TRUE, col = "red")
        } else if (fun == "Wang.and.Singh") {
            curve(WandS(x, A, b), add = TRUE, col = "red")
        }
    }
    redraw <- function(...) {
        fun <- as.character(tclObj(f))
        if (fun == "Page") {
            # state of parameters: A, K, n, b, K0, K1
            states <- c("disabled", "normal", "normal", "disabled", "disabled", "disabled")
            tkfocus(entry.K)
            tkfocus(entry.n)
        } else if (fun == "Henderson.and.Pabis") {
            states <- c("normal", "normal", "disabled", "disabled", "disabled", "disabled")
            tkfocus(entry.A)
            tkfocus(entry.K)
        } else if (fun == "Modified.Henderson.and.Pabis") {
            states <- c("normal", "normal", "disabled", "normal", "normal", "normal")
            tkfocus(entry.A)
            tkfocus(entry.K)
            tkfocus(entry.b)
            tkfocus(entry.K0)
            tkfocus(entry.K1)
        } else if (fun == "Midilli") {
            states <- c("normal", "normal", "normal", "normal", "disabled", "disabled")
            tkfocus(entry.A)
            tkfocus(entry.K)
            tkfocus(entry.n)
            tkfocus(entry.b)
        } else if (fun == "Diffusion.Approximation") {
            states <- c("normal", "normal", "disabled", "normal", "disabled", "disabled")
            tkfocus(entry.A)
            tkfocus(entry.K)
            tkfocus(entry.b)
        } else if (fun == "Two.Terms") {
            states <- c("normal", "disabled", "disabled", "normal", "normal", "normal")
            tkfocus(entry.A)
            tkfocus(entry.b)
            tkfocus(entry.K0)
            tkfocus(entry.K1)
        } else if (fun == "Two.Term.Exponential") {
            states <- c("normal", "normal", "disabled", "disabled", "disabled", "disabled")
            tkfocus(entry.A)
            tkfocus(entry.K)
        } else if (fun == "Logarithmic") {
            states <- c("normal", "normal", "disabled", "normal", "disabled", "disabled")
            tkfocus(entry.A)
            tkfocus(entry.K)
            tkfocus(entry.b)
        } else if (fun == "Thompson") {
            states <- c("normal", "disabled", "disabled", "normal", "disabled", "disabled")
            tkfocus(entry.A)
            tkfocus(entry.b)
        } else if (fun == "Newton") {
            states <- c("disabled", "normal", "disabled", "disabled", "disabled", "disabled")
            tkfocus(entry.K)
        } else if (fun == "Verma") {
            states <- c("normal", "normal", "disabled", "disabled", "disabled", "normal")
            tkfocus(entry.A)
            tkfocus(entry.K)
            tkfocus(entry.K1)
        } else if (fun == "Wang.and.Singh") {
            states <- c("normal", "disabled", "disabled", "normal", "disabled", "disabled")
            tkfocus(entry.A)
            tkfocus(entry.b)
        }
        tkconfigure(entry.A, state = states[1])
        tkconfigure(ts1, state = states[1]) 
        tkconfigure(entry.K, state = states[2])
        tkconfigure(ts3, state = states[2]) 
        tkconfigure(entry.n, state = states[3])
        tkconfigure(ts4, state = states[3])  
        tkconfigure(entry.b, state = states[4])
        tkconfigure(ts6, state = states[4])  
        tkconfigure(entry.K0, state = states[5])
        tkconfigure(ts7, state = states[5])  
        tkconfigure(entry.K1, state = states[6])
        tkconfigure(ts8, state = states[6])  
        replot()
    }
    base <- tktoplevel()
    tkwm.title(base, "Seed Drying Kinetics")
    spec.frm <- tkframe(base, borderwidth = 2)
    left.frm <- tkframe(spec.frm)
    right.frm <- tkframe(spec.frm)  

    # parameter: A
    frame1 <- tkframe(left.frm, relief = "groove", borderwidth = 2)
    tkpack(tklabel(frame1, text = "Parameters"), fill = "both", side = "top")
    entry.A <- tkentry(frame1, width = "5", textvariable = A)
    tkpack(ts1 <- tkscale(frame1, label = "A", command = replot, 
        from = -0.5, to = 5, showvalue = 0, variable = A, 
        resolution = 0.001, orient = "horiz", relief = "groove"), 
        fill = "both", expand = 1, padx = 3, pady = 2, ipadx = 3, 
        ipady = 2, side = "left")
    tkpack(entry.A, fill = "none", side = "right")

    # models
    frame2 <- tkframe(right.frm, relief = "groove", borderwidth = 2)
    tkpack(tklabel(frame2, text = "Models"))
    for (i in c("Page", "Henderson.and.Pabis", 
        "Modified.Henderson.and.Pabis", "Midilli", "Diffusion.Approximation", 
        "Two.Terms", "Two.Term.Exponential", "Logarithmic", "Thompson",
        "Newton", "Verma", "Wang.and.Singh")) {
        tmp <- tkradiobutton(frame2, command = redraw, text = i, 
            value = i, variable = f)
        tkpack(tmp, anchor = "w")
    }

    # parameter: K
    frame3 <- tkframe(left.frm, relief = "groove", borderwidth = 2)
    entry.K <- tkentry(frame3, width = "5", textvariable = K)
    tkpack(ts3 <- tkscale(frame3, label = "K", command = replot, 
        from = 0, to = 5, showvalue = 0, variable = K,
        resolution = 0.01, orient = "horiz", relief = "groove"), 
        fill = "both", expand = 1, padx = 3, pady = 2, ipadx = 3, 
        ipady = 2, side = "left")
    tkpack(entry.K, fill = "none", side = "right")

    # parameter: n
    frame4 <- tkframe(left.frm, relief = "groove", borderwidth = 2)
    entry.n <- tkentry(frame4, width = "5", textvariable = n)
    tkpack(ts4 <- tkscale(frame4, label = "n", 
        command = replot, from = 0, to = 1.5, showvalue = 0, 
        variable = n, resolution = 0.01, orient = "horiz", 
        relief = "groove"), fill = "none", expand = 1, padx = 3, 
        pady = 2, ipadx = 3, ipady = 2, side = "left")
    tkpack(entry.n, side = "right")

    # fitting criteria
    frame5 <- tkframe(right.frm, relief = "groove", borderwidth = 2)
    r2Text <- tclVar("R-sq: ")
    label.r2 <- tklabel(frame5, text=tclvalue(r2Text))
    tkconfigure(label.r2, textvariable=r2Text)
    tkgrid(label.r2)
    r2adjText <- tclVar("Adj. R-sq: ")
    label.r2adj <- tklabel(frame5, text=tclvalue(r2adjText))
    tkconfigure(label.r2adj, textvariable=r2adjText)
    tkgrid(label.r2adj)
    seText <- tclVar("S.E.: ")
    label.se <- tklabel(frame5, text=tclvalue(seText))
    tkconfigure(label.se, textvariable=seText)
    tkgrid(label.se)

    # parameter: b
    frame6 <- tkframe(left.frm, relief = "groove", borderwidth = 2)
    entry.b <- tkentry(frame6, width = "5", textvariable = b)
    tkpack(ts6 <- tkscale(frame6, label = "b", 
        command = replot, from = -1, to = 2, showvalue = 0, 
        variable = b, resolution = 0.001, orient = "horiz", 
        relief = "groove"), fill = "none", expand = 1, padx = 3, 
        pady = 2, ipadx = 3, ipady = 2, side = "left")
    tkpack(entry.b, side = "right")

    # parameter: K0
    frame7 <- tkframe(left.frm, relief = "groove", borderwidth = 2)
    entry.K0 <- tkentry(frame7, width = "5", textvariable = K0)
    tkpack(ts7 <- tkscale(frame7, label = "K0", 
        command = replot, from = -1, to = 1.5, showvalue = 0, 
        variable = K0, resolution = 0.01, orient = "horiz", 
        relief = "groove"), fill = "none", expand = 1, padx = 3, 
        pady = 2, ipadx = 3, ipady = 2, side = "left")
    tkpack(entry.K0, side = "right")

    # parameter: K1
    frame8 <- tkframe(left.frm, relief = "groove", borderwidth = 2)
    entry.K1 <- tkentry(frame8, width = "5", textvariable = K1)
    tkpack(ts8 <- tkscale(frame8, label = "K1", 
        command = replot, from = -1, to = 1.5, showvalue = 0, 
        variable = K1, resolution = 0.01, orient = "horiz", 
        relief = "groove"), fill = "none", expand = 1, padx = 3, 
        pady = 2, ipadx = 3, ipady = 2, side = "left")
    tkpack(entry.K1, side = "right")

    # buttons
    OnQuit <- function() {
        tclvalue(done) <- 2
    }
    OnClear <- function() {
        assign("dryingModels.tmp", list(), envir = dryingModels.env)
        plot(time, Mr, xlab = "Time", ylab = "Moisture ratio")
        f <- as.character(tclObj(f))
        if (f == "Page") {
            curve(page(x, K = 0.1, n = 0.7), add = TRUE, col = "red")
        } else if (f == "Henderson.and.Page") {
            curve(HandP(x, A = 1, K = 0.12), add = TRUE, col = "red")
        } else if (f == "Modified.Henderson.and.Page") {
            curve(mHandP(x, A = 0.9, K = 0.1, b = 0.1, K0 = 0.1, K1 = 0.1), 
               add = TRUE, col = "red")
        } else if (f == "Midilli") {
            curve(midilli(x, A = 0.9, K = 0.1, n = 0.7, b = 0.01), 
               add = TRUE, col = "red")
        } else if (f == "Diffusion.Approximation") {
            curve(diffusion.approx(x, A = 0.9, K = 0.1, b = 0.1), 
               add = TRUE, col = "red")
        } else if (f == "Two.Terms") {
            curve(two.terms(x, A = 0.9, b = 0.1, K0 = 0.1, K1 = 0.1), 
               add = TRUE, col = "red")
        } else if (f == "Two.Term.Exponential") {
            curve(two.term.exp(x, A = 1.5, K = 0.2), 
               add = TRUE, col = "red")
        } else if (f == "Logarithmic") {
            curve(logarithmic(x, A = 0.9, K = 0.1, b = 0.1), 
               add = TRUE, col = "red")
        } else if (f == "Thompson") {
            curve(thompson(x, A = 0.9, b = 0.1), add = TRUE, col = "red")
        } else if (f == "Newton") {
            curve(newton(x, K = 0.1), add = TRUE, col = "red")
        } else if (f == "Verma") {
            curve(verma(x, A = 0.1, K = 5, K1 = 0.1),
               add = TRUE, col = "red")
        } else if (f == "Wang.and.Singh") {
            curve(WandS(x, A = 0.01, b = 0.001),
               add = TRUE, col = "red")
        }
    }
    OnFit <- function() {
        f <- as.character(tclObj(f))
        A <- as.numeric(tclObj(A))
        K <- as.numeric(tclObj(K))
        n <- as.numeric(tclObj(n))
        b <- as.numeric(tclObj(b))
        K0 <- as.numeric(tclObj(K0))
        K1 <- as.numeric(tclObj(K1))
        if (f == "Page") {
           out <- nls(Mr ~ page(x = time, K, n), 
              start = c(K = K, n = n))
        } else if (f == "Henderson.and.Pabis") {
           out <- nls(Mr ~ HandP(x = time, A, K), 
              start = c(A = A, K = K))
        } else if (f == "Modified.Henderson.and.Page") {
           out <- nls(Mr ~ mHandP(x = time, A, K, b, K0, K1), 
              start = c(A = A, K = K, b = b, K0 = K0, K1 = K1))
        } else if (f == "Midilli") {
           out <- nls(Mr ~ midilli(x = time, A, K, n, b), 
              start = c(A = A, K = K, n = n, b = b))
        } else if (f == "Diffusion.Approximation") {
           out <- nls(Mr ~ diffusion.approx(x = time, A, K, b), 
              start = c(A = A, K = K, b = b))
        } else if (f == "Two.Terms") {
           out <- nls(Mr ~ two.terms(x = time, A, b, K0, K1), 
              start = c(A = A, b = b, K0 = K0, K1 = K1))
        } else if (f == "Two.Term.Exponential") {
           out <- nls(Mr ~ two.term.exp(x = time, A, K), 
              start = c(A = A, K = K))
        } else if (f == "Logarithmic") {
           out <- nls(Mr ~ logarithmic(x = time, A, K, b), 
              start = c(A = A, K = K, b = b))
        } else if (f == "Thompson") {
           out <- nls(Mr ~ thompson(x = time, A, b), 
              start = c(A = A, b = b))
        } else if (f == "Newton") {
           out <- nls(Mr ~ newton(x = time, K), start = c(K = K))
        } else if (f == "Verma") {
           out <- nls(Mr ~ verma(x = time, A, K, K1), 
              start = c(A = A, K = K, K1 = K1))
        } else if (f == "Wang.and.Singh") {
           out <- nls(Mr ~ WandS(x = time, A, b), 
              start = c(A = A, b = b))
        }
        beta <- coef(out)
        r2 <- round(Rsq(out)$pseudo, 4)
        r2adj <- round(Rsq(out)$adj, 4)
        se <- round(summary(out)$sigma, 6)
        tclvalue(r2Text) <- paste("R-sq: ", as.character(r2))
        tclvalue(r2adjText) <- paste("Adj. R-sq: ", as.character(r2adj))
        tclvalue(seText) <- paste("S.E.: ", as.character(se))

        assign("dryingModels.tmp", out, envir = dryingModels.env)
        plot(time, Mr, xlab = "Time", ylab = "Moisture ratio")
        if (f == "Page") {
            curve(page(x, K = beta[1], n = beta[2]), 
               add = TRUE, col = "blue")
        } else if (f == "Henderson.and.Pabis") {
            curve(HandP(x, A = beta[1], K = beta[2]), 
               add = TRUE, col = "blue")
        } else if (f == "Modified.Henderson.and.Page") {
            curve(mHandP(x, A = beta[1], K = beta[2], b = beta[3],
               K0 = beta[4], K1 = beta[5]), add = TRUE, col = "blue")
        } else if (f == "Midilli") {
            curve(midilli(x, A = beta[1], K = beta[2], n = beta[3],
               b = beta[4]), add = TRUE, col = "blue")
        } else if (f == "Diffusion.Approximation") {
            curve(diffusion.approx(x, A = beta[1], K = beta[2], 
               b = beta[3]), add = TRUE, col = "blue")
        } else if (f == "Two.Terms") {
            curve(two.terms(x, A = beta[1], b = beta[2], K0 = beta[3], 
               K1 = beta[4]), add = TRUE, col = "blue")
        } else if (f == "Two.Term.Exponential") {
            curve(two.term.exp(x, A = beta[1], K = beta[2]), 
               add = TRUE, col = "blue")
        } else if (f == "Logarithmic") {
            curve(logarithmic(x, A = beta[1], K = beta[2], b = beta[3]), 
               add = TRUE, col = "blue")
        } else if (f == "Thompson") {
            curve(thompson(x, A = beta[1], b = beta[2]), 
               add = TRUE, col = "blue")
        } else if (f == "Newton") {
            curve(newton(x, K = beta[1]), add = TRUE, col = "blue")
        } else if (f == "Verma") {
            curve(verma(x, A = beta[1], K = beta[2], K1 = beta[3]), 
               add = TRUE, col = "blue")
        } else if (f == "Wang.and.Singh") {
            curve(WandS(x, A = beta[1], b = beta[2]), 
               add = TRUE, col = "blue")
        }
    }

    tkpack(frame1, frame2, frame3, frame4, frame5, frame6, 
        frame7, frame8, fill = "x")
    tkpack(left.frm, right.frm, side = "left", anchor = "n")
    c.but <- tkbutton(base, text = "Clear", command = OnClear)
    q.but <- tkbutton(base, text = "Quit", command = OnQuit)
    fit.but <- tkbutton(base, text = "NLS Estimates", command = OnFit, 
        foreground = "white", background = "navy")
    tkpack(spec.frm)
    tkpack(q.but, side = "right")
    tkpack(c.but, side = "left")
    tkpack(fit.but, side = "right")
    replot()
    tkbind(entry.A, "<Return>", function() replot() )
    tkbind(entry.K, "<Return>", function() replot() )
    tkbind(entry.n, "<Return>", function() replot() )
    tkbind(entry.b, "<Return>", function() replot() )
    tkbind(entry.K0, "<Return>", function() replot() )
    tkbind(entry.K1, "<Return>", function() replot() )
    tkbind(base, "<Destroy>", function() tclvalue(done) <- 2)
    tkwait.variable(done)
    tkdestroy(base)

    # output
    fit <- get("dryingModels.tmp", envir = dryingModels.env)
    return(fit)
} 
