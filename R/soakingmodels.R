soakingmodels <- 
function(moisture, time) 
{
    if (!requireNamespace(c("tcltk", "tkrplot"), quietly = TRUE)) 
        stop("packages tcltk and tkrplot are required")
    geterrmessage()
    done <- tclVar(0)
    soakingModels.env <- new.env()
    assign("soakingModels.tmp", list(), envir = soakingModels.env)
    x <- NULL

    # Water absorption (percentual)
    Mi <- min(moisture)
    Wa <- 100*(moisture - Mi)/Mi

    # equations
    peleg <- function(x, k1, k2) x/(k1 + k2*x)
    logi <- function(x, a, b1, c1) a/(1 + exp(-b1*(x - c1)))
    logi2 <- function(x, a, b1, b2, c1, c2) a/(1 + exp(-b1*(x - c1))) + exp(b2*(x - c2))
    peleg2 <- function(x, k1, k2, b2, c2) x/(k1 + k2*x) + exp(b2*(x - c2))

    # input-start variables
    f <- tclVar("Logistic")
    a <- tclVar("70")
    k1 <- tclVar("0.1")
    k2 <- tclVar("0.9")
    b1 <- tclVar("0.1")
    b2 <- tclVar("0.09")
    c1 <- tclVar("10")
    c2 <- tclVar("50")

    replot <- function(...) {
        fun <- as.character(tclObj(f))
        a <- as.numeric(tclObj(a))
        k1 <- as.numeric(tclObj(k1))
        k2 <- as.numeric(tclObj(k2))
        b1 <- as.numeric(tclObj(b1))
        b2 <- as.numeric(tclObj(b2))
        c1 <- as.numeric(tclObj(c1))
        c2 <- as.numeric(tclObj(c2))

        plot(time, Wa, xlab = "Time", ylab = "Water absorption (%)")
        if (fun == "Peleg") {
            curve(peleg(x, k1, k2), add = TRUE, col = "red")
        } else if (fun == "Logistic") {
            curve(logi(x, a, b1, c1), add = TRUE, col = "red")
        } else if (fun == "Logistic.adapted") {
            curve(logi2(x, a, b1, c1, b2, c2), add = TRUE, col = "red")
        } else if (fun == "Peleg.adapted") {
            curve(peleg2(x, k1, k2, b2, c2), add = TRUE, col = "red")
        }
    }
    redraw <- function(...) {
        fun <- as.character(tclObj(f))
        if (fun == "Peleg") {
            # state of parameters: a, k1, k2, b1, b2, c1, c2
            states <- c("disabled", "normal", "normal", "disabled", "disabled", "disabled", "disabled")
            tkfocus(entry.k1)
            tkfocus(entry.k2)
        } else if (fun == "Logistic") {
            states <- c("normal", "disabled", "disabled", "normal", "disabled", "normal", "disabled")
            tkfocus(entry.a)
            tkfocus(entry.b1)
            tkfocus(entry.c1)
        } else if (fun == "Logistic.adapted") {
            states <- c("normal", "disabled", "disabled", "normal", "normal", "normal", "normal")
            tkfocus(entry.a)
            tkfocus(entry.b1)
            tkfocus(entry.b2)
            tkfocus(entry.c1)
            tkfocus(entry.c2)
        } else if (fun == "Peleg.adapted") {
            states <- c("disabled", "normal", "normal", "disabled", "normal", "disabled", "normal")
            tkfocus(entry.k1)
            tkfocus(entry.k2)
            tkfocus(entry.b2)
            tkfocus(entry.c2)
        }
        tkconfigure(entry.a, state = states[1])
        tkconfigure(ts1, state = states[1]) 
        tkconfigure(entry.k1, state = states[2])
        tkconfigure(ts2, state = states[2]) 
        tkconfigure(entry.k2, state = states[3])
        tkconfigure(ts3, state = states[3])  
        tkconfigure(entry.b1, state = states[4])
        tkconfigure(ts4, state = states[4])  
        tkconfigure(entry.b2, state = states[5])
        tkconfigure(ts5, state = states[5])  
        tkconfigure(entry.c1, state = states[6])
        tkconfigure(ts6, state = states[6])  
        tkconfigure(entry.c2, state = states[7])
        tkconfigure(ts7, state = states[7])  
        replot()
    } 

    base <- tktoplevel()
    tkwm.title(base, "Seed Soaking Kinetics")
    spec.frm <- tkframe(base, borderwidth = 2)
    left.frm <- tkframe(spec.frm)
    right.frm <- tkframe(spec.frm) 

    # parameter: a
    frame1 <- tkframe(left.frm, relief = "groove", borderwidth = 2)
    tkpack(tklabel(frame1, text = "Parameters"), fill = "both", side = "top")
    entry.a <- tkentry(frame1, width = "6", textvariable = a)
    tkpack(ts1 <- tkscale(frame1, label = "a", command = replot, 
        from = 0, to = 100, showvalue = 70, variable = a, 
        resolution = 1, orient = "horiz", relief = "groove"), 
        fill = "both", expand = 1, padx = 3, pady = 2, ipadx = 3, 
        ipady = 2, side = "left")
    tkpack(entry.a, fill = "none", side = "right")

    # models
    frame2 <- tkframe(right.frm, relief = "groove", borderwidth = 2)
    tkpack(tklabel(frame2, text = "Models"))
    for (i in c("Peleg", "Logistic", "Logistic.adapted", "Peleg.adapted")) {
        tmp <- tkradiobutton(frame2, command = redraw, text = i, 
            value = i, variable = f)
        tkpack(tmp, anchor = "w")
    }

    # parameter: k1
    frame3 <- tkframe(left.frm, relief = "groove", borderwidth = 2)
    entry.k1 <- tkentry(frame3, width = "6", textvariable = k1)
    tkpack(ts2 <- tkscale(frame3, label = "k1", command = replot, 
        from = 0, to = 2, showvalue = 0.1, variable = k1,
        resolution = 0.001, orient = "horiz", relief = "groove"), 
        fill = "both", expand = 1, padx = 3, pady = 2, ipadx = 3, 
        ipady = 2, side = "left")
    tkpack(entry.k1, fill = "none", side = "right")

    # parameter: k2
    frame4 <- tkframe(left.frm, relief = "groove", borderwidth = 2)
    entry.k2 <- tkentry(frame4, width = "6", textvariable = k2)
    tkpack(ts3 <- tkscale(frame4, label = "k2", 
        command = replot, from = 0, to = 0.5, showvalue = 0.09, 
        variable = k2, resolution = 0.0001, orient = "horiz", 
        relief = "groove"), fill = "none", expand = 1, padx = 3, 
        pady = 2, ipadx = 3, ipady = 2, side = "left")
    tkpack(entry.k2, side = "right")

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

    # parameter: b1
    frame6 <- tkframe(left.frm, relief = "groove", borderwidth = 2)
    entry.b1 <- tkentry(frame6, width = "6", textvariable = b1)
    tkpack(ts4 <- tkscale(frame6, label = "b1", 
        command = replot, from = -1, to = 2, showvalue = 0.1, 
        variable = b1, resolution = 0.001, orient = "horiz", 
        relief = "groove"), fill = "none", expand = 1, padx = 3, 
        pady = 2, ipadx = 3, ipady = 2, side = "left")
    tkpack(entry.b1, side = "right")

    # parameter: b2
    frame7 <- tkframe(left.frm, relief = "groove", borderwidth = 2)
    entry.b2 <- tkentry(frame7, width = "6", textvariable = b2)
    tkpack(ts5 <- tkscale(frame7, label = "b2", 
        command = replot, from = -1, to = 2, showvalue = 0.09, 
        variable = b2, resolution = 0.0001, orient = "horiz", 
        relief = "groove"), fill = "none", expand = 1, padx = 3, 
        pady = 2, ipadx = 3, ipady = 2, side = "left")
    tkpack(entry.b2, side = "right")

    # parameter: c1
    frame8 <- tkframe(left.frm, relief = "groove", borderwidth = 2)
    entry.c1 <- tkentry(frame8, width = "6", textvariable = c1)
    tkpack(ts6 <- tkscale(frame8, label = "c1", 
        command = replot, from = 0, to = 200, showvalue = 15, 
        variable = c1, resolution = 1, orient = "horiz", 
        relief = "groove"), fill = "none", expand = 1, padx = 3, 
        pady = 2, ipadx = 3, ipady = 2, side = "left")
    tkpack(entry.c1, side = "right")

    # parameter: c2
    frame9 <- tkframe(left.frm, relief = "groove", borderwidth = 2)
    entry.c2 <- tkentry(frame9, width = "6", textvariable = c2)
    tkpack(ts7 <- tkscale(frame9, label = "c2", 
        command = replot, from = 0, to = 200, showvalue = 50, 
        variable = c2, resolution = 1, orient = "horiz", 
        relief = "groove"), fill = "none", expand = 1, padx = 3, 
        pady = 2, ipadx = 3, ipady = 2, side = "left")
    tkpack(entry.c2, side = "right")

    # buttons
    OnQuit <- function() {
        tclvalue(done) <- 2
    }
    OnClear <- function() {
        assign("soakingModels.tmp", list(), envir = soakingModels.env)
        plot(time, Wa, xlab = "Time", ylab = "Water absorption (%)")
        f <- as.character(tclObj(f))
        if (f == "Peleg") {
            curve(peleg(x, k1 = 0.1, k2 = 0.9), add = TRUE, col = "red")
        } else if (f == "Logistic") {
            curve(logi(x, a = 70, b1 = 0.1, c1 = 15), add = TRUE, col = "red")
        } else if (f == "Logistic.adapted") {
            curve(logi2(x, a = 70, b = 0.1, c1 = 10, b2 = 0.09, c2 = 50), 
               add = TRUE, col = "red")
        } else if (f == "Peleg.adapted") {
            curve(peleg2(x, k1 = 0.1, k2 = 0.9, b2 = 0.09, c2 = 50), 
               add = TRUE, col = "red")
        }
    }
    OnFit <- function() {
        f <- as.character(tclObj(f))
        a <- as.numeric(tclObj(a))
        k1 <- as.numeric(tclObj(k1))
        k2 <- as.numeric(tclObj(k2))
        b1 <- as.numeric(tclObj(b1))
        b2 <- as.numeric(tclObj(b2))
        c1 <- as.numeric(tclObj(c1))
        c2 <- as.numeric(tclObj(c2))

        if (f == "Peleg") {
           out <- nls(Wa ~ peleg(x = time, k1, k2), 
              start = c(k1 = k1, k2 = k2))
        } else if (f == "Logistic") {
           out <- nls(Wa ~ logi(x = time, a, b1, c1), 
              start = c(a = a, b1 = b1, c1 = c1))
        } else if (f == "Logistic.adapted") {
           out <- nls(Wa ~ logi2(x = time, a, b1, c1, b2, c2), 
              start = c(a = a, b1 = b1, c1 = c1, b1 = b2, c2 = c2))
        } else if (f == "Peleg.adapted") {
           out <- nls(Wa ~ peleg2(x = time, k1, k2, b2, c2), 
              start = c(k1 = k1, k2 = k2, b2 = b2, c2 = c2))
        }
        beta <- coef(out)
        r2 <- round(Rsq(out)$pseudo, 4)
        r2adj <- round(Rsq(out)$adj, 4)
        se <- round(summary(out)$sigma, 6)
        tclvalue(r2Text) <- paste("R-sq: ", as.character(r2))
        tclvalue(r2adjText) <- paste("Adj. R-sq: ", as.character(r2adj))
        tclvalue(seText) <- paste("S.E.: ", as.character(se))

        assign("soakingModels.tmp", out, envir = soakingModels.env)
        plot(time, Wa, xlab = "Time", ylab = "Water absorption (%)")
        if (f == "Peleg") {
            curve(peleg(x, k1 = beta[1], k2 = beta[2]), 
               add = TRUE, col = "blue")
        } else if (f == "Logistic") {
            curve(logi(x, a = beta[1], b1 = beta[2], c1 = beta[3]), 
               add = TRUE, col = "blue")
        } else if (f == "Logistic.adapted") {
            curve(logi2(x, a = beta[1], b1 = beta[2], b2 = beta[3],
               c1 = beta[4], c2 = beta[5]), add = TRUE, col = "blue")
        } else if (f == "Peleg.adapted") {
            curve(peleg2(x, k1 = beta[1], k2 = beta[2], b2 = beta[3],
               c2 = beta[4]), add = TRUE, col = "blue")
        }
    }

    tkpack(frame1, frame2, frame3, frame4, frame5, frame6, 
        frame7, frame8, frame9, fill = "x")
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
    tkbind(entry.a, "<Return>", function() replot() )
    tkbind(entry.k1, "<Return>", function() replot() )
    tkbind(entry.k2, "<Return>", function() replot() )
    tkbind(entry.b1, "<Return>", function() replot() )
    tkbind(entry.b2, "<Return>", function() replot() )
    tkbind(entry.c1, "<Return>", function() replot() )
    tkbind(entry.c2, "<Return>", function() replot() )
    tkbind(base, "<Destroy>", function() tclvalue(done) <- 2)
    tkwait.variable(done)
    tkdestroy(base)

    # output
    fit <- get("soakingModels.tmp", envir = soakingModels.env)
    return(fit)
} 
