RdTextFilter <-  function (ifile, encoding = "unknown", keepSpacing = TRUE, drop = character(), 
    keep = character(), macros = file.path(R.home("share"), "Rd", 
        "macros", "system.Rd")) 
{
    if (inherits(ifile, "srcfile")) 
        ifile <- ifile$filename
    if (inherits(ifile, "Rd")) {
        srcrefs <- sapply(ifile, function(s) attr(s, "srcref"))
        p <- ifile[order(srcrefs[1, ], srcrefs[2, ])]
        class(p) <- class(ifile)
    }
    else p <- parse_Rd(ifile, encoding = encoding, macros = macros)
    tags <- RdTags(p)
    if ("\\encoding" %in% tags) {
        encoding <- p[[which.max(tags == "\\encoding")]][[1L]]
        if (encoding %in% c("UTF-8", "utf-8", "utf8")) 
            encoding <- "UTF-8"
        if (!inherits(ifile, "Rd")) 
            p <- parse_Rd(ifile, encoding = encoding, macros = macros)
    }
    else encoding <- ""
    mycon <- textConnection("myval", open = "w", local = TRUE, 
        encoding = "UTF-8")
    on.exit(close(mycon))
    mycat <- function(...) cat(..., file = mycon)
    prevline <- 1L
    prevcol <- 0L
    doPartialMarkup <- function(x, tags, i) {
        result <- FALSE
        if (i < length(tags) && tags[i + 1L] == "TEXT" && length(x[[i]]) == 
            1L && tags[i] %in% c("\\bold", "\\emph", "\\strong", 
            "\\link") && !(tags[i] %in% drop) && RdTags(x[[i]]) == 
            "TEXT") {
            text1 <- x[[i]][[1L]]
            if (length(grep("[^[:space:]]$", text1))) {
                text2 <- x[[i + 1L]]
                if (length(grep("^[^[:space:]]", text2))) {
                  show(text1)
                  prevcol <<- prevcol + 1L
                  saveline <- prevline
                  show(text2)
                  if (prevline == saveline) 
                    prevcol <<- prevcol - 1L
                  result <- TRUE
                }
            }
        }
        result
    }
    show <- function(x) {
        srcref <- attr(x, "srcref")
        firstline <- srcref[1L]
        lastline <- srcref[3L]
        firstcol <- srcref[5L]
        lastcol <- srcref[6L]
        tag <- attr(x, "Rd_tag")
        if (is.null(tag)) 
            tag <- "NULL"
        if (tag %in% drop) 
            tag <- "DROP"
        else if (tag %in% keep) 
            tag <- "KEEPLIST"
        switch(tag, KEEP = , TEXT = {
            if (prevline < firstline) {
                prevcol <<- 0L
                mycat(rep.int("\n", if (keepSpacing) firstline - 
                  prevline else 1L))
            }
            if (keepSpacing) mycat(rep.int(" ", firstcol - prevcol - 
                1L), sep = "")
            x <- as.character(srcref)
            mycat(x, sep = "")
            prevcol <<- lastcol
            prevline <<- lastline
        }, `\\S3method` = , `\\S4method` = , `\\command` = , 
            `\\docType` = , `\\email` = , `\\encoding` = , `\\file` = , 
            `\\keyword` = , `\\link` = , `\\linkS4class` = , 
            `\\method` = , `\\pkg` = , `\\var` = , DROP = {
            }, `\\tabular` = , `#ifdef` = , `#ifndef` = {
                show(x[[2L]])
            }, `\\item` = {
                if (length(x) == 2L) show(x[[2L]])
            }, {
                if (is.list(x)) {
                  tags <- RdTags(x)
                  i <- 0L
                  while (i < length(x)) {
                    i <- i + 1L
                    if (doPartialMarkup(x, tags, i)) i <- i + 
                      1L else show(x[[i]])
                  }
                } else if (tag == "KEEPLIST") {
                  attr(x, "Rd_tag") <- "KEEP"
                  show(x)
                }
            })
    }
    show(p)
    mycat("\n")
    out <- textConnectionValue(mycon)
    if (encoding == "latin1") 
        out <- iconv(out, "UTF-8", "latin1")
    out
}
