## Author: Ingo Feinerer

readMail <- structure(
function(DateFormat = "%d %B %Y %H:%M:%S")
{
    stopifnot(is.character(DateFormat))

    format <- DateFormat
    function(elem, language, id) {
        mail <- elem$content

        ## The header is separated from the body by a blank line.
        ## http://en.wikipedia.org/wiki/E-mail#Internet_e-mail_format
        for (index in seq_along(mail))
            if (identical(mail[index], "")) break

        header <- mail[1:index]
        content <- mail[(index + 1):length(mail)]

        author <- gsub("From: ", "",
                       grep("^From:", header, value = TRUE, useBytes = TRUE))
        datetimestamp <-
            strptime(gsub("Date: ", "",
                          grep("^Date:", header, value = TRUE, useBytes = TRUE)),
                     format = format,
                     tz = "GMT")
        
        mid.key <- "^Message-ID:"
        mid <- gsub("Message-ID: ", "",
                    grep(mid.key, header, value = TRUE, useBytes = TRUE))
        if (!length(mid)) {
            mid.key <- "^Message-Id:"
            mid <- gsub("Message-Id: ", "",
                        grep(mid.key, header, value = TRUE, useBytes = TRUE))
            if (!length(mid)) {
                mid.key <- "^Message-id:"
                mid <- gsub("Message-id: ", "",
                            grep(mid.key, header, value = TRUE, useBytes = TRUE))
            }
        }
        
        ## if mid is empty string, then the actual Message-ID is given in the subsequent line
        if (identical(mid, "")) {
            mid <- trimws(header[[grep(mid.key, header, value = FALSE, useBytes = TRUE) + 1]])
        }    
        
        origin <- gsub("Newsgroups: ", "",
                       grep("^Newsgroups:", header, value = TRUE, useBytes = TRUE))
        heading <- gsub("Subject: ", "",
                        grep("^Subject:", header, value = TRUE, useBytes = TRUE))

        MailDocument(content, author, datetimestamp, character(0), header, heading,
                     if (length(mid)) mid[1] else id, language, origin)
    }
}, class = c("FunctionGenerator", "function"))
