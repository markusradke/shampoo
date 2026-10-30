send_notification <- function(subject, body) {
  #' Log a notification and send it by email if the NOTIFICATION_* environment variables are set (see `analysis/config.R`). A failed send only gives a warning, so a long run never stops because of it.
  machine <- getOption("shampoo.machine", Sys.info()[["nodename"]])
  subject <- sprintf("[SHAMPOO %s %s] %s", machine, RUN_TAG, subject)
  message(sprintf("%s\n%s", subject, body))
  smtp <- as.list(Sys.getenv(c(
    "NOTIFICATION_SMTP_URL",
    "NOTIFICATION_SMTP_USER",
    "NOTIFICATION_SMTP_PASSWORD",
    "NOTIFICATION_MAIL_TO"
  )))
  names(smtp) <- c("url", "user", "password", "to")
  if (any(smtp == "")) {
    return(invisible(FALSE))
  }
  mail <- paste0(
    "From: ",
    smtp$user,
    "\r\n",
    "To: ",
    smtp$to,
    "\r\n",
    "Subject: ",
    subject,
    "\r\n",
    "Content-Type: text/plain; charset=UTF-8\r\n\r\n",
    gsub("\r?\n", "\r\n", enc2utf8(body))
  )
  tryCatch(
    {
      curl::send_mail(
        mail_from = smtp$user,
        mail_rcpt = smtp$to,
        message = mail,
        smtp_server = smtp$url,
        username = smtp$user,
        password = smtp$password,
        verbose = FALSE
      )
      invisible(TRUE)
    },
    error = function(error) {
      warning(sprintf(
        "Sending the notification failed: %s",
        conditionMessage(error)
      ))
      invisible(FALSE)
    }
  )
}

format_table <- function(data) {
  #' Format a data frame as plain text for a notification.
  paste(
    capture.output(print(as.data.frame(data), row.names = FALSE, digits = 3)),
    collapse = "\n"
  )
}
