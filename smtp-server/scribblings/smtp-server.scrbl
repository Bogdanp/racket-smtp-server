#lang scribble/manual

@(require (for-label net/smtp-server
                     openssl
                     racket/base
                     racket/contract
                     racket/string
                     racket/os))

@title{SMTP Server}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]
@defmodule[net/smtp-server]

This module module provides a minimal implementation of RFC 5321
@cite{RFC5321} that can receive e-mail messages.

@section{Usage}

@racketblock[
  (define stop
    (start-smtp-server println))
]

The example above starts an SMTP server on @tt{localhost} port
@racket[25] that prints all incoming e-mail to standard out.  Calling
the @racket[stop] function terminates any connections in flight and
stops the server.

@(define repo-link
   (link "https://github.com/Bogdanp/racket-smtp-server" "source code repository"))

See "example/" in the @repo-link for an example with @tt{STARTTLS} support.


@section{Reference}

@defthing[tls-encode-proc/c (-> input-port?
                                output-port?
                                #:mode 'tcp
                                #:encrypt 'tls
                                #:close-original? #t
                                (values input-port? output-port?))]{

  The contract for TLS-encoding procedures.  See also
  @racket[ports->ssl-ports].
}

@defparam[current-smtp-hostname hostname non-empty-string? #:value (gethostname)]{
  Controls the host name displayed to clients.
}

@defstruct[envelope ([sender bytes?]
                     [recipients (listof bytes?)]
                     [data bytes?])]{

  Represents the sender, recipients and contents of a receieved e-mail.
}

@defproc[(start-smtp-server [handler (-> envelope? void?)]
                            [#:host host string? "127.0.0.1"]
                            [#:port port (integer-in 0 65535) 25]
                            [#:limits lim smtp-limits? (make-smtp-limits)]
                            [#:tls-encode tls-encode (or/c #f tls-encode-proc/c) #f]) (-> void?)]{

  Starts an SMTP server that listens on @racket[host] and
  @racket[port] and returns a function that will stop the server when
  applied.

  Successfully-received e-mails are passed to @racket[handler].  When
  the @racket[handler] raises an exception, the server notifies the
  client that the message has been rejected.

  The @racket[#:limits] arguments can be used to customize various
  @tech{security limits}.

  If the optional @racket[#:tls-encode] argument supplies a
  @racket[tls-encode-proc/c] value, the server advertises
  @tt{STARTTLS} support and clients may opt in to TLS encryption.
}

@deftogether[(
  @defproc[(smtp-limits? [v any/c]) boolean?]
  @defproc[(make-smtp-limits [#:max-connections max-connections exact-positive-integer? 512]
                             [#:max-line-length max-line-length exact-nonnegative-integer? 1024]
                             [#:max-envelope-length max-envelope-length exact-nonnegative-integer? (* 10 1024 1024)]
                             [#:session-timeout session-timeout (and/c number? positive?) 300]) smtp-limits?]
)]{
  @deftech{Security limits} allow you to configure various
  security-related limits on an SMTP server.

  The @racket[#:max-connections] argument controls the maximum number
  of concurrent client connections that the server will accept at a
  time.

  The @racket[#:max-line-length] argument controls the maximum length
  in bytes of each line received from a client may be.  The server
  will reject lines longer than this amount.

  The @racket[#:max-envelope-length] argument controls the maximum
  length of incoming e-mails from clients.  The total length of an
  envlope includes the length in bytes of the sender and the
  recipients list as well as the message data.

  The @racket[#:session-timeout] argument controls the maximum amount
  of time, in seconds, that a client session may be open for.
}


@bibliography[
  @bib-entry[
    #:key "RFC5321"
    #:title "Simple Mail Transfer Protocol"
    #:author "J. Klensin"
    #:date "2008"
    #:url "https://www.ietf.org/rfc/rfc5321.html"
  ]
]
