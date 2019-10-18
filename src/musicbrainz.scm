(define-module (musicbrainz)
  #:use-module (oop goops)
  #:use-module (sxml simple)
  #:use-module (sxml ssax)
  #:use-module ((sxml xpath) #:prefix sxml:)
  #:use-module (sxml match)
  #:use-module (web uri)
  #:use-module (web client)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 receive))

(define mb-ws-host "musicbrainz.org")
(define mb-ws-path '("ws" "2"))
(define mb-ws-scheme 'https)
(define mb-ws-namespace "http://musicbrainz.org/ns/mmd-2.0#")

(define mb-entities '(area artist event instrument label place recording release release-group series work url))

(define user-agent-header "guile-musicbrainz/0.1.0 (steve@ayerh.art)")
(define accept-header '((application/xml)))

(define-class <mb-sxml-class> (<class>))

(define-class <mb-sxml> ()
  sxml
  #:metaclass <mb-sxml-class>)

(define-method (initialize (class <mb-sxml>) init-args)
  (let* ((entity-class-name ((compose symbol->string class-name class-of) class))
         (entity (regexp-substitute #f (string-match "<mb-(.*)>" entity-class-name 1))))
    (let-keywords init-args #t ((id #f))
                  (when id
                    (slot-set! class 'sxml (mb-lookup entity id)))
                  (next-method))))

                                        ; TODO: check if sxml is unbound instead of if sxpath being set
(let ((slot-value 'unbound))
(define-method (compute-get-n-set (class <mb-sxml-class>) slot)
  (let-keywords (slot-definition-options slot) #t ((sxpath #f))
                (if sxpath
                    (let ((sxpath `(mb:metadata ,@sxpath *text*)))
                      (list (lambda (instance)
                              (if (slot-bound? instance 'sxml)
                                  (car ((sxml:sxpath sxpath) (slot-ref instance 'sxml)))
                                  (if (eq? slot-value 'unbound)
                                      (slot-unbound class instance (slot-definition-name slot))
                                      slot-value)))
                            (lambda (instance val)
                              (if (slot-bound? instance 'sxml)
                                  #f
                                  (slot-set! instance (slot-definition-name slot) val)))))
                    (next-method)))))

                                        ; TODO: add more slots
(define-class <mb-artist> (<mb-sxml>)
  (id #:accessor mbid #:sxpath '(mb:artist @ id))
  (type #:accessor type #:sxpath '(mb:artist @ type))
  (type-id #:accessor type-id #:sxpath '(mb:artist @ type-id))
  (name #:accessor name #:sxpath '(mb:artist mb:name)))

(define (sanitize lst)
  (map (λ (v)
         (cond ((string? v) v)
               ((symbol? v) (symbol->string v))
               (else (error "must be a symbol or a string"))))
       lst))

(define* (build-mb-uri parts inc)
  (let* ((parts (sanitize parts))
         (inc (sanitize inc))
         (path-parts (append mb-ws-path parts))
         (path (string-append "/" (string-join path-parts "/"))))
    (build-uri
     mb-ws-scheme
     #:query (string-append "inc=" (string-join inc "+"))
     #:host mb-ws-host
     #:path path)))

(define* (mb-request parts inc)
  (let ((headers `((accept . ,accept-header)
                   (user-agent . ,user-agent-header))))
    (receive (response body)
        (http-get
         (build-mb-uri parts inc)
         #:streaming? #t
         #:headers headers)
      ;;; TODO: inspect response? adjust rate limiting? etc.
      (call-with-port
       body
       (λ (p)
         (xml->sxml p #:namespaces `((mb . ,mb-ws-namespace))))))))

(define* (mb-lookup entity mbid #:optional (inc '()))
  (mb-request (list entity mbid) inc))

;;;deniro f01846dc-1585-401c-a46a-d0b3a824114a
