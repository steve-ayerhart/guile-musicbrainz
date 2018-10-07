(define-module (musicbrainz)
  #:use-module (sxml simple)
  #:use-module (sxml ssax)
  #:use-module (sxml xpath)
  #:use-module (sxml match)
  #:use-module (oop goops)
  #:use-module (web uri)
  #:use-module (web client)
  #:use-module (rnrs io ports)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 receive))

(define-class <mb-resource> ()
  (mbid #:accessor mbid #:init-keyword mbid))

(define-class <mb-artist> (<mb-resource>)
  (name #:accessor name #:init-keyword name)
  (sort-name #:accessor sort-name)
  (type #:accessor type #:init-keyword type)
  (area #:accessor area #:init-keyword area)
  (date-span #:accessor  date-span #:init-keyword date-span)
  (ipi #:accessor ipi #:init-keyword ipi)
  (isni #:accessor isni #:init-keyword isni)
  (aliases #:accessor aliases #:init-keyword aliases)
  (disambiguation #:accessor disambiguation #:init-keyword disambiguation)
  (annotation #:accessor annotation #:init-keyword annotation))

(define-method (mb-entity-name (self <mb-resource>))
  (let ((class-name ((compose symbol->string class-name class-of) self)))
    (regexp-substitute #f (string-match "<mb-([a-z]+)>" class-name) 1)))

(define mb-ws-host "musicbrainz.org")
(define mb-ws-path "/ws/2/")
(define mb-ws-scheme 'http)

(define user-agent-header "guile-musicbrainz/0.1.0 (steve@ayerh.art)")
(define accept-header '((application/xml)))

(define* (mb-lookup resource #:optional (inc #f))
  (let ((headers `((accept . ,accept-header)
                   (user-agent . ,user-agent-header)))
        (resource-path (string-append
                        mb-ws-path
                        (mb-entity-name resource)
                        "/"
                        (mbid resource))))
    (receive (response body)
        (http-get
         (build-uri mb-ws-scheme
                    #:host mb-ws-host
                    #:path resource-path)
         #:streaming? #t
         #:headers headers)
      ; TODO: inspect response? adjust rate limiting?
      (call-with-port body
                      (λ (p) (xml->sxml p #:namespaces '((mb . "http://musicbrainz.org/ns/mmd-2.0#"))))))))

(define deniro (make <mb-artist>))
(set! (mbid deniro) "f01846dc-1585-401c-a46a-d0b3a824114a")

(define (mb-artist-response->mb-artist sxml)
  (sxml-match sxml
              [(*TOP*
                (*PI* . ,pi)
                (mb:metadata
                 (mb:artist (@ . ,artist-attr)
                  (mb:name ,name)
                  (mb:sort-name ,sort-name)
                  .
                  ,artist-more)
                  .
                  ,metadata-more))
                 (list name sort-name)]))
