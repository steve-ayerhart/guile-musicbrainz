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

(define-class <mb-entity> ()
  (mbid #:accessor mbid #:init-keyword #:mbid #:init-value #f))

(define-method (display (self <mb-entity>) port)
  (format port "#<<mb-entity> ~a>" (mbid self)))

(define-method (write (self <mb-entity>) port)
  (format port "#<<mb-entity> ~a>" (mbid self)))

(define-class <mb-artist> (<mb-entity>)
  (name #:accessor name #:init-keyword #:name)
  (sort-name #:accessor sort-name #:init-keyword #:sort-name)
  (type #:accessor type #:init-keyword #:type)
  (area #:accessor area #:init-keyword #:area)
  (date-span #:accessor  date-span #:init-keyword #:ate-span)
  (ipi #:accessor ipi #:init-keyword #:pi)
  (isni #:accessor isni #:init-keyword #:sni)
  (aliases #:accessor aliases #:init-keyword #:liases)
  (disambiguation #:accessor disambiguation #:init-keyword #:isambiguation)
  (annotation #:accessor annotation #:init-keyword #:nnotation))

(define-method (mb-entity-name (self <mb-entity>))
  (let ((class-name ((compose symbol->string class-name class-of) self)))
    (regexp-substitute #f (string-match "<mb-([a-z]+)>" class-name) 1)))

(define mb-ws-host "musicbrainz.org")
(define mb-ws-path '("ws" "2"))
(define mb-ws-scheme 'https)
(define mb-ws-namespace "http://musicbrainz.org/ns/mmd-2.0#")

(define user-agent-header "guile-musicbrainz/0.1.0 (steve@ayerh.art)")
(define accept-header '((application/xml)))

(define (build-mb-uri parts)
  (display parts)
  (let* ((path-parts (append mb-ws-path parts))
         (path (string-append "/" (string-join path-parts "/"))))
    (build-uri
     mb-ws-scheme
     #:host mb-ws-host
     #:path path)))

(define (mb-request . parts)
  (let ((headers `((accept . ,accept-header)
                   (user-agent . ,user-agent-header))))
    (receive (response body)
        (http-get
         (build-mb-uri parts)
         #:streaming? #t
         #:headers headers)
      ; TODO: inspect response? adjust rate limiting? etc.
      (call-with-port
       body
       (λ (p)
         (xml->sxml p #:namespaces `((mb . ,mb-ws-namespace))))))))

(define-method (lookup (entity <mb-entity>))
  (mb-entity-response->entity
   (mb-request
    (mb-entity-name entity)
    (mbid entity))))

(define-method (lookup (entity <mb-entity>) (inc <list>))
  (lookup entity))

(define deniro
  (make <mb-artist>
    #:mbid "f01846dc-1585-401c-a46a-d0b3a824114a"))

(define (mb-entity-response->entity sxml)
  (define (parse-artist artist)
    (sxml-match artist
                [(mb:artist (@ (id ,mbid)
                               (type-id ,type-id)
                               (type ,type))
                            (mb:name ,name)
                            (mb:sort-name ,sort-name) . ,rest)
                (make <mb-artist>
                  #:mbid mbid
                  #:name name
                  #:sort-name sort-name)]))
  (define (parse-entity entity)
    (sxml-match entity
                [,[parse-artist -> artist] artist]))
  (sxml-match sxml
              [(*TOP*
                (*PI* . ,pi)
                (mb:metadata
                 ,[parse-entity -> entity]))
                entity]))
