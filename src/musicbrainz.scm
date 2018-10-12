(define-module (musicbrainz)
  #:use-module (sxml simple)
  #:use-module (sxml ssax)
  #:use-module (sxml xpath)
  #:use-module (sxml match)
  #:use-module (web uri)
  #:use-module (web client)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 receive))

(define mb-ws-host "musicbrainz.org")
(define mb-ws-path '("ws" "2"))
(define mb-ws-scheme 'https)
(define mb-ws-namespace "http://musicbrainz.org/ns/mmd-2.0#")

(define user-agent-header "guile-musicbrainz/0.1.0 (steve@ayerh.art)")
(define accept-header '((application/xml)))

;(define* (lookup type mbid #:optional (inc '()))
;  (let ((type (symbol->string type))
;        (inc (map symbol->string inc)))
;    (mb-entity-response->entity
;     (mb-request (list type) mbid inc))))

;(define-record-type <mb-artist>
;  (make-mb-artist name sort-name type area date-span ipi isni aliases disambiguation annotation)
;  mb-artist?
;  (name artist-name)
;  (sort-name artist-sort-name)
;  (type artist-type)
;  (area artist-area)
;  (date-span artist-date-span)
;  (ipi artist-ipi)
;  (isni artist-isni)
;  (aliases artist-aliases)
;  (annotation artist-annotation))

(define-record-type <mb-artist>
  (make-mb-artist mbid name sort-name)
  mb-artist?
  (mbid artist-mbid)
  (name artist-name)
  (sort-name artist-sort-name))


(define* (build-mb-uri parts inc)
  (let* ((path-parts (append mb-ws-path parts))
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
      ; TODO: inspect response? adjust rate limiting? etc.
      (call-with-port
       body
       (λ (p)
         (xml->sxml p #:namespaces `((mb . ,mb-ws-namespace))))))))

;(define deniro
;  (make <mb-artist>
;    #:mbid "f01846dc-1585-401c-a46a-d0b3a824114a"))

(define (mb-entity-response->entity sxml)
  (define (parse-artist artist)
    (sxml-match artist
                [(mb:artist
                  (@ (id ,mbid)
                     (type-id ,type-id)
                     (type ,type))
                  (mb:name ,name)
                  (mb:sort-name ,sort-name) . ,rest)
                (make-mb-artist
                  mbid
                  name
                  sort-name)]))

  (define (parse-entity entity)
    (sxml-match entity
                [,[parse-artist -> artist] artist]))

  (sxml-match sxml
              [(*TOP*
                (*PI* . ,pi)
                (mb:metadata
                 ,[parse-entity -> entity]))
                entity]))
