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

(define mb-entities '(area artist event instrument label place recording release release-group series work url))

(define user-agent-header "guile-musicbrainz/0.1.0 (steve@ayerh.art)")
(define accept-header '((application/xml)))

;;;(define* (lookup type mbid #:optional (inc '()))
;;;  (let ((type (symbol->string type))
;;;        (inc (map symbol->string inc)))
;;;    (mb-entity-response->entity
;;;     (mb-request (list type) mbid inc))))

(define-record-type <mb-life-span>
  (make-mb-life-span begin end ended?)
  mb-life-span?
  (begin life-span-begin)
  (end life-span-end)
  (ended? life-span-ended?))

(define-record-type <mb-artist>
  (make-mb-artist mbid name sort-name type area begin-area country date-span ipi isni aliases disambiguation annotation)
  mb-artist?
  (mbid artist-mbid)
  (name artist-name)
  (sort-name artist-sort-name)
  (type artist-type)
  (area artist-area)
  (begin-area artist-begin-area)
  (country artist-country)
  (date-span artist-date-span)
  (ipi artist-ipi)
  (isni artist-isni)
  (aliases artist-aliases)
  (disambiguation artist-disambiguation)
  (annotation artist-annotation))

(set-record-type-printer!
 <mb-artist>
 (λ (mb-artist port)
   (format port "#<<mb-artist> mbid: ~a>" (artist-mbid mb-artist))))

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

(define (mb-entity-response->entity sxml)
  (define (parse-life-span life-span)
    (sxml-match life-span
                [(mb:life-span
                  (mb:begin ,begin))
                 (make-mb-life-span begin #f #f)]
                [(mb:life-span
                  (mb:begin ,begin)
                  (mb:end ,end))
                 (make-mb-life-span begin end #t)]
                [,otherwise #f]))
  (define (parse-artist artist)
    (display "A")
    (newline)
    (sxml-match artist
                [(mb:artist
                  (@ (id ,mbid)
                     (type-id ,type-id)
                     (type ,type))
                  (mb:name ,name)
                  (mb:sort-name ,sort-name)
                  . ,rest)
                 (list mbid name)]))
                                        ;
                                        ;                  (mb:disambiguation ,disambiguation)
                                        ;                  (mb:gender (@ (id ,gender-id)) ,gender)
                                        ;                  (mb:country ,country)
                                        ;                  (mb:area (@ (id ,area-id))
                                        ;                           (mb:name ,area-name)
                                        ;                           (mb:sort-name ,area-sort-name)
                                        ;                           (mb:iso-3166-1-code-list
                                        ;                            (mb:iso-3166-1-code ,iso-code)))
                                        ;                  (mb:begin-area (@ (id ,begin-area-id))
                                        ;                                 (mb:name ,begin-area-name)
                                        ;                                 (mb:sort-name ,begin-area-sort-name))
                                        ;                  (mb:life-span
                                        ;                   (mb:begin ,life-span-begin)))
                                        ;                 (make-mb-artist mbid name sort-name type #f #f #f #f #f #f disambiguation #f)]))

  (define (parse-entity entity)
    (display "E")
    (newline)
    (display entity)
    (newline)
    (sxml-match entity
                [(mb:artist ,[parse-artist -> artist])
                 artist]

                [(mb:release-group (@ (id ,id) (type-id ,type-id) (type ,type))
                                   (mb:title ,title)
                                   (mb:disambiguation ,disamb)
                                        ;                                   (mb:first-release-date ,first-release-date)
                                   . ,rest)
                                        ;                                   (mb:title ,title)
                                        ;                                   (mb:first-release-data ,first-release-date)
                 (list id type)]

                [,otherwise (error "no matching mb entity")]))

  (sxml-match sxml
              [(*TOP*
                (*PI* . ,pi)
                (mb:metadata
                 ,[parse-entity -> entity]))
               entity]))
