#lang racket

(require racket/path)
(require racket/string)
(require pollen/core)
(require pollen/tag)
(require pollen/decode)
(require txexpr)
(require pollen/setup)
(require (for-syntax racket/syntax))
(require (submod hyphenate safe))
(require pollen/citations-mcgill)

; Some globals for managing footnotes/sidenotes.
(define note-mode "footnotes")
(define footnote-list empty)

(define (processed-title src)
  (smart-dashes (select-from-metas 'page-title src)))

; Simple replacements or tag aliases.
(define elide "[…]")

(define ellipsis "…")

(define (nbsp)
  (string->symbol "nbsp"))

(define (non-breaking-hyphen)
  (string->number "8209"))

(define (lozenge)
  (string->symbol "loz"))

; Basic semantic elements.
; ---------------------------------------------------
(define (fig #:src src #:width [width #f] . caption)
  `(figure
    (img (,(when/splice width `[class "specified-width"]) ,(when/splice width `[width ,width])[potential-feature "potential-feature"][src ,src]))
    (figcaption ,@caption)
    ))

(define (title . content)
  `(h1 ((hyphens "none")) ,@content))

(define (subtitle . content)
  `(p ((class "subtitle") (hyphens "none")) ,@content))

(define (heading . content)
  `(h2 ((hyphens "none")) ,@content))

(define (sub-heading . content)
  `(h3 ((hyphens "none")) ,@content))

; Quotation
(define (q . content)
  `(blockquote ,@content))

; Some code styling
(define (codeblock . content)
  `(blockquote [[class "code"]] (pre [[class "code"]] ,@content)))

(define (tt . content)
  `(span [[class "code"]] ,@content))

; Explicit list annotation. First, detects double-line-breaks to
; create top-level block elements, then turns top-level elements
; within the itemize tag into list items. Excludes block-tags to avoid
; decending recursively into these and adding spurious list tags.
(define (itemize . elements)
  ; Surrounds every top-level element in this list with a list tag, but
  ; replaces naked p tags with li directly to avoid (li (p "text")).
  (define (turn-elements-into-list-items elements)
    (map (λ (x) (if (equal? (get-tag x) 'p) `(li ,@(get-elements x)) `(li ,x)))
         elements))
  `(ul ,@(decode-elements (decode-elements
                           elements
                           #:txexpr-elements-proc decode-double-breaks-into-paras)
                          #:txexpr-elements-proc turn-elements-into-list-items
                          #:exclude-tags (setup:block-tags))))

(define (bibliography)
  `(@
    (h1 "Bibliography")
    ,@(map (λ (x) `(p ,(bib-entry x))) (sort (bibliography-ids) #:key (λ (x) (bib-sort-value x)) string<?))))

; Interaction with the citation system in citation-system.rkt
; ------------------------------------------------------------

; Aliases to simplify citations that use signals.
; The "cite" call is this system's interaction with the citation system.
; Don't mess around with what is returned by "cite". The citation system
; is expecting certain elements and attributes to linger, in order to
; optionally transform them later on.
(define-syntax (define-signal stx)
  (syntax-case stx ()
    [(_ SIGNAL TEXT)
     (with-syntax ([NOTE-SIGNAL (format-id #'SIGNAL "note-~a" #'SIGNAL)])
       #'(begin
           (define (SIGNAL id #:pinpoint [pinpoint #f] #:parenthetical [parenthetical #f] #:judge [judge #f] #:speaker [speaker #f] #:terminal [terminal "."])
             (cite id #:pinpoint pinpoint #:parenthetical parenthetical #:judge judge #:speaker speaker #:signal TEXT #:terminal terminal))
           (define (NOTE-SIGNAL id #:pinpoint [pinpoint #f] #:parenthetical [parenthetical #f] #:judge [judge #f] #:speaker [speaker #f])
             (note (SIGNAL id #:pinpoint pinpoint #:parenthetical parenthetical #:judge judge #:speaker speaker))))
       )]))

(define-signal see "See")
(define-signal see-also "See also")
(define-signal but-see "But see")
(define-signal see-eg "See e.g.")
(define-signal see-generally "See generally")

(define (note-cite id #:pinpoint [pinpoint #f] #:parenthetical [parenthetical #f] #:judge [judge #f] #:speaker [speaker #f] #:signal [signal #f])
  (note (cite id #:pinpoint pinpoint #:parenthetical parenthetical #:judge judge #:speaker speaker #:signal signal)))

; Defines a little sidenote or footnote (depending on the mode), numbered, and by default collapsed
; to a small height. In print, these are all footnotes.
(define (note #:expanded [expanded #f] . content)
  (define footnote-number (+ 1 (length footnote-list)))

  ; Letting the citation system do its thing.
  (define transformed-content
    (decode-elements content
                     #:txexpr-proc (λ (x) (transform-cite-in-a-note x footnote-number))))

  (set! footnote-list
        (append footnote-list (list `(p ([class "footnote"] [id ,(format "fn-~a" footnote-number)])
                                        ,(format "~a. " footnote-number) (a [[href ,(format "#fn-source-~a" footnote-number)] [class "backlink undecorated"]] " ↑ ") ,@transformed-content))))
  (define refid (format "fn-~a" footnote-number))
  (define subrefid (format "fn-~a-expand" footnote-number))

  (case (current-poly-target)
    [(html) `(span [[class "sidenote-wrapper"]]
                   (a [[href ,(format "#fn-~a" footnote-number)] [class "undecorated"]]
                      (span [[class "sidenote-number"] [id ,(format "fn-source-~a" footnote-number)]])))]
    [(md pdf docx) `(txt "^[" ,@transformed-content "]")]))

; Helpers for decoding
; ------------------------------------------------------------

; Custom hyphenation that doesn't break URLs.
(define (custom-hyphenation x)
  (define allowed-capitalized-hyphenations
    (list "Atmos-View"))
  (define non-breakable-capitalized? (λ (word) (let ([letter (substring word 0 1)])
                                                 (and (equal? letter (string-upcase letter))
                                                      (not (ormap (λ (hy) (equal? (string-replace hy "-" "") word)) allowed-capitalized-hyphenations))))))
  (define (ligs? word)
    (ormap (λ (lig) (regexp-match lig word))
           '("ff" "fi" "fl" "ffi" "ffl")))
  (define (omission-test tx)
    (and (attrs-have-key? tx 'hyphens)
         (equal? (attr-ref tx 'hyphens) "none")))
  (define hyphenation-exceptions
    `("navcanada"
      "auto-nom-ous-ly"
      ,@allowed-capitalized-hyphenations))
  (hyphenate x
             #:exceptions hyphenation-exceptions
             #:omit-txexpr omission-test
             #:omit-word (λ (x) (or (non-breakable-capitalized? x) (ligs? x)))))

; Ignores single line breaks in paragraph interpretation. They are
; converted to spaces. But, double-breaks demarcate paragraphs.
(define (decode-double-breaks-into-paras elements)
  (decode-paragraphs elements
                     #:linebreak-proc (λ (x) (decode-linebreaks x '" "))))

; Insert commas between successive sidenotes.
(define (insert-sidenote-commas tx)
  (define (is-trigger-triple? x y z)
    (and (is-sidenote-wrapper? x)
         (string? y)
         (equal? (string-trim y) "")
         (is-sidenote-wrapper? z)))
  (define (is-trigger-double? x y)
    (and (is-sidenote-wrapper? x)
         (is-sidenote-wrapper? y)))
  (define elements (get-elements tx))
  (define (is-sidenote-wrapper? tx)
    (and (txexpr? tx)
         (attrs-have-key? tx 'class)
         (equal? (attr-ref tx 'class) "sidenote-wrapper")))
  (txexpr (get-tag tx) (get-attrs tx)
          (let loop ([result empty]
                     [elements elements])
            (if (empty? elements) ; If only zero items.
                result
                (if (empty? (cdr elements)) ; If only one item in elements.
                    (append result elements)
                    (let ([x (car elements)]
                          [y (cadr elements)])
                      (if (empty? (cddr elements)) ; If only two items in elements.
                          ; If they're both span.sidenote-wrapper, put the first one plus a comma into
                          ; results, then recurse, otherwise, just put the first one into results and
                          ; recurse.
                          (if (is-trigger-double? x y)
                              (loop (append result (list x '(span [[class "sidenote-comma"]] ","))) (cdr elements))
                              (loop (append result (list x)) (cdr elements)))
                          ; Otherwise, there are at least three items in elements; check whether the first two
                          ; are successive sidenotes, or whether the three together are a sequence like:
                          ; (sidenote whitespace sidenote).
                          (let ([z (caddr elements)])
                            (if (is-trigger-double? x y)
                                (loop (append result (list x '(span [[class "sidenote-comma"]] ","))) (cdr elements))
                                (if (is-trigger-triple? x y z)
                                    (loop (append result (list x '(span [[class "sidenote-comma"]] ","))) (cddr elements))
                                    (loop (append result (list x)) (cdr elements))))))))))))

(define (add-html-footnotes tx)
  (txexpr (get-tag tx) (get-attrs tx) `(,@(get-elements tx) (div ((class "endnotes")) ,(when/splice (not (empty? footnote-list)) (heading "Notes")) ,@footnote-list))))

(define (flatten-citations-into-md tx)
  (define (is-citation? tx)
    (and (txexpr? tx)
         (attrs-have-key? tx 'class)
         (string-contains? (attr-ref tx 'class) "bibliography-entry")))
  (if (is-citation? tx)
      `(txt ,@(get-elements tx))
      tx))

(define (flatten-short-forms-into-md tx)
  (define (is-short-form? tx)
    (and (txexpr? tx)
         (attrs-have-key? tx 'data-short-form-placeholder)))
  (if (is-short-form? tx)
      `(txt ,@(get-elements tx))
      tx))

(define (strip-pre-placeholders tx)
  (define (is-pre-placeholder? tx)
    (and (txexpr? tx)
         (attrs-have-key? tx 'data-short-form-pre-placeholder)))
  (if (is-pre-placeholder? tx)
      ""
      tx))

(define (convert-other-elements-to-md tx)
  (case (get-tag tx)
    [(p) `(txt "\n\n" ,@(get-elements tx))]
    [(em) `(txt "*" ,@(get-elements tx) "*")]
    [(h1) `(txt "\n\n# " ,@(get-elements tx))]
    [(h2) `(txt "\n\n## " ,@(get-elements tx))]
    [(h3) `(txt "\n\n### " ,@(get-elements tx))]
    [(a) `(txt "[" ,@(get-elements tx) "](" ,(attr-ref tx 'href) ")")]
    [(blockquote) `(txt "\n\n> " ,@(get-elements tx))]
    [else tx]))

(define (root . elements)
  ; This two-level decode is necessary because some of the processing requires paragraphs to be
  ; formed in order to have strings and sidenote-wrappers as txexpr elements.
  (case (current-poly-target)
    [(html) (decode (txexpr 'root empty (get-elements
                                         (decode (add-html-footnotes (txexpr 'root empty elements))
                                                 #:exclude-tags '(pre)
                                                 #:txexpr-proc (compose1 custom-hyphenation show-necessary-short-forms)
                                                 ; Double line breaks create new paragraphs. Single line breaks are ignored.
                                                 #:txexpr-elements-proc (compose1 decode-double-breaks-into-paras)
                                                 #:string-proc (compose1 smart-quotes smart-dashes))))
                    #:exclude-tags '(pre)
                    #:txexpr-proc insert-sidenote-commas)]
    [(md pdf docx) (decode (txexpr 'root empty (get-elements 
                                                                  (decode (txexpr 'root empty elements)
                                                                          #:exclude-tags '(pre)
                                                                          #:txexpr-proc (compose1 strip-pre-placeholders flatten-short-forms-into-md flatten-citations-into-md show-necessary-short-forms)
                                                                          ; Double line breaks create new paragraphs. Single line breaks are ignored.
                                                                          #:txexpr-elements-proc (compose1 decode-double-breaks-into-paras)
                                                                          #:string-proc (compose1 smart-quotes smart-dashes))))
                      #:txexpr-proc (compose1 convert-other-elements-to-md))]))

(provide declare-work)
(provide format-work)
(provide cite)
(provide (all-defined-out))

(module setup racket/base
  (require racket/path)
  (require racket/string)
  (define (omitted-path? path)
    (or (equal? (path->string (file-name-from-path path)) ".travis.yml")
        (equal? (path->string (file-name-from-path path)) "cars.xml")
        (string-suffix? (path->string (file-name-from-path path)) "template.html")
        (string-suffix? (path->string (file-name-from-path path)) "~")))
  (define poly-targets '(html md pdf docx))
  (provide (all-defined-out)))