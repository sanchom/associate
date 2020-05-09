#lang racket

(require pollen/top)
(require racket/path)
(require racket/string)
(require pollen/core)
(require pollen/tag)
(require pollen/decode)
(require txexpr)
(require pollen/setup)
(require sugar/list)
(require (for-syntax racket/syntax))
(require (submod hyphenate safe))
(require pollen/citations-mcgill)

; Some globals for managing footnotes/sidenotes.
(define note-mode "footnotes")
(define footnote-list empty)
(define factum-para-count 0)

(define show-bibliography? #f)

(define (show-bibliography)
  (set! show-bibliography? #t))

(define (is-factum?)
  (equal? (select-from-metas 'doc-type (current-metas)) "factum"))

(define (processed-title src)
  (smart-dashes (if (select-from-metas 'page-title src) (select-from-metas 'page-title src) "untitled")))

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

; This is an element that is created and applied during the
; root decode. It re-organizes the citations within a factum paragraph.
(define (factum-paragraph . content)
  ; (() () #:rest (txexpr-elements?) . ->* . txexpr-element?)
  (set! factum-para-count (+ factum-para-count 1))

  (define paragraphed-content
    (if (not (andmap txexpr? content))
        `((p ,@content))
        content))

  (define (insert-para-number num content)
    (define first-para (first content))
    (cons `(p ,(format "~a." factum-para-count) " " ,@(get-elements first-para)) (rest content)))

  (define/contract (is-citation-placeholder? tx-element)
    (txexpr-element? . -> . boolean?)
    (and (txexpr? tx-element) (equal? (attr-ref tx-element 'class #f) "citation-placeholder")))

  (define/contract (get-citation-id citation-placeholder)
    ((and/c txexpr? is-citation-placeholder?) . -> . string?)
    (attr-ref (first (get-elements citation-placeholder)) 'data-citation-id))

  (define/contract (is-whitespace-or-bib-entry? tx-element)
    (txexpr-element? . -> . boolean?)
    (or (whitespace? tx-element)
        (is-citation-placeholder? tx-element)))

  (define (is-paragraph-of-standalone-citations? tx)
    (and (equal? (get-tag tx) 'p)
         (andmap is-whitespace-or-bib-entry? (get-elements tx))))

  (define (get-list-of-citations para)
    (define/contract (extract-citation-content tx-element)
      (txexpr-element? . -> . (or/c '() list?))
      (if (is-citation-placeholder? tx-element) `(,(get-citation-id tx-element)) '()))
    (apply append (map extract-citation-content (get-elements para))))

  (define (extract-citations-from-paragraph para)
    (if (is-paragraph-of-standalone-citations? para)
        (get-list-of-citations para)
        '()))

  ; TODO: collect more of the citation info (pinpoints, judge, etc.)
  (define (collect-standalone-citations content)
    (if (empty? content)
        '()
        (append (extract-citations-from-paragraph (first content)) (collect-standalone-citations (rest content)))))

  (define/contract (strip-standalone-citations content)
    (txexpr-elements? . -> . txexpr-elements?)
    (apply append (map (λ (x) (if (is-paragraph-of-standalone-citations? x) '() `(,x))) content)))

  (define standalone-citations (collect-standalone-citations paragraphed-content))

  (define main-paragraphs-only (strip-standalone-citations paragraphed-content))

  ; Replacing in text citations with short-forms
  (define/contract (replace-citations-with-short-forms tx)
    (txexpr? . -> . txexpr?)
    (if (is-citation-placeholder? tx)
        `(span "(" ,@(short-form (get-citation-id tx)) ")")
        tx))

  (define main-paragraphs-cited
    (decode-elements main-paragraphs-only
                     #:txexpr-proc replace-citations-with-short-forms))

  ; (define (collect-citations-from-text content) ...)

  `(div [[class "factum-paragraph"]]
        ,@(insert-para-number factum-para-count main-paragraphs-cited)
        ,@(map (λ (citation-key) `(p [[class "para-note"]] ,(note-cite citation-key))) standalone-citations)))

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
                           #:txexpr-elements-proc decode-breaks-into-paras)
                          #:txexpr-elements-proc turn-elements-into-list-items
                          #:exclude-tags (setup:block-tags))))

(define (bibliography)
  (define jurisprudence-ids (bibliography-ids #:category "jurisprudence"))
  (define secondary-ids (bibliography-ids #:category "secondary"))
  (define legislation-ids (bibliography-ids #:category "legislation"))
  (define other-ids (bibliography-ids #:category "other"))
  `(@
    (div [[class "bibliography"]]
         ,(heading "Bibliography")
         ,(if (not (empty? jurisprudence-ids)) (sub-heading "Jurisprudence") "")
         ,@(map (λ (x) `(p ,(bib-entry x))) (sort jurisprudence-ids #:key (λ (x) (bib-sort-value x)) string<?))
         ,(if (not (empty? secondary-ids)) (sub-heading "Secondary materials") "")
         ,@(map (λ (x) `(p ,(bib-entry x))) (sort secondary-ids #:key (λ (x) (bib-sort-value x)) string<?))
         ,(if (not (empty? legislation-ids)) (sub-heading "Legislative materials") "")
         ,@(map (λ (x) `(p ,(bib-entry x))) (sort legislation-ids #:key (λ (x) (bib-sort-value x)) string<?))
         ,(if (not (empty? other-ids)) (sub-heading "Other materials") "")
         ,@(map (λ (x) `(p ,(bib-entry x))) (sort other-ids #:key (λ (x) (bib-sort-value x)) string<?)))))

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

  (if (is-factum?)
      (case (current-poly-target)
        [(html md pdf docx) `(span [[class "citation-placeholder"]] ,@content)])
      (case (current-poly-target)
        [(html) `(span [[class "sidenote-wrapper"]]
                       (a [[href ,(format "#fn-~a" footnote-number)] [class "undecorated"]]
                          (span [[class "sidenote-number"] [id ,(format "fn-source-~a" footnote-number)]])))]
        [(md pdf docx) `(txt "^[" ,@transformed-content "]")])))

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

(define/contract (decode-paragraphs elements-in
                                    [paragraph-separator "\n\n"]
                                    [maybe-wrap-proc 'p]
                                    #:linebreak-proc [linebreak-proc decode-linebreaks]
                                    #:force? [force-paragraph #f])
  ((txexpr-elements?) (string?
                       (or/c txexpr-tag? ((listof xexpr?) . -> . txexpr?))
                       #:linebreak-proc (txexpr-elements? . -> . txexpr-elements?)
                       #:force? boolean?)
                      . ->* . txexpr-elements?)

  (define (prep-paragraph-flow elems)
    (linebreak-proc (merge-newlines (trimf elems whitespace?))))

  (define (paragraph-break? x)
    (define paragraph-pattern (pregexp (format "^~a+$" paragraph-separator)))
    (match x
      [(pregexp paragraph-pattern) #true]
      [_ #false]))

  (define (explicit-or-implicit-paragraph-break? x)
    (or (paragraph-break? x) (block-txexpr? x)))

  (define wrap-proc (match maybe-wrap-proc
                      [(? procedure? proc) proc]
                      [_ (λ (elems) (list* maybe-wrap-proc elems))]))

  (define (wrap-paragraph elems)
    (match elems
      [(list (? block-txexpr?) ...) elems] ; leave a series of block xexprs alone
      [_ (list (wrap-proc elems))])) ; otherwise wrap in p tag

  (define elements (prep-paragraph-flow elements-in))
  (if (ormap explicit-or-implicit-paragraph-break? elements) ; need this condition to prevent infinite recursion
      ;; use `append-map` on `wrap-paragraph` rather than `map` to permit return of multiple elements
      (append-map wrap-paragraph
                  (append-map (λ (es) (filter-split es paragraph-break?)) (slicef elements block-txexpr?))) ; split into ¶¶, using both implied and explicit paragraph breaks
      (if force-paragraph
          ;; upconverts non-block elements to paragraphs
          (append-map wrap-paragraph (slicef elements block-txexpr?))
          elements)))

; Ignores single line breaks in paragraph interpretation. They are
; converted to spaces. But, double-breaks demarcate simple paragraphs.
; In a factum, triple-breaks demarcate paragraphs for numbering and
; para-note purposes.
(define (decode-breaks-into-paras elements)
  (define (replace-factum-paragraph-with-div tx)
    (if (and (equal? (get-tag tx) 'factum-paragraph) (not (empty? (get-elements tx))))
        (apply factum-paragraph (get-elements tx))
        tx))
  (define (decode-simple-paragraphs elements)
    (decode-paragraphs elements "\n\n"
                       #:linebreak-proc (λ (x) (decode-linebreaks x '" "))))
  (if (is-factum?)
      (decode-elements (decode-paragraphs elements "\n\n\n" 'factum-paragraph
                                          #:linebreak-proc (λ (x) (decode-linebreaks x '"\n")))
                       #:txexpr-proc replace-factum-paragraph-with-div
                       #:txexpr-elements-proc decode-simple-paragraphs)
      (decode-paragraphs elements "\n\n"
                         #:linebreak-proc (λ (x) (decode-linebreaks x '" ")))))

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

(define (optionally-add-bibliography tx)
  (txexpr (get-tag tx) (get-attrs tx) `(,@(get-elements tx) ,(if show-bibliography? (bibliography) '(div)))))

; This step is used when actually rendering .md or when
; as an intermediate step before pandoc takes this to .pdf
; or .docx.
(define (flatten-citations-into-md tx)
  (define (is-citation? tx)
    (and (txexpr? tx)
         (attrs-have-key? tx 'class)
         (string-contains? (attr-ref tx 'class) "bibliography-entry")))
  (if (is-citation? tx)
      (if (is-factum?)
          (case (current-poly-target)
            [(md) `(txt "        " ,@(get-elements tx))]
            [(pdf docx) `(txt ,(apply string-append (map (λ (x) "&nbsp;") (range 10))) ,@(get-elements tx))])
          `(txt ,@(get-elements tx)))
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
    [(span) `(txt ,@(get-elements tx))]
    [(div) `(txt "\n\n" ,@(get-elements tx))]
    [else tx]))

(define (root . elements)
  ; This two-level decode is necessary because some of the processing requires paragraphs to be
  ; formed in order to have strings and sidenote-wrappers as txexpr elements.
  (case (current-poly-target)
    [(html) (decode (txexpr 'root empty (get-elements
                                         (decode (if (is-factum?) (txexpr 'root empty elements) (optionally-add-bibliography
                                                                                                 (add-html-footnotes (txexpr 'root empty elements))))
                                                 #:exclude-tags '(pre)
                                                 #:txexpr-proc (compose1 custom-hyphenation show-necessary-short-forms)
                                                 ; Double line breaks create new paragraphs. Single line breaks are ignored.
                                                 #:txexpr-elements-proc (compose1 decode-breaks-into-paras)
                                                 #:string-proc (compose1 smart-quotes smart-dashes))))
                    #:exclude-tags '(pre)
                    #:txexpr-proc insert-sidenote-commas)]
    [(md pdf docx) (decode (txexpr 'root empty (get-elements 
                                                (decode (optionally-add-bibliography (txexpr 'root empty elements))
                                                        #:exclude-tags '(pre)
                                                        #:txexpr-proc (compose1 strip-pre-placeholders flatten-short-forms-into-md show-necessary-short-forms)
                                                        ; Double line breaks create new paragraphs. Single line breaks are ignored.
                                                        #:txexpr-elements-proc (compose1 decode-breaks-into-paras)
                                                        #:string-proc (compose1 smart-quotes smart-dashes))))
                           #:txexpr-proc (compose1 convert-other-elements-to-md flatten-citations-into-md))]))

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
