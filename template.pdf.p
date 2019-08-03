◊(local-require racket/list racket/file racket/system)
◊(define pandoc-source
   ◊(case (select-from-metas 'doc-type metas)
      [("article") ◊string-append{
                                  ---
                                  header-includes:
                                  - \hypersetup{colorlinks=true,
                                                               allbordercolors={0 0 0},
                                                               pdfborderstyle={/S/U/W 1}}
                                  title: ◊(select 'title here)
                                  author:
                                  - ◊(select 'author here)
                                  linestretch: 1.3
                                  fontfamily: charter
                                  ---
                                  ◊(apply string-append (filter string? (flatten doc)))}]
      [("factum") ◊string-append{
                                 ---
                                 numbersections: true
                                 linestretch: 2
                                 fontfamily: charter
                                 fullpage: true
                                 toc: true
                                 factum_title: true
                                 court: ◊(select-from-metas 'court metas)
                                 appeal_from: ◊(select-from-metas 'on-appeal-from metas)
                                 appellant_name: ◊(select-from-metas 'appellant-name metas)
                                 respondent_name: ◊(select-from-metas 'respondent-name metas)
                                 appellant_counsel:
                                 - family_name: My
                                   first_name: Name
                                 - family_name: Other
                                   first_name: Name
                                 respondent_counsel:
                                 - family_name: Another
                                   first_name: Person
                                 - family_name: Random
                                   first_name: Counsel
                                 appellant: true
                                 respondent: false
                                 ...

                                 ◊(apply string-append (filter string? (flatten doc)))}]
      [else (error "invalid doc-type")]))

◊(define working-directory
   (make-temporary-file "pollen-markdown-work-~a" 'directory))
◊(define temp-md-path (build-path working-directory "temp.md"))
◊(define temp-pdf-path (build-path working-directory "temp.pdf"))
◊(define latex-template-path (build-path (current-project-root) (case (select-from-metas 'doc-type metas)
                                                                  [("article") "article-template.latex"]
                                                                  [("factum") "factum-template.latex"])))
◊(display-to-file pandoc-source temp-md-path #:exists 'replace)
◊(define command (format "pandoc ~a --template ~a -o ~a" temp-md-path latex-template-path temp-pdf-path))
◊(unless (system command) (error "pandoc: rendering error"))
◊(let ([pdf (file->bytes temp-pdf-path)])
   (delete-directory/files working-directory)
   pdf)