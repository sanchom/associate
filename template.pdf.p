◊(local-require racket/list racket/file racket/system)
◊(define pandoc-source ◊string-append{
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

                         ◊(apply string-append (filter string? (flatten doc)))})

◊(define working-directory
   (make-temporary-file "pollen-markdown-work-~a" 'directory))
◊(define temp-md-path (build-path working-directory "temp.md"))
◊(define temp-pdf-path (build-path working-directory "temp.pdf"))
◊(define latex-template-path (build-path (current-project-root) "article-template.latex"))
◊(display-to-file pandoc-source temp-md-path #:exists 'replace)
◊(define command (format "pandoc ~a --template ~a -o ~a" temp-md-path latex-template-path temp-pdf-path))
◊(unless (system command) (error "pandoc: rendering error"))
◊(let ([pdf (file->bytes temp-pdf-path)])
   (delete-directory/files working-directory)
   pdf)