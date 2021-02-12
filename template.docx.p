◊(local-require racket/list racket/file racket/system)
◊(define pandoc-source ◊string-append{
                         ---
                         title: ◊(select 'title here)
                         author: ◊(select 'author here)
                         ---

                         ◊(apply string-append (filter string? (flatten doc)))})

◊(define working-directory
   (make-temporary-file "pollen-markdown-work-~a" 'directory))
◊(define temp-md-path (build-path working-directory "temp.md"))
◊(define temp-docx-path (build-path working-directory "temp.docx"))
◊(define reference-doc-path (build-path (current-project-root) "reference-doc.docx"))
◊(display-to-file pandoc-source temp-md-path #:exists 'replace)
◊(define command (format "pandoc --reference-doc=~a ~a -o ~a" reference-doc-path temp-md-path temp-docx-path))
◊(unless (system command) (error "pandoc: rendering error"))
◊(let ([docx (file->bytes temp-docx-path)])
   (delete-directory/files working-directory)
   docx)