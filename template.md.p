◊(local-require racket/list)
◊(apply string-append `("\uFEFF"))

◊(apply string-append (filter string? (flatten doc)))