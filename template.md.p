◊(local-require racket/list)
◊(apply string-append `("\uFEFF"))
---
header-includes:
  - \hypersetup{colorlinks=true,
            allbordercolors={0 0 0},
            pdfborderstyle={/S/U/W 1}}
---

◊(apply string-append (filter string? (flatten doc)))