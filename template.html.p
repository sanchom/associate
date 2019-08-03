<!DOCTYPE html>
<!--
◊|template-message|
-->

<html>
  <head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>◊(processed-title metas)</title>
    <link rel="stylesheet" type="text/css" href="../site-style.css" />
</head>
  <body ◊when/splice[(need-right-margin? doc)]{class="print-with-right-margin"}>
<article>
    ◊(->html doc)
</article>

  </body>
</html>
