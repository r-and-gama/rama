#!/bin/bash
sed -i '' -e 's/<\/head>/<link rel="shortcut icon" type="image\/x-icon" href="favicon.ico">\'$'\n<\/head>/' docs/index.html
