#!/bin/sh
sbcl --eval "(ql:quickload '(cl-scrobbler sb-introspect cl-api))" \
     --eval "(cl-api:api-gen :cl-scrobbler \"docs/index.html\")" \
     --eval "(progn (terpri) (sb-ext:quit))"
