#!/usr/bin/env fish

cd tests
gren make Main --output=run-tests
and node run-tests
rm run-tests
