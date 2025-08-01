#!/usr/bin/env fish

gren make Main --output=run-tests
and node run-tests
rm run-tests
