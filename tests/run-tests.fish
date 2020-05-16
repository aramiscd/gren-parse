#!/usr/bin/env fish

gren make src/Main.gren --output=run-tests
and node run-tests
rm run-tests
