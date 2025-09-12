#!/usr/bin/env fish

cd tests
and npx gren make Main --output=run-tests
and node run-tests
