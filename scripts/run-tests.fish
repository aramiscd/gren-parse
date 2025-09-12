#!/usr/bin/env fish

cd tests
npx gren make Main --output=run-tests
and node run-tests
