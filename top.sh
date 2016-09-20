#!/bin/sh

~/.local/bin/matchmaking input/na0.html > input/na
~/.local/bin/matchmaking input/na1.html >> input/na
~/.local/bin/matchmaking input/na2.html >> input/na
~/.local/bin/matchmaking input/na3.html >> input/na
~/.local/bin/matchmaking input/na4.html >> input/na

echo 'NA players (should be 500):'
< input/na sort | uniq | wc

~/.local/bin/matchmaking input/eu0.html > input/eu
~/.local/bin/matchmaking input/eu1.html >> input/eu
~/.local/bin/matchmaking input/eu2.html >> input/eu
~/.local/bin/matchmaking input/eu3.html >> input/eu
~/.local/bin/matchmaking input/eu4.html >> input/eu

echo 'EU players (should be 500):'
< input/eu sort | uniq | wc

echo 'Total (should be 1000):'
cat input/na input/eu | sort | uniq | wc
