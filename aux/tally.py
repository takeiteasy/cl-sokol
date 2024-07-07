#/usr/bin/env python3
import sys
import json

tally = {}
for line in sys.stdin:
    key = line.rstrip("\n")
    if not key in tally:
        tally[key] = 1
    else:
        tally[key] += 1

print(json.dumps(tally))
