#!/bin/bash

# find all sub-directories with "test" in the name and zip them

#find . -name "*test*" -type d -exec tar czvf {}.tar.gz {} \; \;
find . -name "*test*" -type d -exec zip -r -9 -m {}.zip {} \;