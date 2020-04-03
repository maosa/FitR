#!/bin/bash

echo -e '\nBacking up...\n'

cd /Users/maosa/fitr

git add --all
git commit -m 'minor updates and backup'
git push

cd

cp -r /Users/maosa/fitr/* /Users/maosa/Desktop/programming/my_projects/fitr/

echo -e 'Deploying app...\n'

Rscript /Users/maosa/fitr/deploy_fitr.R

echo -e '\nFitR deployed!\n'

echo -e 'Done!\n'
