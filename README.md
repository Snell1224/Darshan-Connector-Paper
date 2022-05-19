Short paper about the Darshan-LDMS Integration for HPCMASPA.
 
Please use paper.lock to indicate if you are checking out a section of the paper to write on. 

Before commiting, please test in an environment compatible with the SNL latex template, e.g:

```
ssh cee-compute005
git clone git@gitlab-ex.sandia.gov:app-sys-fusion/fy21_milestone_paper.git 
cd fy21_milestone_paper
module load sems-env
module load sems-tex
% edit files after taking section lock in paper.lock
make clean && make (until this passes, do not proceed to add/commit steps)
git add/git commit as needed
git push
```
