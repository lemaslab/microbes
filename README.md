FAQ

Notes on how to push to github

How to pull down from Dom's repo 
- Go to dominick's github profile
- Open the repo that you want. Clone it. 
- Repeat steps above.

How to push to Dom's repo   
- save changes to files
- switch cd
- git add --all
- git commit -m "some note"
- git push
- go online and use github to do a pull request.
- wait for dom to approve

sequence of events
- make change locally to repo files that are branched 
- from master repo (ex. keyword added to cats function)
- push to branched repo
- log on to github. create pull request.

** person that is admin for master/branch then must accept pull request.

How to install from GitHub   
- library(devtools)
- install_github("atomczik/microbes")
- library(microbes)

pull from edited form when you already have a version 
- git pull upstream master

To push from local to remote version 
- git add --all
- git commit -m "whatever comment you want"
- git push

To push to Dominck 
- git push upstream
- git push upstream master

To check locally and build manual (in command prompt)
- R CMD check microbes
- R CMD build microbes
- R CMD INSTALL microbes
