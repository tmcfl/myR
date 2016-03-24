# myR
A place to update and store my R configuration, and record packages downloaded from Github.
- .RProfile
- packages_from_github.R

Excellent idea from https://github.com/tonyfischetti. 
Here's [Tony's blog post](http://www.onthelambda.com/2014/09/17/fun-with-rprofile-and-customizing-r-startup/) 
explaining how he uses custom R configurations.

***

This relies on the R_PROFILE_USER environment variable to be set to source the custom config

`git clone https://github.com/tmcfl/myR.git ~/.myR/`

Create a shell alias to use R with your special configuration, like this:

`alias aR="R_PROFILE_USER=~/.myR/aR.profile R"`
