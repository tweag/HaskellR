HaskellR website
================

This branch contains the source for the HaskellR website, deployed at
https://tweag.github.io/HaskellR. To test locally,

~~~
$ gem install jekyll
$ jekyll serve --baseurl ''
~~~

To update ruby package versions recorded in `Gemfile.lock`:

~~~
$ bundle update
$ nix-shell -p bundix
[nix-shell:docs] $ bundix -l
~~~
