.. _contributing:

Contributing Code
=================

Zelig is an open source project, so we are always looking for contributors. The latest Zelig code-base can be found in our github repo, `IQSS\Zelig5 <https://github.com/IQSS/Zelig5/>`_, and can be cloned locally using git:

::

    git clone https://github.com/IQSS/Zelig5.git

To contribute code first fork the repo and create a branch:

::

    git clone https://github.com/YOUR_USERNAME/Zelig5.git
    git checkout -b my_feature

Then add edited code, push edited code to your fork and submit a pull request to the IQSS repo (i.e., upstream repo):

::
    git add... # stage the files you modified or added
    git commit... # commit the modified or added files
    git push origin my_feature

To keep your branch up-to-date with changes in the upstream repo, set an upstream remote, pull changes from the upstream repo and reabase against the desired branch:

::

    git remote add upstream https://github.com/eventdata/phoenix_piepline.git
    git fetch upstream
    git rebase upstream/development

**Note: You should always issue pull requests to the development branch**. Be sure to also include detailed commit messages with your pull request. For more information on git see `git
documentation <http://git-scm.com/documentation>`_. Github tutorials can be found via simple google search.



 