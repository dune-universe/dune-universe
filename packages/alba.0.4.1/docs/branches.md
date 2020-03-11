# Branches

Branches in the albatross repository:

- master: The most stable not yet released version.

- develop: Stable version. Passes all tests. Master is a predecessor of
           develop, therefore develop can be merged into master by fast
           forward. History usually not changed. History might be changed if
           the merge of a feature branch does not pass all the tests. In that
           case the head of the branch can be reset to its previous fully
           tested state.

- fb_xxxx: Feature branch. Might be untested, even uncompilable. Is
           blacklisted in `.travis.yml`. A feature branch is always based on a
           stable commit of the develop branch. Pushing to the repo is done
           just for backup. No history is guarantteed. Before merging a
           feature branch into the branch develop, it has to be rebased onto
           the branch develop. This is neccessary if the develop branch has
           already moved away from the commit the feature branch is based on.

- test_xxx: Based on the same commit of the branch develop as the feature
            branch fb_xxx.




# Git Commands

    # create a branch
    git checkout -b new_branch
    git branch -c new_branch

    # create a new branch fb_xxx on origin an make the local branch track it.
    git push -u origin fb_xxx

    # delete branches
    git branch -D fb_xxx             # local branch (not yet fully merged)
    git push origin --delete db_xxx  # remote branch

    # rebase (assume you are on a feature branch)
    git rebase develop

    # make 1 commit out of n commits
    git reset develop         # undo commits since it branched off develop
    git reset --soft develop  # the same, but changes are kept in the index
    git commit ...
    # Note: If the feature branch has been pushed to origin and --soft has
    #       been chosen, then a git pull reestablishes the remote backup


<!---
Local Variables:
mode: outline
coding: iso-latin-1
outline-regexp: "#+"
End:
-->
