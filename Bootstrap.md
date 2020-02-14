**CAVEAT:** `checkout -f` is destructive
```console
git init --separate-git-dir="${HOME}/.netsettings"
git remote add hmenke git@henrimenke.com:public/netsettings.git
git fetch hmenke master
git checkout -f master
git submodule update --init
rm .git
```
To restore the `.git` file, run
```console
echo "gitdir: ${HOME}/.netsettings" > .git
```

The method below seems to have difficulties with fetching submodules, but also with the first method submodules seem to be a source of issues.
```console
git clone --bare git@henrimenke.com:public/netsettings.git ~/.netsettings
git --git-dir="${HOME}/.netsettings" --work-tree="${HOME}" checkout -f master
git --git-dir="${HOME}/.netsettings" --work-tree="${HOME}" submodule update --init
```