**CAVEAT:** `checkout -f` is destructive
```console
mkdir -pv ~/.local/share/netsettings
git init --separate-git-dir="${HOME}/.local/share/netsettings/git"
git remote add hmenke git@henrimenke.de:henri/netsettings.git
git fetch hmenke master
git checkout -f master
git config git-crypt.repoStateDir .config/git-crypt
git-crypt unlock # optional
rm .git
```
To restore the `.git` file, run
```console
echo "gitdir: $XDG_DATA_HOME/netsettings/git" > .git
```

The method below seems to have difficulties with fetching submodules, but also with the first method submodules seem to be a source of issues.
```console
git clone --bare git@henrimenke.de:henri/netsettings.git ~/.local/share/netsettings/git
git --git-dir="${HOME}/.local/share/netsettings/git" --work-tree="${HOME}" checkout -f master
git --git-dir="${HOME}/.local/share/netsettings/git" --work-tree="${HOME}" submodule update --init
```
