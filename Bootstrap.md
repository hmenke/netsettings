**CAVEAT:** `checkout -f` is destructive

With keys available:
```console
cd ~
mkdir -pv ~/.local/share/netsettings
git init --separate-git-dir="${HOME}/.local/share/netsettings/git"
git remote add hmenke git@henrimenke.de:henri/netsettings.git
git fetch hmenke master
git checkout -f master
git config git-crypt.repoStateDir .local/share/netsettings/git-crypt
git-crypt unlock # optional
rm .git
chmod go-rwx ~/.gnupg ~/.gnupg/gpg.conf ~/.ssh ~/.ssh/config
```
To restore the `.git` file, run
```console
echo "gitdir: $XDG_DATA_HOME/netsettings/git" > .git
```

With keys unavailable:
```console
cd ~
mkdir -pv ~/.local/share/netsettings
git init --separate-git-dir="${HOME}/.local/share/netsettings/git"
git remote add hmenke https://git.henrimenke.de/henri/netsettings.git
git fetch hmenke master
git checkout -f master
git config git-crypt.repoStateDir .local/share/netsettings/git-crypt
rm .git
chmod go-rwx ~/.gnupg ~/.gnupg/gpg.conf ~/.ssh ~/.ssh/config
```
