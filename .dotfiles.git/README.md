# Setup from existing system
from https://harfangk.github.io/2016/09/18/manage-dotfiles-with-a-git-bare-repository.html
and https://www.atlassian.com/git/tutorials/dotfiles

```
git init --bare $HOME/.dotfiles.git
echo 'alias dotfiles="/usr/bin/git --git-dir=$HOME/.dotfiles.git/ --work-tree=$HOME"' >> $HOME/.bashrc
source ~/.bashrc
dotfiles config --local status.showUntrackedFiles no
```

# Use

move things to `~/.dotfiles.git`, symlink back, add, commit, push
```
mv /some/dir/.file ~/.dotfiles.git
ln -s ~/.dotfiles.git/.file ~/.emacs.d
dotfiles status
dotfiles add some.file
dotfiles commit -m "some message"
dotfiles remote add origin https://www.github.com/whyfarer/dotfiles.git
dotfiles push origin master
```

# Installing to another system
if needed, use different branches for different machines

```
echo 'alias dotfiles="/usr/bin/git --git-dir=$HOME/.dotfiles.git/ --work-tree=$HOME"' >> $HOME/.bashrc
source ~/.bashrc
echo ".dotfiles.git" >> .gitignore
git clone --bare https://www.github.com/whyfarer/dotfiles.git $HOME/.dotfiles.git
dotfiles checkout
dotfiles config --local status.showUntrackedFiles no
```

