#!/bin/bash

if ! brew -v;  then
  # Install brew if not exist
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi

if ! python3 --version; then
  brew install python3
fi

# Backup
mkdir ~/.old_config || cp -Rf ~/.config ~/.old_config 

cp ~/.SpaceVim.d ~/.old_config/
cp -Rf ~/.config ~/.old_config
cp -Rf ~/.doom.d ~/.old_config



# Application install

# Kitty
curl -L https://sw.kovidgoyal.net/kitty/installer.sh | sh /dev/stdin

# Cfiles - simple cli file manager
brew tap mananapr/cfiles && brew install cfiles

# tmux
brew install tmux

# GoTop
brew tap cjbassi/gotop
brew install gotop

# Yaba
brew tap koekeishiya/formulae
brew install koekeishiya/formulae/yabai
sudo yabai --load-sa
brew services start yabai

# skhd
brew install koekeishiya/formulae/skhd
brew services start skhd


# ZSH
brew install zsh
# Oh my zsh
sh -c "$(wget -O- https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
# Plugins
wget https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh &&
    sh install.sh && git clone https://github.com/zsh-users/zsh-completions ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/zsh-completions && git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions

# Install simple bar
git clone https://github.com/Jean-Tinland/simple-bar $HOME/Library/Application\ Support/Übersicht/widgets/simple-bar
cp CustomStyle.js /Library/Application\ Support/Übersicht/widgets/simple-bar/lib/syles/
# Copy configs
cp -Rf .config/* ~/.config/
cp .zshrc ~/.zshrc

# Install fonts
cp ./fonts/* /Library/Fonts/

# Tmux
ln -sv ~/.config/tmux/tmux.conf ~/.tmux.conf

# Vim
curl -sLf https://spacevim.org/install.sh | bash
cp -Rf ~/.SpaceVim.d ~/

# Emacs
brew tap d12frosted/emacs-plus
brew install emacs-plus@28 --with-xwidgets --with-modern-papirus-icon --with-no-titlebar
# Doom
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install

cp -Rf ~/.doom.d ~/
~/.emacs.d/bin/doom sync

# Markdown support for emacs
brew install pandoc
