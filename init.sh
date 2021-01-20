#!/bin/bash

if ! brew -v;  then
  # Install brew if not exist
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi

if ! python3 --version; then
  brew install python3
fi

if ! node --version; then
    brew install node
fi


# Backup
mkdir ~/.old_config || cp -rf ~/.config ~/.old_config

cp ~/.SpaceVim.d ~/.old_config/
cp -rf ~/.config ~/.old_config
cp -rf ~/.doom.d ~/.old_config



# Application install

brew install wget

# Kitty
curl -L https://sw.kovidgoyal.net/kitty/installer.sh | sh /dev/stdin

# Cfiles - simple cli file manager
# brew tap mananapr/cfiles && brew install cfiles
# Ranger - another file manager for cli
brew install ranger

# tmux
brew install tmux

# GoTop
brew tap cjbassi/gotop
brew install gotop

# Yabai
brew tap koekeishiya/formulae
brew install koekeishiya/formulae/yabai
sudo yabai --load-sa
brew services start yabai

# skhd
brew install koekeishiya/formulae/skhd
brew services start skhd

# ranger zip/unzip
brew install atool
# ZSH
brew install zsh
rm -rf ~/.oh-my-zsh
# Oh my zsh
sh -c "$(wget -O- https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
# Plugins
brew install zplug
brew install softmoth/zsh-vim-mode
wget https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh && sh install.sh

git clone https://github.com/zsh-users/zsh-completions ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/zsh-completions
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git $ZSH_CUSTOM/plugins/zsh-syntax-highlighting

# Install simple bar
git clone https://github.com/Jean-Tinland/simple-bar $HOME/Library/Application\ Support/Übersicht/widgets/simple-bar
cp CustomStyle.js /Library/Application\ Support/Übersicht/widgets/simple-bar/lib/syles/
# Copy configs
cp -rf .config/* ~/.config/
cp .zshrc ~/.zshrc

# Install fonts
cp ./fonts/* /Library/Fonts/

# Ranger icons
git clone https://github.com/alexanderjeurissen/ranger_devicons ~/.config/ranger/plugins/ranger_devicons
echo "default_linemode devicons" >> $HOME/.config/ranger/rc.conf


ln -sv ~/.config/kitty ~/Library/Preferences/kitty
# Tmux
ln -sv ~/.config/tmux/tmux.conf ~/.tmux.conf

# Vim
brew install neovim
curl -sLf https://spacevim.org/install.sh | bash

sudo cp -rf .SpaceVim.d/* ~/.SpaceVim.d/
sudo chown -R $(whoami) ~/.cache
pip3 install pynvim

# Emacs
brew tap d12frosted/emacs-plus
brew install emacs-plus@28 --with-xwidgets --with-modern-papirus-icon
# Doom
git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install

cp -rf .doom.d/* ~/.doom.d/
~/.emacs.d/bin/doom sync

# Markdown support for emacs
brew install pandoc

# Ripgrep
brew install ripgrep

# Wakatime
sudo pip3 install wakatime
pip install pillow ranger-fm
