path=$(pwd)

mkdir -p ~/.xmonad
mkdir -p ~/.vim/bundle/
mkdir -p ~/.vim/autoload/

ln -fs $path/.feh.sh   ~/.feh.sh
ln -fs $path/vim/vimrc ~/.vimrc
ln -fs $path/.xinitrc  ~/.xinitrc
ln -fs $path/.xmobarrc ~/.xmobarrc
ln -fs $path/xmonad.hs ~/.xmonad/xmonad.hs

ln -fs $path/vim/bundles/* ~/.vim/bundle/
ln -fs $path/vim/autoload/pathogen.vim/autoload/pathogen.vim ~/.vim/autoload/pathogen.vim
