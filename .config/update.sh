echo 'Removing current nvim directory...'
rm -rf ./nvim
echo 'Copyin nvim directory from ~/.config/nvim...'
cp -r ~/.config/nvim nvim

echo 'Removing current tmux directory...'
rm -rf ./tmux
echo 'Copyin tmux directory from ~/.config/tmux...'
cp -r ~/.config/tmux tmux
