# Linking files
cd ~/.local/bin
ln -s ~/code/dotfiles/.local/bin/export-pdf .

## USING STOW
stow --target=/Users/driden  .

# Fix for zsh insecure directories 

```
compaudit | xargs chmod g-w
```

