#!/bin/sh

stow --dotfiles --delete --target="/Users/$(whoami)" .
