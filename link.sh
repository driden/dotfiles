#!/bin/sh

stow --dotfiles --stow --target="/Users/$(whoami)" .
