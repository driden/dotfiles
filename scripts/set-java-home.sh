#!/usr/bin/env bash

set-java-home() {
    local version
    version=$(ls $HOME/.local/share/mise/installs/java/ | fzf)

    sudo mkdir -p "/Library/Java/JavaVirtualMachines/$version"
    sudo ln -s "$HOME/.local/share/mise/installs/java/$version/Contents" "/Library/Java/JavaVirtualMachines/$version/Contents"

    echo "Linked: $version"
    /usr/libexec/java_home
}
