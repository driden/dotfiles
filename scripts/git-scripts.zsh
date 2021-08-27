local fzf="fzf --border --margin=1 --layout=reverse --padding=1"

delbranch() {
  local branches branch
  branches=$(git for-each-ref --count=30 --sort=-committerdate refs/heads/ --format="%(refname:short)") &&
  branch=$(echo "$branches" | eval ${fzf} --multi) &&
  git branch -D $(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")
}

gcheckout() {
  local branches branch
  branches=$(git for-each-ref --count=30 --sort=-committerdate refs/heads/ --format="%(refname:short)") &&
  branch=$(echo "$branches" | eval $fzf ) &&
  git checkout $(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")
}

add-files() {
  local changed_files files
  selected_files=$(git status -s | awk '{print $2}' | eval $fzf )
  echo "$selected_files" | xargs git add --
}
