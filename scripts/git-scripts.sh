#!/usr/bin/env bash

fzf="fzf --border --margin=1 --layout=reverse --padding=1"

gbd() {
	local branches branch
	branches=$(git for-each-ref --count=30 --sort=-committerdate refs/heads/ --format="%(refname:short)") &&
		branch=$(echo "$branches" | eval "${fzf} --header 'Select branch(es) to delete...' --multi") &&
		git branch -D "$(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")"
}

gbdr() {
	local branches branch
	branches=$(git for-each-ref --count=30 --sort=-committerdate refs/heads/ --format="%(refname:short)") &&
		branch=$(echo "$branches" | eval "${fzf} --header 'Select branch(es) to delete...' --multi") &&
		git branch -D "$(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")"
}

gco() {
	local branches branch
	branches=$(git for-each-ref --count=30 --sort=-committerdate refs/heads/ --format="%(refname:short)") &&
		branch=$(echo "$branches" | eval "${fzf} --header 'Select branch to checkout'") &&
		git checkout "$(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")"
}

gcr() {
	local branches branch
	branches=$(git branch --remote) &&
		branch=$(echo "$branches" | eval "${fzf} --header 'Select branch to checkout'") &&
		git checkout "$branch"
}

ga() {
	local changed_files files
	selected_files=$(git status -s | awk '{print $2}' | eval "$fzf --header 'Select files to add' --multi")
	echo "$selected_files" | xargs git add --
}

function gcob() {
	local branch_name="$1"
	git checkout -b $branch_name
	git push --set-upstream origin $branch_name
}

function gcm() {
	git commit -m "$1"
}
