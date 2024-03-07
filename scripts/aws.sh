#!/usr/bin/env bash

assume() {
	local sess="sess_$(date +%s)"
	local profile="${2:=default}"
	export $(printf "AWS_ACCESS_KEY_ID=%s AWS_SECRET_ACCESS_KEY=%s AWS_SESSION_TOKEN=%s" \
		$(
			aws sts assume-role \
				--role-arn "$1" \
				--role-session-name "$sess" \
				--profile "$profile" \
				--query "Credentials.[AccessKeyId,SecretAccessKey,SessionToken]" \
				--output text
		))
}

select_role() {
	rg --pcre2 -N -o -e '(?<=\[)\S+(?=\])' ~/.aws/credentials | fzf --no-multi --reverse --header 'Select a profile'
}
