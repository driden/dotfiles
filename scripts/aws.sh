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

aws_select_role() {
	rg --pcre2 -N -o -e '(?<=\[)\S+(?=\])' ~/.aws/credentials | sort | fzf --no-multi --reverse --header 'Select a profile'
}

aws_select_region() {
	read -r -d '\n' regions <<EndOfText
us-east-1
us-east-2
us-west-1
us-west-2
EndOfText

	echo "$regions" | fzf --header 'Region'
}

ssm_log_in() {
	if [ -z "$1" ]; then
		echo >&2 "Please provide instance id"
		return 1
	fi

	aws ssm start-session --target "$1" --region us-east-1 --profile "$(aws_select_role)"
}
