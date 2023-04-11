#!/usr/bin/env bash

JAR="$2"
SDK="$HOME/.sdkman/candidates"
JAVA="$SDK/java/17.0.5-tem"

GRADLE_HOME="$SDK/gradle/current/bin/gradle" $JAVA \
	-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=1044 \
	-javaagent:"$HOME/.local/share/nvim/lsp_servers/jdtls/lombok.jar" \
	-Declipse.application=org.eclipse.jdt.ls.core.id1 \
	-Dosgi.bundles.defaultStartLevel=4 \
	-Declipse.product=org.eclipse.jdt.ls.core.product \
	-Dlog.protocol=true \
	-Dlog.level=ALL \
	-Xms1g \
	-Xmx2G \
	-jar "$JAR" \
	-configuration "$HOME/.local/share/lsp/jdtls-server/config_linux" \
	-data "$1" \
	--add-modules=ALL-SYSTEM \
	--add-opens java.base/java.util=ALL-UNNAMED \
	--add-opens java.base/java.lang=ALL-UNNAMED
