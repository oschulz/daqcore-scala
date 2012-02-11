#!/bin/sh

SOURCE=$0;
SCRIPT=`basename "$SOURCE"`;
while [ -h "$SOURCE" ]; do
    SCRIPT=`basename "$SOURCE"`;
    LOOKUP=`ls -ld "$SOURCE"`;
    TARGET=`expr "$LOOKUP" : '.*-> \(.*\)$'`;
    if expr "${TARGET:-.}/" : '/.*/$' > /dev/null; then
        SOURCE=${TARGET:-.};
    else
        SOURCE=`dirname "$SOURCE"`/${TARGET:-.};
    fi;
done;

PROJECT_DIR=`dirname "$SOURCE"`
PROJECT_DIR=`cd "$PROJECT_DIR"; pwd -P`


if [ ! -f "${PROJECT_DIR}/build.sbt" ] ; then
	echo "ERROR: Missing file 'project/build.properties' - invalid sbt project dir or build not run yet?" 1>&2
	exit 1
fi

export SBT_VERSION="0.11.2"
export SCALA_VERSION=`cat "${PROJECT_DIR}/build.sbt" | grep 'scalaVersion' | sed 's/.*:=[[:space:]]*"\([^[:space:]]\+\)"[[:space:]]*/\1/'`

if [ "${SCALA_VERSION}" = "" ] ; then
	echo "ERROR: Could not determine target Scala version." 1>&2
	exit 1
fi

if [ "${SBT_VERSION}" = "" ] ; then
	echo "ERROR: Could not determine SBT version." 1>&2
	exit 1
fi


SBT_JAR="${PROJECT_DIR}/project/cache/sbt-launch-${SBT_VERSION}.jar"


if [ ! -f "${SBT_JAR}" ] ; then
	echo "SBT ${SBT_VERSION} launcher not found in project/cache, downloading it." 1>&2
	
	mkdir -p "${PROJECT_DIR}/project/cache"
	if [ "`which curl`" != "" ] ; then
		curl "http://typesafe.artifactoryonline.com/typesafe/ivy-releases/org.scala-tools.sbt/sbt-launch/${SBT_VERSION}/sbt-launch.jar" > "${SBT_JAR}"
	elif [ "`which wget`" != "" ] ; then
		wget "http://typesafe.artifactoryonline.com/typesafe/ivy-releases/org.scala-tools.sbt/sbt-launch/${SBT_VERSION}/sbt-launch.jar" -O - > "${SBT_JAR}"
	else
		echo "ERROR: Neither curl nor wget tool are available, can't download." 1>&2
		exit 1
	fi

	if [ ! -f "${SBT_JAR}" ] ; then
		echo "ERROR: \"${SBT_JAR}\" not found - download not successfull?" 1>&2
		exit 1
	fi
fi


cd "${PROJECT_DIR}"

if [ "`pwd`" != "${PROJECT_DIR}" ] ; then
	echo "ERROR: Could not cd to project directory" 1>&2
	exit 1
fi


[ -n "$JAVA_OPTS" ] || JAVA_OPTS="-Xmx512M -Xms16M -XX:MaxPermSize=512M"

if [ -z "$JAVACMD" -a -n "$JAVA_HOME" -a -x "$JAVA_HOME/bin/java" ]; then
    JAVACMD="$JAVA_HOME/bin/java"
fi

exec "${JAVACMD:=java}" $JAVA_OPTIONS -jar "${SBT_JAR}" "$@"
