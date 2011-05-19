#!/bin/bash --posix

INSTALL_TARGET="$1"

if [ "${INSTALL_TARGET}" == "" ] ; then
	echo "Syntax: install.sh [TARGET_HOST:]TARGET_DIR" 1>&2
	exit 1
fi


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


if [ ! -f "${PROJECT_DIR}/project/build.properties" ] ; then
	echo "ERROR: Missing file 'project/build.properties' - invalid sbt project dir or build not run yet?" 1>&2
	exit 1
fi

export SCALA_VERSION=`cat "${PROJECT_DIR}/project/build.properties" | grep 'build.scala.versions' | sed 's/^.*=\([^[:space:]]\+\).*/\1/'`

if [ "${SCALA_VERSION}" = "" ] ; then
	echo "ERROR: Could not determine target Scala version." 1>&2
	exit 1
fi


if [ ! -d "${PROJECT_DIR}/target" ] ; then
	echo "ERROR: Missing target directory - invalid sbt project dir or build not run yet?" 1>&2
	exit 1
fi


if cd "${PROJECT_DIR}" ; then
	rsync -rlptvP \
		"project/boot/scala-${SCALA_VERSION}/lib"/*.jar \
		"lib_managed/scala_${SCALA_VERSION}"/{compile,runtime,plugin}/*.jar \
		"target/scala_${SCALA_VERSION}"/*.jar \
		"${INSTALL_TARGET}/lib/"
	
	rsync -rlptvP src/main/shell/scala "${INSTALL_TARGET}/bin/"

	rsync -rlptvP akka.conf logback.xml "${INSTALL_TARGET}/conf/"
else
	echo "ERROR: Could not cd to project directory \"${PROJECT_DIR}\"" 1>&2
fi
