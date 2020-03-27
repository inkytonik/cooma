#!/usr/bin/env bash
cd "$(dirname "$0")"
bash clean_component.sh
echo "Building cooma-component.jar"

SCALA_VERSION="scala-2.13"
GRAALVM_VERSION="20.0.0"
COMPONENT_DIR="component_temp_dir"
LANGUAGE_PATH="$COMPONENT_DIR/jre/languages/cooma"

rm -rf COMPONENT_DIR

mkdir -p "$LANGUAGE_PATH"
mkdir -p "$LANGUAGE_PATH/bin"

cp ../truffle_root/target/$SCALA_VERSION/truffle_root.jar "$LANGUAGE_PATH"
cp ../cooma $LANGUAGE_PATH/bin/


mkdir -p "$COMPONENT_DIR/META-INF"
{
    echo "Bundle-Name: Cooma IR Language";
    echo "Bundle-Symbolic-Name: org.bitbucket.inkytonik.cooma";
    echo "Bundle-Version: $GRAALVM_VERSION";
    echo "Bundle-RequireCapability: org.graalvm; filter:=\"(&(graalvm_version=$GRAALVM_VERSION)(os_arch=amd64))\"";
    echo "x-GraalVM-Polyglot-Part: True"
} > "$COMPONENT_DIR/META-INF/MANIFEST.MF"

(
cd $COMPONENT_DIR || exit 1
jar cfm ../cooma-component.jar META-INF/MANIFEST.MF .

echo "bin/cooma = ../jre/languages/cooma/bin/cooma" > META-INF/symlinks
jar uf ../cooma-component.jar META-INF/symlinks

{
    echo "jre/languages/cooma/bin/cooma = rwxrwxr-x"
} > META-INF/permissions
jar uf ../cooma-component.jar META-INF/permissions
)
rm -rf $COMPONENT_DIR
