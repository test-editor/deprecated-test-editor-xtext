#!/bin/bash
# Execute only on tag builds where the tag starts with 'v'

if [[ -n "$TRAVIS_TAG" && "$TRAVIS_TAG" == v* ]]; then
    version="${TRAVIS_TAG//v}"
    echo "Publishing version: $version"

    # Deploy Maven artifacts
    cp .travis.settings.xml $HOME/.m2/settings.xml
    ./gradlew deploy

    # Deploy P2 update site
    # Manually upload the p2 update site as the Gradle plugin simply does not work
    # see https://github.com/bintray/gradle-bintray-plugin/issues/87
    owner=test-editor
    repo=p2
    package=updatesite
    version="${TRAVIS_TAG//v}"

    function bintrayUpload {
        local path=$1
        echo "Uploading $path"
        echo "   ... $(curl -sT $path \
            -u$BINTRAY_USER:$BINTRAY_KEY \
            -H "X-Bintray-Package:$package" \
            -H "X-Bintray-Version:$version" \
            "https://api.bintray.com/content/$owner/$repo/$package/$version/$path"
        )"
    }

    function bintrayPublish {
        echo "Publishing version: $version"
        echo "   ... $(curl -sX POST \
            -u$BINTRAY_USER:$BINTRAY_KEY \
            https://api.bintray.com/content/$owner/$repo/$package/$version/publish
        )"
    }

    function uploadTargetPlatform() {
        cd rcp/org.testeditor.rcp4.updatesite/target/site
        find . -type f | 
            while read file; do 
                bintrayUpload $file;
            done
        bintrayPublish
    }

    uploadTargetPlatform
fi
