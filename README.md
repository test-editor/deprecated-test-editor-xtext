test-editor-xtext
=================

[![License](http://img.shields.io/badge/license-EPL-blue.svg?style=flat)](https://www.eclipse.org/legal/epl-v10.html)
[ ![Download](https://api.bintray.com/packages/test-editor/maven/test-editor/images/download.svg) ](https://bintray.com/test-editor/maven/test-editor/_latestVersion)

Xtext based languages to specify domain-driven acceptance tests. These language artefacts are used by [test-editor](https://github.com/test-editor/test-editor-web)

## Users

The latest released version of the language artefacts can be found on [bintray](https://bintray.com/test-editor/maven/test-editor).

## Developers

Prerequisites:

- Maven 3.2.5
- JDK 1.8

After checking out the source code we first need to build the Eclipse target platform:

    mvn clean install -f "target-platform/pom.xml"
    
This will take some time for the first run but should be fast afterwards.

Now the Test-Editor Languages can simply be build with:

    mvn clean install
    
## Release process

* merge develop into the master (via pullrequest)
* switch locally to the master branch
* execute `./gradlew release` (allows to set the release version and the new developversion)
  this will trigger the publishing process on travis (check [here][https://bintray.com/test-editor/maven/test-editor] whether publishing was really successful)
* merge master into develop to get the new development version into develop again
* execute maven to set all poms to the new versions
  ```
  mvn -f target-platform/pom.xml build-helper:parse-version org.eclipse.tycho:tycho-versions-plugin:set-version -Dartifacts=org.testeditor.releng.target.parent -DnewVersion=1.16.0-SNAPSHOT -Dtycho.mode=maven
  mvn -f pom.xml build-helper:parse-version org.eclipse.tycho:tycho-versions-plugin:set-version -Dartifacts=org.testeditor.releng.parent -DnewVersion=1.16.0-SNAPSHOT -Dtycho.mode=maven -Prcp,rcpuatest,product
  ```
