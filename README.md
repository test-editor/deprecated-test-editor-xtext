test-editor-xtext
=================

[![License](http://img.shields.io/badge/license-EPL-blue.svg?style=flat)](https://www.eclipse.org/legal/epl-v10.html)

An Xtext based editor to specify domain-driven acceptance tests.

## Users

The latest released version of the plugins can be found on [bintray](https://bintray.com/test-editor/maven/test-editor).

## Plugin-Users

When installing additional testeditor plugins, please provide an additional update site [Eclipse Source](http://hstaudacher.github.io/osgi-jax-rs-connector). This will allow additional dependencies to be resolved.

## Developers

Prerequisites:

- Maven 3.2.5
- JDK 1.8

After checking out the source code we first need to build the Eclipse target platform:

    mvn clean install -f "target-platform/pom.xml"
    
This will take some time for the first run but should be fast afterwards.

The RCP contains some web based views, which are developed in a seperate cycle. They can be build with:

    gradlew preBuildWeb

Now the Test-Editor can simply be build with:

    mvn clean install

Alternatively, for building the full RCP product, add the Maven profile "`product`":

    mvn clean install -Pproduct

## Troubleshooting

### The downloaded application won't start

The editor requires a JDK 1.8 in order to start. If your default system JVM is different, you can set the path to the JDK by opening the `testeditor.ini` file and placing the following **before** the `-vmargs` parameter:
 
    -vm
    <pathToYourJDK8>
    
The path depends on your operating system as described [here](https://wiki.eclipse.org/index.php?title=Eclipse.ini&redirect=no#Specifying_the_JVM). For example (Windows):

    C:\tools\jdk1.8.0_131\bin\javaw.exe

### Tests cannot be started from the RCP / IDE, complaining about missing environment variable TE_MAVEN_HOME

When using maven as test project build/run tool, please make sure to have the environment variable `TE_MAVEN_HOME` set to maven home.

## Release process

* merge develop into the master (via pullrequest)
* switch locally to the master branch
* execute `./gradlew release` (allows to set the release version and the new developversion)
  this will trigger the publishing process on travis (check [here][https://bintray.com/test-editor/maven/test-editor] whether publishing was really successful)
* merge master into develop to get the new development version into develop again
* execute maven to set all poms to the new versions
  ```
  mvn -f target-platform/pom.xml build-helper:parse-version org.eclipse.tycho:tycho-versions-plugin:set-version -Dartifacts=org.testeditor.releng.target.parent -DnewVersion=1.11.0-SNAPSHOT -Dtycho.mode=maven
  mvn -f pom.xml build-helper:parse-version org.eclipse.tycho:tycho-versions-plugin:set-version -Dartifacts=org.testeditor.releng.parent -DnewVersion=1.11.0-SNAPSHOT -Dtycho.mode=maven -Prcp
  ```
