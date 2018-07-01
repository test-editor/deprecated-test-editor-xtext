test-editor-xtext
=================

[![License](http://img.shields.io/badge/license-EPL-blue.svg?style=flat)](https://www.eclipse.org/legal/epl-v10.html)
[ ![Download](https://api.bintray.com/packages/test-editor/maven/test-editor/images/download.svg) ](https://bintray.com/test-editor/maven/test-editor/_latestVersion)

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

Now the Test-Editor Languages can simply be build with:

    mvn clean install
    
THE RCP OF THE TESTEDITOR IS CURRENTLY DISCONTINUED AND THUS DEPRECATED. ERRORS THEREIN ARE NOT FIXED.

DEPRECATED: The RCP contains some web based views, which are developed in a seperate cycle. They can be build with:

    gradlew preBuildWeb
    
DEPRECATED: The RCP and the eclipse plugins can then be built with:

    mvn clean install -Prcp

DEPRECATED: For building the full RCP product, add the Maven profiles "`rcp,product`":

    mvn clean install -Prcp,product
    
DEPRECATED: Executing the User Acceptance Tests for the rcp, add the Maven profile "`rcp,rcpuatest`":

    mvn clean install -Prcp,rcpuatest

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
  mvn -f target-platform/pom.xml build-helper:parse-version org.eclipse.tycho:tycho-versions-plugin:set-version -Dartifacts=org.testeditor.releng.target.parent -DnewVersion=1.16.0-SNAPSHOT -Dtycho.mode=maven
  mvn -f pom.xml build-helper:parse-version org.eclipse.tycho:tycho-versions-plugin:set-version -Dartifacts=org.testeditor.releng.parent -DnewVersion=1.16.0-SNAPSHOT -Dtycho.mode=maven -Prcp,rcpuatest,product
  ```

## (still incomplete) Upgrade from Eclipse Oxygen to Photon, java 8 -> 9

* several command lines (e.g. tycho) had to be appended with `--add-modules=ALL-SYSTEM`
* emf was upgraded such that the GenModel.genmodel included xml tags that are not understood by the current xtext - emf library, see (https://github.com/eclipse/xtext/issues/1233)
  build runs through if the library is patched
* the following dependency for javax.annotation had to be added to some projects (since java 9 jigsaw does no longer provide this out of the box, it could be added with `--add-modules` but even this is depcrecated and removed in java 11)
  ```
    <groupId>javax.annotation</groupId>
    <artifactId>javax.annotation-api</artifactId>
    <version>1.3.1</version>
  ```
  The artifact is downloadad vie maven dependency and is the copied into the lib folder,
  finally it is added ti the build.properties
* gradle had to be upgraded (now 4.7) in order to understand the java version string
* a test keeps failing because the buildship version (gradle eclipse integration) keeps failing with the 'java version string not understood' error, even though it should be fixed by now

* TODO: tcl.ui.tests do not start because of e4 injection problems
* TODO: await a new emf xtext integration library to work with photon (unknown xml tags in genmodel)
* TODO: publishing the product (via tycho) currently fails, reason unknown (root cause: array index 1 out of bounds exception)
* OPEN: gradle integration test is currently deactivated because of 'java version parsing' problem
