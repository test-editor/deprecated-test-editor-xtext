test-editor-xtext
=================

[![License](http://img.shields.io/badge/license-EPL-blue.svg?style=flat)](https://www.eclipse.org/legal/epl-v10.html)
[![Build Status](https://ci.testeditor.org/buildStatus/icon?job=test-editor-xtext_develop)](https://ci.testeditor.org/job/test-editor-xtext_develop)

An Xtext based editor to specify domain-driven acceptance tests.

## Users

The latest released version can be found on [bintray](https://bintray.com/test-editor/test-editor/testeditor-app).

The latest development version can be downloaded [here](https://ci.testeditor.org/job/test-editor/job/test-editor-xtext/job/develop/lastSuccessfulBuild/artifact/rcp/org.testeditor.rcp4.product/target/products/).



## Developers

Prerequisites:

- Maven 3.2.5
- JDK 1.8

After checking out the source code we first need to build the Eclipse target platform:

    mvn clean install -f "target-platform/pom.xml"
    
This will take some time for the first run but should be fast afterwards.

The RCP contains some web based views, which are developed in a seperate cycle. They can be build with:

    gradlew preBuildWeb

After building the target platform, we can simply build the test editor with:

    mvn clean install

For building the full RCP product we simply add the Maven profile "`product`":

    mvn clean install -Pproduct

## Troubleshooting

### The downloaded application won't start

 The editor requires a JDK 1.8 in order to start. If you default system JVM is different, you can set the path to the JDK by opening the `eclipse.ini` file and placing the following at the end:
 
    -vm
    <pathToYourJDK8>
    
The path depends on your operating system as described [here](https://wiki.eclipse.org/index.php?title=Eclipse.ini&redirect=no#Specifying_the_JVM). For example (Windows):

    C:\tools\jdk1.8.0_92\bin\javaw.exe
