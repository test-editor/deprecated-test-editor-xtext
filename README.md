test-editor-xtext
=================

[![License](http://img.shields.io/badge/license-EPL-blue.svg?style=flat)](https://www.eclipse.org/legal/epl-v10.html)
[![Build Status](https://ci.testeditor.org/buildStatus/icon?job=test-editor-xtext_develop)](https://ci.testeditor.org/job/test-editor-xtext_develop)

An Xtext based editor to specify domain-driven acceptance tests.

## Users

The latest released version can be found on [bintray](https://bintray.com/test-editor/test-editor/testeditor-app).

The latest development version can be downloaded [here](https://ci.testeditor.org/job/test-editor-xtext/job/develop/lastSuccessfulBuild/artifact/rcp/org.testeditor.rcp4.product/target/products/).



## Developers

Prerequisites:

- Maven 3.2.5
- JDK 1.8

After checking out the source code we first need to build the Eclipse target platform:

    mvn clean install -f "target-platform/pom.xml"
    
This will take some time for the first run but should be fast afterwards.

After building the target platform, we can simply build the test editor with:

    mvn clean install

For building the full RCP product we simply add the Maven profile "`product`":

    mvn clean install -Pproduct
