test-editor-xtext
=================

[![License](http://img.shields.io/badge/license-EPL-blue.svg?style=flat)](https://www.eclipse.org/legal/epl-v10.html)
[![Build Status](https://travis-ci.org/test-editor/test-editor-xtext.svg?branch=develop)](https://travis-ci.org/test-editor/test-editor-xtext)

An Xtext based editor to specify domain-driven acceptance tests.

## Users

The latest released version can be found on [bintray](https://bintray.com/test-editor/test-editor/testeditor-app).

The latest development version can be downloaded [here](https://ci.testeditor.org/job/Test-Editor-Xtext-Product/lastSuccessfulBuild/artifact/rcp/org.testeditor.aml.rcp.product/target/products/).

## Developers

After checking out the source code we first need to build the Eclipse target platform:

    mvn clean install -f "releng/org.testeditor.aml.releng.target/pom.xml" 
    
This will take some time for the first run but should be fast afterwards.

After building the target platform, we can simply build the test editor with:

    mvn clean install

For building the full RCP product we simply add the Maven profile "`product`":

    mvn clean install -Pproduct
