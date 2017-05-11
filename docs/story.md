Project ideas as stories
========================

# Json as first class citizen 

In order to be able to test rest web services, the test editor needs to provide basic json capabilities that should make writing these tests as easy and straight forward as possible.

## Support for json as data

Given this simple test case:

```java
	Component: GreetingApp
	- request = Create request to <SayHello>
	- Set query parameter "name" to "Peter" on @request
	- response = Send request @request
	- result = Get body of @response     // result is of type com.google.gson.JsonElement
	- assert result.content = "Hello, Peter!"
```

The test-editor is now capable of accessing elements of a json via a path that can be made of either keys or array-indices. In this special
case, `result` is expected to be a `JsonObject` that in turn contains an element keyed `content` with a `JsonString` of the value "Hello,
Peter!".

Making json a first class citizen, it is possible to use json (and the path access to a certain element of it) in all places where regular variables can be used. 
This is in particular within calls to fixtures (via templates) 
```java
    - Type @result.content into <SomeTextField>
```
within calls to macros, within assertions (as used in our first example).

## Support for modification of json data

It is also possible to do modifications to an existing json.
```java
	Component: GreetingApp
	// given
	- request = Create request to <GreetTeam>
	- json = Read json from "example-team.json"
	- json.name = "Hero Team"
	- json.members[0] = {
		"firstName": "Aurelia",
		"lastName": "Yoxall"
	  }
	- json.members[1].firstName = "Maximus"
	- Set body of @request to @json

	// when
	- response = Send request @request

	// then
	- result = Get body of @response
	- assert result.content = "Hello team Hero Team. Warm regards to Aurelia, Maximus!"
```

In this example a json is modified using keys as well as array-indices. These modifications are currently restricted on already existing
objects. The test case language does currently NOT support removing or adding additional elements to a json.

## Outlook
### Support for separation of data from test execution 

Having a separation of test data from test execution will allow for running tests on several different data sets. The tcl allows for macro definition that provide parameters that can be passed on. These parameters however are currently defined without any structural or typing information. 

A test should be able to specify what kind of test data is needed in order to run the given test. Describing validity of data is already
conceptually embedded within the application mapping language, which describes a clean interface between test and application. A similar
interface between test and test data is conceivable. Extending this to data provided as json, this interface could be described by json
schema object (http://json-schema.org/). Validation of test data is done before the test gets executed with this data. This
schema definition could reside the file system or (in case of short definitions) be part of the interface description itself.

Executing tests could then be organized by a test orchestration that executes a test data pipeline (to generate or load a given set of test
data) and executing the test with test data produced by that pipeline.

### Additional (nice to have) features within the test editor rcp/web editor

* Better Json-editing support
* Support for structural information for valid json payloads and http requests (using e.g. swagger)
* Appending new json elements, removing existing elements (evaluation of fixture only solution beforehand).

# Tsl editor in the web

In order to allow non programmers to quickly write down acceptance criteria, an editor should be provided that allows to write test specifications (similar to markdown) without setup.
In particular no installation of any sort should be necessary (provided that a browser is installed, and some necessary back-end infrastructure is present).

## Envisioned work flow

- Project is created (e.g. via JIRA)
- Project repository is created (e.g. GitHub, Bitbucket)
- User Story is created (e.g. JIRA)
    - is alive only as long as this feature is not implemented yet 
- Test specification is written (closely related to the user story)
    - specification persists even if user story was implemented (and is removed)
    - specification is (only) stored within one repository, either within a test-repo or included into the project repo
    - the project team is responsible to provide a build that allows the tests to run against a consistent setup of the application

## Additional comments

- Changes to a test specification are independent from its creating user story and are synchronized with its repository
- Specifications are kept in exactly one repository
- Specifications are kept within the same repository as all implementing test cases

## Ideas to support this work flow

- UI idea: JIRA has buttons on each user story that allow to list/create test specifications
- The TSL editor will be an Angular app (embedding the TSL editor component)
- This Angular app will communicate with a language backend (servlet) generated by Xtext for the TSL
- The app will communicate with restful HTTP services to provided additional services
- Backend and frontend will be served from docker containers
- The backend will be served by dropwizard
- Each project has its own backend
- Each backend has a persistent volume that allows users to save changes into
- Each user has his/her own workspace into which project/feature branches are (sparsely) checked out
- Save, Commit, Fetch, Pull and Push will be supported actions, whereas the UI for these actions are unknown yet
