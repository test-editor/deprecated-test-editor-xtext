# Test-Editor Language Features

## Introduction

The Test-Editor is threefold. 
* It is a client application (RCP) which enables the Tester to create, edit and execute Tests. 
* It is a set of plugins for the eclipse workbench which enable the developer to make use of tests, create, edit and execute them in the
same fashion as the Tester does, without leaving his development environment.
* It is a toolset to be used on a continuous integration platform, executing tests in the same fashion as done by the Tester and the
  Developer.
  
Additionally several fixture packages are provided that allow tests to run against several technologies like html5-applications,
swing-applications, rcp-applciations, web-services, angularjs-applications.

The Test-Editor is primarily targeted at running user acceptance tests. It is however not limited to this. It can as well be used for test
automation in integration- or even unit-test scenarios, thus being useful within all levels of the test pyramid.

## Overview

This document will primarily talk about the domain specific languages used to write tests and application mappings.

In order to understand the bolts and nuts of the test-editor I will give in introduction the the dsls used.

### Languages

The test editor holds three different DSLs. The Test-Case-Language, the Application-Modelling-Language and the
Test-Specification-Language. Each language is targeted at a different abstraction level of an actual Test. Each language is specific to its
domain and is maximized for conciseness and expressiveness in that domain.

The Test-Case-Language (tcl) is used to describe a Test in terms of test steps that need to be taken and assertions to check
expectations. It is designed to explicitly lack features that are technology agnostic. That is, tests written in this language will not be
readily recognizable for what application technology is actually run while running the tests. Each test case is translated into an
executable test implementation (e.g. JUnit-Test).

The Application-Modelling-Language (aml) is the technological bridge from high level test description (Test-Case-Language) to the actual
application to be tested. It will thereby almost certainly contain concepts that are technology dependent. Within this language fixtures are
heavily used, which provide an api for the aml and are written in a general purpose language (e.g. Java).

The Test-Specification-Language is an additional abstraction of the concrete test case. It is a less if not informal description of a test,
which is implemented by a test case.

### File Types

There are five file types written in the domain specific languages of the test editor. 

- Test Cases (file type = 'tcl') : are (naturally) written in the test case language and specify executable tests.
- Configurations (file type = 'config') : are written in the test case language, defining setup and cleanup of test cases.
- Macro Libraries (file type = 'tml') : are written in the test case language, defining reusable lists of test steps.
- Application Mappings (file type 'aml') : are (naturally) written in the application mapping language.
- Test Specification (file type = 'tsl') : are (naturally) written in the test specification language.

How Test Cases, Configurations and Macro Libraries differ is discussed in greated detail later in this document.

## Test-Case-Language

(see [tcl syntax definition](https://ci.testeditor.org/job/test-editor/job/test-editor-xtext/job/feature%252FTE-465_syntax_n_feature_doc-with-product/lastSuccessfulBuild/artifact/docs/output/test-case-language/index.html))

```
# MyFirstTestCase

* Open application and navigate to the question of life
* Enter the question
* Validate the correct answer to be _42_
```

Given this simple test I want to successively add and describe feature of the language.

First of all let's have a look at this *minial* test case.

`# MyFirstTestCase` is part of the header of the test case, naming it. This name has to correspond with the file name used for this test
case. The file name must be `MyFirstTestCase.tcl`.

The Header may as well hold additional information (which I will skip for the moment).

### Specification Steps

The lines starting with `*` are specification steps which informally describe (specify) what should be done. Since these specifications
steps are not formal enough to actually run a test, the test must be elaborated into more specific executable test steps.

### Steps

```
# MyFirstTestCase

require browserPath

* Open application and navigate to the question of life
  
  Component: WebBrowser
  - Open browser @browserPath
  - Navigate to "http://org.question-of-life/index.html"

* Enter the question

  Component: QFLPage
  - Enter "What's the meaning of life?" into <QuestionField>
  - Click <EnterButton>
  
* Validate the correct answer to be _42_

  Component: QFLPage
  - answer = Read <AnswerField>
  - assert answer = "42"
```

Now each specification step is *implemented* using appropriate test steps. Each test step is valid only within a context. The context can be
either a Component / Mask or a Macro. Component and Mask can be used synonymously whereas Macros are somewhat different and will be
discussed later on.

A Component is an element of the application and is defined via the aml. I will make use of Components without going into to much detail for
now. Components will be fully discussed in the AML-Section.

#### Test Step

`- Open browser @browserPath` is an example of a Test Step. It is a concise description of what should happen that will map directly onto an
implementation. Each test step maps exactly onto one fixture call. In this case the fixture is called with one (String) parameter
`browserPath`. This parameter is actually an environment variable (which I will discuss in detail later on). Variables can generally be used
to hold values of type `long`/`Long`, `boolean`/`Boolean`, `String` and `Map<String,Object>`. They can be passed to fixture calls, macro
calls and can be used in assertions.

#### Assignment Step

`- answer = Read <AnswerField>` is an Assignment Step, that makes a call to a fixture using `AnswerField` as *element* parameter (element
parameters are discussed in the application mapping), returning a value that is assigned to `answer`. The assigned variable is accessible
from this statement on until the end of this file. Each variable can only be assigned once. What type the assigned variable actually has
depends on the return type of the fixture that is effectively called. The Test-Editor makes sure that the types are checked before running
the test such that a user should not have to worry about types until type-violations cannot be solved.

#### Assertion Step

`- assert answer = "42"` will fail if the variable `answer` does not hold 42. Assertions are used to specify expectations in a test
case. The expression that follows the assertion cannot make use of recursively nested expression. The following expression are allowed
* simple boolean check (e.g. `- assert boolVar` or `- assert !boolVar` for negation)
* simple comparitive check using operators `=`, `!=`, `>=`, `>`, `<`, `<=` (e.g. `- assert answer = "42"`)
* string matching operations (e.g. `- someString matches "^notmuch.*of.+an$"` or `- someString does not match "[A-Z]+"`)
* values can be either variables or string constants

### Header

The header of a test case has additional elements to handle namespaces and environment variables. It is used to specify the configuration
(setup and teardown) for a test.

#### Namespace (optional)

Each test case must reside within a specific namespace. This will allow for 
1. fully qualified references
2. importing namespaces 

A test case can explicitly specify its namespace by having e.g. `package my.name.space` at the top of the file. The namespace however must
correspond to the file system location of the test case. Given the afore mentioned example, the test case must reside in the folder
`my/name/space` (relative to namespace root). Since the namespace is derived from its location within the file system, the explicit package
command is optional and its use is discouraged from.

#### Imported Namespaces

Within the header additional namespaces can be imported in order to reference artifacts without having to use the fully qualified name for
them. The import statement itself follows (mostly) Java conventions. It is possible to explicitly import one artifact by using the fully
qualified name of it (e.g. `import my.name.space.MyFirstTestCase`). It is additionally possible to use wildcard imports (e.g. `import
my.name.space.*`).

#### Test Case Name

The only non-optional element of the header is the test case name itself.

#### Configuration (Setup/Teardown)

Every test case may define explicit setup and/or teardown steps that are executed before the test itself and respectively after the test has
been executed (regardless of its outcome or exceptions during its run). These setup and teardown steps can be put into separate files called
'configuration'. These configurations can then be used by an arbitrary number of tests.

Setup and cleanup steps are written as regular test steps:

```
Setup:
  Component: Browser
  - Start browser @browserPath

Cleanup:
  Component: Browser
  - Close browser
```

Explicitly using a configuration (which specifies Setup and Cleanup) in a test case:
```
Config: my.name.space.MyFirstConfig
```

#### Required Environment Variables

Test can make use of environment variables to configure test behaviour. In our example the test step `- Open browser @browserPath` makes use
of a variable `browserPath` which is an environment variable made accessible to the test by the require statement (`require browserPath`) in
the header. Environment variables are discussed in more detail later in this document.

### Macros

Macros are resusable test case building blocks that are written in macro libraries. Macro libraries are namespaces / files which hold
arbitrary numbers of macros. Macros are written in the test case language, sharing a large set of syntax / semantics.

Macros are the possibility to encapsulate repeated test steps into reusable modules. A macro is defined by its Template (I will talk about
templates in a moment) and the test steps that are executed upon calling the macro. Sticking to our example a macro could be used to combine
the two steps of entering the question and clicking the enter button.

```
# MyMacroLib

## AskQuestion
template = "Ask" ${question} 
  Component: QFLPage
  - Enter @question into <QuestionField>
  - Click <EnterButton>
  
```

This macro can then be used as a regular test step within a macro context `Macro: MyMacroLib` which defines the context for the macro. 

```
  Macro: MyMacroLib
  - ask "What is the meaning of Life?"
```

Currently macros cannot return any values. Thus the reading of the answer is left out of this macro.

#### Templates

Templates are used to describe the syntax and parameters of a test step. A template consists of words (or rather ids) defined in double
quotes and parameters enclosed in `${}`. Types of parameters are inferred and thus not explicitly stated. An example of a template is given
in the above example `template = "ask" ${question}`. A template must always start with a word. Any number and order of parameters is
valid. A template may end with a punctuation (either `.` or `?`). 

```
template = "Get color at" ${xcoord} ${ycoord} "?"
```

To make use of this template the respective test step would be `-  Get color at "15" "20"?`. Note that even though numbers are
expected, they are enclosed in double quotes. Templates may not hold numbers (directly). Since this template (or the respective fixture) is
expected to return a value, this could be used in an assignment, too (e.g. `- color = Get color at "15" "20"?`).

```
template = "Set" ${color} "at" ${xcoord} ${ycoord}
```

The respective using test step would be `- Set "yellow" at "15" "20"`.

### Variables

Variables are used to store values and can be used within assertions to verify expectations. 

#### Template Parameters

Templates may define parameters that are used as variables within the scope of the template of its definition. If a macro template defines a
parameter, this parameter may be used within this macro only. Template parameters are used in the Application Mapping Language, too, but
this will be discussed later on.

Template parameters are defined within a template (e.g. `template = "Set" ${color}`). `color` is now bound to be a variable that is
accessible within the scope of this element (e.g. macro). 

#### Assignment Variables

Assignment variables are defined by using them in an assignment (no separate declaration necessary). Thus `- color = Get color at "15" "20"`
will define and set the variable `color` with the result of the fixture called by `Get color...`. An assignment variable can be used within
the whole test case, after its assignment. When assigned within a macro, this variable is valid only within the scope of the macro itself,
not for the whole file.

#### Environment Variables

Environment variables are available for test cases only and cannot be used in configurations nor macros. They are declared in the head of
the file by `require` followed by the variable name (e.g. `require browserPath`). Environment variables are accessible throughout the test
case they are required in. Before a test gets executed a validation is made to ensure that all required environment variables are known,
that is, they have to be defined in the environment, they may be empty.

#### Usage

All kind of variables can uniformly be used as parameters in test steps (e.g. `- Start browser @browserPath`), as parameter to macro calls
(e.g. `- Ask @myQuestionVar`, given that `myQuestionVar` is a known variable) and as values in assertion expression (e.g. `- assert
myQuestionVar = "What is the meaning of life?"`). Note that a variable needs to be dereferenced by `@` when used in parameter position
whereas within an assertion, the variable is used directly.

#### Excursion: Types

Variable types are inferred but never explicitly stated within the tcl. If however type inference suggests problems (e.g. map dereference
access on a long value), the user is informed to resolve this issue. The type inference makes use of the types that a fixture defines for
its parameters.

----------

## Application-Mapping-Language
### Components / Masks
### Interactions
### Component Elements
### Component Element Types
### Component Types
## Test-Specification-Language
