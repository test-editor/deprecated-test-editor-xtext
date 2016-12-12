# Internationalization of AML (Application Modeling Language)

The major goal of the test-editor is to be a tool that non-developers can use.
Usually there is a big fraction within this group that prefers to use their native language which
is not necessarily English. Therefore, we should provide some internationalization for AML, at
least for those parts that the "tester" will see.

## Example

The basic assumption is that the AML itself is designed by a developer and the test-cases are writter
by a non-developer.

Imagine a simple date picking dialog with combo boxes for day, month and year.

A simple test could look like this:

```
# MyTest

* Select date "2016/01/01" in dialog

component: DateDialog
    - Select "2016" in <Year>
    - Select "January" in <Month>
    - Select "01" in <Day>
    - Click <Ok>

// ...
```

The (simplified) underlying AML would look like this:

```
// project-specific part

component DateDialog is Dialog {
    element Year is ComboBox
    element Month is ComboBox
    element Day is ComboBox
    element Ok is Button
}

// general / fixture part

element type ComboBox {
    interactions = select
}

element type Button {
    interactions = click
} 

interaction type select {
    template = "Select" ${value} "in" ${element}
}

interaction type click {
    template = "Click" ${element}
}
```

The general / fixture part in the AML is the interesting part for I18n as this should
be reusable accross projects.

Translated to German this would be (not translating the keyword `component` right now):

```
# BeispielTest

* Selektiere "01.01.2016" im Dialog

component: DatumsDialog
    - Wähle "01" für <Tag>
    - Wähle "Januar" für <Month>
    - Wähle "2016" für <Jahr>
    - Klicke <Übernehmen>

// ...
```

There are two issues here:

1. the template should be within a reusable fixture, so it should be translatable
2. the button name `Übernehmen` is currently not supported


## Assumptions

- A few english keywords are acceptable (like `component`)
- The language within a testing project is fixed (not a user-specific property)


## Concept: translatable templates

The main task in providing i18n in AML is to make templates translatable. A main idea is to provide
some basic fixtures that can be used in order to speed up test creation. For example we deliver a web
fixture that can be used to test web projects. Those fixtures contain interactions with templates.
We want to provide those templates in English and German as a default.

### Syntax

The syntax for defining a template looks like this:

```
interaction type select {
    template = "Select" ${value} "in" ${element}
}
```

There are two options how translations can be provided:

1. Provide the translation in the AML itself
2. Provide the translation in a properties file

Option 1 (embedded translation) has the advantage that it's easier to maintain. If the template is changed the author
immediately sees that she should adapt the translation as well. On the other hand, option 2 (separate translation) allows to
add new translations without touching the original - this might be useful for other languages or if a
project uses its own vocabulary.

#### Option 1: embedded translation

Proposal a)

```
interaction type select {
    template = "Select" ${value} "in" ${element}
    template de = "Wähle" ${value} "für" ${element}
}
```

Proposal b)

```
interaction type select {
    template = "Select" ${value} "in" ${element}
          de = "Wähle" ${value} "für" ${element}
}
```

:+1: Easy to maintain<br/>
:+1: Editor support for translations<br/>
:-1: Harder to add a new language<br/>
:-1: Not possible to use project specific vocabulary<br/>

#### Option 2: separate translation

First, we would need to name the file. Let's say the name of the AML would be `WebDriverFixture.aml`.
A properties file could be named `WebDriverFixture_de.properties` and include definitions such as:
```
select = "Wähle" ${value} "für" ${element}
```

:+1: Easier to add a new language<br/>
:+1: Possible to use project specific vocabulary<br/>
:-1: Harder to maintain<br/>
:-1: No editor support in properties file <br/>

#### Translating variables

In the examples above the variables were not translated. If this should be possible we would need to
have a mapping concept.

For example:
```
interaction type select {
    template = "Select" ${value} "in" ${element}
    method = MyFixture.selectDropDown(value, element)
}

// translation (embedded or separate)
"Für" ${element} "wähle den Wert" ${wert} 
```
We would need a way to establish the mapping `wert -> value`.

This could be done by introducing a new syntax such as:
```
"Für" ${element} "wähle den Wert" ${wert:value} 
// or
"Für" ${element} "wähle den Wert" ${wert->value} 
```

### Completion proposals

To provide proposals for the user in the editor and the test step selector we somehow need the information
which language is used within a project. This should not be a user-specific property (see assumptions).
This could be provided in a project properties file or in the Gradle / Maven build.

## Concept: object names

The name of an object is the identifier which can be used to cross-reference an object.
In the above example we defined a button with the name `Ok` using:
```
// definition in AML
component DateDialog is Dialog {
    element Ok is Button
}

// usage in TCL
component: DateDialog
    - Click <Ok>
```

Currently a name is not translatable and not allowed to contain special characters such as ä, ü, ö or ß.
Translating the name might be difficult as we would need to adapt the default scoping mechanisms of Xtext.
We would have not have a single name per object but multiple names and adapt all scoping rules accordingly.

Usually, these objects are defined within a project and not in the reusable parts of the fixture.
Under the assumption that the language is fixed within a testing project there should be no need to make
them translatable.

It should be sufficient to adapt the parser rules to allow those special characters so that a specific
project can, for instance, use German names.


## Other topics

### Macros

Macros share the sample templating mechanism but are part of the TCL. The mechanisms that are applied to
AML interaction type templates should be applicable to macros as well.

### Value spaces

Value spaces are rather project specific and not fixture specific. The reasoning of the object names
applies here as well, there is no need to make them translatable.

### Labels

We should remove labels entirely as they are not used.
