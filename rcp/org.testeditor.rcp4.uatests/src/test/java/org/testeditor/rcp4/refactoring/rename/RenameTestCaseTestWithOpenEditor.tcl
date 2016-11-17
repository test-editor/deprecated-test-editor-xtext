package org.testeditor.rcp4.refactoring.rename

import org.testeditor.fixture.assertion.*
import org.testeditor.rcp4.*
import org.testeditor.fixture.swt.*

# RenameTestCaseTestWithOpenEditor

config TestEditorConfig

// given
Setup:

	Macro: SampleProjects
	- Create a sample web project with Gradle

// when
* Rename "GoogleTest.tcl" to "NewTestCase.tcl"

	Component: ProjectExplorer
	- Select element "demo/Tests/demo/GoogleTest.tcl" in tree <ProjectTree>
	- Execute menu item "Rename..." in tree <ProjectTree>

	Component: RenameDialog
	- Type "NewTestCase.tcl" into <ElementName>
	- Click on <OK>
	- Wait until dialog with title "Rename Resource" is closed

// then
* Verify that the editor contains the line "# NewTestCase"

	Component: MainWindow
	- editor = Get editor with file "/demo/src/test/java/demo/NewTestCase.tcl"
	- editorContents = Get contents of editor @editor

	Component: Assertion
	- Verify that @editorContents contains "# NewTestCase"
