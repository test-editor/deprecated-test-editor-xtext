package org.testeditor.rcp4.refactoring.rename

import org.testeditor.rcp4.*

# RenameTestCaseTest

// given
Setup:

	Component: TestEditor
	- Clean Workspace
	- Reset UI State

* Open "Test-Editor Project" wizard

	Component: ProjectExplorer
	- Execute menu item "New/Project..." in tree <ProjektBaum>

	Component: NewDialog
	- Select element "Test-Editor Project" in tree <ProjectType>
	- Click on <NextButton>
	- Type "demo" into <ProjectName>
	- Click on <NextButton>
	- Select element "Web Fixture" in list <AvailableFixturesList>
	- Click on <AddFixtureButton>
	- Check <GenerateWithExamples>
	- Click on <FinishButton>
	- Wait until dialog with title "Progress Information" is closed

	Component: TestEditor
	- valid = Check if "demo" is a valid testproject
	- assert valid = "true"

	Component: ProjectExplorer
	- Select element "demo/Tests/demo/GoogleTest.tcl" in tree <ProjektBaum>
	- Execute menu item  "Rename..."  in tree <ProjektBaum>
	
	// when
	Component: RenameDialog
	 - Type  "NewTestCase"  into <ElementName>
	- Click on <OK>
	
	// then
	Component: ProjectExplorer
	- Select element "demo/Tests/demo/NewTestCase.tcl" in tree <ProjektBaum>
	
	Component: ActiveEditor
	- check = Contains active editor "# NewTestCase"
	- assert check = "true"