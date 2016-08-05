package org.testeditor.rcp4.createprojects

import org.testeditor.rcp4.*

# CreateWebProjectWithExampleTest implements CreateSimpleTestEditorProject

Setup:

	Component: TestEditor
	- Clean Workspace
	- Reset UI State

* start creation process

	Component: ProjectExplorer
	- Execute menu item "New/Project..." in tree <ProjektBaum>

* Select type Test Editor Project

	Component: NewDialog
	- Select element "Test-Editor Project" in tree <ProjectType>
	- Click on <NextButton>

* Give the Project the name "MyFirstProject"

	// We must select the dialog again, because we are in a new TSL Step.
	Component: NewDialog
	- Type "MyFirstWebProject" into <ProjectName>
	- Click on <NextButton>
	- Select element "Web Fixture" in list <AvailableFixturesList>
	- Click on <AddFixtureButton>
	- Click on <FinishButton>
	- Wait for dialog with title "Progress Information"
	
* Verify that the project is visible in the ui

	Component: ProjectExplorer
	- Select element "MyFirstWebProject" in tree <ProjektBaum>
