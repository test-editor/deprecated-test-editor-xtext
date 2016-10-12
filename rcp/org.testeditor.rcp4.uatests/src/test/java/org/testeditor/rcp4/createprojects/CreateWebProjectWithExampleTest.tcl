package org.testeditor.rcp4.createprojects

import org.testeditor.rcp4.*
import org.testeditor.fixture.swt.*

# CreateWebProjectWithExampleTest implements CreateSimpleTestEditorProject

config TestEditorConfig

* start creation process

	Component: ProjectExplorer
	- Execute menu item "New/Project..." in tree <ProjectTree>

* Select type Test Editor Project

	Component: NewProjectDialog
	- Select element "Test-Editor Project" in tree <ProjectType>
	- Click on <NextButton>

* Give the Project the name "MyFirstProject"

	// We must select the dialog again, because we are in a new TSL Step.
	Component: NewProjectDialog
	- Type "MyFirstWebProject" into <ProjectName>
	- Click on <NextButton>
	- Select element "Web Fixture" in list <AvailableFixturesList>
	- Click on <AddFixtureButton>
	- Click on <FinishButton>
	- Wait until dialog with title "Progress Information" is closed
	
* Verify that the project is visible in the ui

