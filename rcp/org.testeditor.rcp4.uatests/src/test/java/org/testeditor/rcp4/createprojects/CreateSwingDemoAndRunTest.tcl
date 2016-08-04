package org.testeditor.rcp4.createprojects

import org.testeditor.rcp4.*

# CreateSwingDemoAndRunTest

// given
Setup:

	Component: TestEditor
	- Clean Workspace
	- Reset UI State

// when
* Open "Test-Editor Project" wizard

	Component: ProjectExplorer
	- Execute menu item "New/Project..." in tree <ProjektBaum>

	Component: NewDialog
	- Select element "Test-Editor Project" in tree <ProjectType>
	- Click on <NextButton>

* Create project with "Swing fixture"

	Component: NewDialog
	- Type "swingdemo" into <ProjectName>
	- Click on <NextButton>
	- Select element "Swing Fixture" in list <AvailableFixturesList>
	- Click on <AddFixtureButton>
	- Check <GenerateWithExamples>
	- Click on <FinishButton>

* Run "GreetingTest"

	Component: ProjectExplorer
	

// then
// TODO