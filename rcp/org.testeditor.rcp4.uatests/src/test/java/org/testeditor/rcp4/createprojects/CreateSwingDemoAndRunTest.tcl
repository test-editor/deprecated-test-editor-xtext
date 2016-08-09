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
	- Wait until dialog with title "Progress Information" is closed

* Run "GreetingTest"

	Component: ProjectExplorer
	- Wait "2" seconds
	- Select element "swingdemo/Tests/swingdemo/GreetingTest.tcl" in tree <ProjektBaum>
	- Execute menu item "Run As/1 Test" in tree <ProjektBaum>
	- Wait at most "20" seconds until dialog with title "Progress Information" is closed

// then
* Verify test execution result

	Component: HauptFenster
	- Is view <JUnitView> visible
	// TODO verify the contents of the view (test should be green)
