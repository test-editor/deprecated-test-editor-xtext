package org.testeditor.rcp4.createprojects

import org.testeditor.rcp4.*
import org.testeditor.fixture.swt.*

# CreateSwingDemoAndRunTest

// given
config TestEditorConfig

// when
* Open "Test-Editor Project" wizard

	Component: ProjectExplorer
	- Execute menu item "New/Project..." in tree <ProjectTree>

	Component: NewProjectDialog
	- Select element "Test-Editor Project" in tree <ProjectType>
	- Click on <NextButton>

* Create project with "Swing fixture"

	Component: NewProjectDialog
	- Type "swingexecdemo" into <ProjectName>
	- Click on <NextButton>
	- Select value "Maven" in combo box <BuildTool>
	- Select element "Swing Fixture" in list <AvailableFixturesList>
	- Click on <AddFixtureButton>
	- Check <GenerateWithExamples>
	- Click on <FinishButton>
	- Wait at most "30" seconds until dialog with title "Progress Information" is closed
	// on some machines a dialog pops up that expects to acknowledge a file change for editor refresh
	- Wait at most "5" seconds until dialog with title "File Changed" opens and click "Yes"

	Component: TestEditorServices
	- valid = Check if "swingexecdemo" is a valid testproject
	- assert valid = "true"

	Component: Workbench
	- Wait until all jobs finished* Run "GreetingTest"

	Component: ProjectExplorer
	- Select element "swingexecdemo/Tests/swingexecdemo/GreetingTest.tcl" in tree <ProjectTree>
	- Execute menu item "Run test" in tree <ProjectTree>
	- Wait at most "30" seconds until dialog with title "Progress Information" is closed

// then
* Verify test execution result

	Component: MainWindow
	- Is view <JUnitView> visible
	// TODO verify the contents of the view (test should be green)
