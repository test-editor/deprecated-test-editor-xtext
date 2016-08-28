import org.testeditor.rcp4.*

# WriteSimpleTestWithoutPackageTest

//given
config TestEditorConfig

* Given Webproject

	Component: ProjectExplorer
	- Execute menu item "New/Project..." in tree <ProjektBaum>

	Component: NewDialog
	- Select element "Test-Editor Project" in tree <ProjectType>
	- Click on <NextButton>
	- Type "MyFirstWebProject" into <ProjectName>
	- Click on <NextButton>
	- Select element "Web Fixture" in list <AvailableFixturesList>
	- Click on <AddFixtureButton>
	- Check <GenerateWithExamples>
	- Click on <FinishButton>
	- Wait until dialog with title "Progress Information" is closed


//TODO this is disabled until the gradle project creation on the jenkins is fixed.

//	Component: ProjectExplorer
//	- Select element "MyFirstWebProject/Tests/MyFirstWebProject" in tree <ProjektBaum>
//	- Execute menu item "New/Test Case" in tree <ProjektBaum>
//
//	Component: NewDialog
//	- Click on <FinishButton>
	
//TODO continue this test after support for text editor is merged	