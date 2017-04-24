import org.testeditor.rcp4.*
import org.testeditor.fixture.swt.*

# WriteSimpleTestWithoutPackageTest

//given
config TestEditorConfig

* Given Webproject

	Component: ProjectExplorer
	- Execute menu item "New/Project..." in tree <ProjectTree>

	Component: NewProjectDialog
	- Select element "Test-Editor Project" in tree <ProjectType>
	- Click on <NextButton>
	- Type "MyFirstWebProject" into <ProjectName>
	- Click on <NextButton>
	- Select value "Maven" in combo box <BuildTool>
	- Select element "Web Fixture" in list <AvailableFixturesList>
	- Click on <AddFixtureButton>
	- Check <GenerateWithExamples>
	- Click on <FinishButton>
	- Wait for dialog "Progress Information" to popup and then close after at most "3" respectively "120" seconds  

* When creating a test ^case
//when
	Component: ProjectExplorer
	- Select element "MyFirstWebProject/src/test/java/MyFirstWebProject" in tree <ProjectTree>
	- Execute menu item "New/Test Case" in tree <ProjectTree>

	Component: NewTestCaseDialog
	- Type "MyTestcase.tcl" into <TestCaseName>
	- Click on <FinishButton>
	- Wait for dialog "Progress Information" to popup and then close after at most "3" respectively "120" seconds

* Then expect generated java file contains packagename based ondirectory structure of testcase
//then
	Component: ActiveEditor
	- isTclPackage = Contains editor "package MyFirstWebProject"
	- assert !isTclPackage
	 
	Component: TestEditorServices
	- isJavaPackage = Contains file "/MyFirstWebProject/src-gen/test/java/MyFirstWebProject/MyTestcase.java" this "package MyFirstWebProject"
	- assert isJavaPackage
