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

	Component: ProjectExplorer
	- Select element "MyFirstWebProject/Tests/MyFirstWebProject" in tree <ProjectTree>
	- Execute menu item "New/Test Case" in tree <ProjectTree>

	Component: NewTestCaseDialog
	- Type "MyTestcase.tcl" into <TestCaseName>
	- Click on <FinishButton>

* When remove packagename from file and save
//when
	Component: ActiveEditor
	- Remove line "1" from editor
	- Save editor content

	Component: MainWindow
	- Wait until all jobs finished

* Then expect generated java file contains packagename based ondirectory structure of testcase 
//then
	Component: ActiveEditor
	- isTclPackage = Contains editor "package MyFirstWebProject"
	- assert !isTclPackage
	 
	Component: TestEditorServices
	- isJavaPackage = Contains file "/MyFirstWebProject/src-gen/test/java/MyFirstWebProject/MyTestcase.java" this "package MyFirstWebProject"
	- assert isJavaPackage
