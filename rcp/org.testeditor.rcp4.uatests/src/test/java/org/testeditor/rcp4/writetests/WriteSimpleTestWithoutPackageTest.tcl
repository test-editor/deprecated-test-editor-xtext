import org.testeditor.rcp4.*

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
	- Wait at most "30" seconds until dialog with title "Progress Information" is closed

	Component: ProjectExplorer
	- Select element "MyFirstWebProject/Tests/MyFirstWebProject" in tree <ProjectTree>
	- Execute menu item "New/Test Case" in tree <ProjectTree>

	Component: NewTestCaseDialog
	- Type "MyTestcase.tcl" into <TestCaseName>
	- Click on <FinishButton>

//when
	Component: ActiveEditor
	- Remove line "1" 
	- Save content
	
	Component: MainWindow
	- Wait until all jobs finished

//then
	Component: TestEditorServices
	- isJavaPackage = Contains file "/MyFirstWebProject/src-gen/MyFirstWebProject/MyTestcase.java" this "package MyFirstWebProject"
	- assert isJavaPackage = "true"
	
	Component: ActiveEditor
	- isTclPackage = Contains active editor "package MyFirstWebProject"
	- assert isTclPackage = "false" 