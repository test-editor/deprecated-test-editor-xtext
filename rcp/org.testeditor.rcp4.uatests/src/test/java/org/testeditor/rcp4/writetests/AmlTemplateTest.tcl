import org.testeditor.rcp4.*
import org.testeditor.fixture.swt.*

# AmlTemplateTest

config TestEditorConfig

* Given

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
	- Execute menu item "New/Application Mapping" in tree <ProjectTree>

	Component: NewTestCaseDialog
	- Type "App.aml" into <TestCaseName>
	- Click on <FinishButton>

* When

	Component: ActiveEditor
	- Go to line "1" in active editor
	- Use auto completion with "compon" and select "component with type - Create a new component with a type"

* Then
	Component: ActiveEditor
	- Contains editor "component name is org.testeditor.fixture.web.GeneralBrowser {"
