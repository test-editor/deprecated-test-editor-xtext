import org.testeditor.rcp4.*
import org.testeditor.fixture.swt.*

# AmlTemplateTest

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
	- Wait at most "30" seconds until dialog with title "Progress Information" is closed

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
