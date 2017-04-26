import org.testeditor.rcp4.*
import org.testeditor.fixture.swt.*


# TestStepSelectorTest

// Given

config TestEditorConfig

* Create an empty ^default project

	Component: ProjectExplorer
	- Execute menu item "New/Project..." in tree <ProjectTree>

	Component: NewProjectDialog
	- Select element "Test-Editor Project" in tree <ProjectType>
	- Click on <NextButton>
	- Type "TestStepSelectorTestProject" into <ProjectName>
	- Click on <FinishButton>
	- Wait for dialog "Progress Information" to popup and then close after at most "3" respectively "120" seconds

	Mask: Workbench
	- Wait until all jobs finished 

* Create an aml within root that ^is defaul ^package

	Component: ProjectExplorer
	- Select element "TestStepSelectorTestProject/src/test/java" in tree <ProjectTree>
	- Execute menu item "New/Application Mapping" in tree <ProjectTree>

	Mask: NewApplicationMappingDialog
	- Type "Root.aml" into <ApplicationMappingName>
	- Click on <FinishButton>

	Mask: ActiveEditor
	- Type "component type TRootPage { } component RootPage is TRootPage { }" into active editor
	- Save editor content

* Create an aml within ^package "some.other"

	Macro: ProjectFolder
	- Add folder "some" at "TestStepSelectorTestProject/src/test/java"
	- Add folder "other" at "TestStepSelectorTestProject/src/test/java/some"

	Component: ProjectExplorer
	- Select element "TestStepSelectorTestProject/src/test/java/some/other" in tree <ProjectTree>
	- Execute menu item "New/Application Mapping" in tree <ProjectTree>

	Mask: NewApplicationMappingDialog
	- Type "SomeOther.aml" into <ApplicationMappingName>
	- Click on <FinishButton>

	Mask: ActiveEditor
	- Type "component type TSomeOtherPage { } component SomeOtherPage is TSomeOtherPage { }" into active editor
	- Save editor content

// When
* Create and open the editor ^for a test ^case within root

	Component: ProjectExplorer
	- Select element "TestStepSelectorTestProject/src/test/java" in tree <ProjectTree>
	- Execute menu item "New/Test Case" in tree <ProjectTree>

	Mask: NewTestCaseDialog
	- Type "RootTest" into <TestCaseName>
	- Click on <FinishButton>

	Mask: Workbench
	- Wait until all jobs finished 

// Then
* Check that the test step selector contains the expected tree of aml elements

	Mask: MainWindow
	- hasRootPage = Has element "*default*/RootPage" in tree <TestStepSelector>
	- hasSomeOtherPage = Has element "some.other/SomeOtherPage" in tree <TestStepSelector>
	- assert hasRootPage
	- assert hasSomeOtherPage
