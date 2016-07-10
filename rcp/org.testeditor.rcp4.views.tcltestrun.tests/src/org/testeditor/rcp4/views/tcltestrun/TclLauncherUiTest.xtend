package org.testeditor.rcp4.views.tcltestrun

import com.google.inject.Injector
import org.eclipse.core.resources.IResource
import org.eclipse.jface.viewers.IStructuredSelection
import org.eclipse.xtext.naming.QualifiedName
import org.junit.Before
import org.junit.Test
import org.junit.runner.RunWith
import org.mockito.Mock
import org.mockito.runners.MockitoJUnitRunner
import org.testeditor.rcp4.tcltestrun.TclInjectorProvider
import org.testeditor.tcl.dsl.ui.testlaunch.LaunchShortcutUtil

import static org.junit.Assert.*
import static org.mockito.Matchers.*
import static org.mockito.Mockito.*

@RunWith(MockitoJUnitRunner)
class TclLauncherUiTest {

	@Mock
	TclInjectorProvider tclInjectorProvider

	@Mock
	IStructuredSelection selection

	@Mock
	LaunchShortcutUtil launchUtil

	TclLauncherUi launcherUi

	@Before
	def void setup() {
		// given
		val injector = mock(Injector)
		when(tclInjectorProvider.get).thenReturn(injector)
		when(injector.getInstance(LaunchShortcutUtil)).thenReturn(launchUtil)
		launcherUi = new TclLauncherUi(tclInjectorProvider)
	}

	@Test
	def void testGetTestCaseListOnOneSelection() {
		// given
		val resource = mock(IResource)
		when(selection.firstElement).thenReturn(resource)
		when(resource.toString).thenReturn("myprj/mypackage/myTest.tcl");
		when(launchUtil.getQualifiedNameForTestInTcl(any)).thenReturn(QualifiedName.create("mypackage", "myTest"))
		// when
		val testCases = launcherUi.createTestCasesList(selection)
		// then
		assertEquals(1, testCases.size)
		assertTrue(testCases.exists[it == "mypackage.myTest"])
	}

	@Test
	def void testGetTestCaseListOnManySelection() {
		// given
		val resource1 = mock(IResource)
		val resource2 = mock(IResource)
		when(resource1.toString).thenReturn("myprj/mypackage/myTest.tcl");
		when(launchUtil.getQualifiedNameForTestInTcl(resource1)).thenReturn(QualifiedName.create("mypackage", "myTest"))
		when(resource2.toString).thenReturn("myprj/mypackage/mySecondTest.tcl");
		when(launchUtil.getQualifiedNameForTestInTcl(resource2)).thenReturn(
			QualifiedName.create("mypackage", "mySecondTest"))
		when(selection.size).thenReturn(2)
		when(selection.toList).thenReturn(#[resource1, resource2])
		// when
		val testCases = launcherUi.createTestCasesList(selection)
		// then
		assertEquals(2, testCases.size)
		assertTrue(testCases.exists[it == "mypackage.myTest"])
		assertTrue(testCases.exists[it == "mypackage.mySecondTest"])
	}

}
