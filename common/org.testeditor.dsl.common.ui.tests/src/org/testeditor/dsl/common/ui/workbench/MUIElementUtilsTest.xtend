package org.testeditor.dsl.common.ui.workbench

import javax.inject.Inject
import org.junit.Test
import org.mockito.Mock
import org.eclipse.e4.ui.model.application.ui.MUIElement
import org.eclipse.e4.ui.model.application.ui.MElementContainer
import org.junit.Before

import static extension org.mockito.Mockito.*
import org.testeditor.dsl.common.testing.AbstractTest

class MUIElementUtilsTest extends AbstractTest {

	@Inject MUIElementUtils classUnderTest
	
	@Mock MUIElement muiElement1
	@Mock MUIElement muiElement2
	@Mock MUIElement muiElement3
	@Mock MUIElement muiElement4
	@Mock MElementContainer<MUIElement> container1
	@Mock MElementContainer<MUIElement> container2
	@Mock MElementContainer<MUIElement> container3
	
	@Before
	def void setupContainers(){
		when(container1.children).thenReturn(#[muiElement1,container2,muiElement2])
		when(container2.children).thenReturn(#[container3,muiElement3])
		when(container3.children).thenReturn(#[muiElement4])
	}

	@Test
	def void testFlattenTree() {
		// given
		// when
		val flattenedTree=classUnderTest.flattenTree(container1)
		
		// then
		flattenedTree.assertSize(7) => [
			get(0).assertSame(container1)
			get(1).assertSame(muiElement1)
			get(2).assertSame(container2)
			get(3).assertSame(container3)
			get(4).assertSame(muiElement4)
			get(5).assertSame(muiElement3)
			get(6).assertSame(muiElement2)
		]
	}

}
