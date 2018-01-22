package org.testeditor.tcl.dsl.ide

import org.junit.Test
import org.junit.Before
import org.testeditor.dsl.common.testing.DummyFixture

class TclContentProposalProviderTest extends AbstractContentAssistTest {
	

	@Before
	def void parseAmlModel() {
		parseAml(DummyFixture.amlModel)
	}
	
	@Test
	def void testTemplateProposal() {
		// given
		val tclSnippet = '''
		# SomeTest
		
		* Do something
		  Component: GreetingApplication
		  - $
		'''
		// when
		val proposals = getProposalsWithPriority(tclSnippet)
		
		// then
		proposals.assertSize(3) => [
			get(0).value => [
				proposal.assertEquals('')
			]
		]
			
	}
	
}