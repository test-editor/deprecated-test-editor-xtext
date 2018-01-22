package org.testeditor.tcl.dsl.ide

import java.util.HashSet
import java.util.List
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import javax.inject.Inject
import javax.inject.Provider
import org.eclipse.xtext.ide.editor.contentassist.ContentAssistContext
import org.eclipse.xtext.ide.editor.contentassist.ContentAssistEntry
import org.eclipse.xtext.ide.editor.contentassist.IIdeContentProposalAcceptor
import org.eclipse.xtext.ide.editor.contentassist.antlr.ContentAssistContextFactory
import org.eclipse.xtext.resource.XtextResource
import org.eclipse.xtext.util.ITextRegion
import org.eclipse.xtext.util.TextRegion
import org.junit.AfterClass
import org.junit.Assert
import org.junit.BeforeClass
import org.testeditor.tcl.dsl.tests.parser.AbstractParserTest

/**
 * base class for content assist tests
 */
class AbstractContentAssistTest extends AbstractParserTest {

	static val CARET_CHAR = '$'

	static ExecutorService executorService

	@Inject Provider<ContentAssistContextFactory> contextFactoryProvider
	@Inject TclContentProposalProvider proposalProvider

	@BeforeClass
	static def void initExecutorService() {
		executorService = Executors.newCachedThreadPool
	}

	@AfterClass
	static def void disposeExecutorService() {
		executorService.shutdown()
	}

	/**
	 * Provides proposals for the given input.
	 * The cursor location shall be identified by the {@code CARET_CHAR}.
	 */
	def getProposalsWithPriority(String text) {
		val cursor = text.indexOf(CARET_CHAR)
		if (cursor === -1) {
			throw new IllegalArgumentException('''No cursor position set, specfiy it by marking it with caret-char='«CARET_CHAR»'.''')
		}
		val selection = new TextRegion(cursor, 0)

		// parse the text
		val textToParse = text.replaceAll('\\' + CARET_CHAR, '')
		val model = textToParse.parseTcl
		val resource = model.eResource as XtextResource

		// create context
		val contexts = resource.getContexts(selection, cursor)

		// prepare result list and acceptor
		val proposals = new HashSet<Pair<Integer, ContentAssistEntry>>
		val acceptor = new IIdeContentProposalAcceptor {

			override accept(ContentAssistEntry entry, int priority) {
				proposals.add(priority -> entry)
			}

			override canAcceptMoreProposals() {
				true
			}

		}

		// invoke proposalProvider and return results
		proposalProvider.createProposals(contexts, acceptor)
		return proposals
	}

	def List<ContentAssistEntry> getProposals(String text) {
		return text.proposalsWithPriority.map[value].toList
	}

	def ContentAssistEntry expect(List<ContentAssistEntry> proposals, String expectedProposal) {
		val entry = proposals.findFirst[proposal == expectedProposal]
		if (entry === null) {
			Assert.fail('''
				Expected proposal with text="«expectedProposal»", but got:
					«proposals.join(System.lineSeparator)»
			''')
		}
		return entry
	}

	private def ContentAssistContext[] getContexts(XtextResource resource, ITextRegion selection, int caretOffset) {
		val text = resource.parseResult.rootNode.text
		if (caretOffset > text.length) {
			return #[]
		}
		val contextFactory = contextFactoryProvider.get() => [pool = executorService]
		return contextFactory.create(text, selection, caretOffset, resource)
	}

}
