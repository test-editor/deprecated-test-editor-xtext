package org.testeditor.aml.dsl.ui.contentassist

import org.testeditor.aml.dsl.ui.contentassist.AbstractAmlProposalProvider
import org.eclipse.emf.ecore.EObject
import org.eclipse.xtext.Assignment
import org.eclipse.xtext.ui.editor.contentassist.ContentAssistContext
import org.eclipse.xtext.ui.editor.contentassist.ICompletionProposalAcceptor

class AmlProposalProvider extends AbstractAmlProposalProvider {
	
	override completeAmlModel_Package(EObject model, Assignment assignment, ContentAssistContext context, ICompletionProposalAcceptor acceptor) {
		super.completeAmlModel_Package(model, assignment, context, acceptor)
		acceptor.accept(createCompletionProposal('''com.example''', context))
	}
	
	override completeAmlModel_FileNameAllActionGroups(EObject model, Assignment assignment, ContentAssistContext context, ICompletionProposalAcceptor acceptor) {
		super.completeAmlModel_FileNameAllActionGroups(model, assignment, context, acceptor)
		acceptor.accept(createCompletionProposal('''"AllActionGroups.xml"''', context))
	}
	
	
	override completeAmlModel_FileNameTechnicalBindings(EObject model, Assignment assignment, ContentAssistContext context, ICompletionProposalAcceptor acceptor) {
		super.completeAmlModel_FileNameTechnicalBindings(model, assignment, context, acceptor)
		acceptor.accept(createCompletionProposal('''"TechnicalBindingTypeCollection.xml"''', context))
	}
	
}
