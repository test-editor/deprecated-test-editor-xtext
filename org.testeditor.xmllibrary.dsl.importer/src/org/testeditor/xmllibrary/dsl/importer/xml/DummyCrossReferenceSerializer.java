package org.testeditor.xmllibrary.dsl.importer.xml;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.CrossReference;
import org.eclipse.xtext.nodemodel.INode;
import org.eclipse.xtext.serializer.diagnostic.ISerializationDiagnostic;
import org.eclipse.xtext.serializer.tokens.CrossReferenceSerializer;
import org.testeditor.xmllibrary.model.ActionPart;
import org.testeditor.xmllibrary.model.TechnicalBindingType;

import com.google.inject.Binder;
import com.google.inject.Module;

/**
 * This is just a hack for the prototype.
 * 
 * Overrides the Xtext's {@link CrossReferenceSerializer} to simplify serialization
 * of {@link TechnicalBindingType} and {@link ActionPart} without having them in scope.
 */
@SuppressWarnings("all")
public class DummyCrossReferenceSerializer extends CrossReferenceSerializer {

	@Override
	public String serializeCrossRef(final EObject semanticObject, final CrossReference crossref, final EObject target, final INode node, final ISerializationDiagnostic.Acceptor errors) {
		// Simply return the ID for TechnicalBindingType and ActionPart
		if (target instanceof TechnicalBindingType) {
			return ((TechnicalBindingType) target).getId();
		}
		if (target instanceof ActionPart) {
			return ((ActionPart) target).getId();
		}
		return super.serializeCrossRef(semanticObject, crossref, target, node, errors);
	}

	public final static Module module = new Module() {
		
		@Override
		public void configure(final Binder binder) {
			binder.bind(CrossReferenceSerializer.class).to(DummyCrossReferenceSerializer.class);
		}
		
	};

}