package org.testeditor.rcp4.views

import java.io.File
import javax.annotation.PostConstruct
import javax.annotation.PreDestroy
import javax.inject.Inject
import org.eclipse.core.runtime.FileLocator
import org.eclipse.core.runtime.Path
import org.eclipse.e4.core.services.events.IEventBroker
import org.eclipse.e4.ui.di.Focus
import org.eclipse.e4.ui.model.application.ui.basic.MPart
import org.eclipse.e4.ui.workbench.modeling.EPartService
import org.eclipse.emf.edit.provider.ComposedAdapterFactory
import org.eclipse.emf.edit.ui.provider.AdapterFactoryContentProvider
import org.eclipse.emf.edit.ui.provider.AdapterFactoryLabelProvider
import org.eclipse.jface.resource.ImageDescriptor
import org.eclipse.jface.text.IDocument
import org.eclipse.jface.text.ITextOperationTarget
import org.eclipse.jface.text.ITextViewer
import org.eclipse.jface.viewers.TreeViewer
import org.eclipse.swt.SWT
import org.eclipse.swt.dnd.DND
import org.eclipse.swt.dnd.DragSourceEvent
import org.eclipse.swt.dnd.DragSourceListener
import org.eclipse.swt.dnd.TextTransfer
import org.eclipse.swt.widgets.Composite
import org.eclipse.ui.IEditorPart
import org.eclipse.ui.internal.e4.compatibility.CompatibilityEditor
import org.eclipse.ui.texteditor.IDocumentProvider
import org.eclipse.ui.texteditor.ITextEditor
import org.osgi.framework.FrameworkUtil

import static org.testeditor.rcp4.views.XtendSWTLib.*

class MaskStepSelector {

	@Inject
	var IEventBroker broker

	@Inject
	var EPartService partService

	@Inject
	XtextAmlInjectorProvider xtextInjectorProvider

	@Inject
	new() {
		System.out.println("created")
	}

	var TreeViewer viewer

	def void setDefaultProviders(TreeViewer treeViewer) {
		val composedAdapterFactory = new ComposedAdapterFactory(ComposedAdapterFactory.Descriptor.Registry.INSTANCE);

		val labelProvider = new AdapterFactoryLabelProvider(composedAdapterFactory);
		val contentProvider = new AdapterFactoryContentProvider(composedAdapterFactory);

		treeViewer.setLabelProvider(labelProvider);
		treeViewer.setContentProvider(contentProvider);
	}

	@PostConstruct
	def void postConstruct(Composite parent) {
		viewer = newTreeViewer(parent, SWT.V_SCROLL) [
			setDefaultProviders
			input = xtextInjectorProvider.injector.getInstance(MaskStepSelectorInput).packages.head // File.listRoots
			addDragSupport((DND.DROP_COPY.bitwiseOr(DND.DROP_MOVE)), #[TextTransfer.getInstance()],
				new DragSourceListener() {

					override dragFinished(DragSourceEvent event) {
						System.out.println("drag stop");
					}

					override dragSetData(DragSourceEvent event) {
						val selection = viewer.getStructuredSelection();
						val firstElement = selection.getFirstElement() as File;

						if (TextTransfer.getInstance().isSupportedType(event.dataType)) {
							event.data = firstElement.path
						}
					}

					override dragStart(DragSourceEvent event) {
						System.out.println("drag start");
					// addDropSupport
					}

				})
		]
	}

	def ImageDescriptor createImageDescriptor() {
		val bundle = FrameworkUtil.getBundle(MaskStepSelector);
		val url = FileLocator.find(bundle, new Path("icons/folder.png"), null);
		return ImageDescriptor.createFromURL(url);
	}

	@Focus
	def void setFocus() {
		viewer.getControl().setFocus();
	}

	def boolean dslEditorActive(MPart part) {
		(part != null && part.parent.children.exists [
			(it as MPart).object != null && tags.exists[equals("org.testeditor.tcl.dsl.Tcl")]
		])
	}

	def IEditorPart activeDSLEditor(MPart part) {
		if (dslEditorActive(part)) {
			val ce = (part.parent.children.filter[(it as MPart).object != null].head as MPart).object // make sure to grab the editor that is currently open
			val editor = (ce as CompatibilityEditor).editor
			return editor
		}
		return null
	}

	def void insertTextAtCaret(ITextEditor textEditor, String data) {
		val IDocumentProvider dp = textEditor.documentProvider
		val IDocument doc = dp.getDocument(textEditor.editorInput)
		val cpos = (textEditor.getAdapter(ITextOperationTarget) as ITextViewer).textWidget.caretOffset
		doc.replace(cpos, 0, data);
	}

	def MPart compatibilityEditor() {
		partService.findPart("org.eclipse.e4.ui.compatibility.editor")
	}

	def void passToTCLEditor(String data) {
		val MPart part = compatibilityEditor

		if (dslEditorActive(part)) {
			activeDSLEditor(part).getAdapter(ITextEditor).insertTextAtCaret(data)
			// TODO pass focus to editor (again)
			activeDSLEditor(part).setFocus
		}
	}

	@PreDestroy
	def void preDestroy() {
		System.out.println("destroyed")
	}

}
