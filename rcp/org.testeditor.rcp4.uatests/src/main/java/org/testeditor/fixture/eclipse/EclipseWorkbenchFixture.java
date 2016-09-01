package org.testeditor.fixture.eclipse;

import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IPathEditorInput;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.texteditor.ITextEditor;
import org.testeditor.fixture.core.interaction.FixtureMethod;

public class EclipseWorkbenchFixture {

	private Logger logger = LogManager.getLogger(EclipseWorkbenchFixture.class);

	/**
	 * Finds an open editor in the workbench that has the given file as an
	 * input.
	 * 
	 * @param filepath
	 *            the path relative to the workspace root
	 * @return the {@link IEditorPart} of the found editor
	 * @throws IllegalArgumentException
	 *             if there is no open editor with the given file
	 */
	@FixtureMethod
	public IEditorPart getEditorWithFilePath(String filepath) {
		logger.info("Searching for open editor with filepath='{}'", filepath);
		IPath path = getAbsolutelPath(filepath);
		Stream<IEditorReference> editorReferences = getEditorReferences();
		IEditorReference editorReference = editorReferences.filter(editorRef -> hasFileAsInput(editorRef, path)).findFirst().orElseThrow(() -> {
			logger.error("No open editor with filepath='{}' was found.", filepath);
			return getNoEditorFoundWithInputError(filepath);
		});
		return editorReference.getEditor(false);
	}

	@FixtureMethod
	public String getEditorContents(IEditorPart editorPart) {
		if (editorPart instanceof ITextEditor) {
			IDocument document = ((ITextEditor) editorPart).getDocumentProvider().getDocument(editorPart.getEditorInput());
			return document.get();
		}
		throw new IllegalArgumentException("The passed editor was not a text editor!");
	}

	private boolean hasFileAsInput(IEditorReference editorRef, IPath path) {
		try {
			IEditorInput editorInput = editorRef.getEditorInput();
			if (editorInput instanceof IPathEditorInput) {
				IPath editorPath = ((IPathEditorInput) editorInput).getPath();
				return editorPath.equals(path);
			}
		} catch (PartInitException e) {
		}
		return false;
	}

	private IPath getAbsolutelPath(String filepath) {
		IResource resource = ResourcesPlugin.getWorkspace().getRoot().findMember(filepath, false);
		if (resource == null) {
			String message = String.format("Resource with path='%s' does not exist in workspace.", filepath);
			throw new IllegalArgumentException(message);
		}
		return resource.getLocation();
	}

	/**
	 * @return a stream of {@link IEditorReference} containing the references
	 *         for all workbench windows.
	 */
	private Stream<IEditorReference> getEditorReferences() {
		IWorkbenchWindow[] windows = PlatformUI.getWorkbench().getWorkbenchWindows();
		return Stream.of(windows).flatMap(window -> Stream.of(window.getActivePage().getEditorReferences()));
	}

	/**
	 * Creates an error message in case no editor with a desired input was
	 * found. In this case we want to return all editor inputs of the opened
	 * editors for debugging purposes.
	 */
	private IllegalArgumentException getNoEditorFoundWithInputError(String filepath) {
		String editorInputs = getEditorReferences().map(editorRef -> {
			try {
				return editorRef.getEditorInput().toString();
			} catch (PartInitException e) {
			}
			return null;
		}).collect(Collectors.joining(", "));
		String message = String.format("Could not find editor with filepath='%s', but found editors with inputs: %s", filepath, editorInputs);
		return new IllegalArgumentException(message);
	}

}
