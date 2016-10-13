/*******************************************************************************
 * Copyright (c) 2012 - 2016 Signal Iduna Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Signal Iduna Corporation - initial API and implementation
 * akquinet AG
 * itemis AG
 *******************************************************************************/
package org.testeditor.fixture.eclipse;

import java.util.concurrent.Callable;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.text.IDocument;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IPathEditorInput;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.ITextEditor;
import org.testeditor.fixture.core.interaction.FixtureMethod;

public class EclipseWorkbenchFixture {

	private final static int JOB_WAIT_THRESHOLD=100; // in 10^-1 seconds (100 = 10 sec ...)

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
		IPath path = getAbsolutePath(filepath);
		Stream<IEditorReference> editorReferences = getEditorReferences();
		IEditorReference editorReference = editorReferences.filter(editorRef -> hasFileAsInput(editorRef, path))
				.findFirst().orElseThrow(() -> {
					logger.error("No open editor with filepath='{}' was found.", filepath);
					return getNoEditorFoundWithInputError(filepath);
				});
		return editorReference.getEditor(false);
	}

	@FixtureMethod
	public String getEditorContents(IEditorPart editorPart) {
		logger.info("Getting contents for editor with input='{}'.", editorPart.getEditorInput());
		if (editorPart instanceof ITextEditor) {
			IDocumentProvider documentProvider = ((ITextEditor) editorPart).getDocumentProvider();
			IDocument document = documentProvider.getDocument(editorPart.getEditorInput());
			return document.get();
		}
		throw new IllegalArgumentException("The passed editor was not a text editor!");
	}

	@FixtureMethod
	public IEditorPart openEditor(String filepath) {
		logger.info("Opening editor for filepath='{}'.", filepath);
		IFile file = getFile(filepath);
		IWorkbenchPage page = getActivePageOrFirst();
		return syncExec(() -> IDE.openEditor(page, file));
	}

	@FixtureMethod
	public void closeEditor(IEditorPart editorPart) {
		logger.info("Closing editor with input='{}'.", editorPart.getEditorInput());
		IWorkbenchWindow window = editorPart.getSite().getWorkbenchWindow();
		syncExec(() -> window.getActivePage().closeEditor(editorPart, false));
	}
	
	/**
	 * Wait until their is no running job in the background of the RCP/IDE.
	 */
	@FixtureMethod
	public void waitUntilJobsCompleted() {
		try {
			int counter = 0;
			while (!Job.getJobManager().isIdle() || counter < JOB_WAIT_THRESHOLD) {
				Thread.sleep(100);
				counter++;
			}
			if (counter >= JOB_WAIT_THRESHOLD) {
				logger.info("Abort waiting for eclipse job execution after {} seconds.",JOB_WAIT_THRESHOLD/10);
			}
		} catch (InterruptedException e) {
			logger.error("Wait operation is interrupted.", e);
		}
	}

	private IWorkbench getWorkbench() {
		return PlatformUI.getWorkbench();
	}

	private IWorkspaceRoot getWorkspaceRoot() {
		return ResourcesPlugin.getWorkspace().getRoot();
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

	private IPath getAbsolutePath(String filepath) {
		IFile file = getFile(filepath);
		return file.getLocation();
	}

	private IFile getFile(String filepath) {
		IResource resource = getWorkspaceRoot().findMember(filepath);
		if (resource == null) {
			String message = String.format("Resource with path='%s' does not exist in workspace.", filepath);
			throw new IllegalArgumentException(message);
		}
		if (resource instanceof IFile) {
			return (IFile) resource;
		} else {
			String message = String.format("Resource with path='%s' exists but is not a file.", filepath);
			throw new IllegalArgumentException(message);
		}
	}

	/**
	 * @return a stream of {@link IEditorReference} containing the references
	 *         for all workbench windows.
	 */
	private Stream<IEditorReference> getEditorReferences() {
		IWorkbenchWindow[] windows = getWorkbench().getWorkbenchWindows();
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
		String message = String.format("Could not find editor with filepath='%s', but found editors with inputs: %s",
				filepath, editorInputs);
		return new IllegalArgumentException(message);
	}

	/**
	 * @return the active {@link IWorkbenchPage} or if there is no active page,
	 *         the active page of the first workbench window.
	 */
	private IWorkbenchPage getActivePageOrFirst() {
		IWorkbench workbench = getWorkbench();
		IWorkbenchWindow activeWindow = workbench.getActiveWorkbenchWindow();
		if (activeWindow != null) {
			return activeWindow.getActivePage();
		} else {
			IWorkbenchWindow firstWindow = workbench.getWorkbenchWindows()[0];
			return firstWindow.getActivePage();
		}
	}

	private <T> T syncExec(Callable<T> callable) {
		AtomicReference<T> result = new AtomicReference<>();
		Display.getDefault().syncExec(() -> {
			try {
				result.set(callable.call());
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		});
		return result.get();
	}

}
