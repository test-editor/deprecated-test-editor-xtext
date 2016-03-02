package org.testeditor.tcl.dsl.ui.testlaunch

import javax.inject.Inject
import org.apache.log4j.Logger
import org.eclipse.core.resources.IFile
import org.eclipse.core.resources.IResource
import org.eclipse.core.runtime.CoreException
import org.eclipse.emf.common.util.URI
import org.eclipse.jdt.core.IJavaElement
import org.eclipse.jdt.core.IPackageFragment
import org.eclipse.jdt.core.JavaCore
import org.eclipse.xtext.resource.FileExtensionProvider
import org.eclipse.xtext.ui.generator.IDerivedResourceMarkers

class LaunchShortcutUtil {

	val logger = Logger.getLogger(LaunchShortcutUtil)

	@Inject IDerivedResourceMarkers derivedResourceMarkers;
	@Inject FileExtensionProvider fileExtensionProvider;

	/** is a valid object for executing a test on */
	def boolean isValidForTestrun(Object res) {
		res instanceof IResource && fileExtensionProvider.isValid((res as IResource).fileExtension)
	}

	/** get the derived java element of the given resource */
	def IJavaElement getJavaElementForResource(IResource resource) {
		try {
			val sourcePath = URI.createPlatformResourceURI(resource.fullPath.toString, true).toString
			val resources = derivedResourceMarkers.findDerivedResources(resource.project, sourcePath)
			return JavaCore.create(resources.findFirst[containsElementsSearchedFor])
		} catch (CoreException e) {
			logger.debug(e.message, e);
		}
		return null;
	}

	/** allow search in generated artifact to hold all relevant elements e.g. for test execution */
	protected def boolean containsElementsSearchedFor(IFile file) {
		true
	}

	/** translate package to package-name + '.' and java file to file-name w/o extension */
	private def String idPart(IJavaElement je) {
		switch (je) {
			IPackageFragment:
				return je.elementName + "."
			Object:
				if (je.elementName.endsWith(".java")) {
					return je.elementName.replace(".java", "")
				} else {
					return ""
				}
		}
	}

	/** generate [ package {'.' package} '.' ] className */
	def String elementId(IJavaElement je) {
		(je.parent?.elementId ?: "") + je.idPart
	}

}
