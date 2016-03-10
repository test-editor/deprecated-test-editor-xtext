package org.testeditor.tcl.dsl.ui.testlaunch

import javax.inject.Inject
import org.eclipse.core.resources.IFile
import org.eclipse.core.resources.IResource
import org.eclipse.core.runtime.CoreException
import org.eclipse.emf.common.util.URI
import org.eclipse.jdt.core.IJavaElement
import org.eclipse.jdt.core.IPackageFragment
import org.eclipse.jdt.core.JavaCore
import org.eclipse.xtext.resource.FileExtensionProvider
import org.eclipse.xtext.ui.generator.IDerivedResourceMarkers
import org.slf4j.LoggerFactory

class LaunchShortcutUtil {

	static val logger = LoggerFactory.getLogger(LaunchShortcutUtil)

	@Inject IDerivedResourceMarkers derivedResourceMarkers;
	@Inject FileExtensionProvider fileExtensionProvider;

	/** is a valid object for executing a test on */
	def boolean isValidForTestrun(Object res) {
		res instanceof IResource && fileExtensionProvider.isValid((res as IResource).fileExtension) &&
			getJavaElementForResource(res as IResource) != null
	}

	/** get the derived java element of the given resource */
	def IJavaElement getJavaElementForResource(IResource resource) {
		try {
			val sourcePath = URI.createPlatformResourceURI(resource.fullPath.toString, true).toString
			val resources = derivedResourceMarkers.findDerivedResources(resource.project, sourcePath)
			return JavaCore.create(resources.findFirst[containsElementsSearchedFor])
		} catch (CoreException e) {
			logger.error('''could not get derived java-file for resource "«resource.name»": ''' + e.message, e)
		}
		return null;
	}

	/** allow search in generated artifact to hold all relevant elements e.g. for test execution */
	protected def boolean containsElementsSearchedFor(IFile file) {
		true // since java artefacts generated from tcl files always contain unit tests, this is always true
	}

	/** translate package to package-name + '.' and java file to file-name w/o extension */
	private def String idPart(IJavaElement javaElement) {
		switch (javaElement) {
			IPackageFragment:
				return javaElement.elementName + "."
			Object:
				if (javaElement.elementName.endsWith(".java")) {
					return javaElement.elementName.replace(".java", "")
				} else {
					return ""
				}
		}
	}

	/** generate [ package {'.' package} '.' ] className */
	def String toElementId(IJavaElement javaElement) {
		(javaElement.parent?.toElementId ?: "") + javaElement.idPart
	}

}
