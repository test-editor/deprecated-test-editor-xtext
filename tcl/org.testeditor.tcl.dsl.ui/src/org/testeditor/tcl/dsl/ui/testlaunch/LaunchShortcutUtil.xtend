package org.testeditor.tcl.dsl.ui.testlaunch

import javax.inject.Inject
import org.eclipse.core.resources.IFile
import org.eclipse.core.resources.IMarker
import org.eclipse.core.resources.IResource
import org.eclipse.core.resources.IStorage
import org.eclipse.core.runtime.CoreException
import org.eclipse.emf.common.util.URI
import org.eclipse.jdt.core.IJavaElement
import org.eclipse.jdt.core.IPackageFragment
import org.eclipse.jdt.core.JavaCore
import org.eclipse.xtext.naming.QualifiedName
import org.eclipse.xtext.resource.FileExtensionProvider
import org.eclipse.xtext.resource.IResourceDescriptions
import org.eclipse.xtext.ui.generator.IDerivedResourceMarkers
import org.eclipse.xtext.ui.resource.IStorage2UriMapper
import org.slf4j.LoggerFactory

import static org.eclipse.core.resources.IMarker.PROBLEM
import static org.eclipse.core.resources.IMarker.SEVERITY
import static org.eclipse.core.resources.IMarker.SEVERITY_ERROR
import static org.eclipse.core.resources.IResource.DEPTH_ONE

class LaunchShortcutUtil {

	static val logger = LoggerFactory.getLogger(LaunchShortcutUtil)

	@Inject IDerivedResourceMarkers derivedResourceMarkers;
	@Inject FileExtensionProvider fileExtensionProvider;
	@Inject IStorage2UriMapper storageToUriMapper
	@Inject IResourceDescriptions resourceDescriptions

	/** is a valid object to execute a test on */
	def boolean isValidForTestrun(Object res) {
		if (res instanceof IResource) {
			return fileExtensionProvider.isValid(res.fileExtension) && res.hasNoMarkersPreventingTestExecution
		} else {
			return false
		}
	}

	/**
	 * check whether this resource has NO markers that (should) prevent test execution
	 */
	def boolean hasNoMarkersPreventingTestExecution(IResource res) {
		return !res.findMarkers(PROBLEM, true, DEPTH_ONE).exists[markerIndicatesNonExecutableTest]
	}

	/** get the qualified name of the test residing in the tcl resource. if none is found return null */
	def QualifiedName getQualifiedNameForTestInTcl(IResource resource) {
		val uri = storageToUriMapper.getUri(resource.getAdapter(IStorage))
		val resNameWOExtension = resource.name.replace(resource.fileExtension, '').replaceAll('\\.$', '')
		val resourceDescription = resourceDescriptions.getResourceDescription(uri)
		
		
		if (resourceDescription !== null) {
			val qualifiedName = resourceDescription.exportedObjects.map[name].findLast[
				lastSegment == resNameWOExtension
			]

			return qualifiedName
		} else {
			return null
		}
	}

	/**
	 * should this marker prevent test execution?
	 */
	def boolean markerIndicatesNonExecutableTest(IMarker marker) {
		val severity = marker.getAttribute(SEVERITY)
		return severity == SEVERITY_ERROR // could be set in options
	}

	/** get the derived java element of the given resource */
	def IJavaElement getJavaElementForResource(IResource resource) {
		try {
			val sourcePath = URI.createPlatformResourceURI(resource.fullPath.toString, true).toString
			val resources = derivedResourceMarkers.findDerivedResources(resource.project, sourcePath)
			return JavaCore.create(resources.findFirst[containsElementsSearchedFor])
		} catch (CoreException e) {
			logger.error('''could not get derived java-file for resource "«resource.name»": «e.message»''', e)
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
