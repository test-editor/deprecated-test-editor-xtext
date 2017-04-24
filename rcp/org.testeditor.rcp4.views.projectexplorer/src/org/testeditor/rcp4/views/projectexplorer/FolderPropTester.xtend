package org.testeditor.rcp4.views.projectexplorer

import javax.inject.Inject
import org.eclipse.core.expressions.PropertyTester
import org.eclipse.core.resources.IFolder
import org.testeditor.dsl.common.util.classpath.ClasspathUtil

/**
 * Check a folder for being a valid source folder.
 * 
 * This is being used for creating files (test cases, test specifciation, macros ...).
 * Some types allow no creation in the root folder, since the default package is not supported yet!
 */
class FolderPropTester extends PropertyTester {

	@Inject ClasspathUtil classpathUtil

	override test(Object receiver, String property, Object[] args, Object expectedValue) {
		val folder = receiver as IFolder
		val package = classpathUtil.inferPackage(folder.fullPath)
		switch (expectedValue) {
			case "nonroot": return !package.nullOrEmpty // source path is not the root itself (empty package)
			default: return package !== null // any valid source path (any valid package including the default)
		}
	}

}
