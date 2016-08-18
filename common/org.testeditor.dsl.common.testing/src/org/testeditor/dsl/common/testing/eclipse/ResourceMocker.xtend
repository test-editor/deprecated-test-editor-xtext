package org.testeditor.dsl.common.testing.eclipse

import org.eclipse.core.resources.IFile

import static extension org.mockito.Mockito.*

/**
 * Utility class for mocking classes of {@code org.eclipse.core.resources}.
 */
class ResourceMocker {

	def IFile mockFile(String fullName) {
		val split = fullName.split("\\.")
		return IFile.mock => [
			when(name).thenReturn(split.get(0))
			if (split.size > 1) {
				when(fileExtension).thenReturn(split.get(1))
			}
		]
	}

}
