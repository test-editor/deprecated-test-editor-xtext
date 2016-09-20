package org.testeditor.tcl.dsl.jvmmodel

import org.eclipse.core.runtime.OperationCanceledException
import org.eclipse.xtext.builder.BuilderParticipant
import org.eclipse.xtext.generator.IFileSystemAccess
import org.eclipse.xtext.resource.IResourceDescription
import org.slf4j.LoggerFactory

/** In order to catch exceptions in the building process, this class is registered as IXtextBuilderParticipant
 *  in the TclUiModule to replace the existing BuilderParticipant.
 */
class TclBuilderParticipant extends BuilderParticipant{
	
	static val logger = LoggerFactory.getLogger(TclBuilderParticipant)
	static val String ERROR_MSG = 'General test compilation error. Please contact developer.'
	
	protected override boolean doGenerate(IResourceDescription.Delta delta, IBuildContext context,
		IFileSystemAccess access) {
		// copy if BuilderParticipant (except for exception message)
		if (delta.^new !== null) {
			try {
				handleChangedContents(delta, context, access)
			} catch (OperationCanceledException e) {
				throw e
			} catch (Exception e) {
				// { --- this is different from the copy such that the exception is transformed into an error marker that is more descriptive to the user
				addMarkerAndLogError(delta.uri, new RuntimeException(ERROR_MSG, e))
				// and the error is additionally logged
				logger.error(ERROR_MSG, e)
				// --- }
			}
			return true
		}
		return false
	}	
	
}