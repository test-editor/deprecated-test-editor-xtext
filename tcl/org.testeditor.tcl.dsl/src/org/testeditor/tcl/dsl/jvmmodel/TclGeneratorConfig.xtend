package org.testeditor.tcl.dsl.jvmmodel

import javax.inject.Singleton

/** 
 * central configuration class for generator configurations
 */
@Singleton
class TclGeneratorConfig {

	static val REPORTER_CALL_COMMENT_PREFIX_CHAR = 'TE_REPORTER_CALL_COMMENT_PREFIX_CHAR'
	static val REPORTER_CALL_COMMENT_PREFIX_COUNT = 'TE_REPORTER_CALL_COMMENT_PREFIX_COUNT'

	def String getReporterCallCommentPrefixChar() {
		return System.getenv(REPORTER_CALL_COMMENT_PREFIX_CHAR)
	}

	def int getReporterCallCommentPrefixCount() {
		try {
			return Integer.parseInt(System.getenv(REPORTER_CALL_COMMENT_PREFIX_COUNT))
		} catch (NumberFormatException nfe) {
			return 0 // ignore
		}
	}

}
