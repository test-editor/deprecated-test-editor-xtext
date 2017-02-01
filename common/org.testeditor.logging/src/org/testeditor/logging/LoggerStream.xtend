package org.testeditor.logging

import java.io.IOException
import java.io.OutputStream
import org.slf4j.Logger

/**
 * Implementation of an output stream logging every line to a slf4j logger
 */
class LoggerStream extends OutputStream {
	val Logger logger
	val OutputStream outputStream

	val logLineBuffer = new StringBuffer

	new(Logger logger, OutputStream outputStream) {
		super()
		this.logger = logger
		this.outputStream = outputStream
	}

	override void write(byte[] b) throws IOException {
		outputStream.write(b)
		val string = new String(b)
		string.addString
	}

	private def void addString(String string) {
		if (string.contains('\r')) {
			val lines = string.split("\r(\n)?")
			logLineBuffer.append(lines.get(0))
			val completeString = logLineBuffer.toString.trim
			if (!completeString.empty) {
				logger.debug(completeString)
			}
			logLineBuffer.setLength(0)
			if (lines.length > 1) {
				logLineBuffer.append(lines.get(1))
			}
		} else {
			logLineBuffer.append(string)
		}
	}

	override void write(byte[] b, int off, int len) throws IOException {
		outputStream.write(b, off, len)
		val string = new String(b, off, len)
		string.addString
	}

	override void write(int b) throws IOException {
		outputStream.write(b)
		val string = String.valueOf((b as char))
		string.addString
	}

}
