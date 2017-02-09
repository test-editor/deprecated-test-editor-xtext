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

	new(Logger logger) {
		super()
		this.logger = logger
		this.outputStream = null
	}

	new(Logger logger, OutputStream outputStream) {
		super()
		this.logger = logger
		this.outputStream = outputStream
	}

	override void write(byte[] b) throws IOException {
		outputStream?.write(b)
		val string = new String(b)
		string.addString
	}

	override void write(byte[] b, int off, int len) throws IOException {
		outputStream?.write(b, off, len)
		val string = new String(b, off, len)
		string.addString
	}

	override void write(int b) throws IOException {
		outputStream?.write(b)
		val string = String.valueOf((b as char))
		string.addString
	}

	private def void addString(String string) {
		if (string.contains('\n')) {
			val lines = string.split("(\r)?\n")
			if (lines.length == 0) {
				logBuffer
			}
			for (String line : lines) {
				logLineBuffer.append(line)
				logBuffer
			}
		} else {
			logLineBuffer.append(string)
		}
	}

	private def void logBuffer() {
		val completeString = logLineBuffer.toString.trim
		if (!completeString.empty) {
			logger.debug(completeString)
			logLineBuffer.setLength(0)
		}

	}

}
