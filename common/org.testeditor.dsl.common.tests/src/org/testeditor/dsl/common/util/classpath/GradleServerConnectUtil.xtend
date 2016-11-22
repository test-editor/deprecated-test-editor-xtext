package org.testeditor.dsl.common.util.classpath

import java.io.IOException
import java.net.URL

class GradleServerConnectUtil {
	
	def boolean canConnet() {
		try {
			val url = new URL("https://services.gradle.org/")
			val openConnection = url.openConnection()
			openConnection.connect()
			return true
		} catch (IOException e) {
			return false
		}

	}
	
}