/** 
 * Copyright (c) 2012 - 2016 Signal Iduna Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * Contributors:
 * Signal Iduna Corporation - initial API and implementation
 * akquinet AG
 */
package org.testeditor.dsl.common.ide.util

import java.io.File
import java.io.IOException
import java.nio.file.FileVisitResult
import java.nio.file.FileVisitor
import java.nio.file.Files
import java.nio.file.LinkOption
import java.nio.file.Path
import java.nio.file.SimpleFileVisitor
import java.nio.file.StandardCopyOption
import java.nio.file.attribute.BasicFileAttributes
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import java.util.zip.ZipException
import java.util.zip.ZipFile
import java.io.InputStream
import java.io.OutputStream
import com.google.common.io.ByteStreams
import java.util.Set

/** 
 * Util class for file operations.
 */
final class FileUtils {
	/** 
	 */
	private new() { // do nothing
	}

	static Logger logger = LoggerFactory.getLogger(FileUtils)

	/** 
	 * copies the directories.
	 * @param srcsource-directory
	 * @param destdestination-directory
	 * @throws IOExceptionIOException
	 */
	def static void copyFolder(File src, File dest) throws IOException {
		if (src.isDirectory()) {
			// if directory not exists, create it
			if (!dest.exists() && dest.mkdirs()) {
				logger.info("Directory copied from {}  to {} ", src, dest)
			}
			// list all the directory contents
			var String[] files = src.list()
			for (String file : files) {
				// construct the src and dest file structure
				var File srcFile = new File(src, file)
				var File destFile = new File(dest, file)
				// recursive copy
				copyFolder(srcFile, destFile)
			}
		} else {
			Files.copy(src.toPath(), dest.toPath(), StandardCopyOption.REPLACE_EXISTING)
			logger.debug("File copied from {}  to {} ", src, dest)
		}
	}

	/** 
	 * @return FileVisitor to delete a Directory with all content recursive.
	 */
	def static FileVisitor<Path> getDeleteRecursiveVisitor() {
		return new SimpleFileVisitor<Path>() {
			override FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
				Files.delete(file)
				return FileVisitResult.CONTINUE
			}

			override FileVisitResult postVisitDirectory(Path dir, IOException exc) throws IOException {
				if (Files.exists(dir, LinkOption.NOFOLLOW_LINKS)) {
					Files.delete(dir)
				}
				return FileVisitResult.CONTINUE
			}
		}
	}
	
	/**
	 * copy entries starting with filteredFilePrefix from the given zip/jar into the targetDirectory.
	 * since each zip/jar entries has the complete path as prefix, this can be used to copy complete subtrees.
	 * empty directories will NOT be created. target files will have this prefix removed before copied into the targetDirectory.
	 * 
	 * @param archive zip/jar-file to copy from
	 * @param targetDirectory regular file system to copy to
	 * @param filteredFilePrefix path-string that an entry must start with in order to be copied
	 * @return Set<String> of created files as file-path-strings
	 */
	def static Set<String> unpackZipFile(File archive, File targetDirectory, String filteredFilePrefix) throws ZipException, IOException {
		val result=newHashSet
		val zipFile = new ZipFile(archive)
		val entries = zipFile.entries
		val targetDirString=targetDirectory.absolutePath
		while (entries.hasMoreElements) {
			val zipEntry = entries.nextElement
			if (!zipEntry.isDirectory && zipEntry.name.startsWith(filteredFilePrefix)) {
				val targetName = zipEntry.name.replace(filteredFilePrefix, "")
				val targetFile = new File(targetDirectory, targetName)
				result.add('''«targetDirString»/«targetName»''')
				com.google.common.io.Files.createParentDirs(targetFile)
				var InputStream inputStream
				var OutputStream outputStream
				try {
					inputStream = zipFile.getInputStream(zipEntry)
					outputStream = com.google.common.io.Files.newOutputStreamSupplier(targetFile).output
					ByteStreams.copy(inputStream, outputStream)
				} finally {
					inputStream?.close
					outputStream?.close
				}
			}
		}
		return result
	}

	
}
