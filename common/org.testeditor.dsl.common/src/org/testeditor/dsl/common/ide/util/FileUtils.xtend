/** 
 * Copyright (c) 2012 - 2018 Signal Iduna Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * Contributors:
 * Signal Iduna Corporation - initial API and implementation
 * akquinet AG
 */
package org.testeditor.dsl.common.ide.util

import com.google.common.io.ByteStreams
import java.io.BufferedReader
import java.io.File
import java.io.FileInputStream
import java.io.IOException
import java.io.InputStream
import java.io.InputStreamReader
import java.io.OutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.FileVisitResult
import java.nio.file.FileVisitor
import java.nio.file.Files
import java.nio.file.LinkOption
import java.nio.file.Path
import java.nio.file.SimpleFileVisitor
import java.nio.file.StandardCopyOption
import java.nio.file.attribute.BasicFileAttributes
import java.util.Set
import java.util.zip.ZipException
import java.util.zip.ZipFile
import org.slf4j.LoggerFactory

/** 
 * Util class for file operations.
 */
final class FileUtils {

	static val logger = LoggerFactory.getLogger(FileUtils)

	private new() {
	}

	/**
	 * read all lines of a file with utf-8 encoding where possible, replace characters not understood.
	 * 
	 * this method does not fail, even if some character encodings are not understood by UTF-8 encoding.
	 * java.nio.Files.readAllLines does fail if  an encoding does not match, which might not be wanted:
	 *   java.nio.charset.MalformedInputException: Input length = 1
	 * see here:  https://stackoverflow.com/questions/26268132/all-inclusive-charset-to-avoid-java-nio-charset-malformedinputexception-input
	 *
	 */
	def static Iterable<String> readAllLines(File file) {
		val reader = new BufferedReader(new InputStreamReader(new FileInputStream(file),StandardCharsets.UTF_8))
		try {
			val result = newArrayList
			for (;;) {
				val lineRead = reader.readLine
				if (lineRead === null) {
					return result
				}
				result.add(lineRead)
			}
		} finally {
			reader.close
		}		
	}

	/** 
	 * copies the directories.
	 * @param srcsource-directory
	 * @param destdestination-directory
	 * @throws IOExceptionIOException
	 */
	def static void copyFolder(File src, File dest) throws IOException {
		if (src.directory) {
			// if directory not exists, create it
			if (!dest.exists && !dest.mkdirs) {
				logger.error("target directory dest='{}' could not be created, copying of src='{}' omitted.",dest, src)
				return
			}
			// list all the directory contents
			src.list.forEach [
				// construct the src and dest file structure
				var File srcFile = new File(src, it)
				var File destFile = new File(dest, it)
				// recursive copy
				copyFolder(srcFile, destFile)
			]
			logger.info("Directory copied from src='{}' to dest='{}'.", src, dest)			
		} else if(src.file) {
			Files.copy(src.toPath, dest.toPath, StandardCopyOption.REPLACE_EXISTING)
			logger.debug("File copied from {}  to {} ", src, dest)
		} else {
			logger.warn("ignored src='{}' during copy, since it is neither file nor directory.", src)
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
	def static Set<String> unpackZipFile(File archive, File targetDirectory,
		String filteredFilePrefix) throws ZipException, IOException {
		val result = newHashSet
		val zipFile = new ZipFile(archive)
		val entries = zipFile.entries
		val targetDirString = targetDirectory.absolutePath
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
					outputStream = com.google.common.io.Files.asByteSink(targetFile).openBufferedStream
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
