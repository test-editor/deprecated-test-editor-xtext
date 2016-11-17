#!/usr/bin/env groovy

/**
 * This script updates the copyright headers of all .xtend and .java files in
 * the repository. It also replaces Windows line endings with Unix line endings.
 */

header = """
/*******************************************************************************
 * Copyright (c) 2012 - 2016 Signal Iduna Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 * Signal Iduna Corporation - initial API and implementation
 * akquinet AG
 * itemis AG
 *******************************************************************************/
""".trim()

encoding = "UTF-8"

firstHeaderLine = $/^/\*.*/$
consecutiveHeaderLine = $/\r?\n\s*\*.*/$
lastHeaderLine = $/\r?\n\s*\**//$
headerPattern = ~"$firstHeaderLine($consecutiveHeaderLine)*$lastHeaderLine"

// Define a FileFilter to exclude certain directories and filter for .xtend and .java
def excluded = ["bin", "target", "xtend-gen", "src-gen"]
filter = (FileFilter) { file ->
    if (file.isDirectory()) {
        return !excluded.contains(file.name) && !file.isHidden()
    }
    return file.name.endsWith(".xtend") || file.name.endsWith(".java")
}

updateHeaders(new File("."))

def void updateHeaders(File file) {
    file.listFiles(filter).each { child ->
        if (child.isDirectory()) {
            updateHeaders(child)
        } else {
            doUpdate(child)
        }
    }
}

/**
 * Updates the copyright header of the passed file.
 */
def void doUpdate(File file) {
    def content = file.getText(encoding)
    def matcher = content =~ headerPattern
    if (matcher) {
        def match = matcher.group()
        if (match.contains("Signal Iduna Corporation and others")) {
            content = content.replace(match, header)
        } else {
            // Header found but was not the Signal Iduna header - don't replace it
            println "Not updating file header of $file"
        }
    } else {
        content = "$header\n$content"
    }
    file.setText(content.replaceAll("\r\n", "\n"), encoding)
}