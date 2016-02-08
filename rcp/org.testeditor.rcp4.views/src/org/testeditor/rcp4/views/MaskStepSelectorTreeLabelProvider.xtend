package org.testeditor.rcp4.views

import java.io.File
import org.eclipse.jface.resource.ImageDescriptor
import org.eclipse.jface.resource.JFaceResources
import org.eclipse.jface.resource.LocalResourceManager
import org.eclipse.jface.resource.ResourceManager
import org.eclipse.jface.viewers.DelegatingStyledCellLabelProvider.IStyledLabelProvider
import org.eclipse.jface.viewers.LabelProvider
import org.eclipse.jface.viewers.StyledString

class MaskStepSelectorTreeLabelProvider extends LabelProvider implements IStyledLabelProvider {
    
    var ImageDescriptor directoryImage;
    var ResourceManager resourceManager;

    new(ImageDescriptor directoryImage) {
      this.directoryImage = directoryImage;
    }

    override getStyledText(Object element) {
      if(element instanceof File) {
        val file = element as File
        val styledString = new StyledString(getFileName(file))
        val files = file.list
        if (files != null) {
          styledString.append(" (" + files.length + ") ",
              StyledString.COUNTER_STYLER);
        }
        return styledString;
      }
      return null;
    }
    
    override getImage(Object element) {
      if(element instanceof File) {
        if(element.isDirectory()) {
          return getResourceManager().createImage(directoryImage);
        }
      }
      
      return super.getImage(element);
    }
    
    override dispose() {
      // garbage collect system resources
      if(resourceManager != null) {
        resourceManager.dispose();
        resourceManager = null;
      }
    }
    
    def getResourceManager() {
      if(resourceManager == null) {
        resourceManager = new LocalResourceManager(JFaceResources.resources);
      }
      return resourceManager;
    }

    def getFileName(File file) {
      val name = file.getName();
      return (if (name.isEmpty())  file.path else name);
    }
				
  }
  